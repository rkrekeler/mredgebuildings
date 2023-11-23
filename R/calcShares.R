#' Historic carrier-Enduse Shares w.r.t Final Energy
#'
#' Merges and transforms the calculated shares from the Datasets:
#' Odyssee
#' IEA_ETP
#'
#' @returns data.frame with historic energy demands
#'
#' @author Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom dplyr mutate as_tibble filter select rename group_by across
#' all_of ungroup %>% .data
#' @importFrom tidyr replace_na


calcShares <- function() {
  #---Read-In Data
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)


  #---Process EU Shares and EC Shares

  # Mean Odyssee EC-EU Shares are used as optimization estimates for share disaggregation.
  sharesOdyssee <-
    calcOutput("ShareOdyssee", subtype = "enduse_carrier", aggregate = FALSE) %>%
    as.quitte()

  sharesETPeu <- calcOutput("ShareETP", subtype = "enduse", aggregate = FALSE) %>%
    as.quitte()
  sharesETPec <- calcOutput("ShareETP", subtype = "carrier", aggregate = FALSE) %>%
    as.quitte()


  #---Corrections for incorrect enduse-carrier combinations
  # Incorrect EU-EC combinations will be excluded from the dataset
  # for EU-EC disaggregation and will be added later on as zeros.

  exclude <- c("appliances-biomod",
               "appliances-biotrad",
               "appliances-coal",
               "appliances-heat",
               "lighting-biomod",
               "lighting-biotrad",
               "lighting-coal",
               "lighting-heat",
               "cooking-heat",
               "space_cooling-biomod",
               "space_cooling-biotrad",
               "space_cooling-coal",
               "space_cooling-natgas",
               "space_cooling-petrol")


  #---Calculate Combined EU-EC Shares for ETP Data
  sharesETP <- sharesOdyssee %>%
    unite(col = "EUEC", .data[["enduse"]], .data[["carrier"]], sep = "-", remove = FALSE) %>%
    anti_join(data.frame("EUEC" = exclude), by = "EUEC") %>%
    select(-"EUEC") %>%
    toolDisaggregate(sharesETPeu, sharesETPec)


  #---Merge Data
  data <- sharesOdyssee %>%
    select(-"unit", -"variable", -"model", -"scenario") %>%
    left_join(sharesETP, by = c("region", "period", "enduse", "carrier")) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]), .data[["value.y"]], .data[["value.x"]])) %>%
    select(-"value.x", -"value.y")



  #---Fill missing entries and consider excluded carrier-enduse combinations

  # Since IEA does not provide data for any year except 2014, historic data
  # will be filled up with these.
  # This assumes time-invariant shares for all non-Odyssee countries.
  # Alternatively, one could extract a trend from the time-average for the
  # behavior of the shares and extrapolate the missing data of ETP with these.

  data <- data %>%
    left_join(sharesETP %>%
                select(-"model", -"scenario", -"variable", -"unit", -"period"),
              by = c("region", "carrier", "enduse")) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]])) %>%
    select(-"value.x", -"value.y", -"shareEC.x", -"shareEU.x", -"shareEC.y",
           -"shareEU.y", -"model", -"scenario", -"unit", -"variable")


  #---Weights: Regional Shares of FE
  # Weights consist of the share of each region relative to global demand with
  # respect to enduse application. FE data is taken from ieaIO.

  regShare <- ieaIO %>%
    mutate(carrier = .data[["variable"]]) %>%
    select(-"model", -"scenario", -"variable", -"unit") %>%
    group_by(across(all_of(c("period", "region")))) %>%
    filter(!all(.data[["value"]] == 0)) %>%
    ungroup() %>%
    group_by(across(all_of(c("region", "carrier")))) %>%
    complete(period = unique(data$period)) %>%
    ungroup() %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    right_join(data, by = c("region", "period", "carrier")) %>%
    mutate("FE_carrier" = .data[["value.x"]] * .data[["value.y"]],
           "FE_carrier" = replace_na(.data[["FE_carrier"]], 0)) %>%
    group_by(across(all_of(c("region", "period", "enduse")))) %>%
    summarise(value = sum(.data[["FE_carrier"]], na.rm = TRUE), .groups = "drop") %>%
    group_by(across(all_of(c("period", "enduse")))) %>%
    mutate(value = proportions(.data[["value"]])) %>%
    as.magpie() %>%
    collapseDim() %>%
    toolCountryFill(1, verbosity = 2)

  #---Pack Data
  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)


  return(list(x = data,
              weight = regShare,
              unit = "1",
              min = 0,
              max = 1,
              description = "Share of carrier or end use in buildings demand"))
}
