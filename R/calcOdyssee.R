#' @importFrom dplyr mutate filter rename left_join select group_by summarise ungroup across %>% mutate_
#' @importFrom madrat readSource toolCountry2isocode toolGetMapping
#' @importFrom quitte revalue.levels interpolate_missing_periods aggregate_map getRegs
#' @importFrom rlang .data syms
#' @importFrom tidyr replace_na complete separate
#' @importFrom tidyselect all_of one_of
calcOdyssee <- function(subtype = c("enduseShares", "carrierShares")) {
  subtype <- match.arg(subtype)

  odyssee <- as.quitte(readSource("Odyssee"))

  # TODO switch to aggregation via madrat
  regionalMap <- toolGetMapping("regionmappingEDGE.csv", type = "regional")[c("CountryCode", "RegionCodeEUR_ETP")] %>%
    na.omit()

  # aggregate to EDGE regions
  revalCarrier <- c(
    cms = "coal",
    pet = "petrol",
    gaz = "natgas",
    vap = "heat",
    enc = "biomod",
    elc = "elec",
    toc = "total")
  revalSector <- c(
    cfres = "residential",
    cfter = "services")
  revalEnduse <- c(
    chf = "space_heating",
    ecs = "water_heating",
    chc = "space_heating_corrected",
    cui = "cooking",
    cli = "space_cooling",
    els = "appliances_light",
    els1 = "appliances",
    lgt = "lighting")
  redundantEnduse <- c("space_heating_corrected", "appliances_light")
  redundantCarrier <- "total"
  odyssee <- odyssee %>%
    aggregate_map(filter(regionalMap, .data[["CountryCode"]] %in% getRegs(odyssee)),
                  by = c(region = "CountryCode")) %>%
    separate("variable", c("carrier", "sector", "enduse"), c(3, 8)) %>%
    revalue.levels(carrier = revalCarrier,
                   sector  = revalSector,
                   enduse  = revalEnduse) %>%
    filter(!.data[["enduse"]]  %in% redundantEnduse,
           !.data[["carrier"]] %in% redundantCarrier)

  calcShares <- function(data, colShare) {
    cols <- setdiff(colnames(data), c(colShare, "value"))

    res <- data %>%
      group_by(across(all_of(cols))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]])) %>%
      ungroup() %>%
      return()
  }

  computeSharesAndComplete <- function(df, .groups) {
    grReg <- c("region", "period", .groups)
    grAgg <- .groups

    colShares <- tail(.groups, 1)

    dfReg <- df %>%
      group_by(across(all_of(grReg))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      ungroup()
    dfAgg <- df %>%
      group_by(across(all_of(grAgg))) %>%
      summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
      ungroup()

    dfReg <- calcShares(dfReg, colShares)
    dfAgg <- calcShares(dfAgg, colShares)

    # if space cooling is explicitly 0 in the data, it yields a NaN as a share
    dfReg <- dfReg %>%
      mutate(value = replace_na(.data[["value"]], 1))
    dfAgg <- dfAgg %>%
      mutate(value = replace_na(.data[["value"]], 1))

    missingCases <- dfReg %>%
      complete(!!!syms(grReg)) %>%
      filter(is.na(.data[["value"]])) %>%
      select(one_of(grReg)) %>%
      unique()
    completeMissingDf <- left_join(missingCases, dfAgg, by = grAgg)

    result <- rbind(dfReg, completeMissingDf) %>%
      calcShares(colShares)
  }

  switch(subtype,
         enduseShares = {
           odyssee <- odyssee %>%
             computeSharesAndComplete("enduse") %>%
             rename(variable = "enduse")
         },
         carrierShares = {
           odyssee <- odyssee %>%
             computeSharesAndComplete(c("enduse", "carrier")) %>%
             mutate(value = replace_na(.data[["value"]], 0)) # for end uses which are fully electric
           biotrad <- odyssee %>%
             filter(.data[["carrier"]] == "biomod") %>%
             mutate(value = 0, carrier = "biotrad")
           odyssee <- rbind(odyssee, biotrad)
         },
         stop(paste("Invalid subtype:", subtype)))

  odyssee <- odyssee %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2)
  return(list(x = odyssee,
              weight = NULL, # TODO what are reasonable weights here?
              unit = "EJ", # TODO is EJ correct here?
              description = "final energy consumption of residential and commercial buildings in Europe"))
}
