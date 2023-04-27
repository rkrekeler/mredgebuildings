#' Converts Data from Primary Fuel and Useful Energy Data Base (PFUDB)
#'
#' PFU Data is disaggregated into different thermal and non-thermal Enduses
#' using FE Shares from Odyssee and IEA_ETP.
#'
#' This was adapted from EDGE function 'getPFUDB.R'.
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom madrat readSource
#' @importFrom quitte aggregate_map
#'
#' @export


calcPFUDB <- function(){

  # FUNCTIONS ------------------------------------------------------------------

  # Sum to Carrier Level
  sum_df <- function(df, variables, newname) {
    carrierSum <- df %>%
      filter(.data[["carrier"]] %in% variables) %>%
      group_by(across(-any_of(c("value", "carrier")))) %>%
      summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
      mutate(carrier = newname) %>%
      ungroup() %>%
      select(all_of(colnames(df)))
    df %>%
      filter(!(.data[["carrier"]] %in% variables)) %>%
      rbind(carrierSum)
  }


  # PARAMETERS -----------------------------------------------------------------

  # Rename Carrier Items
  carriersnames <- c(
    "Coal Products"      = "coal",
    "Electricity"        = "elec",
    "Natural Gas"        = "natgas",
    "Petroleum Products" = "petrol",
    "heat"               = "heat",
    "Hydro"              = "hydro",
    "Nuclear"            = "nuclear",
    "Other"              = "other",
    "Wind"               = "wind"
  )

  # READ-IN DATA ---------------------------------------------------------------

  pfu <- readSource("PFUDB") %>%
    as.quitte()

  sharesEC <- calcOutput("Shares", aggregate=FALSE) %>%
    as.quitte()
  sharesTh <- calcOutput("Shares", aggregate=FALSE, subtype="thermal") %>%
    as.quitte()


  # PROCESS DATA ---------------------------------------------------------------

  # Generalize Heat Carriers
  pfu <- sum_df(pfu, c("Heat", "Geothermal", "Solar"), "heat")


  # Map Carrier Names and Convert Units
  pfu <- pfu %>%
    revalue.levels(carrier = carriersnames) %>%
    factor.data.frame() %>%
    mutate(value = replace_na(.data[["value"]],0))


  # Reduce the data frames dimensions to the minimal set
  reg_per_minimal_set <- Reduce(inner_join,
                      list(unique(pfu[c("region", "period")]),
                      unique(sharesTh[c("region", "period")]),
                      unique(sharesEC[c("region", "period")])))

  pfu      <- left_join(reg_per_minimal_set, pfu) %>%
    select(-"model",-"scenario",-"variable")
  sharesEC <- left_join(reg_per_minimal_set, sharesEC) %>%
    select(-"model",-"scenario",-"variable",-"unit")
  sharesTh <- left_join(reg_per_minimal_set, sharesTh) %>%
    select(-"model",-"scenario",-"variable",-"unit")


  ## Disaggregate into Thermal and Non-Thermal Part ----------------------------

  # Enduse Mappings
  enduseMapping <- data.frame(enduse = c("Light", "Stationary power", "Other"),
                              newuse = "appliances_light")
  enduseMappingRef <- data.frame(enduse = c("refrigerators", "appliances_light"),
                                 new_use = "appliances_light")


  # Aggregate uses from PFU to appliances_light and keep the df as Non-thermal
  pfuNonTherm <- pfu %>%
    filter(enduse != "Low-T heat") %>%
    aggregate_map(mapping = enduseMapping,
                  by = "enduse",
                  variable = "enduse",
                  forceAggregation = TRUE)


  # Disaggregate Low-T Heat into different enduses
  pfuTherm <- pfu %>%
    filter(enduse == "Low-T heat") %>%
    select(-"enduse") %>%
    left_join(sharesTh, by = c("region","period","carrier")) %>%
    mutate(value = .data[["value.x"]] * .data[["value.y"]]) %>%
    select(-"value.x",-"value.y")


  # Join Non-Thermal and Thermal Part
  pfu_res <- rbind(pfuTherm,pfuNonTherm)


  # Include "refrigerators" in "appliances_light"
  pfu_res <- rbind(
    pfu_res %>%
      filter(!(.data[["enduse"]] %in% enduseMappingRef[[1]])),
    pfu_res %>%
      aggregate_map(mapping = enduseMappingRef,
                    by = "enduse",
                    variable = "enduse",
                    forceAggregation = TRUE)) %>%
    as.data.frame() %>%
    as.magpie()


  # OUTPUT ---------------------------------------------------------------------
  data <- list(
    x = pfu_res,
    weight = NULL,
    unit = "EJ",
    min = 0,
    description = "Primary Fuel and Useful Energy Data Base"
  )

  return(data)





}
