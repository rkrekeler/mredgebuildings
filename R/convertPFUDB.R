#' Converts Data from Primary Fuel and Useful Energy Data Base (PFUDB)
#'
#' This was adapted from EDGE function 'getPFUDB.R'.
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom utils read.csv
#' @importFrom dplyr %>% rename
#' @importFrom rlang .data
#' @importFrom tidyr unite
#' @importFrom quitte as.quitte aggregate_map
#' @importFrom magclass as.magpie
#' @export


convertPFUDB <- function(x) {


  # READ-IN DATA ---------------------------------------------------------------

  data <- x

  # Get Weights
  iea_fe <- calcOutput("IEAPFU", aggregate=FALSE) %>%
    as.quitte()

  # Get Mapping
  regionmapping <- toolGetMapping("pfu_regionmapping.csv", type="regional")

  # Get GDP per Cap
  df_gdppop <- calcOutput("GDPPop", aggregate=FALSE) %>%
    as.quitte() %>%
    select(-"model",-"scenario",-"unit")


  # PARAMETERS -----------------------------------------------------------------

  # Energy Unit Conversion [TJ -> EJ]
  TJ2EJ <- 1e-6


  # PROCESS DATA ---------------------------------------------------------------

  # Make columns lowercase
  colnames(data) <- tolower(colnames(data))

  data <- data %>%
    as.quitte() %>%
    quitte::factor.data.frame()


  # Match Periods
  pfu <- data %>%
    as.quitte() %>%
    filter(period %in% getPeriods(iea_fe)) %>%
    mutate(value = replace_na(.data[["value"]], 0))

  iea_fe <- iea_fe %>%
    as.quitte() %>%
    filter(period %in% getPeriods(pfu)) %>%
    mutate(value = replace_na(.data[["value"]], 0))


  # Filter out Uses where all Values for Region/Period are vanishing
  pfu <- pfu %>%
    group_by(.data[["carrier"]]) %>%
    filter(sum(abs(.data[["value"]])) != 0) %>%
    ungroup() %>%
    group_by(.data[["use"]]) %>%
    filter(sum(abs(.data[["value"]])) != 0) %>%
    ungroup()

  # Temporal Renaming for aggregate_map to work well
  pfu <- pfu %>%
    select(-"variable") %>%
    rename(variable = "carrier")


  # DISAGGREGATE ---------------------------------------------------------------

  # Disaggregate to ISO Level
  pfu <- pfu %>%
    droplevels()%>%
    aggregate_map(
      subset2agg = levels(pfu$variable),
      weights = iea_fe %>%
        rename(weight = "value") %>%
        select(-"model", -"scenario", -"unit") %>%
        droplevels(),
      mapping = regionmapping[!is.na(regionmapping$PFUDB), c("PFUDB", "iso")],
      by = c("region" = "PFUDB"),
      weight_item_col = "region",
      weight_val_col = "weight") %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    toolSplitBiomass(df_gdppop, varName = "Biomass")


    # Adaptation of correct Format
    pfu <- pfu %>%
      mutate(value = .data[['value']] * TJ2EJ) %>%
      rename(carrier = "variable",
             enduse = "use") %>%
      select(c("region","period","carrier","enduse","unit","value"))


    # OUTPUT -------------------------------------------------------------------

    pfu <- pfu %>%
      quitte::factor.data.frame() %>%
      droplevels() %>%
      as.magpie() %>%
      toolCountryFill()


    return(pfu)









}
