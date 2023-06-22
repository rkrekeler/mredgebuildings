#' Convert TCEP data base
#'
#' @returns magpie object
#'
#' @author Hagen Tockhorn
#'
#' @importFrom dplyr %>% select mutate
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export


convertTCEP <- function(x) {

  # READ-IN DATA ---------------------------------------------------------------

  data <- x


  # Load Enduse Mapping (EU -> EDGE)
  euMap <- toolGetMapping("enduse_regional_correspondances.csv", type="regional")

  # Load EDGE Mapping (EDGE -> ISO)
  edgeMap <- toolGetMapping("regionmappingEDGE.csv", type="regional")


  # Population
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte()


  # PARAMETERS -----------------------------------------------------------------

  enduseMapping <- c(
    "Space heating"         = "space_heating",
    "Space cooling"         = "space_cooling",
    "Water heating"         = "water_heating",
    "Lighting"              = "lighting",
    "Cooking"               = "cooking",
    "Appliances and other"  = "appliances"
  )


  # PROCESS DATA ---------------------------------------------------------------

  data <- data %>%
    as.quitte() %>%
    select("region", "period", "variable", "unit", "value") %>%
    revalue.levels(variable = enduseMapping)


  # Aggregate Pop for Data Disaggregation
  popEDGE <- pop %>%
    select("region", "period", "variable", "unit", "value") %>%
    aggregate_map(
      mapping = edgeMap[,c("CountryCode","RegionCode")],
      by = c("region" = "CountryCode"))


  # Disaggregate to ISO Level (Enduse -> EDGE -> ISO)
  data <- data %>%
    mutate(region = gsub("_", "\\.", region)) %>%
    aggregate_map(
      mapping = euMap[,c("region_evolution", "region_target")],
      by = c("region" = "region_evolution"),
      weights = popEDGE %>%
        select(-"unit",-"variable") %>%
        rename(weight = "value"),
      weight_item_col = "region",
      weight_val_col = "weight",
      forceAggregation = TRUE
    ) %>%
    aggregate_map(
      mapping = edgeMap[,c("RegionCode","CountryCode")],
      by = c("region" = "RegionCode"),
      weights = pop %>%
        select(-"unit",-"variable", -"model", -"scenario") %>%
        rename(weight = "value"),
      weight_item_col = "region",
      weight_val_col = "weight",
      forceAggregation = TRUE
    ) %>%
    select("region", "period", "variable", "value") %>%
    rename(enduse = "variable")



  # OUTPUT ---------------------------------------------------------------------

  data <- data %>%
    as.magpie()

  return(data)




}
