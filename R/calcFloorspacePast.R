#' Historical residential floor space demand
#'
#' Data for many European countries is taken from EEA, India from IEA and other
#' countries from Daioglou et al 2012. The result does not cover all countries
#' and has mixed points in time depending on the region.
#'
#' @note RK: In Antoine's EDGE-B, data points associated with an GDP/POP above
#' 70000 USD/cap are dropped here to improve the later regression. This
#' filtering should be moved to getFloorspaceResidential where the regression is
#' performed. Therefore, the high-income data points are kept at this stage.
#'
#' @returns MAgPIE object with historic floor space
#'
#' @author Robin Hasse, Antoine Levesque
#'
#' @importFrom madrat readSource calcOutput toolCountryFill
#' @importFrom quitte as.quitte calc_addVariable
#' @importFrom dplyr filter mutate select anti_join group_by left_join %>%
#' ungroup .data
#' @importFrom magclass mbind as.magpie collapseDim mselect
#' @importFrom tidyr spread
#' @export

calcFloorspacePast <- function() {

  # FUNCTIONS ------------------------------------------------------------------

  # specific floor space from absolute floor space and population
  floorPerCap <- function(floor, pop) {
    floor %>%
      rbind(pop) %>%
      calc_addVariable(`specific floor space` = "`floor space` / population",
                       units = "m2/cap", only.new = TRUE) %>%
      mutate(demographic = "Total")
  }


  # LOAD AND CALCULATE DATA ----------------------------------------------------

  # data from Daioglou et al.
  daioglou <- readSource("Daioglou") %>%
    as.quitte(na.rm = TRUE) %>%
    filter(.data[["quintile"]] == 0) %>%
    select(-"quintile") %>%
    mutate(variable = "specific floor space",
           unit = "m2/cap")

  # EEA data: drop ESP and PRT (too high uncertainty)
  eea <- readSource("EEAfloorspace") %>%
    as.quitte(na.rm = TRUE) %>%
    filter(!.data[["region"]] %in% c("ESP", "PRT")) %>%
    mutate(variable = "floor space",
           unit = "million m2")

  # IEA data: take only India
  ind <- readSource("IEAfloorspace", convert = FALSE) %>%
    as.quitte() %>%
    filter(.data[["region"]] == "India",
           .data[["period"]] %in% c(2000, 2011),
           .data[["subsector"]] == "Residential") %>%
    select(-"subsector") %>%
    mutate(variable = "floor space",
           value = .data[["value"]] * 1000, # billion m2 -> million m2
           unit = "million m2",
           region = "IND")

  # historic population
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte() %>%
    mutate(unit = "million cap")

  # historic GDP per capita
  gdppop <- calcOutput("GDPPast", aggregate = FALSE) %>% # nolint
    as.quitte() %>%
    rbind(pop) %>%
    calc_addVariable(gdppop = "`gdp in constant 2005 Int$PPP` / `population`",
                     units = "USD2005/cap", only.new = TRUE) %>%
    filter(.data[["variable"]] == "gdppop")

  # share of urban population
  urbanshare <- calcOutput("UrbanPast", aggregate = FALSE) %>%
    as.quitte()

  # compute specific floor space
  eea <- floorPerCap(eea, pop)
  ind <- floorPerCap(ind, pop)


  # JOIN DATA ------------------------------------------------------------------

  data <- rbind(eea, ind)
  data <- data %>%
    rbind(anti_join(daioglou, data, by = c("period", "region", "demographic",
                                           "scenario", "variable")))

  # if missing, compute total from urban and rural data
  data <- data %>%
    group_by(across(all_of(c("region", "period")))) %>%
    filter(!any(.data[["demographic"]] == "Total"),
           any(.data[["demographic"]] == "Rural"),
           any(.data[["demographic"]] == "Urban")) %>%
    ungroup() %>%
    unite("variable", "variable", "demographic") %>%
    rbind(urbanshare) %>%
    as.quitte() %>%
    calc_addVariable(`specific floor space` =
                       "`specific floor space_Urban` * urbanPop +
                        `specific floor space_Rural` * (1 - urbanPop)",
                     units = "m2/cap", only.new = TRUE) %>%
    mutate(demographic = "Total") %>%
    rbind(data)

  # remove urban and rural data
  data <- data %>%
    filter(.data[["demographic"]] == "Total") %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim() %>%
    toolCountryFill(verbosity = 2)

  pop <- pop %>%
    as.quitte() %>%
    as.magpie() %>%
    mselect(region = getItems(data, 1), period = getItems(data, 2)) %>%
    collapseDim()


  return(list(x = data,
              weight = pop,
              min = 0,
              unit = "m2/cap",
              description = "floor space per capita"))
}
