#' Calculate techno-economic parameters of renovation alternatives
#'
#' Specific renovation cost are estimated with a linear model with intercept
#' w.r.t. to the renovation depth. We assume 3 different depth of renovation.
#'
#' The specific investment cost for renovation depending on the depth of
#' renovation is calculated using a two-level statistical model fitted with
#' data from a European Commission (EC) report on renovation.
#' In a fist step, we fit a simple linear model with intercept to predict
#' specific investment (€/m2) with relative PE savings. In a second step, this
#' linear model is scaled with one factor for each region. This scaling factor
#' is predicted with GDP/POP using a negative exponential curve. For EU regions
#' that are reported, we correct the result with a region-specific but
#' time-invariant factor to best match the linear model to the data. This way,
#' we get a linear model with intercept for each region that can be extrapolated
#' to other regions (using GDP/POP) which still matches data of EU regions very
#' well. The cost is finally disaggregated across residential building types
#' (SFH, MFH) based on a rough cost factor seen in the ENTRANZE data.
#' @source https://op.europa.eu/s/xnYt
#'
#' @author Robin Hasse
#'
#' @returns MAgPIE object with specific renovation cost
#'
#' @importFrom stats lm nls coef predict
#' @importFrom madrat readSource
#' @importFrom magclass mselect as.magpie getSets<- setNames dimSums
#' @importFrom dplyr %>% filter mutate select group_by summarise ungroup
#' left_join group_modify
#' @importFrom tidyr spread
#' @importFrom quitte as.quitte calc_addVariable revalue.levels
#' inline.data.frame
#' @importFrom rlang .data
#'
#' @export

calcRenovationAlternatives <- function() {

  # ASSUMPTIONS ----------------------------------------------------------------

  # temporal scope of the report and of final data set
  periodsReport <- 2014:2017
  periods <- 2000:2020

  # assume center of depth intervals in report
  # corresponds well to aggregated data in the report
  renovationDepth <- inline.data.frame(
    "depth;   saving",
    "none;    0",
    "light;   0.15",
    "medium;  0.45",
    "deep;    0.65")

  # minimum specific investment relative to average in EU
  minFactor <- 0.15

  # cost ratio SFH/MFH cf. ENTRANZE data
  costRatio <- 1.5



  # READ DATA ------------------------------------------------------------------

  # EC report on renovation in EU member states
  renovation <- readSource("EuropeanCommissionRenovation") %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"model", -"scenario", -"period")

  # average income in reporting period: 2014 - 2017
  pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-"model", -"scenario") %>%
    mutate(unit = "million cap")
  gdp <- calcOutput("GDPPast", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-"model", -"scenario")
  gdppop <- rbind(gdp, pop) %>%
    calc_addVariable(gdppop = "`gdp in constant 2005 Int$PPP` / `population`",
                     units = "USD2005/cap", only.new = TRUE)
  gdppopAvg <- rbind(gdp, pop) %>%
    filter(.data[["period"]] %in% periodsReport) %>%
    group_by(across(-all_of(c("period", "value")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    ungroup() %>%
    calc_addVariable(gdppop = "`gdp in constant 2005 Int$PPP` / `population`",
                     units = "USD2005/cap", only.new = TRUE)

  # ratio of SFH
  typeCode <- c(nbrmpr_1 = "SFH",
                nbripr_1 = "MFH")
  typeShare <- readSource("Odyssee") %>%
    mselect(variable = names(typeCode)) %>%
    as.quitte(na.rm = TRUE) %>%
    select(-"model", -"scenario", -"unit") %>%
    filter(.data[["period"]] %in% periodsReport) %>%
    revalue.levels(variable = typeCode)
  typeShare <- typeShare %>%
    group_by(across(all_of(c("period", "variable")))) %>%
    summarise(region = setdiff(gdppop$region, typeShare$region),
              value = sum(.data[["value"]]),
              .groups = "drop") %>%
    rbind(typeShare)
  typeShare <- typeShare %>%
    group_by(across(all_of(c("region", "variable")))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    group_by(.data[["region"]]) %>%
    mutate(value = proportions(.data[["value"]])) %>%
    spread("variable", "value")

  # FE demand in residential and commercial buildings
  feBuildings <- readSource("IEA", "EnergyBalances") %>%
    mselect(FLOW = c("RESIDENT", "COMMPUB"), TIME = paste0("y", periods)) %>%
    dimSums(3.1) %>%
    `getSets<-`(value = c("region", "period", "buildingType"))



  # TRAIN MODEL ----------------------------------------------------------------

  energyRelatedRenovations <- c("Energy related - below Threshold",
                                "Energy related - Light",
                                "Energy related - Medium",
                                "Energy related - Deep")
  modelVariables <- c(`relative PE savings` = "x",
                      `specific investment` = "y")
  subsectors <- as.character(unique(renovation$subsector))

  # independent variables: rel PE savings (x) and gdppop
  # dependent variable: specific investment (y)
  trainingData <- renovation %>%
    select(-"unit") %>%
    filter(.data[["renovation"]] %in% energyRelatedRenovations,
           .data[["variable"]] %in% names(modelVariables)) %>%
    revalue.levels(variable = modelVariables) %>%
    spread("variable", "value") %>%
    select("region", "subsector", "x", "y") %>%
    left_join(gdppopAvg %>%
                select("region", gdppop = "value"),
              by = "region") %>%
    mutate(minFactor = minFactor)

  # fit model to report data for each subsector
  modelCalibration <- data.frame()
  correctionFactor <- data.frame()

  for (subsec in subsectors) {

    data <- trainingData %>%
      filter(.data[["subsector"]] == subsec)

    # global linear model with intercept
    globalLinearModel <- lm("y~x", data)
    data$yGlobalPredict <- predict(globalLinearModel, newdata = data)

    # regional scaling to best match regional data
    data <- data %>%
      group_by(.data[["region"]]) %>%
      group_modify(function(dataReg, reg) {
        regionalScaleModel <- lm("y~yGlobalPredict - 1", dataReg)
        dataReg[["factor"]] <- coef(regionalScaleModel)
        dataReg
      })

    # explain regional scaling with GDP/POP
    scalingModel <- nls(factor - minFactor ~ a * (1 - exp(-c * gdppop)),
                        unique(data[, c("region", "factor", "gdppop", "minFactor")]),
                        list(a = max(data$factor) - minFactor,
                             c = max(data$factor) / max(data$gdppop)))
    data$factorPredict <- predict(scalingModel, newdata = data)

    # model coefficients
    modelCalibration <- modelCalibration %>%
      rbind(data.frame(
        subsector = subsec,
        submodel = rep(c("global", "scaling"), each = 2),
        coef     = c("intercept", "slope", "Asym", "slope"),
        value    = c(coef(globalLinearModel), coef(scalingModel))
      ))

    # correction factor to reach best regional fit (independent of GDP/POP)
    correctionFactor <- data %>%
      mutate(subsector = subsec,
             correct   = .data[["factor"]] / .data[["factorPredict"]]) %>%
      select("subsector", "region", "correct") %>%
      unique() %>%
      rbind(correctionFactor)
  }



  # PREDICT --------------------------------------------------------------------

  # predict specific investment based on relative PE saving and GDP/POP
  predictModel <- function(data, x = "x", gdppop = "gdppop") {
    data %>%
      left_join(modelCalibration %>%
                  unite("coef", c("submodel", "coef")) %>%
                  spread("coef", "value"),
                by = "subsector") %>%
      left_join(correctionFactor,
                by = c("subsector", "region")) %>%
      mutate(globPredict = .data[["global_intercept"]] + .data[["global_slope"]] * .data[[x]],
             factor      = .data[["scaling_Asym"]] * (1 - exp(-.data[["scaling_slope"]] * .data[[gdppop]])),
             correct     = replace_na(.data[["correct"]], 1),
             yPredict    = .data[["globPredict"]] * .data[["factor"]] * .data[["correct"]]) %>%
      getElement("yPredict")
  }

  # extrapolate to other regions and periods
  specificInvest <- gdppop %>%
    filter(.data[["period"]] %in% periods) %>%
    select("region", "period", gdppop = "value") %>%
    merge(renovationDepth) %>%
    merge(data.frame(subsector = subsectors))
  specificInvest$value <- predictModel(specificInvest, "saving")
  specificInvest$gdppop <- NULL
  specificInvest$saving <- NULL



  # DISAGGREGATE----------------------------------------------------------------

  ## Building Type ====
  specificInvest <- specificInvest %>%
    filter(.data[["subsector"]] == "residential") %>%
    left_join(typeShare, by = "region") %>%
    mutate(MFH = .data[["value"]] / (.data[["SFH"]] * costRatio + .data[["MFH"]]),
           SFH = costRatio * .data[["MFH"]]) %>%
    gather("buildingType", "value", "SFH", "MFH") %>%
    select(-"subsector") %>%
    rbind(specificInvest %>%
            filter(.data[["subsector"]] == "commercial") %>%
            rename(buildingType = "subsector"))



  # RETURN ---------------------------------------------------------------------

  # convert to magpie object
  specificInvest <- specificInvest %>%
    as.magpie()

  # Use FE demand as aggregation weights
  feBuildings <- mbind(
    setNames(feBuildings[, , "RESIDENT"] * 0.5, "SFH"),
    setNames(feBuildings[, , "RESIDENT"] * 0.5, "MFH"),
    setNames(feBuildings[, , "COMMPUB"], "commercial"))
  feBuildings <- do.call("mbind", lapply(getItems(specificInvest, 3.1), function(d) {
    add_dimension(feBuildings, 3.1, "depth", d)
  }))

  return(list(x = specificInvest,
              weight = feBuildings,
              min = 0,
              description = "floor-area specific renovation investment cost",
              unit = "EUR/m2"))
}
