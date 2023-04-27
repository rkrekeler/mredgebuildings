#' Calculation and Projection of Final to Useful Energy
#'
#' Calculate Efficiencies of Final (FE) to Useful (UE) Energy Conversion for all
#' combinations of Energy Carriers and Enduses.
#' The efficiency projections are based on a model by De Stercke et al. which is
#' mainly driven by GDP per Capita. It describes an S-shaped curve approaching
#' assumed efficiency levels. The parameters of that curve are derived by a
#' regression with observations of IEA data.
#'
#' @references De Stercke, S. (2014). Dynamics of Energy Systems: A Useful
#' Perspective (Interim Report, p. 68). IIASA.
#' http://pure.iiasa.ac.at/id/eprint/11254/1/IR-14-013.pdf
#'
#' @author Hagen Tockhorn
#'
#' @export


calcFEUEefficiencies <- function() {

  # FUNCTIONS ------------------------------------------------------------------

  # Extrapolate historic FE-UE Efficiencies from Fit Function
  extrapolEff <- function(df, pars) {

    # Assign Parameters to carrier-enduse Combination
    dataHist <- df %>%
      removeColNa() %>%
      left_join(pars, by = c("carrier", "enduse")) %>%
      na.omit()

    # Predict Historic Efficiencies with Non-Linear Model
    dataHist <- dataHist %>%
      group_by("carrier", "enduse") %>%
      mutate(pred = SSasymp(.data[["gdppop"]], .data[["Asym"]], .data[["R0"]], .data[["lrc"]]))



  }



  # READ-IN DATA ---------------------------------------------------------------

  pfu <- calcOutput("PFUDB", aggregate = FALSE) %>%
    as.quitte()

  gdppop <- calcOutput("GDPPop", aggregate = FALSE) %>%
    as.quitte()

  # Get Mapping (ISO<->PFU)
  # regionmapping <- toolGetMapping("pfu_regionmapping.csv", type="regional")
  #
  # # Get Population Data
  # pop <- calcOutput("PopulationPast", aggregate = FALSE) %>%
  #   as.quitte()


  # PARAMETERS -----------------------------------------------------------------

  exceptions <- c("water_heating.natgas",
                  "space_heating.natgas",
                  "cooking.natgas")


  # PROCESS DATA ---------------------------------------------------------------

  # Combine with GDP per Cap
  data <- pfu %>%
    mutate(value = ifelse(is.na(.data[["value"]]), 0, .data[["value"]])) %>%
    left_join(gdppop %>%
                select(-"model",-"scenario",-"unit",-"variable") %>%
                rename(gdppop = "value"),
              by = c("region","period")) %>%
    select(-"model", -"scenario", -"variable")

  # gdppop <- gdppop %>%
  #   # aggregate_map(
  #   #   mapping = regionmapping[!is.na(regionmapping$PFUDB), c("iso","PFUDB")],
  #   #   by = c("region" = "iso"),
  #   #   forceAggregation = TRUE,
  #   #   weights = pop %>%
  #   #     select(-"unit",-"variable",-"scenario",-"model") %>%
  #   #     rename(weight = "value"),
  #   #   weight_item_col = "region",
  #   #   weight_val_col = "weight") %>%
  #   select(-"model",-"scenario",-"unit",-"variable")


  # Combine with GDP per Cap
  # data <- data %>%
  #   left_join(gdppop %>%
  #             rename(gdppop = "value"),
  #           by = c("region","period"))

  # Calculate Efficiencies as Regression-Input
  # data <- data %>%
  #   select(-"model",-"scenario") %>%
  #   spread(.data[["unit"]], .data[["value"]]) %>%
  #   mutate(value = .data[["ue"]] / .data[["fe"]]) %>%
  #   select(-"fe",-"ue")

  # List of all Carrier-Enduse Combinations
  vars <- data %>%
    filter(!.data[["variable"]] %in% exceptions) %>%
    getElement("variable") %>%
    unique()


  #--- Calculate Efficiency Estimates

  # Regression Parameters for enduse.carrier Combinations
  regPars <- calcOutput("EfficiencyRegression", aggregate = FALSE) %>%
    as.quitte() %>%
    spread(key = "variable", value = "value") %>%
    select(-"model", -"scenario", -"region", -"unit", -"period")


  a <- extrapolEff(data, regPars)


}
