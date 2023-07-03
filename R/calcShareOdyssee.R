#' Share of energy carriers or end uses in historic buildings demand
#'
#' Shares are calculated based on energy demands in households and services from
#' the Odyssee data base (EU member states). In case of multi-level shares, the
#' function gives the share w.r.t. to the last categories but for all
#' categories. E.g. 'enduse_carrier' gives the share of each carrier in the
#' demand from each end use.
#' Missing shares that result from missing demand data are filled with the
#' average share across all regions and periods and then normalised to sum up to
#' one again.
#' Biomass is split according to GDP per Capita (see toolSplitBiomass).
#'
#' @author Robin Hasse, Antoine Levesque, Hagen Tockhorn
#'
#' @param subtype Character, dimension names, level of shares.
#' @returns MAgPIE object with historic shares
#'
#' @importFrom magclass mbind as.magpie
#' @importFrom madrat readSource toolCountryFill
#' @importFrom quitte as.quitte revalue.levels
#' @importFrom dplyr filter %>% mutate group_by across all_of left_join
#' summarise
#' @importFrom rlang .data syms
#' @importFrom tidyr separate replace_na complete
#' @importFrom utils tail
#' @export

calcShareOdyssee <- function(subtype = c("enduse", "carrier", "enduse_carrier")) {
  # FUNCTIONS ------------------------------------------------------------------

  # Calculate Shares w.r.t colShare
  calcShares <- function(data, colShare) {
    data %>%
      group_by(across(-all_of(c(colShare, "value")))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup()
  }


  # READ-IN DATA ---------------------------------------------------------------

  # Read Buildings Data
  odyssee <- mbind(readSource("Odyssee", "households"),
                   readSource("Odyssee", "services"))

  # Get GDP per Cap
  gdppop <- calcOutput("GDPPop", aggregate = FALSE) %>%
    as.quitte() %>%
    select(-"model", -"scenario", -"unit")


  # PARAMETERS -----------------------------------------------------------------

  subtype <- match.arg(subtype)
  shareOf <- strsplit(subtype, "_")[[1]]

  # Variable Mappings
  revalCarrier <- c(
    cms = "coal",
    pet = "petrol",
    gaz = "natgas",
    vap = "heat",
    enc = "biomod",
    elc = "elec")
  revalSector <- c(
    cfres = "residential",
    cfter = "services")
  revalEnduse <- c(
    chf = "space_heating",
    ecs = "water_heating",
    cui = "cooking",
    cli = "space_cooling",
    els1 = "appliances",
    lgt = "lighting")
  vars <- expand.grid(names(revalCarrier),
                      names(revalSector),
                      names(revalEnduse)) %>%
    apply(1, "paste", collapse = "")


  # PROCESS DATA ---------------------------------------------------------------

  # Map Variables
  odyssee <- odyssee %>%
    as.quitte() %>%
    filter(.data[["variable"]] %in% paste0(vars, "_EJ"),
           !is.na(.data[["value"]])) %>%
    mutate(region = droplevels(.data[["region"]]),
           variable = gsub("_.*$", "", .data[["variable"]])) %>%
    separate("variable", c("carrier", "sector", "enduse"), c(3, 8)) %>%
    revalue.levels(carrier = revalCarrier,
                   sector  = revalSector,
                   enduse  = revalEnduse) %>%
    interpolate_missing_periods(expand.values = TRUE)


  # Split Biomass
  if (subtype != "enduse") {
    odyssee <- odyssee %>%
      rename(variable = "carrier") %>%
      toolSplitBiomass(gdppop, varName = "biomod") %>%
      rename(carrier = "variable")
  }


  #---Calculate Shares

  shareGlobal <- odyssee %>%
    group_by(across(all_of(shareOf))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE))

  share <- odyssee %>%
    group_by(across(all_of(c("region", "period", shareOf)))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    calcShares(if (subtype == "enduse_carrier") shareOf else tail(shareOf, 1)) %>%
    mutate(value = replace_na(.data[["value"]], 0)) %>%
    complete(!!!syms(c("region", "period", shareOf))) %>%
    left_join(shareGlobal, by = shareOf) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]])) %>%
    select(-"value.x", -"value.y") %>%
    calcShares(if (subtype == "enduse_carrier") shareOf else tail(shareOf, 1))


  # Weights: regional share of final energy
  regShare <- odyssee %>%
    complete(!!!syms(c("region", "period", "sector", "carrier", "enduse"))) %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    group_by(across(all_of(c("region", "period", head(shareOf, -1))))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
    group_by(across(all_of(c("period", head(shareOf, -1))))) %>%
    mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE))



  # OUTPUT ---------------------------------------------------------------------

  # Convert to magpie object
  share <- share %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2)

  regShare <- regShare %>%
    as.magpie() %>%
    collapseDim() %>%
    toolCountryFill(1, verbosity = 2)


  return(list(x = share,
              weight = regShare,
              unit = "1",
              min = 0,
              max = 1,
              description = "Share of carrier or end use in buildings demand"))
}
