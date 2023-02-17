#' Share of energy carriers or end uses in historic buildings demand
#'
#' Shares are calculated based on energy demands in households and services from
#' the Odyssee data base (EU member states). In case of multi-level shares, the
#' function gives the share w.r.t. to the last categories but for all
#' categories. E.g. 'enduse_carrier' gives the share of each carrier in the
#' demand from each end use.
#' Missing shares that result from missing demand data are filled with the
#' average share across all regions and periods and then normalised to sum up to
#' one again. Traditional biomass use is assumed to be zero.
#'
#' @author Robin Hasse, Antoine Levesque
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

calcShareOdyssee <- function(subtype = c("enduse", "enduse_carrier")) {
  subtype <- match.arg(subtype)
  shareOf <- strsplit(subtype, "_")[[1]]

  # read buildings data
  odyssee <- mbind(readSource("Odyssee", "households"),
                   readSource("Odyssee", "services"))

  # variable mappings
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

  # map variables
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

  # calculate shares
  calcShares <- function(data, colShare) {
    data %>%
      group_by(across(-all_of(c(colShare, "value")))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]])) %>%
      ungroup()
  }
  shareGlobal <- odyssee %>%
    group_by(across(all_of(shareOf))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    ungroup() %>%
    calcShares(tail(shareOf, 1)) %>%
    mutate(value = replace_na(.data[["value"]], 1))
  share <- odyssee %>%
    group_by(across(all_of(c("region", "period", shareOf)))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    ungroup() %>%
    calcShares(tail(shareOf, 1)) %>%
    mutate(value = replace_na(.data[["value"]], 1)) %>%
    complete(!!!syms(c("region", "period", shareOf))) %>%
    left_join(shareGlobal, by = shareOf) %>%
    mutate(value = ifelse(is.na(.data[["value.x"]]),
                          .data[["value.y"]],
                          .data[["value.x"]]),
           value = replace_na(.data[["value"]], 0)) %>%
    select(-"value.x", -"value.y") %>%
    calcShares(tail(shareOf, 1))

  # add zero traditional biomass
  if (subtype == "enduse_carrier") {
    share <- share %>%
      filter(.data[["carrier"]] == "biomod") %>%
      mutate(value = 0,
             carrier = "biotrad") %>%
      rbind(share)
  }

  # convert to magpie object
  share <- share %>%
    as.magpie() %>%
    toolCountryFill(verbosity = 2)

  # weights: regional share of final energy
  regShare <- odyssee %>%
    complete(!!!syms(c("region", "period", "sector", "carrier", "enduse"))) %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    group_by(across(all_of(c("region", "period", head(shareOf, -1))))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
    group_by(across(all_of(c("period", head(shareOf, -1))))) %>%
    mutate(value = .data[["value"]] / sum(.data[["value"]])) %>%
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
