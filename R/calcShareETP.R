#' Share of energy carriers or end uses in historic buildings demand
#'
#' Shares are calculated based on energy demands in households and services from
#' the IEA ETP data base.
#' Missing shares that result from missing demand data are filled with the
#' average share across all regions and periods and then normalised to sum up to
#' one again. Traditional biomass use is assumed to be zero.
#'
#' @returns MAgPIE object with historic shares
#'
#' @author Robin Hasse, Antoine Levesque, Hagen Tockhorn
#'
#' @importFrom magclass mbind as.magpie
#' @importFrom madrat readSource toolCountryFill
#' @importFrom quitte as.quitte revalue.levels
#' @importFrom dplyr filter %>% mutate group_by across all_of left_join
#' summarise
#' @importFrom plyr revalue
#' @importFrom rlang .data syms
#' @importFrom tidyr separate replace_na complete
#' @importFrom utils tail
#' @export

calcShareETP <- function(subtype = c("enduse", "carrier")) {
  subtype <- match.arg(subtype)

  # rename variable out of convention
  shareOf <- subtype

  # filtered periods and scenarios
  periods <- c("2014")
  scen <- c("RTS")

  # read buildings data
  etp <- readSource("IEA_ETP", "buildings")

  # variable mappings
  reval <- switch(shareOf,
    enduse = c(
      `Buildings|Buildings - Total final energy consumption by end-use|Space heating` = "space_heating",
      `Buildings|Buildings - Total final energy consumption by end-use|Water heating` = "water_heating",
      `Buildings|Buildings - Total final energy consumption by end-use|Space cooling` = "space_cooling",
      `Buildings|Buildings - Total final energy consumption by end-use|Lighting` = "lighting",
      `Buildings|Buildings - Total final energy consumption by end-use|Appliances and miscellaneous equipments`
      = "appliances",
      `Buildings|Buildings - Total final energy consumption by end-use|Cooking` = "cooking"),

    carrier = c(
      `Buildings|Buildings - Total final energy consumption|Coal` = "coal",
      `Buildings|Buildings - Total final energy consumption|Oil products` = "petrol",
      `Buildings|Buildings - Total final energy consumption|Natural gas` = "natgas",
      `Buildings|Buildings - Total final energy consumption|Commercial heat` = "heat",
      `Buildings|Buildings - Total final energy consumption|Electricity` = "elec",
      `Buildings|Buildings - Total final energy consumption|Biomass, waste and other renewables` = "biomod")
  )

  # map variables
  etpFilter <- etp %>%
    as.quitte() %>%
    filter(.data[["period"]] %in% periods,
           .data[["data"]] %in% scen) %>%
    filter(.data[["data1"]] %in% names(reval),
           !is.na(.data[["value"]])) %>%
    mutate(data1 = droplevels(revalue(.data[["data1"]], reval)))

  names(etpFilter)[names(etpFilter) == "data1"] <- shareOf


  # Extrapolate 'biotrad' share from 'biomod' values for carrier separation
  if (subtype == "carrier") {
    edgeBio <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings", aggregate = FALSE)
    feBio <- calcOutput("IO", subtype = "output_biomass", aggregate = FALSE)
    shareBiotrad <- edgeBio[, , "biotrad"] / (feBio[, , "sesobio.fesob.tdbiosob"]
                                            + feBio[, , "sesobio.fesoi.tdbiosoi"])
    shareBiotrad[is.na(shareBiotrad)] <- 0
    shareBiotrad <- shareBiotrad %>%
      as.quitte() %>%
      mutate(share = .data[["value"]]) %>%
      select(-"value", -"model", -"scenario", -"variable", -"unit",
             -"d3", -"d31", -"data", -"data1", -"data2", -"data11")

    etpFilter <- etpFilter %>%
      filter(.data[["carrier"]] == "biomod") %>%
      left_join(shareBiotrad, by = c("region", "period")) %>%
      mutate(biotrad = .data[["value"]] * .data[["share"]],
             biomod = .data[["value"]] * (1 - .data[["share"]])) %>%
      select(-"value", -"share") %>%
      gather(key = "carrier", value = "value", "biotrad", "biomod") %>%
      rbind(etpFilter %>% filter(.data[["carrier"]] != "biomod"))
  }


  # calculate shares
  calcShares <- function(data, colShare) {
    data %>%
      group_by(across(-all_of(c(colShare, "value")))) %>%
      mutate(value = proportions(.data[["value"]])) %>%
      ungroup()
  }

  # global shares
  shareGlobal <- etpFilter %>%
    group_by(across(all_of(shareOf))) %>%
    summarise(value = sum(.data[["value"]]), .groups = "drop") %>%
    ungroup() %>%
    calcShares(shareOf) %>%
    mutate(value = replace_na(.data[["value"]], 1))

  # local shares
  share <- etpFilter %>%
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
  regShare <- etpFilter %>%
    complete(!!!syms(c("region", "period", shareOf))) %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    group_by(across(all_of(c("region", "period", head(shareOf, -1))))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
    group_by(across(all_of(c("period", head(shareOf, -1))))) %>%
    mutate(value = .data[["value"]] / sum(.data[["value"]])) %>%
    as.magpie() %>%
    collapseDim() %>%
    toolCountryFill(1, verbosity = 2)



  # generate output
  return(list(x = share,
              weight = regShare,
              unit = "1",
              min = 0,
              max = 1,
              description = "Share of carrier or end use in buildings demand"))
}
