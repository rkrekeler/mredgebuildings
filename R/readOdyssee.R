#' Read Odyssee data base
#'
#' Read and tidy data on final energy consumption for different end uses in
#' residential (households) and commercial (services) buildings in Europe.
#' Calculates end use or carrier shares for European EDGE regions.
#'
#' @param subtype type of result (\code{enduseShares} or \code{carrierShares})
#' @returns data.frame with historic enduse or carrier shares
#'
#' @author Pascal Führlich, Antoine Levesque
#'
#' @references Enerdata. Odyssee-Mure database. https://www.odyssee-mure.eu/
#' @importFrom countrycode countrycode
#' @importFrom magrittr %>%
#' @importFrom rlang .data syms
#' @importFrom dplyr mutate filter rename left_join select group_by summarise ungroup across
#' @importFrom tidyr replace_na complete separate
#' @importFrom tidyselect all_of one_of
#' @importFrom quitte revalue.levels interpolate_missing_periods aggregate_map getRegs
readOdyssee <- function(subtype = "enduseShares") {
  services <- read.csv("export_enerdata_7793_105638_services.csv", skip = 1, na.strings = c("n.a.", ""))
  households <- read.csv("export_enerdata_7793_105710_households.csv", skip = 1, na.strings = c("n.a.", ""))
  regionalTargetDimension <- "EDGE_EUR_ETP" # TODO correct? previously: getOption("regional_target_dimension")

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

  regionalMap <- na.omit(readRDS("regionalmap.rds")[c("iso", regionalTargetDimension)]) # TODO proper regionalmap?

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

  unit2EJ <- c(Mtoe = 4.1868E-2, ktoe = 4.1868E-5, Tj = 1E-6)
  redundantEnduse <- c("space_heating_corrected", "appliances_light")
  redundantCarrier <- "total"


  # ==== PROCESS DATA ==========================
  odyssee <- rbind(households, services)
  colnames(odyssee) <- c("variable", "region", "unit", "period", "value",
                         "note", "title")

  # ISO3 country codes, unit conversion and gap filling
  odyssee <- odyssee %>%
    filter(!is.na(.data[["value"]])) %>%
    mutate(region = countrycode(.data[["region"]], "iso2c", "iso3c"), # TODO possible without countrycode?
           value = .data[["value"]] * unit2EJ[.data[["unit"]]],
           unit = "EJ") %>%
    select(-"note", -"title") %>%
    interpolate_missing_periods(expand.values = TRUE)

  # aggregate to EDGE regions,
  odyssee <- odyssee %>%
    aggregate_map(filter(regionalMap, .data[["iso"]] %in% getRegs(odyssee)),
                  by = c(region = "iso")) %>%
    separate("variable", c("carrier", "sector", "enduse"), c(3, 8)) %>%
    revalue.levels(carrier = revalCarrier,
                   sector  = revalSector,
                   enduse  = revalEnduse) %>%
    filter(!.data[["enduse"]]  %in% redundantEnduse,
           !.data[["carrier"]] %in% redundantCarrier)

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
             mutate(value = 0,
                    carrier = "biotrad")
           odyssee <- rbind(odyssee, biotrad)
         },
         stop(paste("Invalid subtype:", subtype))
  )

  return(as.magpie(odyssee)) # TODO conversion as.magpie correct?
}


#' @importFrom lazyeval interp
#' @importFrom dplyr mutate_ ungroup group_by ungroup across
#' @importFrom tidyr %>%
#' @importFrom stats setNames
#' @importFrom tidyselect all_of
#'
#' @author Antoine Levesque
calcShares <- function(data, colShare, colVal = "value", ignoreColumns = NULL) {
  # TODO is this function necessary?
  cols <- setdiff(colnames(data), c(colShare, colVal, ignoreColumns))
  form <- interp(~ x / sum(x, na.rm = TRUE), x = as.name(colVal))

  # TODO: find a way to replace mutate_ by mutate
  res <- data %>%
    group_by(across(all_of(cols))) %>%
    mutate_(.dots = setNames(list(form), colVal)) %>%
    ungroup()

  return(res)
}
