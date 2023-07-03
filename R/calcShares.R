#' Historic carrier-Enduse Shares w.r.t Final Energy
#'
#' Merges and transforms the calculated shares from the Datasets:
#' Odyssee
#' IEA_ETP
#' TCEP
#'
#' Enduse shares are extrapolated using a linear regression where the ETP datapoint
#' serves as origin and the TCEP dataset provides the growth factor. Carrier shares
#' are exported w.r.t. to all carriers per region per period. In the case of
#' "carrierCorrection = TRUE", carrier shares are exported w.r.t. their contribution
#' to each individual enduse. The reason for the latter is that the implemantation
#' here is closer to EDGE-B, but didn't work well with further data processing
#' steps (e.g. toolDisaggregate) where EC shares are not necessarily used.
#' Hence, this case is by default disabled.
#'
#' In the thermal case, the enduse "appliances" is transformed to "refrigerators"
#' using the region-specific refrigerator share used in EDGE-B. Higher-resoluted
#' data was not available.
#'
#' @param subtype specifies share
#' @param carrierCorrection allows additional corrections
#' @param feOnly specifies if shares or quantities are returned
#'
#' @note The parameter "feOnly" is only applicable to IEA_ETP and TCEP data,
#' since this is the necessary data to do a full disaggregation of EU and EC
#' data, whereas Odyssee already gives disaggregated data for countries of the
#' European Union.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate as_tibble filter select rename group_by across
#' all_of ungroup %>%
#' @importFrom tidyr replace_na


calcShares <- function(subtype = c("carrier_nonthermal", "carrier_thermal", "enduse_nonthermal", "enduse_thermal"),
                       carrierCorrection = FALSE,
                       feOnly = FALSE) {
  # PARAMETERS -----------------------------------------------------------------
  subtype <- match.arg(subtype)

  shareOf  <- strsplit(subtype, "_")[[1]][1]
  thermVar <- strsplit(subtype, "_")[[1]][2]



  #---Corrections for specific carrier shares
  correction <- inline.data.frame(
    "region;        enduse;                                carrier; value",
    ".*;            water_heating;                         heat;    0",
    "(OCD|EUR|USA); (water_heating|cooking);               biomod;  0",
    "EUR;           (water_heating|cooking);               biotrad; 0",
    "RUS;           space_heating;                         heat;    0.60",
    "RUS;           (water_heating|cooking);               natgas;  0.70",
    "RUS;           (water_heating|cooking);               elec;    0.07",
    "RUS;           space_heating;                         elec;    0.03",
    "RUS;           (space_heating|water_heating|cooking); coal;    0.04",
    "RUS;           (space_heating|water_heating|cooking); petrol;  0.10",
    "IND;           (space_heating|water_heating);         biotrad; 0.80",
    "IND;           (space_heating|water_heating|cooking); coal;    0.10",
    "IND;           (space_heating|water_heating);         petrol;  0.12",
    "IND;           (space_heating|water_heating);         natgas;  0",
    "IND;           (space_heating|water_heating);         elec;    0.02",
    "IND;           cooking;                               natgas;  0.04",
    "IND;           cooking;                      (biotrad|petrol); 0.50"
    ) %>%
    unite("regex", all_of(c("region", "enduse", "carrier")), sep = "\\.")


  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- c("appliances-natgas",
               "appliances-petrol",
               "appliances-biomod",
               "appliances-biotrad",
               "appliances-coal",
               "appliances-heat",
               "refrigerators-natgas",
               "refrigerators-petrol",
               "refrigerators-biomod",
               "refrigerators-biotrad",
               "refrigerators-coal",
               "refrigerators-heat",
               "lighting-biomod",
               "lighting-biotrad",
               "lighting-coal",
               "lighting-heat",
               "cooking-heat",
               "space_cooling-heat",
               "space_cooling-biomod",
               "space_cooling-biotrad",
               "space_cooling-coal",
               "space_cooling-natgas",
               "space_cooling-petrol")


  # Percentage of Appliances for Refrigerators
  fridgeShare <- rbind(
    data.frame(RegionCode = "USA", share  = 0.12),
    data.frame(RegionCode = c("EUR", "OCD", "RUS", "JPN"), share = 0.17),
    data.frame(RegionCode = c("CHN", "IND", "NCD", "AFR", "MIE", "OAS"), share = 0.3))



  # READ-IN DATA ---------------------------------------------------------------

  # Main Datasets
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                      aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)

  sharesOdyssee <-
    calcOutput("ShareOdyssee", subtype = shareOf, aggregate = FALSE) %>%
    as.quitte()

  sharesETP <-
    calcOutput("ShareETP", subtype = shareOf, feOnly = feOnly, aggregate = FALSE) %>%
    as.quitte()

  if (shareOf == "enduse") {
    if (feOnly) {
      dataTCEP <- readSource("TCEP") %>%
        as.quitte()
    } else {
      sharesTCEP <- calcOutput("ShareTCEP", aggregate = FALSE) %>%
        as.quitte()
    }
  }


  # EDGE mapping
  edgeMap <- toolGetMapping("regionmappingEDGE.csv", type = "regional")

  # ETP mapping
  regmapping <- toolGetMapping("regionmappingIEA_ETP.csv", where = "mappingfolder", type = "regional")



  # FUNCTIONS ------------------------------------------------------------------

  normalize <- function(df, shareOf) {
    df <- df %>%
      group_by(across(-all_of(c(shareOf, "value")))) %>%
      mutate(value = .data[["value"]] / sum(.data[["value"]], na.rm = TRUE)) %>%
      ungroup()
  }


  addThermal <- function(df, mapping, fridgeShare) {
    df <- df %>%
      filter(.data[["enduse"]] != "lighting") %>%
      left_join(edgeMap %>%
                  select(-"RegionCodeEUR", -"RegionCodeEUR_ETP", -"X") %>%
                  rename(region = "CountryCode") %>%
                  left_join(fridgeShare, by = "RegionCode") %>%
                  select(-"RegionCode"),
                by = "region") %>%
      mutate(value = ifelse(.data[["enduse"]] != "appliances",
                            .data[["value"]],
                            .data[["value"]] * .data[["share"]]),
             enduse = ifelse(.data[["enduse"]] == "appliances",
                             "refrigerators",
                             as.character(.data[["enduse"]]))) %>%
      select(-"share")

    if (!feOnly) {
      df <- normalize(df, shareOf)
      return(df)
    }

    return(df)
  }


  extrapolateMissingPeriods <- function(chunk, key, slopeOfLast = 5) {
    # remove NAs
    outChunk <- chunk
    chunk <- chunk[!is.na(chunk$value), ]
    upperPeriod <- max(chunk$period)
    lowerPeriod <- min(chunk$period)

    # linear regression at upper and lower end
    mUpper <- lm(value ~ period, tail(arrange(chunk, "period"), slopeOfLast))
    mLower <- lm(value ~ period, head(arrange(chunk, "period"), slopeOfLast))

    # extrapolate both ends
    outChunk[["valueUpper"]] <- predict(mUpper, newdata = outChunk["period"])
    outChunk[["valueLower"]] <- predict(mLower, newdata = outChunk["period"])

    # shift extrapolation to match last data points
    outChunk[["valueUpper"]] <- outChunk[["valueUpper"]] *
      as.numeric(outChunk[outChunk$period == upperPeriod, "value"] /
                outChunk[outChunk$period == upperPeriod, "valueUpper"])
    outChunk[["valueLower"]] <- outChunk[["valueLower"]] *
      as.numeric(outChunk[outChunk$period == lowerPeriod, "value"] /
                outChunk[outChunk$period == lowerPeriod, "valueLower"])

    # fill missing lower/upper ends
    outChunk[["value"]] <- ifelse(outChunk[["period"]] > max(chunk$period),
                                  outChunk[["valueUpper"]],
                                  outChunk[["value"]])
    outChunk[["value"]] <- ifelse(outChunk[["period"]] < min(chunk$period),
                                  outChunk[["valueLower"]],
                                  outChunk[["value"]])
    outChunk[["valueUpper"]] <- NULL
    outChunk[["valueLower"]] <- NULL

    return(outChunk)
  }



  # PROCESS DATA ---------------------------------------------------------------

  # Adjust ETP Mapping
  regmapping <- regmapping %>%
    mutate(EEAReg = ifelse(.data[["EEAReg"]] == "rest",
                           .data[["OECD"]],
                           .data[["EEAReg"]]))


  if (shareOf == "enduse") {
    # Analogous to EDGE-B, the TCEP Dataset is used to determine a growth / evolution
    # factor of a given Enduse Share. This factor will be used to extrapolate a
    # second Datapoint for the ETP Dataset (highest priority), enabling a linear
    # regression of the time evolution.

    if (!feOnly) {
      # Extrapolate ETP FE Data
      evolutionFactor <- sharesTCEP %>%
        left_join(regmapping %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "region") %>%
        group_by(across(all_of(c("regionAgg", "enduse", "period")))) %>%
        summarise(value = sum(.data[["value"]], na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(across(all_of(c("regionAgg", "enduse")))) %>%
        summarise(factor = .data[["value"]] / dplyr::lead(.data[["value"]])) %>%
        ungroup() %>%
        filter(!is.na(.data[["factor"]])) %>%
        left_join(regmapping %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "regionAgg") %>%
        select(-"regionAgg")


      dataETPstart <- sharesETP %>%
        left_join(evolutionFactor, by = c("region", "enduse")) %>%
        mutate(value = .data[["value"]] * .data[["factor"]],
               period = 2000) %>%
        select(-"factor") %>%
        filter(!is.na(.data[["value"]]))


      dataETPfull <- rbind(dataETPstart,
                           sharesETP %>%
                             filter(!is.na(.data[["value"]])))


      dataETPfull <- dataETPfull %>%
        select(-"unit") %>%
        quitte::factor.data.frame() %>%
        as.quitte() %>%
        interpolate_missing_periods(period = seq(1990, 2020)) %>%
        group_by(across(all_of(c("region", "enduse")))) %>%
        group_modify(extrapolateMissingPeriods) %>%
        ungroup() %>%
        select("region", "period", "enduse", "value")


      # NOTE: The linear regression might lead to negative values which will be
      # filled up with zeros and then re-normalized.
      # (However, this is a very practical fix...)

      sharesETPfull <- dataETPfull %>%
        mutate(value = ifelse(.data[["value"]] < 0, 0, .data[["value"]]))


      # Merge Data
      data <- sharesOdyssee %>%
        select("region", "period", "enduse", "value") %>%
        left_join(sharesETPfull, by = c("region", "period", shareOf)) %>%
        mutate(value = ifelse(is.na(.data[["value.x"]]),
                              .data[["value.y"]],
                              .data[["value.x"]])) %>%
        select(-"value.x", -"value.y")
    } else {
      # Extrapolate ETP FE Data
      evolutionFactor <- dataTCEP %>%
        left_join(regmapping %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "region") %>%
        group_by(across(all_of(c("regionAgg", "enduse", "period")))) %>%
        summarise(value = sum(.data[["value"]], na.rm = TRUE)) %>%
        ungroup() %>%
        group_by(across(all_of(c("regionAgg", "enduse")))) %>%
        summarise(factor = .data[["value"]] / dplyr::lead(.data[["value"]])) %>%
        ungroup() %>%
        filter(!is.na(.data[["factor"]])) %>%
        left_join(regmapping %>%
                    select("CountryCode", "EEAReg") %>%
                    rename(region = "CountryCode",
                           regionAgg = "EEAReg"),
                  by = "regionAgg") %>%
        select(-"regionAgg")


      dataETPstart <- sharesETP %>%
        left_join(evolutionFactor, by = c("region", "enduse")) %>%
        mutate(value = .data[["value"]] * .data[["factor"]],
               period = 2000) %>%
        select(-"factor") %>%
        filter(!is.na(.data[["value"]]))


      dataETPfull <- rbind(dataETPstart,
                             sharesETP %>%
                               filter(!is.na(.data[["value"]])))


      dataETPfull <- dataETPfull %>%
        select(-"unit") %>%
        quitte::factor.data.frame() %>%
        as.quitte() %>%
        interpolate_missing_periods(period = seq(1990, 2020)) %>%
        group_by(across(all_of(c("region", "enduse")))) %>%
        group_modify(extrapolateMissingPeriods) %>%
        ungroup() %>%
        select("region", "period", "enduse", "value")


      # NOTE: The linear regression might lead to negative values which will be
      # filled up with zeros and then re-normalized.
      # (However, this is a very practical fix...)

      dataETPfull <- dataETPfull %>%
        mutate(value = ifelse(.data[["value"]] < 0, 0, .data[["value"]]))

      data <- dataETPfull
    }
  }



  #---Carrier Resolution needs special Attention
  if (shareOf == "carrier") {
    # Merge Data
    data <- sharesOdyssee %>%
      select(-"unit", -"variable", -"model", -"scenario") %>%
      left_join(sharesETP, by = c("region", "period", shareOf)) %>%
      mutate(value = ifelse(is.na(.data[["value.x"]]),
                            .data[["value.y"]],
                            .data[["value.x"]])) %>%
      select(-"value.x", -"value.y", -"unit", -"variable", -"model", -"scenario")

    # Interpolate Data
    data <- data %>%
      interpolate_missing_periods(period = seq(1990, 2020), expand.values = TRUE)


    if (carrierCorrection == TRUE) {
      # The Carrier resolution will be extended by a further added resolution for
      # Enduses, which are however not disaggregated. This only serves the purpose
      # of facilitating specific corrections.
      # However, this made the handling in toolDisaggregate more error-prone.

      # NOTE: I haven't followed this approach any further since toolDisaggregate
      # was changed to only take enduse data from an external dataset for its
      # calculation. Hence, "carrierCorrection = FALSE" by default.

      # Add EDGE Mapping
      data <- left_join(data,
                        edgeMap %>%
                          select(-"RegionCodeEUR", -"RegionCodeEUR_ETP", -"X") %>%
                          rename(region = "CountryCode"),
                        by = "region")


      # Load Enduses
      enduses <- calcOutput("ShareOdyssee", subtype = "enduse", aggregate = FALSE) %>%
        as.quitte(na.rm = TRUE) %>%
        select("enduse") %>%
        unique()


      # Cross-join Enduses to Carriers to give further level of resolution
      data <- dplyr::cross_join(data, enduses)

      # Re-normalize to give Enduse-specific Carrier Share
      data <- normalize(data, shareOf)



      #---Correct some specific EC-EU combinations

      # Specific Exclusion
      data <- data %>%
        unite(col = "EUEC", .data[["enduse"]], .data[["carrier"]], sep = "-", remove = FALSE) %>%
        # anti_join(data.frame("EUEC" = c(exclude, excludeNEW)), by = "EUEC") %>%
        mutate(value = ifelse(.data[["EUEC"]] %in% exclude,
                              0,
                              .data[["value"]])) %>%
        select(-"EUEC")


      # (1) Make "space_cooling" and "appliances" fully electric
      # (2) If only one of "water_heating", "space_heating" is non-empty, use  one for the other
      data <- data %>%
        spread("enduse", "value") %>%
        mutate(space_cooling = ifelse(.data[["carrier"]] == "elec", 1, 0),
               appliances    = ifelse(.data[["carrier"]] == "elec", 1, 0)) %>%
        mutate(space_heating = ifelse(is.na(.data[["space_heating"]]) & !is.na(.data[["water_heating"]]),
                                      .data[["water_heating"]],
                                      .data[["space_heating"]]),
               water_heating = ifelse(is.na(.data[["water_heating"]]) & !is.na(.data[["space_heating"]]),
                                      .data[["space_heating"]],
                                      .data[["water_heating"]])) %>%
        gather("enduse", "value", -any_of(colnames(data))) %>%
        mutate(value = replace_na(.data[["value"]], 0))


      # Make more specific corrections
      for (i in rownames(correction)) {
        data <- data %>%
          mutate(value = unlist(ifelse(
            grepl(correction[i, "regex"],
                  paste(.data[["RegionCode"]], .data[["enduse"]], .data[["carrier"]],
                        sep = ".")),
            correction[i, "value"],
            .data[["value"]]
          )))
      }


      # Remove Mapping Column
      data <- select(data, -"RegionCode")

      # Re-Normalize
      data <- normalize(data, shareOf)
    }
  }


  #---Thermal Part
  if (thermVar == "thermal") {
    if (shareOf == "enduse") {
      data <- data %>%
        addThermal(edgeMap, fridgeShare)
    }
  }


  #---Weights: Regional Shares of FE
  # Weights consist of the share of each region's FE demand relative to global FE demand.
  # FE data is taken from ieaIO.

  regShare <- ieaIO %>%
    mutate(carrier = .data[["variable"]]) %>%
    select("region", "period", "carrier", "value") %>%
    group_by(across(all_of(c("period", "region")))) %>%
    filter(!all(.data[["value"]] == 0)) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(across(all_of(c("region")))) %>%
    complete(period = unique(data$period)) %>%
    ungroup() %>%
    interpolate_missing_periods(expand.values = TRUE) %>%
    group_by(across(all_of(c("period")))) %>%
    mutate(value = proportions(.data[["value"]]))


  # OUTPUT ---------------------------------------------------------------------

  regShare <- regShare %>%
    as.magpie() %>%
    collapseDim() %>%
    toolCountryFill(1, verbosity = 2)


  data <- data %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)


  if (feOnly) {
    return(list(x = data,
                weight = NULL,
                unit = "EJ",
                min = 0,
                description = "Energy of carrier or end use in buildings demand"))
  } else {
    return(list(x = data,
                weight = NULL,
                unit = "1",
                min = 0,
                max = 1,
                description = "Share of carrier or end use in buildings demand"))
  }
}
