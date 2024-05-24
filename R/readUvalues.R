#' Historic and projected conductivity of building shell
#'

readUvalues <- function() {

  # PARAMETER ------------------------------------------------------------------

  # revalue IEA ETSAP value column
  revalueVec <- c("Roof.U.value..W.m2K."   = "roof",
                  "Wall.U.value..W.m2K."   = "wall",
                  "Window.U.value..W.m2K." = "window")

  # revalue IEA ETSAP regions
  revalueRegion <- c("Beijing"   = "CHN",
                     "EU"        = "EUR",
                     "Germany"   = "DEU",
                     "S. Sweden" = "SWE",
                     "Shanghai"  = "CHN",
                     "USA"       = "USA")

  regionSelection <- c("Shanghai", "Beijing", "USA")



  # READ-IN DATA ---------------------------------------------------------------

  uvaluesRes <- read.csv("../inputdata/sources/Uvalues/Uvalues_Res_export-eu-buildings-20170503115017.csv",
                         na.strings = "-", comment.char = "#", stringsAsFactors = F)

  uvaluesCom <- read.csv("../inputdata/sources/Uvalues/Uvalues_NonRes_export-eu-buildings-20170503115017.csv",
                         na.strings = "-", comment.char = "#", stringsAsFactors = F)

  uvaluesETSAP <- read.csv2("../inputdata/sources/Uvalues/IEA_ETSAP_Table2.csv", stringsAsFactors = F)


  # PROCESS DATA ---------------------------------------------------------------

  #--- Clean Data

  # Residential
  uvaluesRes <- uvaluesRes %>%
    select(-all_of(c("ISO.code", "Unit", "Source", "Long.source",
                            "Link", "Data.quality", "Comment"))) %>%
    mutate(variable = "uvalue_res") %>%
    gather("period", "value", all_of(grep("X\\d{4}", colnames(uvaluesRes), value = TRUE))) %>%
    mutate(period = as.integer(gsub("X", "", .data[["period"]]))) %>%
    select(-"period") %>%
    na.omit() %>%
    rename("region" = "countryname")


  # Commercial
  uvaluesCom <- uvaluesCom %>%
    select(-all_of(c("ISO.code", "Unit", "Source", "Long.source",
                     "Link", "Data.quality", "Comment"))) %>%
    mutate(variable = "uvalue_com") %>%
    gather("period", "value", all_of(grep("X\\d{4}", colnames(uvaluesCom), value = TRUE))) %>%
    mutate(period = as.integer(gsub("X", "", .data[["period"]]))) %>%
    select(-"period") %>%
    na.omit() %>%
    rename("region" = "countryname")


  # IEA ETSAP
  uvaluesETSAP <- uvaluesETSAP %>%
    removeColNa() %>%
    gather("variable", "value", names(revalueVec)) %>%
    revalue.levels(variable = revalueVec) %>%
    separate(col = "value", into = c("value1", "value2"), sep = "-") %>%
    mutate(value2 = ifelse(is.na(.data[["value2"]]), .data[["value1"]], .data[["value2"]]),
           value1 = as.double(.data[["value1"]]),
           value2 = as.double(.data[["value2"]]),
           value = 0.5 * (.data[["value1"]] + .data[["value2"]])) %>%
    dplyr::select(-"value1", -"value2") %>%
    calc_addVariable("uvalue" = "window * 0.25 + roof * 0.375 + wall * 0.375", only.new = TRUE)

  # Note: ETSAP estimates for DEU and SWE are approximately half as high as the data for
  #       European Regions only. Therefore the values are doubled to get comparable estimates.

  uvaluesETSAP <- uvaluesETSAP %>%
    mutate(value = .data[["value"]] * 2) %>%
    filter(.data[["Region"]] %in% regionSelection) %>%
    revalue.levels(Region = revalueRegion) %>%
    rename("region" = "Region") %>%
    group_by(across(all_of("region"))) %>%
    reframe(uvalue = mean(.data[["value"]]))





}
