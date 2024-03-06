#' Historic Final Energy Demand by Carrier and Enduse
#'
#' Takes the historic final energy demand by carriers from IEA and disaggregates
#' it into different end uses.
#'
#' @returns data.frame with historic energy demands
#'
#' @author Hagen Tockhorn, Robin Hasse
#'
#' @importFrom dplyr mutate semi_join right_join left_join as_tibble filter
#'   select %>% .data
#' @importFrom madrat toolCountryFill calcOutput toolGetMapping
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

calcFEbyEUEC <- function() {

  # READ-IN DATA ---------------------------------------------------------------

  # FE Data
  ieaIO <- calcOutput("IOEdgeBuildings", subtype = "output_EDGE_buildings",
                      aggregate = FALSE) %>%
    as.quitte(na.rm = TRUE)


  # EU Shares
  sharesEU <- calcOutput("Shares",
                         subtype = "enduse_nonthermal",
                         aggregate = FALSE) %>%
              as.quitte()


  # FE EU Data
  feOdyssee <- calcOutput("ShareOdyssee",
                          subtype = "enduse_carrier",
                          feOnly = TRUE,
                          aggregate = FALSE) %>%
    as.quitte()


  # ETP mapping
  regmapping <- toolGetMapping(name  = "regionmappingIEA_ETP.csv",
                               type  = "regional",
                               where = "mredgebuildings")



  # PARAMETERS -----------------------------------------------------------------

  # Enduse-Carrier combinations which will be systematically excluded
  exclude <- toolGetMapping(name  = "excludeEnduseCarrier.csv",
                            type  = "sectoral",
                            where = "mredgebuildings")



  # PROCESS DATA ---------------------------------------------------------------

  # Reduce the data frames dimensions to the minimal set
  ieaIO <- ieaIO %>%
    semi_join(sharesEU, by = c("region", "period")) %>%
    rename(carrier = "variable")
  sharesEU <- sharesEU %>%
    semi_join(ieaIO, by = c("region", "period"))

  # Prepare toolDisaggregate Input

  # Extract regions with existing disaggregated FE shares
  replaceRegs <- feOdyssee %>%
    filter(!is.na(.data[["value"]])) %>%
    pull("region") %>%
    droplevels() %>%
    unique()

  feOdyssee <- feOdyssee %>%
    select("region", "period", "carrier", "enduse", "value") %>%
    mutate(unit = "fe") %>%
    semi_join(ieaIO, by = c("region", "period", "carrier"))

  # Use EEA regions but split rest into OECD/non-OECD
  regmapping <- regmapping %>%
    mutate(regionAgg = ifelse(.data[["EEAReg"]] == "rest",
                              .data[["OECD"]],
                              .data[["EEAReg"]])) %>%
    select(region = "CountryCode", "regionAgg")

  # reaggregate end use shares to ETP regions
  # TODO: This aggregation should be done within calcOutput but for this, we
  # need a full region mapping that doesn't require the post processing done
  # above. I don't know yet, why in come cases, there are different values in
  # one region but we take the mean in this case. To be checked.
  sharesEU <- sharesEU %>%
    left_join(regmapping, by = "region") %>%
    group_by(across(all_of(c("regionAgg", "period", "enduse")))) %>%
    summarise(value = mean(.data[["value"]]), .groups = "drop") %>%
    rename(region = "regionAgg")

  # Disaggregate FE with EU/EC Shares
  ieaIODis <- ieaIO %>%
    select(-"model", -"scenario", -"unit") %>%
    mutate(unit = "fe") %>%
    toolDisaggregate(enduseShares  = sharesEU,
                     exclude       = exclude,
                     dataDisagg    = feOdyssee,
                     regionMapping = regmapping) %>%
    select("region", "period", "unit", "carrier", "enduse", "value")


  data <- rbind(ieaIODis %>%
                  filter(!(.data[["region"]] %in% replaceRegs)),
                feOdyssee %>%
                  filter(.data[["region"]] %in% replaceRegs))



  # RETURN DATA ----------------------------------------------------------------

  # Pack Data
  ieaIO <- ieaIO %>%
    as.quitte() %>%
    as.magpie() %>%
    toolCountryFill(1, verbosity = 2)

  data <- list(
    x = ieaIO,
    weight = NULL,
    unit = "EJ",
    description = "Historic Final Energy Data from IEA"
  )

  return(data)

}
