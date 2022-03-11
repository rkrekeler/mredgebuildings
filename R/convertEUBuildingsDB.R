#' convertEUBuildingsDB
#'
#' Convert to common units.
#'
#' @param subtype character <category>.<variable>
#' @param x MAgPIE object with data from EU Buildings Database
#' @return clean MAgPIE object with
#'
#' @author Robin Krekeler
#'
#' @importFrom quitte inline.data.frame
#' @importFrom magclass getNames getNames<- getItems getItems<- getSets<-
#' @importFrom madrat toolCountryFill
#' @export

convertEUBuildingsDB <- function(x, subtype) {

  data <- x

  # split subtype
  category <- strsplit(subtype, ".", fixed = TRUE)[[1]][1]
  variable <- strsplit(subtype, ".", fixed = TRUE)[[1]][2]

  # unit conversion
  unitConversion <- inline.data.frame(
    "from;     to;    factor",
    "Mm²;      m2;    1", # per dwelling
    "m²;       m2;    1",
    "thousand; 1;     1E3",
    "%;        1;     1E-2",
    "index;    1;     1",
    "No-of;    1;     1",
    "Mtoe;     EJ;    0.041868",
    "MJ/kg;    MJ/kg; 1"
  )
  uTail <- function(u) paste0("_", u, "$")
  uConv <- function(unit, col) {
    unlist(unitConversion[unitConversion$from == unit, col])
  }
  units <- unique(gsub("^.*_", "", getNames(data)))
  if (!all(units %in% unitConversion$from)) {
    stop("There is no conversion defined for the following units: ",
         paste(setdiff(units, unitConversion$from)), collapse = ", ")
  }
  for (unit in units) {
    varsUnit <- grep(uTail(unit), getNames(data), value = TRUE)
    data[, , varsUnit] <- data[, , varsUnit] * uConv(unit, "factor")
    getNames(data) <- gsub(uTail(unit), paste0("_", uConv(unit, "to")),
                           getNames(data))
  }

  # drop EU sum and rename regions
  data <- data["EU", , invert = TRUE]
  getItems(data, 1) <- toolCountry2isocode(getItems(data, 1))

  # fill missing regions with NA
  data <- toolCountryFill(data, verbosity = 2)

  if (identical(variable, as.character(NA))) {
    return(data)
  }

  # special conversion of selected variables
  switch(subtype,
    `BuildingStockCharacteristics.residentialVintageShares` = {
      getNames(data) <- gsub(" and ", "-",
                             gsub("Share of dwellings built |between |_1$", "",
                                  gsub("before ", "<",
                                       gsub("after ", ">", getNames(data)))))
      getSets(data)[3] <- "cohort"
    },
    `BuildingStockCharacteristics.shareHouseholdSize` = {
      # the shares perfectly add up to 1, so 'more than 5' means '5 or more'
      getNames(data) <- gsub(
        "^(Number|Share) of dwelling(s|) |occupied by | persons.*|_1$", "",
         gsub("with single-person households", "1",
              gsub("more than ", ">=", getNames(data))))
      getSets(data)[3] <- "householdSize"
    },
      stop("'", subtype, "' is an invalid subtype.")
  )


  return(data)
}
