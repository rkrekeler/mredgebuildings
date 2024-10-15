#' Read residential technology data from IEA
#'
#' techno-economic data of residential heating technologies
#'
#' @author Robin Hasse
#'
#' @source https://www.iea.org/articles/are-renewable-heating-options-cost-
#'   competitive-with-fossil-fuels-in-the-residential-sector
#'
#' @importFrom dplyr %>% select
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie collapseDim

readIEAResidentialTechnologies <- function() {

  # read data from regional files and write in data frame
  data <- do.call(rbind, lapply(list.files(pattern = "\\.txt$"), function(file) {
    reg <- sub("\\.txt$", "", file)
    txt <- readLines(file)
    txt <- trimws(txt)
    techIndex <- cumsum(txt == "")
    dataRegion <- do.call(rbind, lapply(unique(techIndex), function(i) {
      lines <- txt[techIndex == i]
      lines <- lines[lines != ""]
      tech <- lines[1]
      params <- tail(lines, -1)
      dataTech <- do.call(rbind, lapply(params, function(param) {
        tmp <- data.frame(
          variable = sub(" [0-9\\.]* [a-zA-Z]*$", "", param),
          value = as.numeric(sub("^.* ([0-9\\.]*) .*$", "\\1", param)),
          unit = sub("^.* [0-9\\.]* ", "", param)
        )
        return(tmp)
      }))
      dataTech[["technology"]] <- tech
      return(dataTech)
    }))
    dataRegion[["region"]] <- reg
    return(dataRegion)
  }))

  data %>%
    select("region", "variable", "technology", "value", "unit") %>%
    as.quitte() %>%
    as.magpie() %>%
    collapseDim()
}
