#' Read EHI Heating Market Report
#'
#' PDF market reports on Renewables in Europe
#'
#' @source https://www.eurobserv-er.org/
#' @note I download the report and snip a png of the table. Then I extract the
#' data with [this online tool](https://extracttable.com/) and save it as a
#' tabular csv in the source folder. The Tool is not fully reliable. This
#' function throws an error if values don't sum up. You then have to correct the
#' csv file manually.
#'
#' @param subtype >report series>.<variable>
#' @returns magpie object
#'
#' @author Robin Hasse
#'
#' @importFrom utils read.csv capture.output
#' @importFrom dplyr %>% select mutate group_by filter rename
#' @importFrom rlang .data
#' @importFrom tidyr gather
#' @importFrom quitte as.quitte
#' @importFrom magclass as.magpie
#' @export

readEurObservER <- function(subtype) {

  # FUNCTIONS ------------------------------------------------------------------


  # PREPARE --------------------------------------------------------------------

  # split subtype
  report   <- gsub("\\..*$", "", subtype)
  variable <- gsub("^.*\\.", "", subtype)

  # pick file
  file <- switch(report,
    `2021`    = "heatingMarketReport2021.csv",
    `2021raw` = "EHI_Heating_Market_Report_2021.pdf",
    stop("Invalid subtype. The report '", report, "' is not available."))



  # PROCESS DATA ---------------------------------------------------------------

  switch(subtype,
    `2021.stockHeaters` = {
      var <- "^Installed stock of heaters\\|"
      data <- read.csv(file) %>%
        filter(grepl(var, .data[["variable"]])) %>%
        mutate(variable = gsub(var, "", .data[["variable"]]))
    },
    `2021raw.stockHeaters` = {

      # extract pages with data for each region
      txt <- pdf_text(file)
      txt <- txt[grepl("Installed stock of heaters in .* in", txt)]
      regions <- lapply(txt, function(page) {
        idx <- regexec("Installed stock of heaters in (.*?) in \\d{4}", page)[[1]]
        region <- substr(page, idx[2], idx[2] + attr(idx, "match.length")[2] - 1)
        gsub("\\n *and ", ", ", region)
      })
      txt <- as.list(txt)
      names(txt) <- regions

      # filter data from plots
      txt2 <- lapply(txt, function(page) {
        # browser()
        # remove unnecessary lines
        textAbove <- gregexpr("\\n[ a-zA-Z0-9]*Installed stock of heaters in .* in \\d{4}((, | and )\\d{4})*", page)[[1]]
        graph <- substr(page, textAbove[1] + 1, nchar(page))
        # graph <- gsub(".*Installed stock of heaters in .* in \\d{4}((, | and )\\d{4})*", "", page)
        # graph <- gsub(".*THOUSANDS\\n*", "", graph)
        # graph <- gsub("^\\n*", "", graph)
        graph <- gsub("\\n+", "\n", graph)
        graph <- gsub("\\n *\\d{2}\\n$", "", graph)

        # trim lines to remove y-axis
        yLabels <- gregexpr("\n +0\n", graph)[[1]]
        yLabels <- substr(graph, yLabels[1], yLabels[1] + attr(yLabels, "match.length") - 1)
        yLabelWidth <- nchar(yLabels) - 2
        graph <- gsub(paste0("(^|\\n)[ 0-9\\.]{0,", yLabelWidth + 1, "}"), "\n", graph)

        # remove x-axis
        graph <- gsub("(\\n +(15|17|19)){12,}(\n +20){12,}", "\n", graph)

        # extract numbers from left to right
        dataPos <- gregexpr(" *[0-9\\.]+\\n", graph)[[1]]
        data <- lapply(seq_along(dataPos), function(i) {
          substr(graph, dataPos[i], dataPos[i] + attr(dataPos, "match.length")[i] - 1)
        })
        dataPosFromLeft <- nchar(gsub("[0-9\\n\\.]", "", data))
        data <- data[order(dataPosFromLeft)]
        data <- as.numeric(gsub(" *([0-9]+)\n", "\\1", gsub("\\.", "", data)))

        # extract heater types
        xLabels <- gregexpr(" *[A-Z].*\\n( *[a-zA-Z \\-]*){0,}( *[a-zA-Z \\-]*)", graph)[[1]]
        xLabels <- substr(graph, xLabels[1], xLabels[1] + attr(xLabels, "match.length") - 1)
        xLabels <- strsplit(xLabels, "\\n")[[1]]
        xLabelPos <- gregexpr("[A-Z]", xLabels[1])[[1]]
        xLabels <- do.call("paste", lapply(xLabels, function(ln) {
          unlist(lapply(seq_along(xLabelPos), function(i) {
            str <- substr(ln, xLabelPos[i] - 1, ifelse(i < length(xLabelPos), xLabelPos[i + 1] - 1, nchar(ln)))[[1]]
            gsub("^ *| *$", "", str)
          }))
        }))
        xLabels <- gsub("^ *| *$", "", xLabels)

        # build data frame
        df <- data.frame(period = rep(c(2015, 2017, 2019), length(xLabels)),
                         heater = rep(xLabels, each = 3),
                         value = data)

        return(df)
      })

    },
    stop("Invalid subtype. The variable '", variable,
         "' is not supported for the report '", report, "'.")
  )

  data <- data %>%
    as.quitte() %>%
    as.magpie()

  return(data)
}
