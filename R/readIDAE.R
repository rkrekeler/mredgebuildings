#' Read IDAE data for Spain
#'
#' @source http://estadisticas-bombasdecalor.idae.es/

readIDAE <- function(subtype = "bombasDeCalor") {
  switch(subtype,
    bombasDeCalor = {

      files <- list.files(".", "IDAE_bombas_de_calor_\\d{4}.csv")

      # bind all files and take period from file name
      data <- do.call(rbind, lapply(files, function(file) {
        read.csv(file, encoding = "UTF-8", colClasses = "character") %>%
          mutate(period = as.numeric(sub(".*(\\d{4})\\.csv", "\\1", file)))
      }))

      # numeric columns
      for (col in 5:ncol(data)) {
        data[, col] <- as.numeric(sub("\\,", ".", gsub("\\.", "", data[, col])))
      }

      # remove dots from column names
      colnames(data) <- gsub("\\.+", "_", sub("\\.+$", "", colnames(data)))
    },
    stop("Unknown subtype")
  )
}
