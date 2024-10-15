
readIEA_SHC <- function(subtype = "SolarHeatWorldwide") { # nolint: object_name_linter.

  # list of all report PDFs
  files <- list.files(subtype, pattern = "\\.pdf$", full.names = TRUE)
  names(files) <- sub(".*(20\\d{2}).*", "\\1", files)
  files <- files[order(names(files))]

  # raw text of all files
  raw <- files %>%
    lapply(pdftools::pdf_text)

  tables <- lapply(raw, function(rawfile) {
    lapply(seq_along(rawfile), function(rawpage) {
      page <- strsplit(rawpage, "\\n")
      tableLines <- grepl("(^|\\n) *Table \\d+[\\.,\\:]", rawfile)
    })
    tablePages
    sum(tablePages)
  })


}
