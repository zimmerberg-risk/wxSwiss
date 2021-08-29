library(readxl)
library(usethis)
library(data.table)

# https://r-pkgs.org/data.html

## --------------------------- Meta Data ------------------------------
file <- "inst/para/wxPara.xlsx"
sheets <- readxl::excel_sheets(file)

l <- lapply(sheets, function(i){
  n <- paste("smn", i, sep = ".")
  dat <- as.data.table(readxl::read_xlsx(path = file, sheet = i))
  assign(n, dat, envir = rlang::global_env())
  do.call("use_data", list(as.name(n), overwrite = TRUE))
  paste0("#' ", n, "\n", "#'\n", "#' @docType data\n", "#' @keywords dataset\n", "#'\n", "'", n, "'\n")
})
do.call(cat, args = c(l, list(sep = "\n\n")))
