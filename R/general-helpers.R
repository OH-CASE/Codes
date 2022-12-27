paste_ <- function(...) paste(..., sep = "_")

p_com <- function(...) paste(..., collapse = ", ")

load_libraries <- function(libs) {
  for (l in libs) library(l, character.only = T)
  invisible(T)
}

writer <- function(dta) {
  function(file) write.csv(dta, file, row.names = FALSE)
}

csv_nm <- function(f) {
  function() paste(f, "-", Sys.Date(), ".csv", sep="")
}

text_message <- function(a) {
  stopifnot(is.character(a) & length(a) == 1)
  message(a)
  a
}

load_app_stuff <- function() {
  source(system.file("app.R", package = "ociss"))
}

run_app <- function() {
  load_app_stuff()
  shinyApp(ui = ui, server = server, options = c(launch.browser = TRUE))
}



