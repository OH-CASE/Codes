
#' This function puts all filter criteria into a neat format for display
filters_printer <- function(input) {
  inputs <- reactiveValuesToList(input)
  fs <- grep("^filter", names(inputs), value = T)
  fs <- gsub("-selecti.*", "", fs) %>% unique()
  these <- 
      c(
        "filter_RuralUrbanCon2013", 
        "filter_GEOID_County", 
        "filter_AgeDxC_OCISS2",
        "filter_Sex", 
        "filter_Race1", 
        "filter_Hispanic", 
        "filter_PSite", 
        "filter_CSSSF16",
        "filter_OHStage"
      )
  fs <- fs[match(these, fs)]
  #message(paste("Names of inputs are", paste(fs, collapse = ", \n")))
  f_txts <- c()
  #if (length(fs) == 0) { return(as.character(tags$br("These data use no filtering criteria."))) }
  for (i in seq_along(fs)) {
    f_nm <- paste(gsub("^filter_", "", fs[[i]]), "=") %>% sanitize_filters()
    fs_vs <- inputs[[fs[[i]]]]
    #if (length(fs_vs) == 0) vs <- "None" else vs <- fs_vs
    f_txt <- 
      paste(f_nm, paste(fs_vs, collapse = "; "), "", collapse = "")
    if (length(fs_vs) == 0) { 
    } else {
      f_txts <- c(f_txts, as.character(tags$br(f_txt)))
    }
  }
  fst <- as.character(tags$br("These data use the following inclusion criteria:"))
  dates_stuff <- 
    as.character(tags$br(paste0("Date range = ", inputs[["date_range"]], " - ", inputs[["date_range_end"]])))
  c(
    fst, 
    dates_stuff,
    f_txts
    )
}
