

ag <- function(this) {
  check_this <- function(a) {
    fls <- list.files(paste0("ociss/", a), pattern = "*.R", include.dirs = FALSE, full.names = TRUE)
    fls %>% 
      set_names(fls) %>% 
      map(compose(suppressWarnings, suppressMessages, readLines)) %>% 
      map(~ grep(this, .x, value = T))
  }

  map(c("R", "data-raw", "inst"), check_this) %>% 
    flatten() %>% 
    Filter(function(a) length(a) != 0, .)
}