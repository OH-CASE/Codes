
#' Retrieve filters from input
get_filters <- function(filters_ui, input) {
  filters <- imap(filters_ui, function(a, z) input[[paste_("filter", z)]])
  filters[!(map_int(filters, length) == 0)]
}

sort_for_presentation <- function(z) {
  if (any(grepl("Other|Unknown", z)) & !(sum(grepl("Local|Distant|Unknown", z)) == 3)) {
    last_option <- z[grepl("Other|Unknown", z)]
    rest_option <- z[!grepl("Other|Unknown", z)]
    return(c(sort(rest_option), last_option))
  } else if (any(grepl("metro area", z))) {
    first <- z[!grepl("[0-9]", z)]
    secon <- z[grepl("250", z)]
    third <- z[grepl("greater", z)]
    return(c(first, secon, third))
  } else if (sum(grepl("Local|Distant|Unknown", z)) == 3) {
    first <- z[!grepl("Local", z)]
    secon <- z[grepl("Regional", z)]
    third <- z[grepl("Distant", z)]
    last_ <- z[grepl("Unknown", z)]
    return(c(first, secon, third, last_))
  } else {
    return(sort(z))
  }
}

selectize_values <- function(vr, vr_nm) {
  t_and_or <- function(z) {
    stringr::str_replace_all(z, c(" Or " = " or ", " And " = " and "))
  }
  
  #unique_var_values$dssg00
  #vr_nm is the list of options
  # before sort
  #[1] "Distant"        "Not applicable" "Local"          "Regional"       "In situ"       
  #[6] "Unstaged"  
  
  vr <- sort_for_presentation(vr)
  #AFter sort
  #[1] "Distant"        "In situ"        "Local"          "Not applicable" "Regional"      
  #[6] "Unstaged"

  
  
  if (any(grepl("nknown", vr))) {
    vr <- c(vr[-grep("nknown|^[oO]ther", vr)], vr[grep("nknown|^[oO]ther", vr)])
  }
  vr_label <- stringr::str_to_title(vr_nm) %>% t_and_or()
  
  if(vr_nm == "OHStage")
    vr = c("In Situ","Local","Regional","Distant","Unstaged","Not Applicable")
  vr_choices <- c("", vr)
  
  if(vr_nm == "GEOID_County")
    vr_choices = c("","15 county Northeast Ohio CCCC Catchment Area","14 county (excluding Cuyahoga) Catchment Area",vr)
  

   
  names(vr_choices) <- c("", stringr::str_to_title(vr_choices[-1])) %>% t_and_or()
  
  
  #message(paste("Making a selector for", vr_label))
  selectInput(paste_("filter", vr_nm), clean_nm(vr_nm), vr_choices, multiple = T, selectize = T)
  
  
  
}



