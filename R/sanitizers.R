
#' Used to generate filter names
#' To relabel a filter, include text to replace (first character vector)
#'   and replacement text (second character vector)
#' See also: ociss/R/ui-input.R
clean_nm <- function(z) {
  reduce2(
    c( "TumRecNum",         "Race1",             
       "AgeDxC_.*",        "RuralUrbanCon2013", 
       "PSite",             "OHStage",     
 "GEOID_County", 
       "Hispanic",          "VitalStatus"
       ), 
    c("Tumor Number", "Race", 
      "Age at Diagnosis", "Rural Urban Continuum", 
      "Primary Site", "SEER Summary Stage", 
      "County",
      "Ethnicity",          "Living/Deceased"
      ), 
    function(a, b, d) gsub(b, d, a),
    .init = z
  )
}

#' Used to clean output table names
#' See: ociss/R/assemble_data.R
clean_table_names <- function(the_table) {
 # browser()
  names(the_table) <- 
    reduce2(
      c(
        "Place", 
        "ZCTA",
        "OCISS_Crude_Count",
        "OCISS_Male",
        "OCISS_Female",
        "OCISS_Local_Stage",
        #"OCISS_Distant_Stage",
        "OCISS_Regional_Stage",
        #"OCISS_Unknown_Stage",
        "OCISS_Deceased", 
        "OCISS_Black",
        "OCISS_NonBlack",
        "OCISS_Hispanic",
        "OCISS_NonHispanic",
        "OCISS_([0-9].*)",
        "acs_age_denom",
        "acs_age_",
        "([0-9][0-9])_([0-9][0-9])",
        "acs_r_black_p",
        "acs_r_nonblack",
        "acs_e_hispanic_p",
        "acs_e_nonhispanic",
        "acs_pov_rate",
        "acs_house_income",
        "lt_14",
        "gt_75",
        "time_to_treatment",
        "time_to_fu",
        "acs_house.*",
        "acs_adi",
		"^everyone$"
        ), 
      c(
        "Municipality",
        "Zip Code",
        "Crude Case Count",
        "Cases: Male",
        "Cases: Female",
        "Cases: Local Stage",
        #"Distant Stage",
        "Cases: Regional Stage",
        #"Unknown Stage",
        "Deceased",
        "Cases: African American",
        "Cases: Non-African American",
        "Cases: Hispanic",
        "Cases: Non-Hispanic",
        "Cases: \\1",
        "Total Population",
        "Population: ",
        "\\1-\\2", 
        "% African American",
        "Population: Non-African American",
        "% Hispanic",
        "Population: Non-Hispanic",
        "Poverty rate",
        "Median Household Income",
        "14 and younger",
        "75 and older",
        "Median Time to Treatment(Days)",
        "Median Follow-up Time(Years)",
        "Median Household Income",
        "% Community in High ADI",
		"Catchment Area"
      ), 
      function(a, b, d) gsub(b, d, a),
      .init = names(the_table)
    )
  #names(the_table)[grepl("%", names(the_table))] <- "%"
  #names(the_table)[grepl("_p$", names(the_table))] <- "%"
  names(the_table)[grepl("Poverty", names(the_table))] <- "Poverty rate (% of individuals living in households < 100% FPL)"
  names(the_table)[grepl("OCISS_Distant_Stage_%", names(the_table))] <- "% Distant Stage"
  names(the_table)[grepl("OCISS_Unknown_Stage_%", names(the_table))] <- "% Unknown Stage"
  names(the_table)[grepl("OCISS_Uninsured_%", names(the_table))] <- "% Uninsured at Diagnosis"
  names(the_table)[grepl("acs_B01001e26_p", names(the_table))] <- "% Female"
  the_table
}

#' This function converts filter names to something easier to read. 
#' See also: ociss/R/filters-printer.R
sanitize_filters <- function(z) {
  reduce2(
    c( "TumRecNum",         "Race1",             
       "AgeDxC_OCISS2",        "RuralUrbanCon2013", 
       "PSite",             "OHStage",     
       "VitalStatus",
       "GEOID_County","CSSSF16"), 
    c("Tumor Sequence", "Race", 
      "Age at Dx", "Rural/Urban", 
      "Primary Site", "SEER Summary Stage",
      "Living/Deceased", 
      "County","Breast Cancer Receptor Status"), 
    function(a, b, d) gsub(b, d, a),
    .init = z
  )
}

