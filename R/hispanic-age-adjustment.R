
#' Calculate the age-adjusted incidence rate of hispanic, with confidence intervals 
age_adj_inc_rate_hisp<- function(con, input) {
  
  cond = "( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )"

    ## Need to use Direct method
    if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {

      r <-
        full_join(
          reference_hispanic_pop(input),
          ociss_case_count(con, input, cond, FALSE)
        )
      
      ## if everyone, '0' for age count and '1' for GEOID if it's 'NA'
      if (input$gis_group == "everyone")
      {
        r$GEOID = "1"
        r$case_count = with(r, ifelse(is.na(case_count),0,case_count))
      }
      
      sir_df = get_direct_df(input, r)
    }
    ## Indirect method
    else
    {
      ##
      # if any filters other than primary or sex specified, only crude
      aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df(con, input, cond,aa_df)
    }

  names(sir_df) <- c(input$gis_group, "Hispanic Age-Adjusted Incidence Rate","Hispanic IR LCI", "Hispanic IR UCI")
  
  if (input$gis_group == "everyone")
    names(sir_df) <- c("joiner", "Hispanic Age-Adjusted Incidence Rate","Hispanic IR LCI", "Hispanic IR UCI")
  
  sir_df  
  
}

#' Calculate the age-adjusted incidence rate of Non-hispanic, with confidence intervals 
age_adj_inc_rate_nonhisp<- function(con, input) {
  
  cond = "( Hispanic in (0, 9) )"

    ## Need to use Direct method
    if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
      r <-
        full_join(
          reference_nonhispanic_pop(input),
          ociss_case_count(con, input, cond, FALSE)
        )
      
      ## if everyone, '0' for age count and '1' for GEOID if it's 'NA'
      if (input$gis_group == "everyone")
      {
        r$GEOID = "1"
        r$case_count = with(r, ifelse(is.na(case_count),0,case_count))
      }
      
      sir_df = get_direct_df(input, r)
    }
    ## Indirect method
    else
    {
      ##
      # if any filters other than primary or sex specified, only crude
      aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df(con, input, cond,aa_df)
    }

  names(sir_df) <- c(input$gis_group, "Non-Hispanic Age-Adjusted Incidence Rate","Non-Hispanic IR LCI", "Non-Hispanic IR UCI")
  
  if (input$gis_group == "everyone")
    names(sir_df) <- c("joiner", "Non-Hispanic Age-Adjusted Incidence Rate","Non-Hispanic IR LCI", "Non-Hispanic IR UCI")
  
  sir_df  
  
}

#' return specific reference population counts--these come from the ACS data
reference_hispanic_pop <- function(input) {
  if (input$gis_group == "everyone") {
    
    acs_as <- ociss::acs_specific_pop_ages$county %>% 
      filter(pop == 'Hispanic or Latino' & age != 'total')
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]
    r <- agg_sum(pop_count ~ age, acs_as)  
    r <- data.frame("age"=r$age,GEIOID="1", "pop_count"=r$pop_count)
    
  } 
  else {

    ## Default : empty population
    #acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #acs_as$pop_count <- 0     
    
    ## no race, no ethnicity, use hispanic population  
    if(is.null(input$filter_Race1) && is.null(input$filter_Hispanic))
    {
      acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>% 
        filter (pop == 'Hispanic or Latino')
    }
    ## both race AND ethnicity
    else if(!is.null(input$filter_Race1) & !is.null(input$filter_Hispanic))
    {
      acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      acs_as$pop_count <- 0  
    }
    ## no race, ethnicity : Single Hispanic or  Multiple with Hispanic
    else if(is.null(input$filter_Race1) && !is.null(input$filter_Hispanic)) 
    {
      if(input$filter_Hispanic=='Hispanic or Latino' || length(input$filter_Hispanic)==2)
        acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>% 
          filter(pop == 'Hispanic or Latino')
      else
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
        acs_as$pop_count <- 0       
      }
    }
    ## race, no ethnicity
    else if(!is.null(input$filter_Race1) && is.null(input$filter_Hispanic))
    {
      if(input$filter_Race1 == 'White')
      acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>% 
        filter(pop == 'White Hispanic')
      else
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
        acs_as$pop_count <- 0  
      }
    }
    else
    {
      acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      acs_as$pop_count <- 0  
    }
    
    

    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL
    
    print(unique(acs_as[["pop"]]))
    acs_as[["pop"]] <- NULL
    
    r <- acs_as[acs_as$age != "total",]
    r[["GEOID"]] <- as.character(r[["GEOID"]])
    r <- agg_sum(pop_count ~ age + GEOID, acs_as)
  } 
  
  r 
}

reference_nonhispanic_pop <- function(input) {
  if (input$gis_group == "everyone") {
    
    acs_as <- ociss::acs_specific_pop_ages$county %>% 
      filter(pop == 'Non-Hispanic or Latino' & age != 'total')
    
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]
    
    
    r <- agg_sum(pop_count ~ age, acs_as)  
    r <- data.frame("age"=r$age,GEIOID="1", "pop_count"=r$pop_count)
    
  } 
  else {
    ## Default : empty population
   #acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #acs_as$pop_count <- 0   
    
    ## no race, no ethnicity, use general population  
    if(is.null(input$filter_Race1) && is.null(input$filter_Hispanic))
    {
      acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>% 
        filter (pop == 'Non-Hispanic or Latino')
    }
    ## Race AND Ethnicity
    else if(!is.null(input$filter_Race1) && !is.null(input$filter_Hispanic))
    {
      ## if Race AND Ethnicity entered
      if(input$filter_Race1 == 'White' & input$filter_Hispanic =='Non-Hispanic or Latino')
      {
        acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>% 
          filter(pop %in% 'White Non-Hispanic')
      }
      else
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
        acs_as$pop_count <- 0       
      }
    }
    ## no race, ethnicity :  Single Non-Hispanic or Latino or  Multiple with Non-Hispanic or Latino
    else if(is.null(input$filter_Race1) && !is.null(input$filter_Hispanic)) 
    {
      #browser()
      if(input$filter_Hispanic=='Non-Hispanic or Latino')
      acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>% 
        filter(pop == 'Non-Hispanic or Latino')
      else
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
        acs_as$pop_count <- 0       
      }
      #browser()
    }
    ## race, no ethnicity
    else if(!is.null(input$filter_Race1) && is.null(input$filter_Hispanic))
    {
      if(input$filter_Race1 == 'White')
      acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>% 
        filter(pop == 'White Non-Hispanic')
      else
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
        acs_as$pop_count <- 0       
      }
    }
    #else
    #{
    #  acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #  acs_as$pop_count <- 0       
    #}
    
    
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL
    
    print(unique(acs_as[["pop"]]))
    acs_as[["pop"]] <- NULL
    
    r <- acs_as[acs_as$age != "total",]
    #r[["GEOID"]] <- as.character(r[["GEOID"]])
    r <- agg_sum(pop_count ~ age + GEOID, acs_as)
  } 
  
  r
}


