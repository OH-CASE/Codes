
 #' Calculate the age-adjusted mortality rate, with confidence intervals 
 age_adj_death_rate <- function(con, input) {
   
   cond = NULL

   ## Need to use Direct method
   if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
     r <-
       full_join(
         reference_general_pop(input),
         ociss_death_count(con, input, cond, FALSE)
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
     aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)

     sir_df = get_indirect_df_mortality(con, input, cond, aa_df)
   }
   
   names(sir_df) <- c(input$gis_group, "Age-Adjusted Mortality Rate","MR LCI", "MR UCI")
   
   if (input$gis_group == "everyone")
     names(sir_df) <- c("joiner", "Age-Adjusted Mortality Rate","MR LCI", "MR UCI")
   
   sir_df  
 }


 get_indirect_df_mortality <-function(con, input,cond, aa_df)
 {
   lu <- compose(length, unique)
   #stopifnot (lu(aa_df$age) == lu(aa_df$agg_pop_count))
   
   aa_df[["expected_cases"]] <-
     (aa_df[["agg_case_inc"]] * aa_df[["pop_count"]]) / 1e5 #this 1e5 cancels out, right?
   
   sir_df <-
     agg_sum(case_count ~ GEOID, aa_df) %>%
     full_join(agg_sum(expected_cases ~ GEOID, unique(aa_df[,c("GEOID", "expected_cases")])))
   
   ## crude rate
   sir_df[["overall_inc"]] <-
     1e5 * sum(ociss_death_count(con, input,cond, T)[["agg_case_count"]]) /
     (n_years(input) * sum(reference_pop(input, T)[["agg_pop_count"]]))

   sir_df[["sir"]] <- sir_df[["case_count"]] / sir_df[["expected_cases"]]
   
   sir_df[["age_adj_inc"]] <- sir_df[["sir"]] * sir_df[["overall_inc"]]
   
   sir_df[["age_adj_ci_low"]] <-
     sir_df[["overall_inc"]] * (exp(log(sir_df[["sir"]]) - 1.96 * (1 / sqrt(sir_df[["case_count"]]))))
   
   sir_df[["age_adj_ci_high"]] <-
     sir_df[["overall_inc"]] * (exp(log(sir_df[["sir"]]) + 1.96 * (1 / sqrt(sir_df[["case_count"]]))))
   
   sir_df <- sir_df[,c("GEOID", "age_adj_inc", "age_adj_ci_low", "age_adj_ci_high")]
   
   sir_df[["GEOID"]] <- as.numeric(sir_df[["GEOID"]])
   
   sir_df[,c("age_adj_inc", "age_adj_ci_low", "age_adj_ci_high")] <-
     lapply(
       sir_df[,c("age_adj_inc", "age_adj_ci_low", "age_adj_ci_high")],
       round
     )
   
   sir_df <- sir_df[,c("GEOID", "age_adj_inc", "age_adj_ci_low", "age_adj_ci_high")]
   sir_df
 }

#' retrieve reference and case death counts, with death incidence
death_tables <- function(con, input, cond=NULL, agg = FALSE) {
  if(agg) pp <- function(...) paste0("agg_", ...) else pp <- function(z) z
  
  #browser()
  r <- 
    full_join(
      reference_pop(input, agg),
      ociss_death_count(con, input, cond, agg)
    ) %>% 
    modify_at(c(pp("case_count")), ~ .x / n_years(input))
  r[[pp("case_inc")]] <- (r[[pp("case_count")]] / r[[pp("pop_count")]]) * 1e5
  
  r
}


#' Get numbers of death from ociss data, with user input
ociss_death_count <- function(con, input, cond, agg = FALSE) {
  # capture those whose DateLastContact were during the years of interest,
  # DateLastContact
  # and DateLastContact >= '2015-01-01' and DateLastContact <= '2020-12-31' 
  
  r <- ociss_death_sql(input, gand = "AgeDx", wand=cond) %>% query(con)

    ## If no data, return empty
  if(length(r$case_count)==0)
  {
    #r <- ociss_death_sql(input, gand = "AgeDx",wand=NULL) %>% query(con)
    #r$case_count = 0
    
    r = ociss::ociss_empty;
  }
  
  if (input$gis_group == "everyone") {
    names(r) <- gsub("joiner", "GEOID", names(r)) 
  }
  
  names(r) <- gsub("_County|_ZCTA|_Place", "", names(r))
  
    r[["age"]] <- 
      cut(
        r[["AgeDx"]], 
        breaks = 
          c(-Inf, 4, 9, 14, 17, 19, 24, 29, 
            34, 44, 54, 64,74, 84, Inf), 
        include.lowest = FALSE, 
        labels = 
          c("Under.5.years",      "5.to.9.years", 
            "10.to.14.years",     "15.to.17.years", 
            "18.and.19.years",    "20.to.24.years", 
            "25.to.29.years",     "30.to.34.years", 
            "35.to.44.years", 
            "45.to.54.years", 
            "55.to.64.years",     
            "65.to.74.years", 
            "75.to.84.years", 
            "85.years.and.over")
      ) %>% 
      as.factor() %>% 
      as.character()

  if (agg) {
    r <- agg_sum(case_count ~ age, r) %>% repl_names(c("case_count" = "agg_case_count"))
  } else {
    r[["GEOID"]] <- as.character(r[["GEOID"]])
    r <- agg_sum(case_count ~ age + GEOID, r)
  }
  
  r
}

#' Calculate the Total age-adjusted incidence rate, with confidence intervals 
age_adj_death_rate_total <- function(con, cond, input, rGEOIDs) {
  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
    r <-
      full_join(
        reference_general_pop(input),
        ociss_death_count(con, input, cond, FALSE)
      )
    
    ## if everyone, '0' for age count and '1' for GEOID if it's 'NA'
    if (input$gis_group == "everyone")
    {
      r$GEOID = "1"
      r$case_count = with(r, ifelse(is.na(case_count),0,case_count))
    }
    
    sir_df = get_direct_df_total(input, r,rGEOIDs)
  }
  ## Indirect method
  else
  {
    ##
    # if any filters other than primary or sex specified, only crude
    aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)
    sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
  }
  sir_df
}


age_adj_death_rate_aa_total <- function(con, cond, input,rGEOIDs) {
  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
    r <-
      full_join(
        reference_black_pop(input),
        ociss_death_count(con, input, cond, FALSE)
      )
    
    ## if everyone, '0' for age count and '1' for GEOID if it's 'NA'
    if (input$gis_group == "everyone")
    {
      r$GEOID = "1"
      r$case_count = with(r, ifelse(is.na(case_count),0,case_count))
    }
    
    sir_df = get_direct_df_total(input, r,rGEOIDs)
  }
  ## Indirect method
  else
  {
    ##
    # if any filters other than primary or sex specified, only crude
    aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)
    sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
  }
  sir_df
}

age_adj_death_rate_nonaa_total <- function(con, cond, input,rGEOIDs) {
  
  ## if no race, no ethnicity
  #if(is.null(input$filter_Race1) & is.null(input$filter_Race1))
  #{
    if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
      r <-
        full_join(
          reference_nonblack_pop(input),
          ociss_death_count(con, input, cond, FALSE)
        )
      
      ## if everyone, '0' for age count and '1' for GEOID if it's 'NA'
      if (input$gis_group == "everyone")
      {
        r$GEOID = "1"
        r$case_count = with(r, ifelse(is.na(case_count),0,case_count))
      }
      
      sir_df = get_direct_df_total(input, r,rGEOIDs)
    }
    ## Indirect method
    else
    {
      ##
      # if any filters other than primary or sex specified, only crude
      aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
    }
  # }
  # else if(length(input$filter_Race1)==1)
  # {
  #   if(input$filter_Race1 == 'Black or African American')
  #   {
  #     adj.rate <- NA
  #     lci <- NA
  #     uci <- NA
  #     
  #     sir_df <- data.frame(adj.rate, lci, uci)
  #   }
  # }
  
  sir_df
}

age_adj_death_rate_hispanic_total <- function(con, cond, input,rGEOIDs) {
  ## if no race, no ethnicity
  # if(is.null(input$filter_Race1) & is.null(input$filter_Race1))
  # {
    if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
      r <-
        full_join(
          reference_hispanic_pop(input),
          ociss_death_count(con, input, cond, FALSE)
        )
      
      ## if everyone, '0' for age count and '1' for GEOID if it's 'NA'
      if (input$gis_group == "everyone")
      {
        r$GEOID = "1"
        r$case_count = with(r, ifelse(is.na(case_count),0,case_count))
      }
      
      sir_df = get_direct_df_total(input, r,rGEOIDs)
    }
    ## Indirect method
    else
    {
      ##
      # if any filters other than primary or sex specified, only crude
      aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
    }    
  # }
  # else if(length(input$filter_Race1)==1)
  # {
  #   if(input$filter_Race1 == 'Black or African American')
  #   {
  #     adj.rate <- NA
  #     lci <- NA
  #     uci <- NA
  #     
  #     sir_df <- data.frame(adj.rate, lci, uci)
  #   }
  # }
  sir_df
}

age_adj_death_rate_nonhispanic_total <- function(con, cond, input,rGEOIDs) {
  ## if no race, no ethnicity
  # if(is.null(input$filter_Race1) & is.null(input$filter_Race1))
  # {
    if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
      r <-
        full_join(
          reference_nonhispanic_pop(input),
          ociss_death_count(con, input, cond, FALSE)
        )
      
      ## if everyone, '0' for age count and '1' for GEOID if it's 'NA'
      if (input$gis_group == "everyone")
      {
        r$GEOID = "1"
        r$case_count = with(r, ifelse(is.na(case_count),0,case_count))
      }
      
      sir_df = get_direct_df_total(input, r,rGEOIDs)
    }
    ## Indirect method
    else
    {
      ##
      # if any filters other than primary or sex specified, only crude
      aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
    }

  sir_df
}

