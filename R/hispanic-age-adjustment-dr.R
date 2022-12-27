
 #' Calculate the age-adjusted mortality rate of hispanic, with confidence intervals 
 age_adj_death_rate_hisp<- function(con, input) {
   
   cond = "( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )"
  
     ## Need to use Direct method
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
       
       sir_df = get_direct_df(input, r)
     }
     ## Indirect method
     else
     {
       ##
       # if any filters other than primary or sex specified, only crude
       aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)
       sir_df = get_indirect_df_mortality(con, input, cond,aa_df)
     }
   
   names(sir_df) <- c(input$gis_group, "Hispanic Age-Adjusted Mortality Rate","Hispanic MR LCI", "Hispanic MR UCI")
   
   if (input$gis_group == "everyone")
     names(sir_df) <- c("joiner", "Hispanic Age-Adjusted Mortality Rate","Hispanic MR LCI", "Hispanic MR UCI")

   sir_df  
   
 }
 
  #' Calculate the age-adjusted mortality rate of Non-hispanic, with confidence intervals 
 age_adj_death_rate_nonhisp<- function(con, input) {
   
   cond = "( Hispanic in (0, 9) )"
   
     ## Need to use Direct method
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
       
       sir_df = get_direct_df(input, r)
     }
     ## Indirect method
     else
     {
       ##
       # if any filters other than primary or sex specified, only crude
       aa_df <- map(c(F, T), ~ death_tables(con, input, cond, .x)) %>% reduce(full_join)
       sir_df = get_indirect_df_mortality(con, input, cond,aa_df)
     }     

   names(sir_df) <- c(input$gis_group, "Non-Hispanic Age-Adjusted Mortality Rate","Non-Hispanic MR LCI", "Non-Hispanic MR UCI")

   if (input$gis_group == "everyone")
     names(sir_df) <- c("joiner", "Non-Hispanic Age-Adjusted Mortality Rate","Non-Hispanic MR LCI", "Non-Hispanic MR UCI")

   sir_df  
   
 }
 