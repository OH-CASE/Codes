
 #' Calculate the age-adjusted mortality rate of AA, with confidence intervals 
 age_adj_death_rate_aa<- function(con, input) {
   
   cond = "( Race1 in ('2') )"
   
   ## Need to use Direct method
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
   
   names(sir_df) <- c(input$gis_group, "AA Age-Adjusted Mortality Rate","AA MR LCI", "AA MR UCI")
   
   if (input$gis_group == "everyone")
     names(sir_df) <- c("joiner", "AA Age-Adjusted Mortality Rate","AA MR LCI", "AA MR UCI")
   
   sir_df  
   
 }
 
 #' Calculate the age-adjusted mortality rate of Non-AA, with confidence intervals 
 age_adj_death_rate_nonaa<- function(con, input) {
   
   cond = "not (Race1 in ( '2' ) )"
   
     ## Need to use Direct method
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
  
   names(sir_df) <- c(input$gis_group, "Non-AA Age-Adjusted Mortality Rate","Non-AA MR LCI", "Non-AA MR UCI")
   
   if (input$gis_group == "everyone")
     names(sir_df) <- c("joiner", "Non-AA Age-Adjusted Mortality Rate","Non-AA MR LCI", "Non-AA MR UCI")
   
   sir_df  
   
 }
