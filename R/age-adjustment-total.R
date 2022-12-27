
#' Calculate the Total age-adjusted incidence rate, with confidence intervals 
age_adj_inc_rate_total <- function(con, cond, input, rGEOIDs) {
  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
    r <-
      full_join(
        reference_general_pop(input),
        ociss_case_count(con, input, cond, FALSE)
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
    aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)
    sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
  }
  sir_df
}

#' Calculate the Total age-adjusted incidence rate, with confidence intervals 
age_adj_inc_rate_aa_total <- function(con, cond, input,rGEOIDs) {
  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
    r <-
      full_join(
        reference_black_pop(input),
        ociss_case_count(con, input, cond, FALSE)
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
    aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)
    
    
    sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
  }
  sir_df
}

age_adj_inc_rate_nonaa_total <- function(con, cond, input,rGEOIDs) {
  ## if no race, no ethnicity
  #if(is.null(input$filter_Race1) & is.null(input$filter_Race1))
  #{
    if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
      r <-
        full_join(
          reference_nonblack_pop(input),
          ociss_case_count(con, input, cond, FALSE)
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
      aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
    }

  sir_df
}

age_adj_inc_rate_hispanic_total <- function(con, cond, input,rGEOIDs) {
  ## if no race, no ethnicity
  #if(is.null(input$filter_Race1) & is.null(input$filter_Race1))
  #{
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
      
      sir_df = get_direct_df_total(input, r,rGEOIDs)
    }
    ## Indirect method
    else
    {
      ##
      # if any filters other than primary or sex specified, only crude
      aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
    }    


  sir_df
}

age_adj_inc_rate_nonhispanic_total <- function(con, cond, input,rGEOIDs) {
  
  ## if no race, no ethnicity
 # if(is.null(input$filter_Race1) & is.null(input$filter_Race1))
#  {
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
      
      sir_df = get_direct_df_total(input, r,rGEOIDs)
    }
    ## Indirect method
    else
    {
      ##
      # if any filters other than primary or sex specified, only crude
      aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)
      sir_df = get_indirect_df_total(con, input, cond,aa_df,rGEOIDs)
    }

  sir_df
}

get_direct_df_total <- function(input, r,rGEOIDs)
{
  r$age[r$age=="15.to.17.years"] = "15.to.19.years"
  r$age[r$age=="18.and.19.years"] = "15.to.19.years"
  
  ## us standard population
  us_as <- usstandard_pop() 
  
  ## with updated age category
  r_new <- full_join(agg_sum(pop_count ~ age+GEOID, r),agg_sum(case_count ~ age+GEOID, r)) %>%
    modify_at("case_count", ~ ifelse(is.na(.x),0,.x)) %>%
    modify_at("case_count", ~ .x / n_years(input)) 
  
  r_new[["case_inc"]] <- (r_new[["case_count"]] / r_new[["pop_count"]]) * 1e5
  ## if pop is zero, then error, fix this error
  r_new <- r_new %>%
    modify_at("case_inc", ~ ifelse(.x=='Inf',0,.x))%>%
    modify_at("case_inc", ~ ifelse(.x=='NaN',0,.x))
  
  df <- full_join(r_new,us_as)
  
  ## add all GEOID counts by age 
  case_df <-
    agg_sum(case_count ~ age, df)
  
  pop_df <-
    agg_sum(pop_count ~ age, df)
  
  usst_pop_df <- unique(df[c("age","usst_count")])
  
  sir_df = merge(merge(case_df,pop_df, by="age"), usst_pop_df, by="age")
  sir_df[["case_inc"]] <- (sir_df[["case_count"]] / sir_df[["pop_count"]]) * 1e5
  sir_df <- sir_df %>%
    modify_at("case_inc", ~ ifelse(.x=='Inf',0,.x))
  
  ## weight crude age-specific rates by standard population
  sir_df[["weight_case_inc"]] <- (sir_df[["usst_count"]] / 1e6) * sir_df[["case_inc"]]
  
  addf <- sir_df %>% 
    summarise(age_adjust = list(ageadjust.direct(case_count,
                                                 pop_count, rate = NULL, stdpop = usst_count, conf.level = 0.95))) %>%
    mutate(age_adjust = map(age_adjust, as.data.frame.list))  %>% 
    unnest (cols = c(age_adjust)) %>% 
    modify_at(c("adj.rate"), ~round(.x* 1e5)) %>% 
    modify_at(c("lci","uci"), ~round(.x* 1e5,2))
  
  addf[["adj.rate"]] <- sum(round(sir_df[["weight_case_inc"]]), na.rm = TRUE)
  
  addf[,c("adj.rate","lci","uci")]
}

get_indirect_df_total <-function(con, input,cond, aa_df,rGEOIDs)
{
  lu <- compose(length, unique)
  
  ## get only result of GEOID
  if(length(rGEOIDs$GEOID)>0)
    aa_df <- aa_df %>%
    filter(GEOID %in% rGEOIDs$GEOID)
  
  aa_df[["expected_cases"]] <- 
    (aa_df[["agg_case_inc"]] * aa_df[["pop_count"]]) / 1e5 #this 1e5 cancels out, right?
  
  sir_df <- 
    agg_sum(case_count ~ GEOID, aa_df) %>% 
    full_join(agg_sum(expected_cases ~ GEOID, unique(aa_df[,c("GEOID", "expected_cases")])))
  
  overall_inc <- 
    1e5 * sum(ociss_case_count(con, input,cond, T)[["agg_case_count"]]) / 
    (n_years(input) * sum(reference_pop(input, T)[["agg_pop_count"]]))
  

  #sir <- sum(sir_df$case_count, na.rm = TRUE) / sum(sir_df$expected_cases, na.rm = TRUE)
  #print(sir)
  sir <-sum(sir_df[["case_count"]], na.rm = TRUE) / sum(sir_df[["expected_cases"]], na.rm = TRUE)
  
  adj.rate <-round(sir * overall_inc)
  lci <- round(overall_inc * (exp(log(sir) - 1.96 * (1 / sqrt(sum(sir_df[["case_count"]], na.rm = TRUE))))))
  uci <- round(overall_inc * (exp(log(sir) + 1.96 * (1 / sqrt(sum(sir_df[["case_count"]], na.rm = TRUE))))))
  
  sir_df <- data.frame(adj.rate, lci, uci)
  
  sir_df
}



