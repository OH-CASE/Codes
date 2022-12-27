
#' Aggregate and take the sum over a formula. Values are returned as integers.
agg_sum <- function(.frm, .df) {
  r <- aggregate(.frm, .df, sum, na.rm = T)
  over <- all.vars(update(.frm, . ~ 1))
  r[[over]] <- round(r[[over]])
  r
}

# Convenience function to replace names
repl_names <- function(.df, .repls) {
  names(.df) <- stringr::str_replace_all(names(.df), .repls)
  .df
}

#' Calculate the number of years that a user has requested
# n_years <- function(input) {
#   do.call("difftime", c(map(input$date_range, lubridate::ymd), units = "weeks")) %>%
#     as.numeric() %>%
#     (function(a) a / 52) %>%
#     abs() %>%
#     round()
# }
 n_years <- function(input) {
  difftime(as.Date(as.yearqtr(input$date_range, format = "%Y Q%q")), as.Date(as.yearqtr(input$date_range_end, format = "%Y Q%q")), units = "weeks") %>%
    as.numeric() %>%
    (function(a) a / 52) %>%
    abs() %>%
    round()
}

 get_direct_df <- function(input, r)
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


   ## weight crude age-specific rates by standard population
   df[["weight_case_inc"]] <- (df[["usst_count"]] / 1e6) * df[["case_inc"]]


   ## add all weighted rates by GEOID - the age adjusted incidence rate (direct method)
   sir_df <-
     agg_sum(weight_case_inc ~ GEOID, df)

   addf <- df %>% group_by(GEOID) %>%
     summarise(age_adjust = list(ageadjust.direct(case_count,
                                                  pop_count, rate = NULL, stdpop = usst_count, conf.level = 0.95))) %>%
     mutate(age_adjust = map(age_adjust, as.data.frame.list))  %>%
     unnest (cols = c(age_adjust)) %>%
     modify_at(c("adj.rate"), ~round(.x* 1e5, 2)) %>%
     modify_at(c("lci","uci"), ~round(.x* 1e5,2))

   sir_df2  <- merge(addf, sir_df, by = "GEOID")
   
  # browser()
   
   #sir_df <- sir_df2[,c("GEOID","weight_case_inc","lci","uci")]
   sir_df <- sir_df2[,c("GEOID","adj.rate","lci","uci")]
   colnames(sir_df)[2]="weight_case_inc"
   sir_df[["GEOID"]] <- as.numeric(sir_df[["GEOID"]])

   sir_df$weight_case_inc[sir_df$weight_case_inc==0] = NA
   sir_df$lci[sir_df$lci=='NaN'] = NA
   sir_df$uci[sir_df$uci=='NaN'] = NA

   sir_df
 }

 get_indirect_df <-function(con, input,cond, aa_df)
 {
   #browser()
   ##
   # if any filters other than primary or sex specified, only crude
   #aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)

   lu <- compose(length, unique)

   ## please check data again for municipality
   ## 10.to.14.years: 483174
   ## Under.5.years: 483174
   #stopifnot (lu(aa_df$age) == lu(aa_df$agg_pop_count))

   aa_df[["expected_cases"]] <-
     (aa_df[["agg_case_inc"]] * aa_df[["pop_count"]]) / 1e5 #this 1e5 cancels out, right?

   sir_df <-
     agg_sum(case_count ~ GEOID, aa_df) %>%
     full_join(agg_sum(expected_cases ~ GEOID, unique(aa_df[,c("GEOID", "expected_cases")])))

   ## crude rate
   sir_df[["overall_inc"]] <-
     1e5 * sum(ociss_case_count(con, input,cond, T)[["agg_case_count"]]) /
     (n_years(input) * sum(reference_pop(input, T)[["agg_pop_count"]]))

   #crude_rate_df <- unique(aa_df[,c("agg_pop_count", "agg_case_count")])
   #crude_rate = (sum(crude_rate_df$agg_case_count)/sum(crude_rate_df$agg_pop_count))*1e5

   #print(crude_rate)
   #print(sir_df[["overall_inc"]][1])

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


#' Calculate the age-adjusted incidence rate, with confidence intervals
age_adj_inc_rate <- function(con, input) {

  cond = NULL

  ## Need to use Direct method
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

    ###########
    
    sir_df = get_direct_df(input, r)
  }
  ## Indirect method
  else
  {
    # if any filters other than primary or sex specified, only crude
    aa_df <- map(c(F, T), ~ count_tables(con, input, cond, .x)) %>% reduce(full_join)

    sir_df = get_indirect_df(con, input, cond,aa_df)
  }

  names(sir_df) <- c(input$gis_group, "Age-Adjusted Incidence Rate", "IR LCI", "IR UCI")

  if (input$gis_group == "everyone")
    names(sir_df) <- c("joiner", "Age-Adjusted Incidence Rate", "IR LCI", "IR UCI")

  sir_df
}

#' return reference population counts--these come from the U.S. standard population data
usstandard_pop <- function() {

    xage <- c("Under.5.years","5.to.9.years",
              "10.to.14.years","15.to.19.years","20.to.24.years", "25.to.29.years",
              "30.to.34.years","35.to.44.years","45.to.54.years","55.to.64.years",
              "65.to.74.years","75.to.84.years","85.years.and.over")

    xpop <- c(69135,72533,
              73032,72169,66478,64529,
              71044,80762+81851,72118+62716,48454+38793,
              34264+31773,26999+17842,15508)

    uspop_as = data.frame("age"=xage, "usst_count"=xpop)

    uspop_as

  }

#' retrieve reference and case counts, with case incidence
count_tables <- function(con, input, cond=NULL, agg = FALSE) {
  if(agg) pp <- function(...) paste0("agg_", ...) else pp <- function(z) z

    #browser()
  r <-
    full_join(
      reference_pop(input, agg),
      ociss_case_count(con, input, cond, agg)
    ) %>%
    modify_at(c(pp("case_count")), ~ .x / n_years(input))
  r[[pp("case_inc")]] <- (r[[pp("case_count")]] / r[[pp("pop_count")]]) * 1e5

  r
}

#' Report ACS values over GEOIDs
acs_by_geoid <- function(input) {
  acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
  agg_sum(pop_count ~ GEOID, acs_as) %>% repl_names(c("pop_count" = "geo_pop_count"))
}

#' Get numbers of cases from ociss data, with user input
ociss_case_count <- function(con, input, cond, agg = FALSE) {
  r <- ociss_sql(input, gand = "AgeDx",wand=cond) %>% query(con) #,wand=cond

  ## If no data, return empty
  if(length(r$case_count)==0)
  {
    #r <- ociss_sql(input, gand = "AgeDx",wand=NULL) %>% query(con)
    #r$case_count = 0
    #if assign NA then 'no rows to aggregate' error
    
    r = ociss::ociss_empty;

  }

  if (input$gis_group == "everyone") {
    names(r) <- gsub("joiner", "GEOID", names(r))
  }

  names(r) <- gsub("_County|_ZCTA|_Place", "", names(r))

    ## use same age category with reference census data for county, everyone
  #if (input$gis_group == "everyone" || input$gis_group == "GEOID_County")
  #{
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
  #}
  # else ## for ZCTA, Place, we need different age category since we use general population (i.e. B01001e2)
  # {
  #   r[["age"]] <-
  #     cut(
  #       r[["AgeDx"]],
  #       breaks =
  #         c(-Inf, 4, 9, 14, 17, 19, 20, 21, 24, 29,
  #           34, 39, 44, 49, 54, 59, 61, 64,
  #           66, 69, 74, 79, 84, Inf),
  #       include.lowest = FALSE,
  #       labels =
  #         c("Under.5.years",      "5.to.9.years",
  #           "10.to.14.years",     "15.to.17.years",
  #           "18.and.19.years",    "20.years",
  #           "21.years",           "22.to.24.years",
  #           "25.to.29.years",     "30.to.34.years",
  #           "35.to.39.years",     "40.to.44.years",
  #           "45.to.49.years",     "50.to.54.years",
  #           "55.to.59.years",     "60.and.61.years",
  #           "62.to.64.years",     "65.and.66.years",
  #           "67.to.69.years",     "70.to.74.years",
  #           "75.to.79.years",     "80.to.84.years",
  #           "85.years.and.over")
  #     ) %>%
  #     as.factor() %>%
  #     as.character()
  # }

  if (agg) {
    r <- agg_sum(case_count ~ age, r) %>% repl_names(c("case_count" = "agg_case_count"))
  } else {
    r[["GEOID"]] <- as.character(r[["GEOID"]])
    r <- agg_sum(case_count ~ age + GEOID, r)
  }

  r
}



#' Calculate crude incidence
ociss_crude_inc <- function(con, input) {
  #if (input$gis_group == "everyone") {
  #  warning("Figure out how to integrate 'everyone' into inc. calcs")
  #  return(tibble(joiner = 1, 'Crude Incidence' = 1))
  #}

  r <- ociss_sql(input) %>% query(con)

  if (input$gis_group == "everyone") {

    names(r) <- c("case_count","GEOID")
    r[["GEOID"]] <- as.character(r[["GEOID"]])

    acs_as = data.frame("pop_count"=5760113,"GEOID"="1")
  }
  else {
    names(r) <- gsub("_County|_ZCTA|_Place", "", names(r))
    r[["GEOID"]] <- as.character(r[["GEOID"]])
    r <- agg_sum(case_count ~ GEOID, r)

    acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]
    acs_as[["sex"]] <- NULL

    acs_as <- acs_as[acs_as$age != "total",]
    acs_as[["GEOID"]] <- as.character(acs_as[["GEOID"]])
    acs_as <- agg_sum(pop_count ~ GEOID, acs_as)

  }

  r <- left_join(r, acs_as, by = "GEOID")

  r[["case_inc"]] <- 1e5 * (r[["case_count"]] / (r[["pop_count"]] * n_years(input)))

  r <- r[,c("GEOID", "case_inc")]
  r[["GEOID"]] <- as.numeric(r[["GEOID"]])
  r[["case_inc"]] <- round(r[["case_inc"]])
  names(r) <- c(input$gis_group, "Crude Incidence")

  if (input$gis_group == "everyone")
    names(r) <- c("joiner", "Crude Incidence")

  r
}

## For Indirect methods
reference_pop <- function(input, agg = FALSE) {

  if (input$gis_group == "everyone") {
    
    acs_as <- ociss::acs_general_pop_ages$county
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL
    
    # browser()
    if (agg) {
      r <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))
    } else {
      r <- agg_sum(pop_count ~ age, acs_as)
      r$GEOID = "1"
    }  
    
  } else {
    ## no race/ethnicity - use general population (A+B+C+D+E+F+G)
    if(is.null(input$filter_Race1) && is.null(input$filter_Hispanic))
    {
      acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    }
    ## both race AND ethnicity
    else if(!is.null(input$filter_Race1) & !is.null(input$filter_Hispanic))
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
    ## race, no ethnicity
    else if(!is.null(input$filter_Race1) & is.null(input$filter_Hispanic))
    {
      ## if Race only
      acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>%
        filter(pop %in% input$filter_Race1)
      
      ## if 'unknown' selected, return empty 'NA' population
      if(length(acs_as$pop_count)==0)
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
        acs_as$pop_count <- 0
      }
    }
    ## no race, ethnicity
    else if(is.null(input$filter_Race1) & !is.null(input$filter_Hispanic))
    {
      acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>%
        filter(pop %in% input$filter_Hispanic)
      
      ## if 'unknown' selected, return empty 'NA' population
      if(length(acs_as$pop_count)==0)
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
        acs_as$pop_count <- 0
      }
    }
    
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL  
    
    if (agg) {
      r <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))
    } else {
      r <- acs_as[acs_as$age != "total",]
      r[["GEOID"]] <- as.character(r[["GEOID"]])
      r <- agg_sum(pop_count ~ age + GEOID, acs_as)
    }
    
  }  
  r
}

## For Direct method -- need to use race/ethnicity specific population
#' return general reference population counts--these come from the ACS data
reference_general_pop <- function(input) {
    if (input$gis_group == "everyone") {
      
      acs_as <- ociss::acs_general_pop_ages$county
      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      
      ### need to filter acs_as data only for 15 counties
      if(!is.null(input$filter_GEOID_County))
      {
        nm = "GEOID_County"
        ## lookup list
        lu <- ociss::lookups[[nm]]
        ## option entered
        which_lu <- which(lu[[paste_(nm, "v")]] %in% input$filter_GEOID_County)
        fs <- lu[[nm]][which_lu]
        fs_list <- str_replace_all(fs, "'", "")
        
        ## subset of acs_as using GEOID filter  
        acs_as <- acs_as%>% filter (GEOID %in% unlist(strsplit(fs_list, ",")))
      }
      
     # browser()
      
      ### need to filter acs_as data for age category
      if(!is.null(input$filter_AgeDxC_OCISS2))
      {
        ## subset of acs_as using GEOID filter  
        acs_as <- acs_as %>% filter (age_c %in% input$filter_AgeDxC_OCISS2)
        
      }
      
      
      r <- agg_sum(pop_count ~ age, acs_as) %>% filter (age != 'total')
      
      r <- data.frame("age"=r$age,GEOID="1", "pop_count"=r$pop_count)
    }
    else {
      ## no race, no ethnicity, use general population
      if(is.null(input$filter_Race1) && is.null(input$filter_Hispanic))
      {
        acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      }
      ## both race AND ethnicity
      else if(!is.null(input$filter_Race1) & !is.null(input$filter_Hispanic))
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
      ## race, no ethnicity
      else if(!is.null(input$filter_Race1) & is.null(input$filter_Hispanic))
      {
        ## if Race only
        acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>%
          filter(pop %in% input$filter_Race1)

        ## if 'unknown' selected, return empty 'NA' population
        if(length(acs_as$pop_count)==0)
        {
          acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
          acs_as$pop_count <- 0
        }
      }
      ## no race, ethnicity
      else if(is.null(input$filter_Race1) & !is.null(input$filter_Hispanic))
      {
        acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]] %>%
            filter(pop %in% input$filter_Hispanic)

        ## if 'unknown' selected, return empty 'NA' population
        if(length(acs_as$pop_count)==0)
        {
          acs_as <- ociss::acs_general_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
          acs_as$pop_count <- 0
        }
      }

      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      acs_as[["sex"]] <- NULL

      acs_as[["pop"]] <- NULL

      r <- acs_as[acs_as$age != "total",]
      r[["GEOID"]] <- as.character(r[["GEOID"]])
      r <- agg_sum(pop_count ~ age + GEOID, acs_as)
    }

  r
}


#' Determine which sex to include in the denominator of population calculations
acs_sex <- function(input) {

 #browser()

  fc <- c("male", "female")

  ss <- map(ociss::site_sex, tolower)
  if (!is.null(input$filter_PSite)) {
    if (all(tolower(input$filter_PSite) %in% ss$female)) {
      message("Using only female denominator for age-adjusted incidence calculations")
      #return("female")
      fc <- "female"
    } else if (all(tolower(input$filter_PSite) %in% ss$male)) {
      message("Using only male denominator for age-adjusted incidence calculations")
      #return("male")
      fc <- "male"
    }
  }

  if (!is.null(input$filter_Sex)) {
    if (all(tolower(input$filter_Sex) %in% "female")) {
      message("Using only female denominator for age-adjusted incidence calculations")
      #return("female")
      fc <- "female"
    } else if (all(tolower(input$filter_Sex) %in% "male")) {
      message("Using only male denominator for age-adjusted incidence calculations")
      #return("male")
      fc <- "male"
    }
  }

  if(is.null(input$filter_Sex) && is.null(input$filter_PSite) ) {
    warning("No sex criteria found, not filtering reference population")
    #return(c("male", "female"))
    fc <- c("male", "female")
  }

  #return(c("male", "female"))
  fc
}

