
#' Calculate crude incidence for everyone - OH TOTAL ROW
ociss_crude_inc_everyone <- function(con, input) {

  r <- ociss_sql_everyone(input) %>% query(con)

  names(r) <- c("case_count","GEOID")
  r[["GEOID"]] <- as.character(r[["GEOID"]])

  acs_as = data.frame("pop_count"=5760113,"GEOID"="1")

  r <- left_join(r, acs_as, by = "GEOID")

  r[["case_inc"]] <- 1e5 * (r[["case_count"]] / (r[["pop_count"]] * n_years(input)))

  r <- r[,c("GEOID", "case_inc")]
  r[["GEOID"]] <- as.numeric(r[["GEOID"]])
  r[["case_inc"]] <- round(r[["case_inc"]])
  names(r) <- c("joiner", "Crude Incidence")

  r
}



#' Calculate the age-adjusted incidence rate, with confidence intervals for everyone - OH TOTAL ROW
age_adj_inc_rate_everyone <- function(con, input) {

  cond = NULL

    ## Need to use Direct method
    if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {

      acs_as <- ociss::acs_general_pop_ages$county
      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]

      pop <- agg_sum(pop_count ~ age, acs_as)
      pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

      r <-
        full_join(
          pop,
          ociss_case_count_everyone(con, input, cond, FALSE)
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
      acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      acs_as[["sex"]] <- NULL

      ## aggregated population for 23 age groups : 23 by 2 (age, agg_pop_count)
      acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

      ## total ohio case count for 23 age grgoups : 23 by 3 (age, GEIOID 1, case_count)
      case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
        modify_at(c("case_count"), ~ .x / n_years(input))


      #r <-  full_join(acs_agg_pop, case_agg)
      #r$case_inc <- with(r, case_count / agg_pop_count * 1e5)
      ## crude rate of total ohio population - one value
      #r$crude_rate <- with (r, sum(r$case_count)/sum(r$agg_pop_count) * 1e5)

      ## this is same as crude_rate
      #crude_rate <-
      #  1e5 * sum(ociss_case_count(con, input,cond, T)[["agg_case_count"]]) /
      #  (n_years(input) * sum(reference_pop(input, T)[["agg_pop_count"]]))

      #r[["expected_cases"]] <-
       # (r[["case_inc"]] * r[["agg_pop_count"]]) / 1e5

      #expected_cases <- sum(r$expected_cases)

      #case_count <- sum(r$case_count)

      #sir <- case_count/expected_cases

      #age_adj_inc  <- sir * crude_rate

      #age_adj_ci_low <- crude_rate * (exp(log(sir) - 1.96 * (1 / sqrt(case_count))))

      #age_adj_ci_high <- crude_rate * (exp(log(sir) + 1.96 * (1 / sqrt(case_count))))

      isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                                stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

      sir_df = data.frame("joiner"=1,
                          "A"=round(100000*isr$rate[2], 2),
                          "IR LCI"=round(100000*isr$rate[3], 2),
                          "IR UCI"=round(100000*isr$rate[4], 2))
    }

  names(sir_df) <- c("joiner", "Age-Adjusted Incidence Rate", "IR LCI", "IR UCI")

  sir_df
}


#' #' retrieve reference and case counts, with case incidence
#' count_tables_ohtotal <- function(con, input, cond=NULL, agg = FALSE) {
#'   if(agg) pp <- function(...) paste0("agg_", ...) else pp <- function(z) z
#'
#'
#'   r <-
#'     full_join(
#'       reference_pop(input, agg),
#'       ociss_case_count_everyone(con, input, cond, agg)
#'     ) %>%
#'     modify_at(c(pp("case_count")), ~ .x / n_years(input))
#'   r[[pp("case_inc")]] <- (r[[pp("case_count")]] / r[[pp("pop_count")]]) * 1e5
#'
#'   r
#' }


#' Get numbers of cases from ociss data, with user input
ociss_case_count_everyone <- function(con, input, cond, agg = FALSE) {
  r <- ociss_sql_allage_everyone(input, gand = "AgeDx",wand=cond) %>% query(con)
  #browser()
  
  ## If no data, return empty
  if(length(r$case_count)==0)
  {

   # r <- ociss_sql_allage_everyone(input, gand = "AgeDx",wand=NULL) %>% query(con)
    #  r$case_count = 0
    #if assign NA then 'no rows to aggregate' error
    
    r = ociss::ociss_empty;
  }

  names(r) <- gsub("joiner", "GEOID", names(r))


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


    r[["GEOID"]] <- as.character(r[["GEOID"]])
    r <- agg_sum(case_count ~ age + GEOID, r)

  r
}




#' Calculate the age-adjusted incidence rate of AA, with confidence intervals
age_adj_inc_rate_aa_everyone<- function(con, input) {

  cond = "( Race1 in ('2') )"

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {

  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Black or African American' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

    r <-
      full_join(
        pop,
        ociss_case_count_everyone(con, input, cond, FALSE)
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
    #acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    #acs_as <- acs_as[acs_as$pop == 'Black or African American',]
    #acs_as[["sex"]] <- NULL
    acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL

    acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

    case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
      modify_at(c("case_count"), ~ .x / n_years(input))

    isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                              stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

    sir_df = data.frame("joiner"=1,
                        "A"=round(100000*isr$rate[2], 2),
                        "IR LCI"=round(100000*isr$rate[3], 2),
                        "IR UCI"=round(100000*isr$rate[4], 2))
  }

  names(sir_df) <- c("joiner", "AA Age-Adjusted Incidence Rate","AA IR LCI", "AA IR UCI")

  sir_df

}

#' Calculate the age-adjusted incidence rate of Non-AA, with confidence intervals
age_adj_inc_rate_nonaa_everyone<- function(con, input) {

  cond = "not (Race1 in ( '2' ) )"

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Non-AA' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

    r <-
      full_join(
        pop,
        ociss_case_count_everyone(con, input, cond, FALSE)
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
    #acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    #acs_as <- acs_as[acs_as$pop == 'Non-AA',]
    #acs_as[["sex"]] <- NULL
    acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL

    acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

    case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
      modify_at(c("case_count"), ~ .x / n_years(input))

    isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                              stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

    sir_df = data.frame("joiner"=1,
                        "A"=round(100000*isr$rate[2], 2),
                        "IR LCI"=round(100000*isr$rate[3], 2),
                        "IR UCI"=round(100000*isr$rate[4], 2))
  }
  names(sir_df) <- c("joiner", "Non-AA Age-Adjusted Incidence Rate","Non-AA IR LCI", "Non-AA IR UCI")

  sir_df
}


#' Calculate the age-adjusted incidence rate of hispanic, with confidence intervals
age_adj_inc_rate_hisp_everyone<- function(con, input) {

  cond = "( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )"

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {

  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Hispanic or Latino' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

  r <-
    full_join(
      pop,
      ociss_case_count_everyone(con, input, cond, FALSE)
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
    #acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    #acs_as <- acs_as[acs_as$pop == 'Hispanic or Latino',]
    #acs_as[["sex"]] <- NULL
    acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL
    acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

    case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
      modify_at(c("case_count"), ~ .x / n_years(input))

    isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                              stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

    sir_df = data.frame("joiner"=1,
                        "A"=round(100000*isr$rate[2], 2),
                        "IR LCI"=round(100000*isr$rate[3], 2),
                        "IR UCI"=round(100000*isr$rate[4], 2))
  }
  names(sir_df) <- c("joiner", "Hispanic Age-Adjusted Incidence Rate","Hispanic IR LCI", "Hispanic IR UCI")

  sir_df

}

#' Calculate the age-adjusted incidence rate of Non-hispanic, with confidence intervals
age_adj_inc_rate_nonhisp_everyone<- function(con, input) {

  cond = "( Hispanic in (0, 9) )"

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Non-Hispanic or Latino' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

  r <-
    full_join(
      pop,
      ociss_case_count_everyone(con, input, cond, FALSE)
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
    #acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    #acs_as <- acs_as[acs_as$pop == 'Non-Hispanic or Latino',]
    #acs_as[["sex"]] <- NULL
    acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL

    acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

    case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
      modify_at(c("case_count"), ~ .x / n_years(input))

    isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                              stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

    sir_df = data.frame("joiner"=1,
                        "A"=round(100000*isr$rate[2], 2),
                        "IR LCI"=round(100000*isr$rate[3], 2),
                        "IR UCI"=round(100000*isr$rate[4], 2))
  }
    names(sir_df) <- c("joiner", "Non-Hispanic Age-Adjusted Incidence Rate","Non-Hispanic IR LCI", "Non-Hispanic IR UCI")

  sir_df

}

#' #' retrieve reference and case death counts, with death incidence
#' death_tables_ohtotal <- function(con, input, cond=NULL, agg = FALSE) {
#'   if(agg) pp <- function(...) paste0("agg_", ...) else pp <- function(z) z
#'
#'   #browser()
#'   r <-
#'     full_join(
#'       reference_pop(input, agg),
#'       ociss_death_count_everyone(con, input, cond, agg)
#'     ) %>%
#'     modify_at(c(pp("case_count")), ~ .x / n_years(input))
#'   r[[pp("case_inc")]] <- (r[[pp("case_count")]] / r[[pp("pop_count")]]) * 1e5
#'
#'   r
#' }

#' Get numbers of death from ociss data, with user input
ociss_death_count_everyone <- function(con, input, cond, agg = FALSE) {

  r <- ociss_death_sql_allage_everyone(input, gand = "AgeDx",wand=c(cond,"( VitalStatus = 0 )")) %>% query(con)

  ## If no data, return empty
  if(length(r$case_count)==0)
  {
    #r <- ociss_death_sql_allage_everyone(input, gand = "AgeDx",wand=NULL) %>% query(con)
    #r$case_count = 0
    #if assign NA then 'no rows to aggregate' error
  
    r = ociss::ociss_empty;
  }

  names(r) <- gsub("joiner", "GEOID", names(r))

   # if (input$gis_group == "everyone" || input$gis_group == "GEOID_County")
  #  {
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
#     }else ## for ZCTA, Place, we need different age category since we use general population (i.e. B01001e2)
#     {
#       r[["age"]] <-
#         cut(
#           r[["AgeDx"]],
#           breaks =
#             c(-Inf, 4, 9, 14, 17, 19, 20, 21, 24, 29,
#               34, 39, 44, 49, 54, 59, 61, 64,
#               66, 69, 74, 79, 84, Inf),
#           include.lowest = FALSE,
#           labels =
#             c("Under.5.years",      "5.to.9.years",
#               "10.to.14.years",     "15.to.17.years",
#               "18.and.19.years",    "20.years",
#               "21.years",           "22.to.24.years",
#               "25.to.29.years",     "30.to.34.years",
#               "35.to.39.years",     "40.to.44.years",
#               "45.to.49.years",     "50.to.54.years",
#               "55.to.59.years",     "60.and.61.years",
#               "62.to.64.years",     "65.and.66.years",
#               "67.to.69.years",     "70.to.74.years",
#               "75.to.79.years",     "80.to.84.years",
#               "85.years.and.over"))%>%
#         as.factor() %>%
#         as.character()
# 
# }
    r[["GEOID"]] <- as.character(r[["GEOID"]])
    r <- agg_sum(case_count ~ age + GEOID, r)

  r
}



#' Calculate the age-adjusted mortality rate, with confidence intervals
age_adj_death_rate_everyone <- function(con, input) {

  cond = NULL

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {

    acs_as <- ociss::acs_general_pop_ages$county
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]

    pop <- agg_sum(pop_count ~ age, acs_as)
    #pop <- agg_sum(pop_count ~ age, acs_general_pop_ages$county) %>% filter (age != 'total')
     pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

    r <-
      full_join(
        pop,
        ociss_death_count_everyone(con, input, cond, FALSE)
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
    #acs_as <- ociss::acs_specific_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    #acs_as[["sex"]] <- NULL
    acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
    acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
    acs_as[["sex"]] <- NULL

    acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

    case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
      modify_at(c("case_count"), ~ .x / n_years(input))

    isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                              stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

    sir_df = data.frame("joiner"=1,
                        "A"=round(100000*isr$rate[2], 2),
                        "IR LCI"=round(100000*isr$rate[3], 2),
                        "IR UCI"=round(100000*isr$rate[4], 2))
  }
  names(sir_df) <- c("joiner", "Age-Adjusted Mortality Rate","MR LCI", "MR UCI")

  sir_df
}



#' Calculate the age-adjusted mortality rate of AA, with confidence intervals
age_adj_death_rate_aa_everyone<- function(con, input) {

  cond = "( Race1 in ('2') )"

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Black or African American' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)
    r <-
      full_join(
        pop,
        ociss_death_count_everyone(con, input, cond, FALSE)
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
      #acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      #acs_as <- acs_as[acs_as$pop == 'Black or African American',]
      #acs_as[["sex"]] <- NULL
      acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      acs_as[["sex"]] <- NULL

      ## aggregated population for 23 age groups : 23 by 2 (age, agg_pop_count)
      acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

      ## total ohio case count for 23 age grgoups : 23 by 3 (age, GEIOID 1, case_count)
      case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
        modify_at(c("case_count"), ~ .x / n_years(input))

      isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                                stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

      sir_df = data.frame("joiner"=1,
                          "A"=round(100000*isr$rate[2], 2),
                          "IR LCI"=round(100000*isr$rate[3], 2),
                          "IR UCI"=round(100000*isr$rate[4], 2))
    }
    names(sir_df) <- c("joiner", "AA Age-Adjusted Mortality Rate","AA MR LCI", "AA MR UCI")

  sir_df

}

#' Calculate the age-adjusted mortality rate of Non-AA, with confidence intervals
age_adj_death_rate_nonaa_everyone<- function(con, input) {

  cond = "not (Race1 in ( '2' ) )"

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Non-AA' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

    r <-
      full_join(
        pop,
        ociss_death_count_everyone(con, input, cond, FALSE)
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
      #acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      #acs_as <- acs_as[acs_as$pop == 'Non-AA',]
      #acs_as[["sex"]] <- NULL
      acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      acs_as[["sex"]] <- NULL

      ## aggregated population for 23 age groups : 23 by 2 (age, agg_pop_count)
      acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

      ## total ohio case count for 23 age grgoups : 23 by 3 (age, GEIOID 1, case_count)
      case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
        modify_at(c("case_count"), ~ .x / n_years(input))

      isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                                stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

      sir_df = data.frame("joiner"=1,
                          "A"=round(100000*isr$rate[2], 2),
                          "IR LCI"=round(100000*isr$rate[3], 2),
                          "IR UCI"=round(100000*isr$rate[4], 2))
    }
    names(sir_df) <- c("joiner", "Non-AA Age-Adjusted Mortality Rate","Non-AA MR LCI", "Non-AA MR UCI")

  sir_df

}



#' Calculate the age-adjusted mortality rate of hispanic, with confidence intervals
age_adj_death_rate_hisp_everyone<- function(con, input) {

  cond = "( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )"


  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Hispanic or Latino' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

    r <-
      full_join(
        pop,
        ociss_death_count_everyone(con, input, cond, FALSE)
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
      #acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      #acs_as <- acs_as[acs_as$pop == 'Hispanic or Latino',]
      #acs_as[["sex"]] <- NULL
      acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      acs_as[["sex"]] <- NULL

      ## aggregated population for 23 age groups : 23 by 2 (age, agg_pop_count)
      acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

      ## total ohio case count for 23 age grgoups : 23 by 3 (age, GEIOID 1, case_count)
      case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
        modify_at(c("case_count"), ~ .x / n_years(input))

      isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                                stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

      sir_df = data.frame("joiner"=1,
                          "A"=round(100000*isr$rate[2], 2),
                          "IR LCI"=round(100000*isr$rate[3], 2),
                          "IR UCI"=round(100000*isr$rate[4], 2))
    }
      names(sir_df) <- c("joiner", "Hispanic Age-Adjusted Mortality Rate","Hispanic MR LCI", "Hispanic MR UCI")

  sir_df

}

#' Calculate the age-adjusted mortality rate of Non-hispanic, with confidence intervals
age_adj_death_rate_nonhisp_everyone<- function(con, input) {

  cond = "( Hispanic in (0, 9) )"

  if (input$gis_group == "everyone" || input$gis_group == "GEOID_County") {
  acs_as <- ociss::acs_specific_pop_ages$county %>%
    filter(pop == 'Non-Hispanic or Latino' & age != 'total')
  acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input),]

  pop <- agg_sum(pop_count ~ age, acs_as)
  pop <- data.frame("age"=pop$age,GEIOID="1", "pop_count"=pop$pop_count)

  r <-
      full_join(
        pop,
        ociss_death_count_everyone(con, input, cond, FALSE)
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
      #acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      #acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      #acs_as <- acs_as[acs_as$pop == 'Non-Hispanic or Latino',]
      #acs_as[["sex"]] <- NULL
      acs_as <- ociss::acs_pop_ages[[tolower(gsub("GEOID_", "", input$gis_group))]]
      acs_as <- acs_as[tolower(acs_as[["sex"]]) %in% acs_sex(input) & acs_as$age != "total",]
      acs_as[["sex"]] <- NULL

      ## aggregated population for 23 age groups : 23 by 2 (age, agg_pop_count)
      acs_agg_pop <- agg_sum(pop_count ~ age, acs_as) %>% repl_names(c("pop_count" = "agg_pop_count"))

      ## total ohio case count for 23 age grgoups : 23 by 3 (age, GEIOID 1, case_count)
      case_agg <- ociss_case_count_everyone(con, input, cond, TRUE) %>%
        modify_at(c("case_count"), ~ .x / n_years(input))

      isr <- ageadjust.indirect(count = case_agg$case_count, pop = acs_agg_pop$agg_pop_count,
                                stdcount = case_agg$case_count, stdpop = acs_agg_pop$agg_pop_count)

      sir_df = data.frame("joiner"=1,
                          "A"=round(100000*isr$rate[2], 2),
                          "IR LCI"=round(100000*isr$rate[3], 2),
                          "IR UCI"=round(100000*isr$rate[4], 2))
    }
        names(sir_df) <- c("joiner", "Non-Hispanic Age-Adjusted Mortality Rate","Non-Hispanic MR LCI", "Non-Hispanic MR UCI")

  sir_df

}

