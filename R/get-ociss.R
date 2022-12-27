
#' @import purrr
#' @import dplyr
#' @import DBI
totals <- function(con, input) {
  # the_sql <- paste(ociss_sql(input), "order by case_count desc")
  # structure(query(the_sql, con), class = c(input$gis_group, "data.frame"))
  paste(ociss_sql(input), "order by case_count desc") %>% 
    query(con)
}

#' Execute a query
query <- function(statement, con) {
  db_q <- dbSendQuery(con, statement)
  on.exit(odbc::dbClearResult(db_q))
  dbFetch(db_q)
}

#' In a query that returns case counts, rename the 'case_count' column to 
#' something that is easier to read
rename_cc <- function(df, .nm) {
  names(df)[names(df) == "case_count"] <- .nm
  df
}

#' Use this to generate a simple query, defined as one requiring a single extra string to filter
simple_q_gen <- function(cond, nm) {
  
  fn <- 
    function(con, input) {
      ociss_sql(input, wand = cond) %>% query(con) %>% rename_cc(nm)
    }
  list(fn = fn, nm = nm)
}

#' Create a list of (1) a function to pull data for time to event and (2) the name for that column to be added
tt_gen <- function(.f, .nm) {
  function(input) {
    fn <- 
      function(con, input) {
        r <- .f(input) %>% query(con)
        names(r)[1] <- .nm
        r
      }
    list(fn = fn, nm = .nm)
  }
}

#' Use the above functions to incorporate times to events
tt_tx_fn <- tt_gen(ociss_tx_sql, "time_to_treatment")
tt_fu_fn <- tt_gen(ociss_fu_sql, "time_to_fu")



get_ociss <- function(con, input) {
  geo_lookup <- ociss::geo_lookups[[input$gis_group]]

  first_col  <- names(geo_lookup)[2]
  rm_this    <- names(geo_lookup)[1]
  join_by    <- ifelse(input$gis_group == "everyone", "joiner", input$gis_group)
  
  r <- list(
    simple_q_gen("( Sex = 1 )",                      "OCISS_Male")
    ,simple_q_gen("( Sex = 2 )",                      "OCISS_Female")
    , simple_q_gen("( Race1 in ('2') )",                      "OCISS_Black")
    , simple_q_gen("not (Race1 in ( '2' ) )",                 "OCISS_NonBlack")
    , simple_q_gen("( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )", "OCISS_Hispanic")
    , simple_q_gen("Hispanic in (0,9)",                             "OCISS_NonHispanic")
    , age_strat()
    , simple_q_gen("(OHStage in ('7'))", "OCISS_Distant_Stage")
    , simple_q_gen("(OHStage not in ('0', '1','2', '3', '4', '5','7'))",                "OCISS_Unknown_Stage")
    , tt_tx_fn(input)
    , simple_q_gen("(PriPayerDx in ('1', '2'))", "OCISS_Uninsured")
    , tt_fu_fn(input)
    , list(fn = ociss_crude_inc,  nm = "Crude Incidence")
    , list(fn = age_adj_inc_rate, nm = "Age-Adjusted Incidence")
    , list(fn = age_adj_inc_rate_aa, nm = "AA Age-Adjusted Incidence")
    , list(fn = age_adj_inc_rate_nonaa, nm = "Non-AA Age-Adjusted Incidence")
    , list(fn = age_adj_inc_rate_hisp, nm = "Hispanic Age-Adjusted Incidence")
    , list(fn = age_adj_inc_rate_nonhisp, nm = "Non-Hispanic Age-Adjusted Incidence")
    , list(fn = age_adj_death_rate, nm = "Age-Adjusted Mortality")
    , list(fn = age_adj_death_rate_aa, nm = "AA Age-Adjusted Mortality")
    , list(fn = age_adj_death_rate_nonaa, nm = "Non-AA Age-Adjusted Mortality")
    , list(fn = age_adj_death_rate_hisp, nm = "Hispanic Age-Adjusted Mortality")
    , list(fn = age_adj_death_rate_nonhisp, nm = "Non-Hispanic Age-Adjusted Mortality")
  ) %>% 
    map(function(a) { 
      message(paste("Pulling", a$nm))
      a$fn(con, input)
     }) %>%
     reduce(
         function(a, b) left_join(a, b, by = join_by),
         .init = rename_cc(totals(con, input), "OCISS_Crude_Count"))%>% 
     left_join(geo_lookup, by = join_by) %>%   
     modify_at(names(geo_lookup)[1], as.character)%>%
    modify_at(c("OCISS_Distant_Stage","OCISS_Unknown_Stage","OCISS_Uninsured"), ~ ifelse(is.na(.x),0,.x)) 
  
}

age_strat <- function() {
  list(fn =  function(con, input) {
    ociss_sql(input, sand = "AgeDxC_OCISS2") %>% 
      query(con) %>% 
      spread(AgeDxC_OCISS2, case_count) %>% 
      (function(a) {
        names(a)[!grepl("GEOID|joiner", names(a))] <-
          paste("OCISS", names(a)[!grepl("GEOID|joiner", names(a))], sep = "_")
        a
      })
  }
  , nm = "age")
}

tt_tx_fn_everyone <- tt_gen(ociss_tx_sql_everyone, "time_to_treatment")
tt_fu_fn_everyone <- tt_gen(ociss_fu_sql_everyone, "time_to_fu")

#' Use this to generate a simple query for 'everyone' level
simple_q_gen_everyone <- function(cond, nm) {
  
  fn <- 
    function(con, input) {
      ociss_sql_everyone(input, wand = cond) %>% query(con) %>% rename_cc(nm)
    }
  list(fn = fn, nm = nm)
}

totals_everyone <- function(con, input) {
  # the_sql <- paste(ociss_sql(input), "order by case_count desc")
  # structure(query(the_sql, con), class = c(input$gis_group, "data.frame"))
  paste(ociss_sql_everyone(input), "order by case_count desc") %>% 
    query(con)
}

get_ociss_everyone <- function(con, input) {
  geo_lookup <- ociss::geo_lookups[["everyone"]]
  
  join_by    <- "joiner"
 # browser()
  
  r <- list(
    simple_q_gen_everyone("( Sex = 1 )",                      "OCISS_Male")
    ,simple_q_gen_everyone("( Sex = 2 )",                      "OCISS_Female")
    , simple_q_gen_everyone("( Race1 in ('2') )",                      "OCISS_Black")
    , simple_q_gen_everyone("not (Race1 in ( '2' ) )",                 "OCISS_NonBlack")
    , simple_q_gen_everyone("( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )", "OCISS_Hispanic")
    , simple_q_gen_everyone("Hispanic  in (0,9)",                             "OCISS_NonHispanic")
    , age_strat_everyone()
    , simple_q_gen_everyone("(OHStage in ('7'))", "OCISS_Distant_Stage")
    , simple_q_gen_everyone("(OHStage not in ('0', '1','2', '3', '4', '5','7'))",                "OCISS_Unknown_Stage")
    , tt_tx_fn_everyone(input)
    , simple_q_gen_everyone("(PriPayerDx in ('1', '2'))", "OCISS_Uninsured")
    , tt_fu_fn_everyone(input)
    , list(fn = ociss_crude_inc_everyone,  nm = "Crude Incidence OH Total")
    , list(fn = age_adj_inc_rate_everyone, nm = "Age-Adjusted Incidence OH Total")
    , list(fn = age_adj_inc_rate_aa_everyone, nm = "AA Age-Adjusted Incidence OH Total")
    , list(fn = age_adj_inc_rate_nonaa_everyone, nm = "Non-AA Age-Adjusted Incidence OH Total")
    , list(fn = age_adj_inc_rate_hisp_everyone, nm = "Hispanic Age-Adjusted Incidence OH Total")
    , list(fn = age_adj_inc_rate_nonhisp_everyone, nm = "Non-Hispanic Age-Adjusted Incidence OH Total")
    , list(fn = age_adj_death_rate_everyone, nm = "Age-Adjusted Mortality OH Total")
    , list(fn = age_adj_death_rate_aa_everyone, nm = "AA Age-Adjusted Mortality OH Total")
    , list(fn = age_adj_death_rate_nonaa_everyone, nm = "Non-AA Age-Adjusted Mortality OH Total")
    , list(fn = age_adj_death_rate_hisp_everyone, nm = "Hispanic Age-Adjusted Mortality OH Total")
    , list(fn = age_adj_death_rate_nonhisp_everyone, nm = "Non-Hispanic Age-Adjusted Mortality OH Total")
  ) %>% 
    map(function(a) { 
      message(paste("Pulling", a$nm))
      a$fn(con, input)
    }) %>%
    reduce(
      function(a, b) left_join(a, b, by = "joiner"),
      .init = rename_cc(totals_everyone(con, input), "OCISS_Crude_Count")) %>% 
    left_join(geo_lookup, by = join_by) %>%   
    modify_at(names(geo_lookup)[1], as.character) 
}

age_strat_everyone <- function() {
  list(fn =  function(con, input) {
    ociss_sql_age_everyone(input, sand = "AgeDxC_OCISS2") %>% 
      query(con) %>% 
      spread(AgeDxC_OCISS2, case_count) %>% 
      (function(a) {
        names(a)[!grepl("GEOID|joiner", names(a))] <-
          paste("OCISS", names(a)[!grepl("GEOID|joiner", names(a))], sep = "_")
        a
      })
  }
  , nm = "age")
}
