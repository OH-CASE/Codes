
#' Create an ociss filter, given user input. 
ociss_filter <- function(input, and = NULL) {
  #message(paste("Date range is", paste(input$date_range, collapse = " and ")))
  
  filters <- get_filters(ociss::filters_ui, input)
  
  collapse_w_and <- 
    c(
      imap_chr(filters, ociss_in), 
      and, 
      paste("DxDate >= \'", as.Date(as.yearqtr(input$date_range, format = "%Y Q%q")), "\'", sep = ""),
      paste("DxDate <= \'", as.Date(as.yearqtr(input$date_range_end, format = "%Y Q%q"), frac=1), "\'", sep = "")
      )

  filtered <- paste("where", paste(collapse_w_and, collapse = " and "))
  
  if(stringr::str_count(filtered,"OHStage")==2)
  {
    ## need to remove "and OHStage not in ('0')" if exist
    filtered <- stringr::str_replace(filtered,"and OHStage not in \\('0'\\)","")
  }

  filtered
  
}


#' Create an ociss filter, given user input for mortality rate calculation 
ociss_death_filter <- function(input, and = NULL) {
  #message(paste("Date range is", paste(input$date_range, collapse = " and ")))
  
  filters <- get_filters(ociss::filters_ui, input)
  s<-NULL
  
  # if Psite entered
  if(!is.null(filters$PSite))
  {
    nm = "DthCause"
    ## lookup list
    lu <- ociss::lookups[[nm]]
    ## option entered
    which_lu <- which(lu[[paste_(nm, "v")]] %in% filters$PSite)
    
    fs <- lu[[nm]][which_lu]
    #"(DthCause like 'C00%')" "(DthCause like 'C53%')"
    
    psite_fn <- function(z) paste(z, collapse = "or ")
    
    s<- paste("(",psite_fn(fs), ")")
    
    filters[["PSite"]] <- NULL
  }
  

  collapse_w_and <- 
    c(
      imap_chr(filters, ociss_in), 
      and, 
      s,
      paste("VitalStatus = 0"),
      paste("DateLastContact >= \'", as.Date(as.yearqtr(input$date_range, format = "%Y Q%q")), "\'", sep = ""),
      paste("DateLastContact <= \'", as.Date(as.yearqtr(input$date_range_end, format = "%Y Q%q"), frac=1), "\'", sep = "")
    )
  
  filtered <- paste("where", paste(collapse_w_and, collapse = " and "))

  
  filtered
  
}

#' Create an ociss filter without regions, given user input. 
ociss_death_filter_noregion <- function(input, and = NULL) {
  #message(paste("Date range is", paste(input$date_range, collapse = " and ")))
  
  filters <- get_filters(ociss::filters_ui, input) 
  
  if(!is.null(input$filter_RuralUrbanCon2013))
    filters[["RuralUrbanCon2013"]] <- NULL
      
  if(!is.null(input$filter_GEOID_County))
    filters[["GEOID_County"]] <- NULL

  s<-NULL
  
  # if Psite entered
  if(!is.null(filters$PSite))
  {
    nm = "DthCause"
    ## lookup list
    lu <- ociss::lookups[[nm]]
    ## option entered
    which_lu <- which(lu[[paste_(nm, "v")]] %in% filters$PSite)
    
    fs <- lu[[nm]][which_lu]
    #"(DthCause like 'C00%')" "(DthCause like 'C53%')"
    
    psite_fn <- function(z) paste(z, collapse = "or ")
    
    s<- paste("(",psite_fn(fs), ")")
    
    filters[["PSite"]] <- NULL
  }
  
  collapse_w_and <- 
    c(
      imap_chr(filters, ociss_in), 
      and, 
      s, 
      paste("VitalStatus = 0"),
      paste("DateLastContact >= \'", as.Date(as.yearqtr(input$date_range, format = "%Y Q%q")), "\'", sep = ""),
      paste("DateLastContact <= \'", as.Date(as.yearqtr(input$date_range_end, format = "%Y Q%q"), frac=1), "\'", sep = "")
    )
  
  filtered <- paste("where", paste(collapse_w_and, collapse = " and "))

  filtered
  
}


#' Create an ociss filter without regions, given user input. 
ociss_filter_noregion <- function(input, and = NULL) {
  #message(paste("Date range is", paste(input$date_range, collapse = " and ")))
  
  filters <- get_filters(ociss::filters_ui, input) 
  
  if(!is.null(input$filter_RuralUrbanCon2013))
    filters[["RuralUrbanCon2013"]] <- NULL
  
  if(!is.null(input$filter_GEOID_County))
    filters[["GEOID_County"]] <- NULL
  
  collapse_w_and <- 
    c(
      imap_chr(filters, ociss_in), 
      and, 
      paste("DxDate >= \'", as.Date(as.yearqtr(input$date_range, format = "%Y Q%q")), "\'", sep = ""),
      paste("DxDate <= \'", as.Date(as.yearqtr(input$date_range_end, format = "%Y Q%q"), frac=1), "\'", sep = "")
    )
  
  filtered <- paste("where", paste(collapse_w_and, collapse = " and "))
  
  if(stringr::str_count(filtered,"OHStage")==2)
  {
    ## need to remove "and OHStage not in ('0')" if exist
    filtered <- stringr::str_replace(filtered,"and OHStage not in \\('0'\\)","")
  }

  filtered
  
}
#' This function shouldn't be called unless there's something to put in 'in' 
#' Create an 'in' statement from filters and names
#' This function automatically identifies which lookup table to use
ociss_in <- function(fs2, nm) {
  # figure out the OCISS_Patients values
  ## lookup list
  lu <- ociss::lookups[[nm]]
  ## option entered
  which_lu <- which(lu[[paste_(nm, "v")]] %in% fs2)

  fs <- lu[[nm]][which_lu]

  if (length(fs) == 0) stop("Filter condition not found in lookup table")
  
  if (is.character(fs)) p_fn <- function(z) paste("'", paste(z, collapse = "', '"), "'", sep = '')
  else  p_fn <- function(z) paste(z, collapse = ", ")
  
  psite_fn <- function(z) paste(z, collapse = "or ")
  
  s <- paste(nm, "in (", p_fn(fs), ")")
  
  if(nm == "PSite")
  {
    if(any(fs2 == "Urinary Bladder"))
    {
      s <- psite_fn(fs)
    }
    else {
      s<- paste("(",psite_fn(fs), ") and OHStage not in ('0')") 
    }
  }
  
  s
}

#' Generate a SQL statement, given user input and/or developer 
#' choices of extra where classes ('wand'), select columns ('sand') and 
#' grouping columns ('gand')
#' @param wand 'Where' + wand
#' @param sand 'select' + sand
ociss_sql <- function(input, wand = NULL, sand = NULL, gand = NULL ) {
  stopifnot(input$gis_group %in% ociss::geo_levels)
  
  #clauses
  selected <- safely_select(c(input$gis_group, sand, gand))
  filtered <- ociss_filter(input, and = wand)
  grouped  <- safely_group(c(input$gis_group, sand, gand))

  sql <- paste("select", selected, "from OCISS_Patients", filtered, grouped) %>% 
    trimws() %>% 
    gsub(" +", " ", .)
  
  #print(sql)
  
  sql
  
}

ociss_death_sql <- function(input, wand = NULL, sand = NULL, gand = NULL ) {
  stopifnot(input$gis_group %in% ociss::geo_levels)
  
  #clauses
  selected <- safely_select(c(input$gis_group, sand, gand))
  filtered <- ociss_death_filter(input, and = wand)
  grouped  <- safely_group(c(input$gis_group, sand, gand))
  
  sql <- paste("select", selected, "from OCISS_Patients", filtered, grouped) %>% 
    trimws() %>% 
    gsub(" +", " ", .)
  
  #print(sql)
  
  sql
  
}


#' Generate sql to retrieve time to treatment from OCISS data, given user input
ociss_tx_sql <- function(input) {
  #OCISS time to treatment
  selected <- safely_select(input$gis_group, fst = "")
  filtered <- ociss_filter(input)
  grouped  <- safely_group(c(input$gis_group, "tt_tx"))
  
  if (input$gis_group == 'everyone') p_by <- "'1'" else p_by <- input$gis_group
  
  # # PERCENTILE_CONT used to get median
  # sql <- paste(
  #   "select distinct tt_tx,",
  #   selected, 
  #   "from (
  #   select PERCENTILE_CONT(0.5) within group (order by datediff(dd, o.DxDate, r.the_date)) over (partition by",
  #   p_by, 
  #   ") as tt_tx, 
  #   o.*
  #   from OCISS_Patients o
  #     inner join (
  #       select PatientID, DxDate, FirstRxDateCOC the_date
  #     from OCISS_Patients", filtered, "
  #     ) r on o.PatientID = r.PatientID and o.DxDate = r.DxDate
  #     where abs(datediff(dd, o.DxDate, r.the_date)) > 1 and o.DxDate >= '2010-01-01' and RXSummRXStatus = '1'
  #   ) z
  #   ", 
  #   filtered, 
  #   grouped
  #   ) 

  ## Median time to Treatment (Days)
   sql <- paste(
     "select distinct tt_tx,",
     selected, 
     "from (
     select PERCENTILE_CONT(0.5) within group (order by datediff(dd, o.DxDate, r.the_date)) over (partition by",
     p_by, 
     ") as tt_tx, 
     o.*
     from OCISS_Patients o
       inner join (
         select PatientID, DxDate, FirstRxDateCOC the_date
       from OCISS_Patients", filtered, "
       ) r on o.PatientID = r.PatientID and o.DxDate = r.DxDate
       where not (o.FirstRxDateCOC is NULL or o.FirstRxDateCOC=o.DxDate)
     ) z
     ", 
     grouped
     )
  
  # print(sql)
  
  sql
  
}

#' Generate sql to retrieve time to follow up among user-selected cases
ociss_fu_sql <- function(input) {
  #OCISS follow-up time
  selected <- safely_select(input$gis_group, fst = "")
  filtered <- ociss_filter(input)
  grouped  <- safely_group(c(input$gis_group, "tt_fu"))
  
  if (input$gis_group == 'everyone') p_by <- "'1'" else p_by <- input$gis_group
  
  # PERCENTILE_CONT used to get median
  sql = paste(
    "select distinct round(tt_fu, 1),", selected, 
    "from (
      select 
      PERCENTILE_CONT(0.5) within group (order by datediff(dd, o.DxDate, r.fu_date) / 30.44) over (partition by",
      p_by, ") as tt_fu, 
      o.*
    from OCISS_Patients o
    inner join (
      select PatientID, DxDate, DateLastContact as fu_date
      from OCISS_Patients", filtered,
      "
    ) r on o.PatientID = r.PatientID and o.DxDate = r.DxDate
    ) z
    ", 
    filtered, 
    grouped
    ) 

  sql
  
}





ociss_sql_age_everyone <- function(input, wand = NULL, sand = NULL, gand = NULL ) {
  
  filtered <- ociss_filter_noregion(input, and = wand)
  
  sql <- paste("select count(*) as case_count, AgeDxC_OCISS2, 1 as joiner from OCISS_Patients", 
               filtered," group by AgeDxC_OCISS2") %>% 
    trimws() %>% 
    gsub(" +", " ", .)

  sql
  
}


## total OHIO population with input filters except region filters
ociss_sql_allage_everyone <- function(input, wand = NULL, sand = NULL, gand = NULL ) {
  
  filtered <- ociss_filter_noregion(input, and = wand)
  
  sql <- paste("select count(*) as case_count, AgeDx, 1 as GEOID from OCISS_Patients", 
               filtered," group by AgeDx") %>% 
    trimws() %>% 
    gsub(" +", " ", .)
  
  #print(sql)
  sql
  
}

## total OHIO population with input filters except region filters
ociss_death_sql_allage_everyone <- function(input, wand = NULL, sand = NULL, gand = NULL ) {
  
  filtered <- ociss_death_filter_noregion(input, and = wand)
  
  sql <- paste("select count(*) as case_count, AgeDx, 1 as GEOID from OCISS_Patients", 
               filtered," group by AgeDx") %>% 
    trimws() %>% 
    gsub(" +", " ", .)
  
  #print(sql)
  sql
  
}

ociss_sql_everyone <- function(input, wand = NULL, sand = NULL, gand = NULL ) {
  
  filtered <- ociss_filter_noregion(input, and = wand)

  sql <- paste("select count(*) as case_count, 1 as joiner from OCISS_Patients", 
               filtered) %>% 
    trimws() %>% 
    gsub(" +", " ", .)
  
  sql
  
}


#' Generate sql to retrieve time to treatment from OCISS data, given user input
ociss_tx_sql_everyone <- function(input) {
  #OCISS time to treatment
  #selected <- safely_select(input$gis_group, fst = "")
  filtered <- ociss_filter(input)
  #grouped  <- safely_group(c(input$gis_group, "tt_tx"))
  
  #if (input$gis_group == 'everyone') p_by <- "'1'" else p_by <- input$gis_group
  
  # PERCENTILE_CONT used to get median
  sql <- paste(
    "select distinct tt_tx, 1 as joiner from (
    select PERCENTILE_CONT(0.5) within group (order by datediff(dd, o.DxDate, r.the_date)) over (partition by '1' ) as tt_tx, 
    o.*
    from OCISS_Patients o
      inner join (
        select PatientID, DxDate, FirstRxDateCOC the_date
      from OCISS_Patients", filtered, "
      ) r on o.PatientID = r.PatientID and o.DxDate = r.DxDate
      where abs(datediff(dd, o.DxDate, r.the_date)) > 1 and o.DxDate >= '2010-01-01' and RXSummRXStatus = '1'
    ) z
    ", 
    filtered, 
    " group by tt_tx"
  ) 
  
    sql
  
}


#' Generate sql to retrieve time to follow up among user-selected cases
ociss_fu_sql_everyone <- function(input) {
  #OCISS follow-up time
  #selected <- safely_select(input$gis_group, fst = "")
  filtered <- ociss_filter(input)
  #grouped  <- safely_group(c(input$gis_group, "tt_fu"))
  
  #if (input$gis_group == 'everyone') p_by <- "'1'" else p_by <- input$gis_group
  
  # PERCENTILE_CONT used to get median
  sql <- paste(
    "select distinct round(tt_fu, 1), 1 as joiner ", 
    "from (
      select 
      PERCENTILE_CONT(0.5) within group (order by datediff(dd, o.DxDate, r.fu_date) / 30.44) over (partition by '1' ) as tt_fu, 
      o.*
    from OCISS_Patients o
    inner join (
      select PatientID, DxDate, DateLastContact as fu_date
      from OCISS_Patients", filtered,
    "
    ) r on o.PatientID = r.PatientID and o.DxDate = r.DxDate
    ) z
    ", 
    filtered, 
    " group by tt_fu"
  ) 
  
  sql
}