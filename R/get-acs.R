
#' In the event that ACS calculations can't be returned, use this
empty_acs <- function(input) {
  join_by <- ifelse(input$gis_group == "everyone", "joiner", input$gis_group)
  r <- 
    data.frame(
      GEOID = "1"
      #total of county
      , age_denom = 11549590
      , B01001e26 =5906924, B01001e26_p = NA 
      , age_lt_14 = 2218676, age_lt_14_p = NA 
      , age_15_44 = 3071278, age_15_44_p = NA 
      , age_45_64 = 3195189, age_45_64_p = NA 
      , age_65_74 = 890162, age_65_74_p = NA 
      , age_gt_75 = 776484, age_gt_75_p = NA 
      #, r_native_american = 1, r_native_american_p = 1
      #, r_asian = 1, r_asian_p = 1
      , r_black = 1556296, r_black_p = NA
      , r_nonblack = 9993294, r_nonblack_p = NA
      #, r_hawaiian = 1, r_hawaiian_p = 1
      #, r_white = 1, r_white_p = 1
      , e_hispanic = 367394, e_hispanic_p = NA
      , e_nonhispanic = 11182196, e_nonhispanic_p = NA
      , pov_rate = NA
      , house_income = NA
      , adi_pop = NA
      , adi_tot_pop = NA
      , adi = NA
      , stringsAsFactors = FALSE
    )
  names(r)[1] <- join_by
  names(r)[-1] <- paste("acs", names(r)[-1], sep = "_")
  r
}

get_ADI<- function(geo_level)
{
  #browser()
  
  adi <- county_adi
  
  if (geo_level == "GEOID_ZCTA")
    adi <- ZCTA_adi
  
  if (geo_level == "GEOID_Place")
    adi <- place_ADI
  
  
  adi %>%
    select(GEOID,adi_pop,tot_pop) %>%
    rename(adi_tot_pop = tot_pop)
   # rename(adi = pct_adi910)
}

#' Retrieve population-level data, calculate relevant proportions
get_acs <- function(con, input) {
  if (input$gis_group == "everyone") return(empty_acs(input))
  
  geo_level <- input$gis_group
  
  #print(geo_level)

  ar <- acs_result <- dbGetQuery(con, acs_sql(gsub("GEOID", "NeoCASE", geo_level)))


  #browser()
  ages <- c("B01001e26","age_lt_14", "age_15_44", "age_45_64", "age_65_74", "age_gt_75")
  rces <- c("r_nonblack", "r_black")
  ethn <- c("e_hispanic", "e_nonhispanic")
  
  p_p <- function(a) paste(a, "p", sep = "_")
  
  these_by_this <- function(these, this) {
    lapply(ar[,these], function(z) sprintf("%.1f", 100 * z / ar[[this]]))
  }
  
  # don't divide by zero
  ar[,grepl("denom", names(ar))] <- 
    lapply(ar[,grepl("denom", names(ar))], function(b) ifelse(b == 0, 1, b))
  
  ar[,p_p(ages)] <- these_by_this(ages, "age_denom")
  ar[,p_p(rces)] <- these_by_this(rces, "race_denom")
  ar[,p_p(ethn)] <- these_by_this(ethn, "ethnicity_denom")
  ar[,"pov_rate"] <- sprintf("%.1f", 100 * ar[["pov_numer"]] / ar[["pov_denom"]])
  ar[,input$gis_group] <- 
    map_chr(strsplit(as.character(ar[["GEOID"]]), split = "US"), 2)

  
  #browser()
  ## need to add ADI here
  adi <- get_ADI(geo_level)
  ## end ADI
  
  #browser()
  
  
  ar2 <- merge(ar, adi, by.x=geo_level, by.y = "GEOID") #%>%
    #rename(adi = adi.y)
  
  
  ar2[,"adi"] <- sprintf("%.1f", 100 * ar2[["adi_pop"]] / ar2[["adi_tot_pop"]])
  
  
   # browser()
  select_these <- c(
      geo_level, "age_denom","B01001e26","B01001e26_p"
      , "age_lt_14"      , "age_lt_14_p"
      , "age_15_44"      , "age_15_44_p"
      , "age_45_64"      , "age_45_64_p"
      , "age_65_74"      , "age_65_74_p"
      , "age_gt_75"      , "age_gt_75_p"
      , "r_black"        , "r_black_p"
      , "r_nonblack"     , "r_nonblack_p"
      , "e_hispanic"     , "e_hispanic_p"
      , "e_nonhispanic"  , "e_nonhispanic_p"
      , "pov_rate"       #, "pov_rate_p"
      , "house_incom"
      , "adi_pop"
      , "adi_tot_pop"
      , "adi"
    )
  ar2 <- ar2[,select_these]
   names(ar2)[-1] <- paste("acs", names(ar2)[-1], sep = "_")
   ar2
}



