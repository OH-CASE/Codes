#' This function calculates the proportions of each column in a table
ociss_proportions_by_row <- function(a_table) {
  
  ociss_cols <- c("OCISS_Distant_Stage","OCISS_Unknown_Stage","OCISS_Uninsured")

   crudes <- a_table[,"OCISS_Crude_Count"]
  a_table[,"OCISS_Crude_Count"] <- a_table[,"OCISS_Crude_Count"]
  a_table[,ociss_cols] <- 
    lapply(
      a_table[,ociss_cols], 
      function(z) {
        props <- sprintf("%.1f", 100 * (z / crudes))
        r <- paste(z, "zz", props, sep = "")
        ifelse(r == "NAzzNA", NA, r)
      })
  a_table %>% 
    reduce(
      ociss_cols, 
      function(the_df, the_c) {
        separate(the_df, the_c, c(the_c, paste0(the_c, "_%")), sep = "zz", fill = "right")
      }, 
      .init = .
    )
}

#' This function describes the logic to be used for selecting columns
#' This is a good place to hide anything (e.g. incidence calculations censoring per investigators)
tbl_selector <- function(.df, input) {
  
  geo_lookup <- ociss::geo_lookups[[input$gis_group]]

  first_col <- names(geo_lookup)[2]

  rm_this <- names(geo_lookup)[1]
  
  inputs <- reactiveValuesToList(input)

  join_by <- ifelse(input$gis_group == "everyone", "joiner", input$gis_group)
  
  rm_more <- c(
                "acs_age_lt_14_p"
               , "acs_age_15_44_p"
               , "acs_age_45_64_p"
               , "acs_age_65_74_p"
               , "acs_age_gt_75_p"
               ,"acs_r_nonblack"     
               ,"acs_r_nonblack_p"
               , "acs_e_nonhispanic"  
               , "acs_e_nonhispanic_p"
               ,"acs_pov_rate")

  .df %>% 
   select(-one_of(rm_this)) %>% 
    select(-rm_more) %>% 
    select(first_col, everything()) %>% 
    ociss_proportions_by_row() 
}


#' This function puts together the two datasets that will be presented to the user
assemble_data <- function(con, input) {
  
  ## get ohio total row
  oh_total <-  get_ociss_everyone(con, input)

  ## get ociss data
  ociss_data <- get_ociss(con, input)
  
  ## get acs data
  acs_data   <- get_acs(con, input)

  rGEOIDs <- ociss_data[2]
  colnames(rGEOIDs)[1] = "GEOID"
  
  colnames(oh_total)[2] = colnames(ociss_data)[2]
  colnames(oh_total)[length(colnames(oh_total))] = colnames(ociss_data)[length(colnames(oh_total))]
  oh_acs_data <- empty_acs(input)
      
  return_table <-  left_join(ociss_data, acs_data) %>% 
      tbl_selector(input) %>% 
      clean_table_names() 
  
  oh_total_row <- left_join(oh_total, oh_acs_data)%>% 
    tbl_selector(input) %>% 
    clean_table_names() 

  oh_total_row[1] = "Ohio TOTAL"
  
  return_table_mask <- return_table# %>% 
                        #mask_table()
  


  if (input$gis_group == 'GEOID_Place') {
    return_table[["Municipality"]][which(is.na(return_table[["Municipality"]]))[1]] <- 
      "No place designation/Rural"
  }
  
  if (input$gis_group != 'everyone') {
    totals <- vapply(return_table, function(a) sum(as.numeric(a), na.rm = T), double(1))
    
    totals[grepl("% Distant Stage", names(totals))] <-
      round((totals[grepl("OCISS_Distant_Stage", names(totals))] / totals[grepl("Crude Case Count", names(totals))]) *
              100, 1)
    totals[grepl("% Unknown Stage", names(totals))] <-
      round((totals[grepl("OCISS_Unknown_Stage", names(totals))] / totals[grepl("Crude Case Count", names(totals))]) *
              100, 1)
    totals[grepl("% Uninsured at Diagnosis", names(totals))]  <-
      round((totals[grepl("OCISS_Uninsured", names(totals))] / totals[grepl("Crude Case Count", names(totals))]) *
              100, 1)
    totals[grepl("% Community in High ADI", names(totals))] <-
      round((totals[grepl("% Community in High ADI_pop", names(totals))] / totals[grepl("% Community in High ADI_tot_pop", names(totals))]) *
              100, 1)
    
    totals[grepl("% Female", names(totals))] <-
      round((totals[grepl("acs_B01001e26", names(totals))] / totals[grepl("Total Population", names(totals))]) *
              100, 1)
    totals[grepl("% African American", names(totals))]  <-
      round((totals[grepl("acs_r_black", names(totals))] / totals[grepl("Total Population", names(totals))]) *
              100, 1)
    totals[grepl("% Hispanic", names(totals))]  <-
      round((totals[grepl("acs_e_hispanic", names(totals))] / totals[grepl("Total Population", names(totals))]) *
              100, 1)
    
    
    totals[1] <- "TOTAL"
    totals[grepl("Median Household Income", names(totals))] <- NA
    
    # for "Median Time to Treatment (Days)" and "Median Follow-up Time (Years)"
    medians <- vapply(return_table, function(a) mean(as.numeric(a), na.rm = T), double(1))
    
    totals[grepl("Median Time to Treatment", names(totals))] <- sprintf("%.2f",medians[grepl("Median Time to Treatment", names(totals))])
    totals[grepl("Median Follow-up Time", names(totals))] <- sprintf("%.2f", medians[grepl("Median Follow-up Time", names(totals))])
    
    ## Age adjusted incidence rate
    totals[c("Age-Adjusted Incidence Rate","IR LCI","IR UCI")] <-  
      age_adj_inc_rate_total(con, cond=NULL, input,rGEOIDs)
    totals[c("AA Age-Adjusted Incidence Rate", "AA IR LCI", "AA IR UCI")] <-  
      age_adj_inc_rate_aa_total(con,  cond = "( Race1 in ('2') )", input,rGEOIDs)
    totals[c("Non-AA Age-Adjusted Incidence Rate","Non-AA IR LCI", "Non-AA IR UCI")] <-  
      age_adj_inc_rate_nonaa_total(con, cond = "not (Race1 in ( '2' ) )", input,rGEOIDs)
    totals[c("Hispanic Age-Adjusted Incidence Rate","Hispanic IR LCI", "Hispanic IR UCI")] <-  
      age_adj_inc_rate_hispanic_total(con, cond = "( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )", input,rGEOIDs)
    totals[c("Non-Hispanic Age-Adjusted Incidence Rate","Non-Hispanic IR LCI", "Non-Hispanic IR UCI")] <-  
      age_adj_inc_rate_nonhispanic_total(con, cond = "Hispanic = 0", input,rGEOIDs)

    ## Age adjusted mortality rate
    totals[c("Age-Adjusted Mortality Rate", "MR LCI", "MR UCI")] <-  
      age_adj_death_rate_total(con, cond=NULL, input,rGEOIDs)
    totals[c("AA Age-Adjusted Mortality Rate", "AA MR LCI", "AA MR UCI")] <-  
      age_adj_death_rate_aa_total(con,  cond = "( Race1 in ('2') )", input,rGEOIDs)
     totals[c("Non-AA Age-Adjusted Mortality Rate","Non-AA MR LCI", "Non-AA MR UCI")] <-  
       age_adj_death_rate_nonaa_total(con, cond = "not (Race1 in ( '2' ) )", input,rGEOIDs)
     totals[c("Hispanic Age-Adjusted Mortality Rate","Hispanic MR LCI", "Hispanic MR UCI")] <-  
       age_adj_death_rate_hispanic_total(con, cond = "( Hispanic in (1, 2, 3, 4, 5, 6, 7, 8) )", input,rGEOIDs)
     totals[c("Non-Hispanic Age-Adjusted Mortality Rate","Non-Hispanic MR LCI", "Non-Hispanic MR UCI")] <-  
       age_adj_death_rate_nonhispanic_total(con, cond = "Hispanic = 0", input,rGEOIDs)
    
    return_table_mask <- rbind(return_table_mask, totals)
  }

  return_table_mask <- rbind(return_table_mask, oh_total_row)

  rm_last <- c("acs_B01001e26"
               ,"acs_r_black"
               ,"acs_e_hispanic"
               ,"OCISS_Distant_Stage"
               ,"OCISS_Unknown_Stage"
               ,"OCISS_Uninsured"
               ,"% Community in High ADI_tot_pop"
               ,"% Community in High ADI_pop")
  
  return_table2 <- return_table_mask %>% 
    dplyr::select(-rm_last)
  
  
  ## get SEER total row
  seer_total <- get_seertotal(colnames(return_table2)[1],input) %>%select(colnames(return_table2))
  
  return_table2 <- rbind(return_table2, seer_total) %>%
    modify_at(c("Age-Adjusted Incidence Rate","IR LCI","IR UCI",
                "AA Age-Adjusted Incidence Rate", "AA IR LCI", "AA IR UCI",
                "Non-AA Age-Adjusted Incidence Rate","Non-AA IR LCI", "Non-AA IR UCI",
                "Hispanic Age-Adjusted Incidence Rate","Hispanic IR LCI", "Hispanic IR UCI",
                "Non-Hispanic Age-Adjusted Incidence Rate","Non-Hispanic IR LCI", "Non-Hispanic IR UCI",
                "Age-Adjusted Mortality Rate", "MR LCI", "MR UCI",
                "AA Age-Adjusted Mortality Rate", "AA MR LCI", "AA MR UCI",
                "Non-AA Age-Adjusted Mortality Rate","Non-AA MR LCI", "Non-AA MR UCI",
                "Hispanic Age-Adjusted Mortality Rate","Hispanic MR LCI", "Hispanic MR UCI",
                "Non-Hispanic Age-Adjusted Mortality Rate","Non-Hispanic MR LCI", "Non-Hispanic MR UCI"), ~ ifelse(is.na(.x) | is.infinite(.x),0,.x))  
  
  
  return_table2
}

#' This function get SEER total row
get_seertotal <- function(joinid,input)
{
  seertotal <- 
    data.frame(
      'National TOTAL'
      ,NA, NA, NA, NA, NA
      ,NA,  NA,  NA, NA, NA
      ,NA, NA
      ,NA  #Distant Stage(%)
      ,NA  #Unknown Stage(%)
      ,NA    #Median Time to Treatment(Days)"
      ,NA   #Uninsured at Diagnosis(%)
      ,NA   #Median Follow-up Time(Years)
      ,NA #"Crude Incidence"
      ,NA #"Age-Adjusted Incidence"
      ,NA , NA
      , NA,NA ,NA #AA
      , NA,NA ,NA #Non-AA
      , NA,NA , NA # Hispanic
      ,NA ,NA ,NA #Non-Hispanic      
      , NA #"Age-Adjusted Mortality"
      , NA ,NA 
      , NA,NA ,NA #AA
      , NA,NA ,NA #Non-AA
      , NA,NA , NA # Hispanic
      ,NA ,NA ,NA #Non-Hispanic 
      ## pop
      ,NA ,NA ,NA ,NA ,NA ,NA ,NA ,NA  ,NA  ,NA, NA
      
    )
  
  header <- c(joinid,
              "Crude Case Count", "Cases: Male", "Cases: Female",
              "Cases: African American", "Cases: Non-African American",
              "Cases: Hispanic", "Cases: Non-Hispanic",
              "Cases: 14 or under",
              "Cases: 15-44",
              "Cases: 45-64",
              "Cases: 65-74",
              "Cases: 75+",
              "% Distant Stage",
              "% Unknown Stage",
              "Median Time to Treatment(Days)", ## 28.32
              "% Uninsured at Diagnosis",
              "Median Follow-up Time(Years)", ##7.11
              "Crude Incidence",
              "Age-Adjusted Incidence Rate", "IR LCI", "IR UCI",
              "AA Age-Adjusted Incidence Rate", "AA IR LCI", "AA IR UCI",
              "Non-AA Age-Adjusted Incidence Rate","Non-AA IR LCI", "Non-AA IR UCI",
              "Hispanic Age-Adjusted Incidence Rate","Hispanic IR LCI", "Hispanic IR UCI",
              "Non-Hispanic Age-Adjusted Incidence Rate","Non-Hispanic IR LCI", "Non-Hispanic IR UCI",
              "Age-Adjusted Mortality Rate", "MR LCI", "MR UCI",
              "AA Age-Adjusted Mortality Rate", "AA MR LCI", "AA MR UCI",
              "Non-AA Age-Adjusted Mortality Rate","Non-AA MR LCI", "Non-AA MR UCI",
              "Hispanic Age-Adjusted Mortality Rate","Hispanic MR LCI", "Hispanic MR UCI",
              "Non-Hispanic Age-Adjusted Mortality Rate","Non-Hispanic MR LCI", "Non-Hispanic MR UCI",
              "Total Population",
              "% Female",
              "Population: 14 and younger",
              "Population: 15-44",
              "Population: 45-64",
              "Population: 65-74",
              "Population: 75 and older",
              "% African American",
              "% Hispanic",
              "Median Household Income","% Community in High ADI"
  )

  colnames(seertotal) <- header
  
  startyear = substr(input$date_range,1,4)
  endyear = substr(input$date_range_end,1,4)

  distant_per <- NA
  inc_per <- NA
  mor_per <- NA
  
  distant_site <- input$filter_PSite
  mor_site <- input$filter_PSite
  
  if(length(input$filter_PSite) >0)
  {
     distant_site = input$filter_PSite[!input$filter_PSite %in% c("Cervix Uteri","Corpus Uteri","Uterus, NOS",
                                                    "Ovary","Vagina","Vulva",
                                                    "Other Female Genital Organs",
                                                    "Prostate","Testis","Penis",
                                                    "Other Male Genital Organs")]
     
     mor_site = input$filter_PSite[!input$filter_PSite %in% c("Mesothelioma","Kaposi Sarcoma")]
     
     if (length(distant_site) > 0)
     {
       ## Distant
       distant <- seer_distant %>%
         filter(Psite %in% distant_site) %>%
         filter(Stage == 'Distant') %>%
         select(startyear:endyear)
       
       pop <- seer_distant %>%
         filter(Psite %in% distant_site) %>%
         filter(Stage == 'AllKnownStage') %>%
         select(startyear:endyear)
       
       distant_per <- round(sum(distant) / sum(pop), 2)
     }
     ## Incidence
     inc_rate <- seer_incidence %>%
       filter(Psite %in% input$filter_PSite) %>%
       select(startyear:endyear) %>% colSums()
     
     inc_pop <- seer_incidence_pop %>%
       filter(Psite %in% input$filter_PSite) %>%
       select(startyear:endyear) %>% colMeans()
     
     inc_per <- round(sum(inc_rate * inc_pop) / sum(inc_pop), 2)
     
     ## Mortality
     if (length(mor_site) > 0) {
       mor_rate <- seer_mortality %>%
         filter(Psite %in% mor_site) %>%
         select(startyear:endyear) %>% colSums()
       
       mor_pop <- seer_mortality_pop %>%
         filter(Psite %in% mor_site) %>%
         select(startyear:endyear) %>% colMeans()
       
       mor_per <- round(sum(mor_rate * mor_pop) / sum(mor_pop), 2)
     }
  }
  
 seertotal[grepl("Distant Stage", names(seertotal))] <- distant_per
 seertotal[grepl("Age-Adjusted Mortality Rate", names(seertotal))] <- mor_per
 seertotal[grepl("Age-Adjusted Incidence Rate", names(seertotal))] <- inc_per

 seertotal   
  
}

#' This function mask small numbers in a result table
mask_table <- function(return_table)
{
  #browser()
  
  m_return_table = return_table
  
  # index of columns to mask
  mt_index <- which(colnames(m_return_table) %in% c("Crude Case Count",
                                                    "Cases: Male",
                                                    "Cases: Female",
                                                    "Cases: African American",
                                                    "Cases: Non-African American",
                                                    "Cases: Hispanic",
                                                    "Cases: Non-Hispanic",
                                                    "Cases: 14 or under",
                                                    "Cases: 15-44",
                                                    "Cases: 45-64",
                                                    "Cases: 65-74",
                                                    "Cases: 75+"))
                                                   # ,"OCISS_Distant_Stage"
                                                  #  ,"OCISS_Unknown_Stage"
                                                  #  ,"OCISS_Uninsured"))
  
  #browser()
    m_return_table[c(mt_index)][return_table[c(mt_index)]<11] <- 'M'
    m_return_table[c(mt_index)][return_table[c(mt_index)]=="NaN"] <- 'M'
    m_return_table[c(mt_index)][return_table[c(mt_index)]=="Inf"] <- 'M'

  #   If only one cell in the column has <11, mask that cell and the cell with the next lowest count. 
  m_return_table[c(mt_index)] <- sapply(m_return_table[c(mt_index)],replacemask)
  
  
  #if count is 'M', then % should be 'M' for 
  # m_return_table[which(colnames(m_return_table) %in% c("% Distant Stage")][m_return_table[which(colnames(m_return_table) %in% c("OCISS_Distant_Stage"))] ==
  #                                                                            'M'] <- 'M'
  # 
  # m_return_table[which(colnames(m_return_table) %in% c("% Unknown Stage")][m_return_table[which(colnames(m_return_table) %in% c("OCISS_Unknown_Stage"))] ==
  #                                                                            'M'] <- 'M'
  # m_return_table[which(colnames(m_return_table) %in% c("% Uninsured at Diagnosis")][m_return_table[which(colnames(m_return_table) %in% c("OCISS_Uninsured"))] ==
  #                                                                            'M'] <- 'M'
  # 
  # 
  
  #browser()
  
  m_return_table
  
  
  
}

#Mask Function  
#Need to masking small values
#Rules
#1. In a given column containing case counts or numbers of death, mask any cell with a count <11.  
#   If only one cell in the column has <11, mask that cell and the cell with the next lowest count.  
#2. Total (bottom) row: If any cell containing counts of cases or deaths in the total (bottom row) is <11, mask that cell.
#3. Rates: If the total (i.e. not subdivided) number of cases for a given row is <11, 
#   mask the incidence and mortality rates (including age-adjusted) and confidence intervals.   

mask <- function(x){
  D = sum(x < 11,na.rm = TRUE)
  ## only one cell < 11 and there is other lowest count value
  if(D==1)
  {
    x[x<11] <- min(x[x!=min(x,na.rm = TRUE)],na.rm = TRUE)
  }
  else
    x[x<11] <- 'M'
  return(x)
}

replacemask <- function(x){
  D = sum(x=='M',na.rm = TRUE)
  ## only one cell < 11 and there is other lowest count value
  if(D==1)
  {
    x[x==min(as.numeric(x),na.rm = TRUE)] <- 'M'
  }
  else
    x[as.numeric(x)<11] <- 'M'
  return(x)
}
