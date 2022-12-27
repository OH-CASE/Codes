#!/usr/bin/R

{ # setup
  devtools::load_all("ociss")
  load_libraries(ociss::libs)
  #library(shinyjs)
  con <- cancer_con()
  write(x = timestamp(), file = "last-run", append = TRUE)

  # make ui
  sd_header  <- dashboardHeader(title = "OHCase Cancer Monitor", titleWidth = 290)
  sd_sidebar <- mk_sidebar()
  sd_body    <- mk_body(ociss = mk_dat_page(), a = fluidPage())
  ui         <- dashboardPage(sd_header, sd_sidebar, sd_body)
  
  #shinyjs::disabled("filter_CSSSF16")
}

reset_these <- 
  c(
    "filter_RuralUrbanCon2013",
    "filter_GEOID_County", 
    "filter_AgeDxC_OCISS2",
    "filter_Sex", 
    "filter_Race1", 
    "filter_Hispanic",
    "filter_PSite", 
    "filter_CSSSF16", 
    "filter_OHStage" 
  )


# Define server logic
server <- function(input, output, session) {
  
  sql_rct  <- reactive({ ociss_sql(input) }) 
  data_rct <- eventReactive(input$refresh, { assemble_data(con, input) })

  # store as a vector to be called wherever desired
  # evaluated whenever inputs change
  columnsetInput <- reactive({
    df <- data_rct() 
    cols = which(colnames(df) %in% input$show_vars_case)
    cols_pop = which(colnames(df) %in% input$show_vars_pop)
    
    #if(match("Sex", input$show_vars,nomatch=0)>0)
    if("Sex" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("Cases: Male","Cases: Female")))
    }

    if("Age" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("Cases: 14 or under", "Cases: 15-44",
                                                "Cases: 45-64", "Cases: 65-74",
                                                "Cases: 75+"  )))
    }
    if("Race" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("Cases: African American",
                                                "Cases: Non-African American"   )) )
    }
    if("Ethnicity" %in% input$show_vars_case)
    {
      cols <- c(cols,which(colnames(df) %in% c("Cases: Hispanic",
                                               "Cases: Non-Hispanic" )))
    }
    if("Age-Adjusted Incidence Rate" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("IR LCI",
                                                "IR UCI"   )))
    }
    if("Age-Adjusted Mortality Rate" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("MR LCI",
                                                "MR UCI"   )))
    } 
    if("AA/Non-AA Age-Adjusted Incidence Rate" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("AA Age-Adjusted Incidence Rate", "AA IR LCI", "AA IR UCI",
                                                "Non-AA Age-Adjusted Incidence Rate","Non-AA IR LCI", "Non-AA IR UCI" )))
    }
    if("Hispanic/Non-Hispanic Age-Adjusted Incidence Rate" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("Hispanic Age-Adjusted Incidence Rate","Hispanic IR LCI", "Hispanic IR UCI",
                                                "Non-Hispanic Age-Adjusted Incidence Rate","Non-Hispanic IR LCI", "Non-Hispanic IR UCI")))
    }
    if("AA/Non-AA Age-Adjusted Mortality Rate" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("AA Age-Adjusted Mortality Rate", "AA MR LCI", "AA MR UCI",
                                                "Non-AA Age-Adjusted Mortality Rate","Non-AA MR LCI", "Non-AA MR UCI")))
    }
    if("Hispanic/Non-Hispanic Age-Adjusted Mortality Rate" %in% input$show_vars_case)
    {
      cols <- c(cols, which(colnames(df) %in% c("Hispanic Age-Adjusted Mortality Rate","Hispanic MR LCI", "Hispanic MR UCI",
                                                "Non-Hispanic Age-Adjusted Mortality Rate","Non-Hispanic MR LCI", "Non-Hispanic MR UCI")))
    }
    if("By Age" %in% input$show_vars_pop)
    {
      cols_pop <- c(cols_pop, which(colnames(df) %in% c("Population: 14 and younger",
                                                "Population: 15-44",
                                                "Population: 45-64",
                                                "Population: 65-74",
                                                "Population: 75 and older"   )))
    }
 
    
    
    colsall = c(1,cols,cols_pop)
    df[,sort(colsall)]
    
  }) 
  
  
  observeEvent(input$reset, {
    for (i in seq_along(reset_these)) { 
      updateSelectizeInput(session, reset_these[[i]], selected = "") 
    }
    updateSelectInput(session, "gis_group", selected = ociss::geo_levels[[1]])
    
    updateSelectInput(session, "date_range", selected="2006 Q1")
    updateSelectInput(session, "date_range_end", selected="2017 Q4")
    
  })
  
  observe({
    x <- input$filter_PSite
    
    if (is.null(x))
    {
      shinyjs::disable("filter_CSSSF16")
      updateSelectizeInput(session, "filter_CSSSF16", selected = "") 
    }
  })
  
  observeEvent(input$filter_PSite, {
    if(any(input$filter_PSite == "Breast"))
      shinyjs::enable("filter_CSSSF16")
    else
    {
      shinyjs::disable("filter_CSSSF16")
      updateSelectizeInput(session, "filter_CSSSF16", selected = "") 
    }
  })
  
  
  # observe({
  #   x <- input$filter_Hispanic
  #   y <- input$filter_Race1
  #   
  #   if (!is.null(x) && !is.null(y))
  #   {
  #     
  #     shinyjs::disable(selector = "#show_vars_case input[value='Age-Adjusted Incidence Rate']")
  #     shinyjs::disable(selector = "#show_vars_case input[value='Age-Adjusted Mortality Rate']")
  #     
  #     
  #     
  #     updateCheckboxGroupInput(session = session, inputId = "show_vars_case", choices = 
  #                              c("Crude Case Count",
  #                                "Sex",
  #                                "Age",
  #                                "Race",
  #                                "Ethnicity",
  #                                "Age-Adjusted Incidence Rate",
  #                                "AA Age-Adjusted Incidence Rate",
  #                                "% Distant Stage",
  #                                "% Unknown Stage",
  #                                "Median Time to Treatment(Days)",
  #                                "% Uninsured at Diagnosis",
  #                                "Age-Adjusted Mortality Rate",
  #                                "Median Follow-up Time(Years)"
  #                              ), selected = c("Crude Case Count",
  #                                              #"Age-Adjusted Incidence Rate",
  #                                              "% Distant Stage",
  #                                              "% Unknown Stage",
  #                                              "Median Time to Treatment(Days)",
  #                                              "% Uninsured at Diagnosis",
  #                                              #"Age-Adjusted Mortality Rate",
  #                                              "Median Follow-up Time(Years)")) 
  #     
  #     
  #   }
  #   else
  #   {
  #      updateCheckboxGroupInput(session = session, inputId = "show_vars_case", choices = 
  #                                c("Crude Case Count",
  #                                  "Sex",
  #                                  "Age",
  #                                  "Race",
  #                                  "Ethnicity",
  #                                  "Age-Adjusted Incidence Rate",
  #                                  "% Distant Stage",
  #                                  "% Unknown Stage",
  #                                  "Median Time to Treatment(Days)",
  #                                  "% Uninsured at Diagnosis",
  #                                  "Age-Adjusted Mortality Rate",
  #                                  "Median Follow-up Time(Years)"
  #                                ), selected = c("Crude Case Count",
  #                                                "Age-Adjusted Incidence Rate",
  #                                                "% Distant Stage",
  #                                                "% Unknown Stage",
  #                                                "Median Time to Treatment(Days)",
  #                                                "% Uninsured at Diagnosis",
  #                                                "Age-Adjusted Mortality Rate",
  #                                                "Median Follow-up Time(Years)"))
  #     
  #     shinyjs::enable(selector = "#show_vars_case input[value='Age-Adjusted Incidence Rate']")
  #     shinyjs::enable(selector = "#show_vars_case input[value='Age-Adjusted Mortality Rate']")
  #     
  #   }
  # })
  

  observeEvent(input$refresh, {
   output$ociss_data_mask <-
      renderDataTable({
        DT::datatable(
          columnsetInput(),
          rownames = FALSE,
          extensions = 'FixedColumns',
          options = list(
            scrollX = TRUE
            , fixedColumns = list(leftColumns = 1, rightColumns = 0)
          )
        )
      })

  })
  
  
  
  #test_rct <- eventReactive(input$refresh, { reference_pop(input, agg = TRUE) })
  #output$test_data <- renderDataTable(DT::datatable(test_rct()))

  # output$ociss_data <-
  #   renderDataTable({
  #     DT::datatable(
  #       data_rct(),
  #       rownames = FALSE,
  #       extensions = 'FixedColumns',
  #       options = list(
  #         scrollX = TRUE
  #         , fixedColumns = list(leftColumns = 1, rightColumns = 0)
  #         )
  #       )
  #     })
  
  
  # output$ociss_data_mask <- 
  #   renderDataTable({
  #     DT::datatable(
  #       #df[, input$show_vars, drop = FALSE], 
  #       data_rct(),
  #       rownames = FALSE,
  #       extensions = 'FixedColumns',
  #       options = list(
  #         scrollX = TRUE
  #         , fixedColumns = list(leftColumns = 1, rightColumns = 0)
  #       )
  #     )
  #   })
  
  
  output$ociss_sql    <- renderText(sql_rct())
  
  filters_txt              <- reactive({ filters_printer(input) })
  output$filters_explained <- renderUI(HTML(filters_txt()))
  

  
  # for debugging
  # observeEvent(input$save_input, {
  #   saved_input <- map(c(reset_these, "gis_group"), input$get) %>% set_names(c(reset_these, "gis_group"))
  #   devtools::use_data(saved_input, pkg = "ociss")
  # })
    
  output$neoccm <- downloadHandler(file = "neoccm.csv", 
                                   content = function(file)
                                     {
                                     write.csv(columnsetInput(),file, row.names=FALSE)
                                   }) #writer(data_rct())
  
}


