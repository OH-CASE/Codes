
#' Make the user interface page for OCISS table construction
mk_dat_page <- function() {
  geo_levels <- ociss::geo_levels
  len_fil <- length(ociss::filters_ui)
  filters_ui <- ociss::filters_ui
  filters_col1 <- filters_ui[1:(round(len_fil/2))]
  filters_col2 <- filters_ui[(round(len_fil/2) + 1):len_fil]
  
  fluidPage(
    shinyjs::useShinyjs(),
    br(),
    titlePanel("Select population of interest"),
    tags$strong(p("Describe how to aggregate your data.")),
    br(), 
    tags$strong(p("Start by describing how to group the data (i.e. the categories over which to report numbers of cases):")),
    br(), br(), 
    fluidPage(
      fluidRow(
        column(6, selectInput("gis_group", "Geographic level", geo_levels))
      )
    ), br(),
    tags$strong(p("Next, choose how you want to filter the data to keep only certain patients:")),
    # two columns of filter options
    fluidPage(
      fluidRow(
        column(width = 6, filters_col1), 
        column(width = 6, filters_col2$Race1, filters_col2$Hispanic, filters_col2$PSite, shinyjs::disabled(filters_col2$CSSSF16), filters_col2$OHStage)
      )
    ),
    
    #disabled(selectInput("filter_CSSSF16","Breast Cancer Receptor Status",lookups$CSSSF16[,2] ,multiple = T)),
    
    tags$em(p("*Note that reporting of in situ and borderline cases to cancer registry is inconsistent.")),
    tags$em(p("*In situ cases are excluded by default, except in the case of bladder cancer.")),
    tags$em(p("*Available U.S. Census data does not permit age-adjustment for groups 
              specified by both race AND ethnicity. Age adjusted estimates can be provided for specific race OR groups.")),
    br(),  br(),
    box(htmlOutput("filters_explained")),
     fluidPage(
      fluidRow(
        column(width = 6), 
        column(width = 6,
               p("When you are ready, click 'Generate Table' to see your table."),
               actionButton("refresh", "Generate Table"),
               br(), br(), 
               p("Click the button below to reset your filtering criteria."),
               actionButton("reset", "Reset filters"),
               br(), br(),
               p("Please click on \"Download Table\" to obtain the full table."),
               downloadButton("neoccm", "Download table", class = "btn-default btn-xs"),
               br(), br()
               # , p("Click the button below to save input."),
               # actionButton("save_input", "Save input"),
               # br(), br()
        )
      )
    ),
    br(), br(),
    #box(DT::dataTableOutput("ociss_data"), width = 12),
    #box(width=12,
    #    tabBox(width=12,id="tabBox",
    #           tabPanel("Original Table",DT::dataTableOutput("ociss_data")),
    #           tabPanel("Masked Table",DT::dataTableOutput("ociss_data_mask"))
     #   )),
    br(), br(),
    box(width=12,
        tabBox(width=12,id="tabBox",
               tabPanel("Detail View",#column(width = 10, DT::dataTableOutput("ociss_data_mask"))),
                 fluidPage(
                    fluidRow(
                      column(width = 3,
                              wellPanel(
                        checkboxGroupInput(
                           "show_vars_case",
                           "Case Information:",
                            c("Crude Case Count",
                              "Sex",
                              "Age",
                              "Race",
                              "Ethnicity",
                              "Age-Adjusted Incidence Rate",
                              "AA/Non-AA Age-Adjusted Incidence Rate",
                              "Hispanic/Non-Hispanic Age-Adjusted Incidence Rate",
                              "% Distant Stage",
                              "% Unknown Stage",
                              "Median Time to Treatment(Days)",
                              "% Uninsured at Diagnosis",
                              "Age-Adjusted Mortality Rate",
                              "AA/Non-AA Age-Adjusted Mortality Rate",
                              "Hispanic/Non-Hispanic Age-Adjusted Mortality Rate",
                              "Median Follow-up Time(Years)"
                              )
                            , selected = c("Crude Case Count",
                                           "Age-Adjusted Incidence Rate",
                                           "% Distant Stage",
                                           "% Unknown Stage",
                                           "Median Time to Treatment(Days)",
                                           "% Uninsured at Diagnosis",
                                           "Age-Adjusted Mortality Rate",
                                           "Median Follow-up Time(Years)")
                         ),
                        checkboxGroupInput(
                          "show_vars_pop",
                          "Population Information:",
                          c("Total Population",
                            "% Female",
                            "By Age",
                            "% African American",
                            "% Hispanic",
                            "Median Household Income",
                            "% Community in High ADI"
                          )
                          , selected = c("Total Population","Median Household Income","% Community in High ADI")
                        )
                       
                      )
                      ),
                      column(width = 9, DT::dataTableOutput("ociss_data_mask"))),
                    fluidRow(
                      p("Note: Time to treatment is calculated only for cases diagnosed on or after January 2010"),
                      p("'M' indicates that a value is masked to protect the privacy of individuals when the number of cases or deaths in an area is <11."),
                      p("‘% Community in High ADI’ represents the proportion of the population residing in census block groups with Area Deprivation Index values of 9 or 10 out of a possible 10 (representing the highest degree of deprivation)")
                      ))
                 ),
               tabPanel("Summary View",h2("This is the Summary View panel.")),
               tabPanel("Geographic View",h2("This is the Geographic View panel."))
        )
    ),
    br(),
    
    
    #p("Note: Time to treatment is calculated only for cases diagnosed on or after January 2010"),
    #p("'M' indicates that a value is masked to protect the privacy of individuals when the number of cases or deaths in an area is <11."),
    box(textOutput("ociss_sql"), width = 12),
    br()
  )
}