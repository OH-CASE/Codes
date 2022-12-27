# The second page of the interface, this will likely need to be updated as project goals grow
mk_acs_page <- function() {
  fluidPage(
    br(),
    titlePanel("Select your NEO-Case denominator"),
    br(), br(), 
    p("Features in this section are under development, but will initially focus on total"),
    p("population comparisons. Once these (10) tables are done, we plan to move on to the"),
    p("filtered tables (e.g. age by sex - asian people only)."),
    br(), br(), 
    p("There are many ways to count the population, especially in census data."),
    p("Use the following selectors to decide which census denominator you would like to use."),
    br(), br(), 
    fluidPage(
      fluidRow(
         selectInput("which_acs", 
                     "American Community Survey - Census table selection", 
                     ociss::acs$totals$fn1)
      )
    ), br(),
    p("Please note that this is only a preview, click on \"Download Table\" to obtain the full table."),
    box(tableOutput("joined_data"), width = 12)
    ,
    br(), 
    downloadButton("neoccm_joi", "Download Table", class = "btn-default btn-xs")
  )
}

















