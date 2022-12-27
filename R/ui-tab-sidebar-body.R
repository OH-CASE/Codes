
#' Set up items for the left-hand side tab in the interface
mk_tab_items <- function(ociss, a) {
  tabItems(
    tabItem(ociss, tabName = "oci_page"),
    #tabItem(acs,   tabName = "acs_page"),
    tabItem(a,     tabName = "ana_page")
  )
}

#' Make the body of the app
mk_body <- function(...) {
  dashboardBody(mk_tab_items(...))
}

#' Make the sidebar menu for the interface
mk_sidebar <- function() {
  dashboardSidebar(
    sidebarMenu(
      menuItem("Define your dataset", tabName = "oci_page"),
      #menuItem("2. Define a denominator (ACS)", tabName = "acs_page"),
      menuItem("Visualize",           tabName = "ana_page")
    )
  )
}