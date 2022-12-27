
#' Create a group by clause from a set of groupings
safely_group <- function(gps) {
  #if (length(gps) > 0 & !(any(gps %in% c("", "everyone")))) return(paste("group by", p_com(gps)))
  gps <- gps[!(gps %in% c(1, "everyone"))]
  if (length(gps) == 0) return("")
  paste("group by", p_com(gps))
}

#' Create a select clause from a set of groups and/or a first-selected column
safely_select <- function(gps, fst = "count(*) as case_count,") {
  if (any(gps %in% c(1, "everyone"))) gps <- c(gps, '1 as joiner')
  gps <- gps[!(gps %in% c(1, "everyone"))]
  if (length(gps) == 0) return(fst)
  paste(fst, p_com(gps))
}

#' Return several 'in' statements, given a named pairlist of things to have 'in'
safely_in <- function(...) {
  the_dts <- dots_list(...)
  if (any(map_lgl(the_dts, is.list))) stop("This function only accepts atomic vectors")

  each_in_statement <- 
    imap(
      the_dts, 
      function(xs, nm) {
        if (is.character(xs)) xs <- paste("'", xs, "'", sep = "")
        paste(nm,
             " in (", 
             paste(xs, collapse = ", "), 
             ") ", 
             sep = " "
             )
      })
  paste(each_in_statement, collapse = " and ")
}

# drop_tables <- function(con, z) {
#   ifob <- "if OBJECT_ID('tempdb..##"
#   nndr <- "') is not null DROP TABLE ##"
#   map(z, ~ paste(ifob, .x, nndr, .x, sep = "")) %>% 
#     map(partial(dbSendStatement, conn = con))
# }
# 
# exists_tmp <- function(con, tmp_tbl, l = FALSE) {
#   query <- 
#     paste(
#       "SELECT [name] FROM tempdb.dbo.sysobjects WHERE xtype = 'U' and [name] like '##", 
#       tmp_tbl,
#       "%'", 
#       sep = ""  
#     )
#   if (l) dbGetQuery(con, query)
#   else nrow(dbGetQuery(con, query)) >= 1
# }

