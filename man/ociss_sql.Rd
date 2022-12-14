% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sql-ociss.R
\name{ociss_sql}
\alias{ociss_sql}
\title{Generate a SQL statement, given user input and/or developer 
choices of extra where classes ('wand'), select columns ('sand') and 
grouping columns ('gand')}
\usage{
ociss_sql(input, wand = NULL, sand = NULL, gand = NULL)
}
\arguments{
\item{input}{A Shiny input object}

\item{wand}{'Where' + wand}

\item{sand}{'select' + sand}

\item{gand}{'group by' + gand, plus 'select' + gand}
}
\value{
Character vector containing a SQL statement constructed according
    to input and formal argument parameters
}
\description{
Generate a SQL statement, given user input and/or developer 
choices of extra where classes ('wand'), select columns ('sand') and 
grouping columns ('gand')
}
