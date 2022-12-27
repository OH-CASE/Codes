# Codes

Project structure
------------------

The project itself is (effectively) entirely contained in an R packge, 'OH-CASE'. 

The `app.R` file is contained in `ociss/inst`, and is the script that is run by `run_app`. 

In general, data are left in the SQL server until they are necessary; this reduces overhead in the R session. Some objects, specifically those that are unaffected by user input, are created before the app itself is run. Again, this reduces overhead in the R session. 

If asked to change anything (e.g. labels for filters), first check these scripts to see whether such a change can be specified up-front, thus allowing the change to propagate through the app without requiring any (or very little) manual intervention.

Project layouts and user interface specification occurs in files with the prefix 'ui', these are located in `/R`. 

Age adjusted incidence and mortality calculation occurs in files with the prefix 'age-adjustment', these are located in `/R`.


Design
---------------

This app builds a final 'output' table, column-by-column for OCISS data and all-at-once for ACS data. The main driver is `assemble_data` (in `/Man` folder), which accepts a shiny input object and returns a table meeting filter/grouping criteria specified within that input. See also: `ociss_sql`, `acs_sql`, `get_ociss`, and `get_acs`

UI elements are generated programmatically using the '/data-raw/verbose-objects.R' script. To change any UI elements, first check here to see if you can modify the script to programmatically create whatever UI elements that the team decides it would like to include. 

Each of the columns reported in OH-CASE is pulled on its own using a list that contains (1) the function to pull data and (2) a name for message output to indicate the app is pulling that column. Though not strictly necessary, this is nice because it lets you know what is going on in the app (by showing a message). 



Functions in `/Man` folder
-----------------------

acs_sex.Rd: Determines which sex to include in the denominator of population calculations

age_adj_inc_rate.Rd: Calculates the age-adjusted incidence rate, with confidence intervals

age_strat.Rd: Retrieves age-stratified counts

agg_sum.Rd: Aggregates and takes the sum over a formula. Values are returned as integers

assemble_data.Rd: Puts together the two datasets that will be presented to the user

clean_nm.Rd: Is used by 'assets.R' to generate clean filter names

clean_table_names.Rd: Is used to clean output table names

count_tables.Rd: Retrieves reference and case counts, with case incidence

csv_nm.Rd: Generates a name for a CSV file. Returns a function of no arguments

empty_acs.Rd: In the event that ACS calculations can't be returned, this is returned instead

everyone_crude_inc.Rd: Gets crude incidence for overall population, after filtering by county

fct_lump.Rd: Lump together factor levels into "other" (borrowed from https://github.com/tidyverse/forcats/blob/master/R/lump.R)

filters_printer.Rd: Puts all filter criteria into a neat format for display

get_acs.Rd: Retrieves population-level data, calculate relevant proportions

get_filters.Rd: Retrieves filters from input

get_ociss.Rd: Retrieves input-appropriate OCISS data

load_app_stuff.Rd: Sources the `inst/app.R` file

load_libraries.Rd: Loads all libraries

mk_acs_page.Rd: The second page of the interface (This page has yet to be implemented, the present function is a placeholder)

mk_body.Rd: Makes the body of the app

mk_dat_page.Rd: Makes the user interface page for OCISS table construction

mk_sidebar.Rd: Makes the sidebar menu for the interface

mk_tab_items.Rd: Sets up items for the left-hand side tab in the interface

n_years.Rd: Calculates the number of years that a user has requested

ociss_case_count.Rd: Gets numbers of cases from ociss data, with user input

ociss_crude_inc.Rd: Calculates crude incidence

ociss_filter.Rd: Creates an ociss filter, given user input

ociss_fu_sql.Rd: Generates sql to retrieve time to follow up among user-selected cases

ociss_in.Rd: This function shouldn't be called unless there's something to put in 'in'

ociss_proportions_by_row.Rd: Calculates the proportions of each column in a table

ociss_sql.Rd: Generates a SQL statement, given user input and/or developer choices of extra where classes ('wand'), select columns ('sand') and grouping columns ('gand')

ociss_tx_sql.Rd: Generates SQL to retrieve time to treatment from OCISS data, given user input

p_com.Rd: Pastes with `,` as a collapser

query.Rd: Executes a query

reference_pop.Rd: Returns reference population counts--these come from the ACS data

rename_cc.Rd: In a query that returns case counts, renames the 'case_count' column to something that is easier to read

repl_names.Rd: Convenience function to replace names

run_app.Rd: Sources the `inst/app.R` file and runs the app

safely_group.Rd: Creates a group by clause from a set of groupings

safely_in.Rd: Returns several 'in' statements, given a named pairlist of things to have 'in'

safely_select.Rd: Creates a select clause from a set of groups and/or a first-selected column

sanitize_filters.Rd: Converts filter names to something easier to read. 

simple_q_gen.Rd: Uses this to generate a simple query, defined as one requiring a single extra string to filter

tbl_selector.Rd: Describes the logic to be used for selecting columns

text_message.Rd: Sends a message

totals.Rd: Retrieves case counts

tt_gen.Rd: Creates a list of (1) a function to pull data for time to event and (2) the name for that column to be added

tt_tx_fn.Rd: Uses the above functions to incorporate times to events

writer.Rd: Writes a CSV file, organized to curry


Functions in `/R` folder:
----------------------

age-adjustment.R: Calculates the age-adjusted incidence rate, with confidence intervals

assemble-data.R: Calculates the proportions of each column in a table

filters-printer.R: Puts all filter criteria into a neat format for display

get-acs.R: Used in the event that ACS calculations can't be returned

lump.R: Lumps together least/most common factor levels into "other"

sanitizers.R: Used to generate filter names

sql-acs.R: Determines which metadata table to use

sql-helpers.R: Creates a group by clause from a set of groupings

sql-ociss.R: Create an ociss filter, given user input

ui-page-ociss.R: Makes the user interface page for OCISS table construction
