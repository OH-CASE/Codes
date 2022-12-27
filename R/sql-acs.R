
# # Determine which metadata table to use
# which_table <- function(.in, short_names) {
#   if(.in == "everyone") stop("This function can't be called for everyone")
#   stopifnot(.in %in% c("GEOID_County"))
#   in_here <- ociss::acs_metadata[[.in]]
#   in_here[["t_nm"]][in_here[["c_nm"]] %in% short_names]
# }

acs_sql <- function(table_catalog) {
  paste(
    "select
    	a.GEOID
  	  , age_denom
  	  , B01001e26
  	  , age_lt_14
      , age_15_44
      , age_45_64
      , age_65_74
      , age_gt_75
      , race_denom
      --, r_native_american + r_asian + r_hawaiian + r_white as r_nonblack
      , race_denom - r_black as r_nonblack
      --, r_asian
      , r_black
      --, r_hawaiian
      --, r_white
      , ethnicity_denom
      , e_hispanic
      , e_nonhispanic
      , pov_numer
      , pov_denom
      , house_incom
      , NULL as adi 
    from (
      SELECT [GEOID]
        , B02001e1 race_denom
        , B02010e1 r_native_american
        , B02011e1 r_asian
        , B02009e1 r_black
        , B02012e1 r_hawaiian
        , B02008e1 r_white
      FROM [", table_catalog, "].[dbo].[X02_RACE] 
    ) a
    full join (
      SELECT [GEOID]
        , B03001e1 ethnicity_denom
        , B03001e3 e_hispanic
        , B03001e2 e_nonhispanic
      FROM [", table_catalog, "].[dbo].[X03_HISPANIC_OR_LATINO_ORIGIN]
    ) z on a.GEOID = z.GEOID
    full join (
      SELECT [GEOID]
        , B17017e2 pov_numer
        , B17017e1 pov_denom
      FROM [", table_catalog, "].[dbo].[X17_POVERTY_D]
    ) x on a.GEOID = x.GEOID
    full join (
      SELECT GEOID, B19013e1 as house_incom FROM [", table_catalog, "].[dbo].[X19_INCOME_A]
    ) y on a.GEOID = y.GEOID
  	full join (
  	  select GEOID
  	  , B01001e26 
      , B01001e3 + B01001e4 + B01001e5 + B01001e27 + B01001e28 + B01001e29 age_lt_14
      , B01001e6 + B01001e7 + B01001m8 + B01001m9 + B01001m10 + B01001m11 + B01001e30 + B01001e31 + B01001m32 + B01001m33 + B01001m34 + B01001m35 + B01001e12 + B01001e13 + B01001e14 + B01001e36 + B01001e37 + B01001e38 age_15_44
      , B01001e15 + B01001e16 + B01001e39+ B01001e40+B01001e17 + B01001e18 + B01001e19 +B01001e41+ B01001e42 + B01001e43 age_45_64
      , B01001e20 + B01001e21 + B01001e22 +B01001e44 + B01001e45+ B01001e46 age_65_74
      , B01001e23  + B01001e24 + B01001e47+ B01001e48+B01001e25 +B01001e49 age_gt_75
      , B01001e1 age_denom
      from [", table_catalog, "].[dbo].[X01_AGE_AND_SEX]
    ) w on a.GEOID = w.GEOID
    ", 
    sep = ""
  )
}

