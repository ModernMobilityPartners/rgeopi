
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rgeopi

<!-- badges: start -->

<!-- badges: end -->

rgeopi exists to extract project information from GDOT’s public-facing
GeoPI system.

## Installation

You can install the development version of rgeopi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ModernMobilityPartners/rgeopi")
```

This package was developed by Modern Mobility Partners.

## GeoPI Data

Pass Project IDs to the function `get_geopi_ef`, choose what data you
want from GeoPI, and whether or not you want spatial data.

You can also call `get_geopi_sf` if you only want spatial data.

`get_geopi_ef` has the following parameters:

``` r

get_geopi_ef(
  
  gdot_pi, # Vector of GDOT PIs
  
  session = NULL, # By default will create a new GeoPI session.
  # You may pass it an existing session if desired.
  # Sessions can be created alone with `geopi_session()`.
  
  features = c("overview",     # Project details at the top of the page 
               "phases",       # Phases, years, and costs
               "documents"),   # Documents at the bottom of the page
  # `features` defaults to all
  
  
  doc_mode = c("cr_only",      # Summary of the construction reports only
               "cr_check",     # Whether or not there are Construction Reports
               "doc_summary"), # What type of documents there are
  # `doc_mode` defaults to 'cr_only'
  
  geometry = FALSE, # TRUE/FALSE - get spatial data for the project(s)
  
  pi_check = TRUE,
  # TRUE/FALSE - Check if the PIs are valid and only request data for valid PIs
  
  gather_date = NULL
  # Provide the date you're gathering the information or it will assign today
)
```

## Exporting

The result of `get_geopi_ef` is a list. You can choose explore/analyze
the data in R, or export it to your whim.

If you pass it to `format_place_gis()`, it will format an sf object
based on the geometry and overview data to be used with the PLA+CE GIS
Tool.
