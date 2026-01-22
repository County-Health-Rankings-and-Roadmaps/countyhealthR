# countyhealthR
an R package to quickly pull CHR&amp;R data into R, modeled after tidycensus

Run the following R code to quickly install and load `countyhealthR` into your local environment: 
```r
# Quick Start 
library(devtools)
devtools::install_github("County-Health-Rankings-and-Roadmaps/countyhealthR")
library(countyhealthR)

# Example: county-level "premature death" data for 2024
get_chrr_measure_data("county", "premature death", 2024)
``` 
