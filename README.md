# Data-Content-Standards
Data Content Standards Report

This project provides R code with SQL queries to produce NatureServe's Biodiversity Data Content Standards Report. The code chunks are designed to be run sequentially. The first script creates a file containing the primary element dataset and outputs it to a folder named "Output." The subsequent scripts make additional queries to biotics and modify and re-write the output dataset. The final scripts wrangle the data, create plots, and create either a word document or powerpoint presentation of the plots and figure captions. Users need to first set up an odbc connection to a Biotics snapshot for the code to run.

Script steps
1. select-primarysubsetglobal.R
2. select-threatcategory.R
3. select-rankchangereason.R
4. select-habitatcategory.R
5. wrangle-data.R
6. summarize-data.R
7. select-primarysubsetglobalecosystems.R
8. Select-EO-attributes.R
9. Summarize-Data-EO.R
10. Select-Ecosystem-EO-Attributes.R
11. plot-standards.R
12. plot-standards-multiyear.R
13. data-content-standards-doc.Rmd

Update notes:
PrimarySubsetGlobal datasets 2022-2024 include exotics and GNAs. These elements were retroactively identified for exclusion using the select-native.R script