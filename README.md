# Data-Content-Standards
Data Content Standards Report

This project provides R code with SQL queries to produce NatureServe's Biodiversity Data Content Standards Report. The code chunks are designed to be run sequentially. The first script creates a file containing the primary element dataset and outputs it to a folder named "Output." The subsequent scripts make additional queries to biotics and modify and re-write the output dataset. The final scripts wrangle the data, create plots, and create either a word document or powerpoint presentation of the plots and figure captions. Users need to first set up an odbc connection to a Biotics snapshot for the code to run.
