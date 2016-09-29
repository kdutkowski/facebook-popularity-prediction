# Facebook Popularity Prediction

The R script performs linear regression on number of views of a video in first 24h
to predict the number of views after 168h.

## To run the script
Open `popularityPrediction.R` in R Studio and run parts of it.

## PDF generation
To make reading of results easier, there's a Sweave document with a report
about what's been done in the script. It contains copied pieces of the script.

To generate PDF, make sure you have `pdflatex` installed and then run `make`, the
PDF will be generated in `output/`.
