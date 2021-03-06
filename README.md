
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bper: Bayesian Prediction for Ethnicity and Race

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/bwilden/bper/branch/main/graph/badge.svg)](https://codecov.io/gh/bwilden/bper?branch=main)
[![R-CMD-check](https://github.com/bwilden/bper/workflows/R-CMD-check/badge.svg)](https://github.com/bwilden/bper/actions)
<!-- badges: end -->

This package provides functions for predicting an individual’s
race/ethnicity given their first name, last name, geolocation, political
party, age, gender, and address characteristics. The method is based on
a Naive Bayes classification algorithm which incorporates known
ethnorace distributions over the observed characteristics to generate
posterior probabilities for each individual.

## Installation

You can install the development version from with:

``` r
# install.packages("devtools")
devtools::install_github("bwilden/bper")
```
