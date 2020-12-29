
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bper: Bayesian Prediction for Ethnicity and Race

<!-- badges: start -->

<!-- badges: end -->

This package provides functions for predicting an individual’s
race/ethnicity given their first name, last name, geo-location,
political party, age, gender, and address characteristics. The method is
based on a Naive Bayes classification algorithm which incorporates known
ethnorace distributions over the observed characteristics to generate
posterior probabilities for each individual.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bwilden/bper")
```

## Usage

#### Step 1

Start by loading the `bper` package and preparing your input dataframe
containing individuals whose ethnorace you wish to predict. The
following columns are supported:

  - `first_name`. All names should be uppercase.
  - `last_name`. Remove all punctuation (e.g. O’NEAL becomes ONEAL) and
    all spaces to increase chances of finding a match. All names should
    be uppercase.
  - `birth_year`. Currently only individuals born between the years 1911
    and 2010 will match.
  - `female`. A binary 1/0 indicator for female or not-female.
  - `party`. Options are “DEM” for Democrat, “REP” for Republican, and
    “UNA” for Independents or other political parties.
  - `apartment`. A binary 1/0 indicator for whether the individual lives
    in multi-unit housing or not.
  - `zip`. 5 digit ZIP Code.
  - `block`. 15 digit complete US Census block code. This is comprised
    of a 2-digit State code + 3-digit County code + 6-digit Census Tract
    code + 4-digit Census Block code.

If any of the above columns are not present in your input dataframe, a
column of NA’s will be appended so that the `predict_race` function
works. See example below. The more input information the into the
algorithm, the better the predictions\!

``` r
library(bper)

example_persons
#>   first_name last_name birth_year female party apartment   zip           block
#> 1       BERT    WILDEN       1992      0   DEM         1 92092 060730083052007
#> 2     LYNDON    WITHER       1963      1   DEM         0 53146 220419502002064
#> 3    BELINDA     LOBOS       1989      1   DEM         1 57551 420199112002033
#> 4       ANNA     ARENA       1920      1   REP         1 92844            <NA>
#> 5       KARL       SOM       1978      0   UNA         1 03862 480739503004014
#> 6    MATHIEU      TURA       1913      0  <NA>         1 65557 482239503001080
#> 7       LIAM SZYMONIAK       1932      1   REP         0 59730 450630210201000
```

#### Step 2

The `predict_race` function works by merging nationwide ethnorace
distribution data into the original data frame to perform the
computations. This data is stored externally and must be downloaded and
then loaded into your global environment before running `predict_race`.
Do this with:

``` r
load_bperdata(download = TRUE, save_files = FALSE)
#> [1] "Downloading APARTMENTS data set..."
#> [1] "Downloading BIRTH_YEARS data set..."
#> [1] "Downloading BLOCKS data set..."
#> [1] "Downloading FIRSTNAMES data set..."
#> [1] "Downloading GENDERS data set..."
#> [1] "Downloading PARTIES data set..."
#> [1] "Downloading SURNAMES data set..."
#> [1] "Downloading ZIPS data set..."
```

For more details see `?load_bperdata` and
<https://github.com/bwilden/bperdata.> Note: you do not need to install
the `bperdata` package.

#### Step 3

Now you are ready to run `predict_race`\!

``` r
predict_race(example_persons)
#>   first_name last_name birth_year female party apartment   zip           block
#> 1       BERT    WILDEN       1992      0   DEM         1 92092 060730083052007
#> 2     LYNDON    WITHER       1963      1   DEM         0 53146 220419502002064
#> 3    BELINDA     LOBOS       1989      1   DEM         1 57551 420199112002033
#> 4       KARL       SOM       1978      0   UNA         1 03862 480739503004014
#> 5    MATHIEU      TURA       1913      0  <NA>         1 65557 482239503001080
#> 6       LIAM SZYMONIAK       1932      1   REP         0 59730 450630210201000
#> 7       ANNA     ARENA       1920      1   REP         1 92844            <NA>
#>     prob_black prob_white prob_hispanic     prob_api    prob_aian  prob_other
#> 1 1.090312e-02  0.8022624  0.0011623158 4.684676e-02 2.690605e-02 0.111919402
#> 2 5.601808e-01  0.2198627  0.0200225590 2.484031e-02 1.410100e-01 0.034083674
#> 3 2.319391e-03  0.4143154  0.5172463096 2.639766e-02 1.149642e-03 0.038571642
#> 4 1.881264e-04  0.1783591  0.0025416061 7.907874e-01 1.152816e-03 0.026970856
#> 5 1.177303e-01  0.5579595  0.0033732256 2.178709e-02 1.423139e-01 0.156835973
#> 6 2.992587e-06  0.9975689  0.0001117273 3.312029e-05 9.586343e-04 0.001324647
#> 7 1.204901e-04  0.7300824  0.0606607735 2.068635e-01 4.930641e-05 0.002223559
#>   pred_race
#> 1     white
#> 2     black
#> 3  hispanic
#> 4       api
#> 5     white
#> 6     white
#> 7     white
```
