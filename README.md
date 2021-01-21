
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bper: Bayesian Prediction for Ethnicity and Race

<!-- badges: start -->

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

## Usage

### Step 1

Start by loading the `bper` package and preparing your input data set
containing individuals whose ethnorace you wish to predict. The
following columns are supported:

  - `first_name`. All names should be uppercase.
  - `last_name`. Remove all punctuation and all spaces to increase
    chances of finding a match (e.g. “O’NEAL” or “O NEAL” should be
    “ONEAL”) . All names should be uppercase.
  - `birth_year`. Currently only individuals born between the years 1911
    and 2010 will match.
  - `female`. A 1/0 indicator for female or not-female.
  - `party`. Options are “DEM” for Democrat, “REP” for Republican, and
    “UNA” for Independents or other political parties.
  - `apartment`. A 1/0 indicator for whether the individual lives in
    multi-unit housing or not.
  - `state`. 2 character State abbreviation code.
  - `county`. 5 digit County, or FIPS, code. This is comprised of a
    2-digit State code + 3-digit County code.
  - `place`. 7 digit Census Place code. This is comprised of a 2-digit
    State code + 5-digit Place code.
  - `zip`. 5 digit ZIP Code.
  - `block`. 15 digit complete US Census block code. This is comprised
    of a 2-digit State code + 3-digit County code + 6-digit Census Tract
    code + 4-digit Census Block code. Make sure this is a character
    string to preserve leading 0’s in State codes.

If any of the above columns are not present in your input data set, a
column of NA’s will be appended so that the `predict_ethnorace` function
runs. Predictive performance may suffer, however, depending on which
columns are missing. The most important columns are `first_name`,
`last_name`, and one geolocation variable (`state`, `county`, `place`,
`zip`, or `block`). Predictive accuracy improves following a decrease in
average population size for your geolocation unit. So Census blocks are
better than ZIP Codes, which are better than County codes, which are
better than State codes.

See example data frame below.

``` r
library(bper)

example_persons
#>   first_name last_name birth_year female party apartment state county   place
#> 1       BERT    WILDEN       1992      0   DEM         1    CA  06073    <NA>
#> 2     LYNDON    WITHER       1963      0   DEM         0    VA  51173 5187430
#> 3    BELINDA     LOBOS       1989      1   DEM         1    FL  12009 1206625
#> 4       ANNA     ARENA       1920      1   REP         1    HI   <NA>    <NA>
#> 5       KARL       SOM       1978      0   UNA         1    TX  48073 4824864
#> 6    MATHIEU      TURA       1913      0  <NA>         1    NC  37097    <NA>
#> 7       LIAM SZYMONIAK       1932      1   REP         0    MI  26103 2605920
#> 8        KAI     WALKO       1990      1   UNA        NA    CA  06025 0607372
#> 9    PAMELLA    CHANEL         NA      1   DEM         0  <NA>   <NA>    <NA>
#>     zip           block
#> 1 92092 060730083052007
#> 2 53146 511730301002001
#> 3 57551 120090647001042
#> 4 92844            <NA>
#> 5 03862 480739503004014
#> 6 65557 370970614081008
#> 7 59730 261030022003121
#> 8  <NA>            <NA>
#> 9  <NA>            <NA>
```

### Step 2

The `predict_ethnorace` function works by merging nationwide ethnorace
distribution data into the input data set to perform calculations. This
data is stored externally and must be downloaded and then loaded into
your global environment before running `predict_ethnorace`. Do this
with:

``` r
load_bperdata(download = TRUE, save_files = FALSE, include_blocks = TRUE)
#> [1] "Downloading APARTMENTS data set..."
#> [1] "Downloading BIRTH_YEARS data set..."
#> [1] "Downloading COUNTIES data set..."
#> [1] "Downloading FIRSTNAMES data set..."
#> [1] "Downloading GENDERS data set..."
#> [1] "Downloading NATIONWIDE data set..."
#> [1] "Downloading PARTIES data set..."
#> [1] "Downloading PLACES data set..."
#> [1] "Downloading STATES data set..."
#> [1] "Downloading SURNAMES data set..."
#> [1] "Downloading ZIPS data set..."
#> [1] "Downloading BLOCKS data set..."
```

Warning: the total size of these file is about 60 MB. If you are not
using Census Blocks for your geolocation variable, set `include_blocks =
FALSE` to substantially reduce the download time. For more details see
`?load_bperdata` and <https://github.com/bwilden/bperdata.> Note: you do
**not** need to install the `bperdata` package.

### Step 3

Now you are ready to run `predict_ethnorace`.

``` r
predict_ethnorace(example_persons)
#>   first_name last_name birth_year female party apartment state county   place
#> 1       BERT    WILDEN       1992      0   DEM         1    CA  06073    <NA>
#> 2     LYNDON    WITHER       1963      0   DEM         0    VA  51173 5187430
#> 3    BELINDA     LOBOS       1989      1   DEM         1    FL  12009 1206625
#> 4       KARL       SOM       1978      0   UNA         1    TX  48073 4824864
#> 5    MATHIEU      TURA       1913      0  <NA>         1    NC  37097    <NA>
#> 6       LIAM SZYMONIAK       1932      1   REP         0    MI  26103 2605920
#> 7       ANNA     ARENA       1920      1   REP         1    HI   <NA>    <NA>
#> 8        KAI     WALKO       1990      1   UNA        NA    CA  06025 0607372
#> 9    PAMELLA    CHANEL         NA      1   DEM         0  <NA>   <NA>    <NA>
#>     zip           block   prob_black prob_white prob_hispanic     prob_api
#> 1 92092 060730083052007 1.092932e-02 0.80415305  0.0011649979 0.0469310417
#> 2 53146 511730301002001 3.837506e-01 0.45542449  0.0154608693 0.0160470444
#> 3 57551 120090647001042 3.909189e-03 0.06532124  0.8745245882 0.0237805071
#> 4 03862 480739503004014 1.883515e-04 0.17856680  0.0025445430 0.7915533837
#> 5 65557 370970614081008 9.839557e-02 0.68121507  0.0027913710 0.0186780229
#> 6 59730 261030022003121 2.106265e-05 0.99095267  0.0002621028 0.0002331892
#> 7 92844            <NA> 1.204909e-04 0.73008927  0.0606608908 0.2068636286
#> 8  <NA>            <NA> 9.613178e-04 0.82790551  0.0030590092 0.0474529196
#> 9  <NA>            <NA> 5.518835e-01 0.12964501  0.0241070378 0.0011440123
#>      prob_aian  prob_other pred_race
#> 1 2.693714e-02 0.109884456     white
#> 2 1.051242e-01 0.024192852     white
#> 3 1.903647e-03 0.030560825  hispanic
#> 4 1.153939e-03 0.025992980       api
#> 5 1.499252e-01 0.048994742     white
#> 6 6.730664e-03 0.001800313     white
#> 7 4.930721e-05 0.002216409     white
#> 8 3.720050e-02 0.083420750     white
#> 9 3.318994e-02 0.260030529     black
```
