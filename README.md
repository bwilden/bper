
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bper: Bayesian Prediction for Ethnicity and Race

<!-- badges: start -->
<!-- badges: end -->

This package provides functions for imputing an individual’s
race/ethnicity given their first name, last name, geolocation, political
party, age, gender, and address characteristics. The method is based on
a Naive Bayes classification algorithm which incorporates known
ethnorace distributions over the observed characteristics to generate
posterior probabilities for each individual.

## Installation

You can install the development version with:

``` r
# install.packages("devtools")
devtools::install_github("bwilden/bper")
```

## Usage

The data necessary for the imputation algorithm come from the US Census
API. You will need a Census API key in order to use the package
(<https://api.census.gov/data/key_signup.html>).

``` r
library(bper)

example_persons
#> # A tibble: 10 × 12
#>    first_name last_name   age   sex party multi_unit state county tract  block
#>    <chr>      <chr>     <dbl> <dbl> <chr>      <dbl> <chr> <chr>  <chr>  <chr>
#>  1 BERT       WILDEN       28     0 DEM            1 LA    019    002201 3036 
#>  2 SAM        WITHER       27     0 DEM            1 LA    119    031400 1015 
#>  3 BELINDA    LOBOS        33     1 DEM            1 LA    071    000616 3024 
#>  4 ANNAA      ARENA        90     1 REP            1 NM    013    000700 3008 
#>  5 KARL       SOM          13     0 UNA            1 NM    001    000108 1000 
#>  6 WILLIAMS   TURA         50     0 DEM            1 LA    029    000400 5015 
#>  7 LIAM       SZYMONIAK    20     1 REP            0 NM    045    000401 1000 
#>  8 KAI        WALKO        78     1 UNA            1 LA    051    021804 1021 
#>  9 PAMELLA    CHANELLL     65     1 DEM            0 LA    047    953101 2040 
#> 10 WALT       BROOKE       18     0 REP            0 NM    001    940600 1185 
#> # … with 2 more variables: place <chr>, zip <chr>
```

### Step 1: Prepare input data

The `bper` package works by matching Census data to column names in your
input data frame. To ensure all possible information is used in the
imputation algorithm, reformat your variables in the following way:

-   `first_name`. A character variable. Remove all punctuation and white
    spaces.
-   `last_name`. A character variable. Remove all punctuation and white
    spaces.
-   `age`. A numeric variable.
-   `sex`. A numeric variable with `1` corresponding to female and `0`
    corresponding to male.
-   `party`. A character variable. Available categories are `DEM` for
    Democratic, `REP` for Republican, and `UNA` for independent/other.
-   `multi_unit`. A numeric variable with `1` corresponding to residence
    in multi-unit housing (typically an address containing “Apt”,
    “Unit”, “#”) and `0` corresponding to residence in single-unit
    housing.
-   `state`. A character variable with the 2 letter state abbreviation.
-   `county`. A character variable with the 3 digit FIPS county code.
-   `tract`. A character variable with the 6 digit Census tract code.
-   `block`. A character variable with the 4 digit Census block code.
-   `place`. A character variable with the 6 digit Census place code.
-   `zip`. A character variable with the 5 digit ZIP code.

Not all of the above variables are required (only the most detailed
geographic variable is used), but more information leads to better
predictions.

### Step 2: Pre-load Census data (optional)

The function `load_bper_data` allows you to load the Census data
necessary for the imputation algorithm ahead of time. This can be a big
time-saver particularly when using detailed geographic inputs (tracts or
blocks).

``` r
my_bper_data <- load_bper_data(
  input_data = example_persons,
  year = 2010,
  census_key = "your_census_key"
)

save(my_bper_data, file = "my_bper_data.rda")
```

### Step 3: Impute race/ethnicity

Use `impute_ethnorace` to implement the race/ethnicity imputation
algorithm.

``` r
# Method 1, loading Census data and imputing ethnorace
imputed_data <- impute_ethnorace(
  input_data = example_persons,
  year = 2010,
  census_key = "your_census_key"
)

# Method 2, using pre-loaded Census data
imputed_data <- impute_ethnorace(
  input_data = example_persons,
  bper_data = my_bper_data,
  year = 2010,
  census_key = "your_census_key"
)
```

### Final result:

    #> [1] "bper_data loaded:"
    #> Time difference of 27.90702 secs
    #> [1] "Posterior probs for last calculated:"
    #> Time difference of 28.13111 secs
    #> [1] "Posterior probs for first calculated:"
    #> Time difference of 28.15991 secs
    #> [1] "Posterior probs for sex-age calculated:"
    #> Time difference of 28.18821 secs
    #> [1] "Posterior probs for geo calculated:"
    #> Time difference of 28.21754 secs
    #> [1] "Mean posterior probs calculated:"
    #> Time difference of 28.23636 secs
    #> [1] "Finished in:"
    #> Time difference of 28.23857 secs
    #>    first_name last_name age sex state county pred_race    pred_aian
    #> 1        BERT    WILDEN  28   0    LA    019     white 3.646338e-02
    #> 2         SAM    WITHER  27   0    LA    119     white 3.012393e-03
    #> 3     BELINDA     LOBOS  33   1    LA    071  hispanic 8.773998e-05
    #> 4       ANNAA     ARENA  90   1    NM    013     white 6.019420e-04
    #> 5        KARL       SOM  13   0    NM    001      aapi 1.599352e-03
    #> 6    WILLIAMS      TURA  50   0    LA    029     black 1.070799e-02
    #> 7        LIAM SZYMONIAK  20   1    NM    045     white 1.935840e-01
    #> 8         KAI     WALKO  78   1    LA    051     white 7.827831e-04
    #> 9     PAMELLA  CHANELLL  65   1    LA    047     black 9.184231e-03
    #> 10       WALT    BROOKE  18   0    NM    001     white 4.062276e-02
    #>       pred_aapi   pred_black pred_hispanic  pred_other pred_white
    #> 1  0.0018217451 0.0109320647   0.000239096 0.028848044 0.92169567
    #> 2  0.0062408894 0.2190506591   0.028581543 0.001694742 0.74141977
    #> 3  0.0216805661 0.0073021278   0.923029212 0.011058083 0.03684227
    #> 4  0.0272175236 0.0065705053   0.092833337 0.014747527 0.85802916
    #> 5  0.8241366429 0.0005829070   0.014509009 0.055554774 0.10361731
    #> 6  0.0244483033 0.5128534394   0.008161518 0.008564163 0.43526459
    #> 7  0.0003462365 0.0002913733   0.006343657 0.014598751 0.78483601
    #> 8  0.2122197738 0.0006754651   0.001150117 0.026110991 0.75906087
    #> 9  0.0032803760 0.4650688611   0.016907922 0.077724253 0.42783436
    #> 10 0.0027136339 0.0219253926   0.013253895 0.166511018 0.75497330
