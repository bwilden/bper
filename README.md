# bper: Bayesian Prediction for Ethnicity and Race

This package provides functions for predicting individuals' race/ethnicity given their first name, last name, geo-location, political party, age, gender, and address characteristics. The method is based on a Naive Bayes classification algorithm which incorporates known ethnorace distributions over the observed characteristics to generate posterior probabilities for each individual. Ethnorace groups follow US Census conventions: American Indian and Alaska Native, Asian/Pacific Islander, African American, Hispanic, White, and Other Race.

### Installation

```{r}
install.packages("devtools")
devtools::install_github("bwilden/bper")
```
