# bper: Bayesian Prediction for Ethnicity and Race

This package provides functions for predicting individuals' race/ethnicity given their first name, last name, geo-location, political party, age, gender, and address characteristics. The method is based on a Naive Bayes classification algorithm which incorporates known ethnorace distributions over the previously-mentioned characteristics to produce posterior probabilities for each category. Ethnorace groups follow US Census conventions: American Indian and Alaska Native, Asian/Pacific Islander, African American, Hispanic, White, and Other Race.

```{r}
install.packages("devtools")
devtools::install_github("bwilden/bper")

```
