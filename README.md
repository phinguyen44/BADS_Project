# BADS_Project

BADS WS17/18 project

## Important Dates

1. 01.12.2017 - Individual assignment due
2. 05.02.2017 - Group predictions due
3. 12.02.2017 - Final term paper due

## Notes

1. R packages to use for production: docopt, infuser/glue, magrittr
2. R packages for imputation: MICE, Amelia
3. R packages to try for algorithm: glmnet, splines
4. [Handling Class Imbalance Problems](https://www.analyticsvidhya.com/blog/2016/03/practical-guide-deal-imbalanced-classification-problems/)
5. Consider mixed interaction terms or power terms
6. [Predicting Binary Outcomes in R](https://amunategui.github.io/binary-outcome-modeling/)
7. KNN / SVC / Extreme Random Forests
8. [Standardization of Categorical Variables](https://stats.stackexchange.com/questions/68077/are-categorical-variables-standardized-differently-in-penalized-regression)
9. Make a few candidate models
10. Consider parallelization: multi-fit models, cross-validation, imputation methods
11. Separate models for user_title
12. How to handle factors with so many levels?

## Variables (what to change)

1. `order_item_id` is a unique identifier of order item. doesn't matter **REMOVE**
2. `order_date` and `delivery_date` should be coerced into a date diff variable
3. `item_id` is a factor. lots of small values - maybe discretize into frequency of purchase.
4. `item_size` factor. probably not relevant on it's own **REMOVE**
5. `item_color` probably not relevant on it's own **REMOVE**
6. `brand_id` is a factor. some different in test / training, test set should be scaled down. see `item_id` note
7. `item_price` numeric, but in test set there are higher values. handle 0 values. appears to have logarithmic relationship
8. `user_id` is a factor. see `item_id` note
9. `user_title` is a factor but high class imbalance, follows same distribution and appears to be decent signal
10. `user_dob` is date of birth. consider discretizing and removing bad values (impute?)
11. `user_state` is a factor. follows same distribution and appears to be decent signal
12. `user_reg_date` is a date. consider changing to "length of account" and "time between open and order

## Features
1. `user_age`: (numeric) age of person who purchased. use spline transform (under 40, 40-65, 65+)
2. `user_state`: (factor) location
3. `user_title`: (factor) proxy for gender. high class imbalance. consider split models
4. `days_to_deliv`: (numeric) difference between `delivery_date` and `order_date`. little relationship. heteroscedastic
5. `days_from_open`: (numeric) difference between `order_date` and `user_reg_date`. small linear relationship.
6. `order_day`: (factor) which weekday?
7. `order_month`: (factor) which month?
8. `item_id`: (factor) which item?
9. `item_brand_id`: (factor) which brand?
10. `item_price`: (numeric) item price. consider log relationship.
