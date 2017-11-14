# BADS_Project

Files and data - BADS WS17/18 project

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

## Important Dates

1. 01.12.2017 - Individual assignment due

## Variables

1. `order_item_id` is a unique identifier of order item. doesn't matter
2. `order_date` and `delivery_date` should be coerced into a date diff variable
3. `item_id` is a factor. note that test and training set have different `item_id`'s. some different values
4. `item_size` factor
5. `item_color` probably not relevant on it's own
6. `brand_id` is a factor. some different in test / training, test set should be scaled down
7. `item_price` numeric, but in test set there are higher values
8. `user_id` is a factor
9. `user_title` is a factor but high class imbalance
10. `user_dob` is date of birth. consider discretizing and removing bad values (impute?)
11. `user_state` is a factor
12. `user_reg_date` is a date. consider changing to "length of account" and "time between open and order"
