# BADS_Project

BADS WS17/18 project

## Important Dates

1. 01.12.2017 - Individual assignment due
2. 05.02.2017 - Group predictions due
3. 12.02.2017 - Final term paper due

## Notes

### Prepare Data

1. Data Imputation: [Imputation using MICE](https://datascienceplus.com/imputing-missing-data-with-r-mice-package/)
2. Feature Selection: Consider creating a variable based on return rate (Weight of Evidence), identify orders with the same items
3. Consider mixed interaction terms or power terms (for non-linear relationships) - *Is this necessary for non-regression-based models?*

### Build Model

1. **Models to try**: 
    - ANN or [Mxnet](http://mxnet.incubator.apache.org/tutorials/r/fiveMinutesNeuralNetwork.html) (can create custom loss functions)
    - [Gradient Boosted Trees](http://xgboost.readthedocs.io/en/latest/model.html#why-introduce-the-general-principle) (xgboost) w/ Platt Scaling
2. **Parallelization**: metaparameter tuning, cross-validation
3. [Platt scaling](https://www.analyticsvidhya.com/blog/2016/07/platt-scaling-isotonic-regression-minimize-logloss-error/) to convert outputs into a probabilistic distribution.

### Predict

1. Cross-validation techniques
2. Figure out why the span of the predictive space is too narrow (only from 0.2 - 0.7)

### Evaluate

1. Cost-sensitive learning or a [custom cost-function](http://willwolf.io/2015/11/18/so-you-want-to-implement-a-custom-loss-function/)

## Variables (what to change)

1. `order_item_id` is a unique identifier of order item. doesn't matter **REMOVE**
2. `order_date` and `delivery_date` should be coerced into a date diff variable
3. `item_id` is a factor. lots of small values - maybe discretize into frequency of purchase.
4. `item_size` factor. probably not relevant on it's own **REMOVE**
5. `item_color` probably not relevant on it's own **REMOVE**
6. `brand_id` is a factor. some different in test / training, test set should be scaled down. see `item_id` note
7. `item_price` numeric, but in test set there are higher values. handle 0 values
8. `user_id` is a factor. see `item_id` note
9. `user_title` is a factor but high class imbalance, follows same distribution and appears to be decent signal
10. `user_dob` is date of birth. consider discretizing and removing bad values (impute?)
11. `user_state` is a factor. follows same distribution and appears to be decent signal
12. `user_reg_date` is a date. consider changing to "length of account" and "time between open and order"

## Features
1. `user_age`: (numeric) age of person who purchased. consider discretization
2. `user_state`: (factor) location
3. `user_title`: (factor) proxy for gender. high class imbalance. consider split models
4. `days_to_deliv`: (numeric) difference between `delivery_date` and `order_date`. little relationship. is possible not a relevant metric based on assignment description
5. `days_from_open`: (numeric) difference between `order_date` and `user_reg_date`. small linear relationship.
6. `order_day`: (factor) which weekday?
7. `order_month`: (factor) which month?
9. `item_brand_id`: (factor) which brand? consider using WOE?
10. `item_price`: (numeric) item price
