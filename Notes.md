# Final Write Up Notes

## Data Preparation

1. Which variables did we think were relevant?
2. How did we transform variables? Discretization? WOE? Power-terms, mixed interaction terms for non-linear relationships?
3. How did we handle data imputation? Any cases where imputation yielded different results?
4. Orders with same item (e.g. "It looks like you have two of the same item in the cart. Do you want this?")
5. Which variables could we actually use, given the problem setting?

## Model Building

1. Start with parsimonious logistic regression model
2. Add regularization
3. Try advanced models such as: 
    - Random Forests w/ Platt Scaling (can natively handle categorical variables)
    - ANN
    - [Gradient Boosted Trees](http://xgboost.readthedocs.io/en/latest/model.html#why-introduce-the-general-principle) (xgboost)
2. **Parallelization**: Models, Metaparameter tuning, cross-validation
3. Platt scaling to convert outputs into a probabilistic distribution

## Prediction

1. Cross-validation techniques

## Evaluation

1. Log loss error or Brier score or custom penalty function
2. Add thoughts on
