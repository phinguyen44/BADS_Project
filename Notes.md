# Final Write Up Notes

## Data Preparation

1. Which variables did we think were relevant?
    - Demographic information (gender, age, location, number of prior purchases made)
    - Order information (basket size, number of similar items, total cost, order day / month)
    - Item-level information (item size / misfit, color, brand, cost)
    - Other, that we can't use (peer reviews, shopping cart experience, sales, competition)
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
4. As an additional note for potential model approaches: constructing "user profiles" - this mimics what is done in a real-world scenario

## Prediction

1. Cross-validation techniques

## Evaluation

1. Log loss error or Brier score or custom penalty function
2. Add thoughts on the custom penalty function. Is it the right way to think about the problem? What about the one you've conceived? Don't forget to add the maths on it.
