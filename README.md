# Predictive-modeling
This case is from my first Kaggle competition- House Pricing. Predicting sales prices for the houses using different modeling algorithms on R. This is a large dataset with 79 explanatory variables including categorical and numerical. This dataset provides an opportunity to do data preparation, managing missing variable, exploratory data analysis, and feature engineering before modeling. Considering this was my first Kaggle competition, I wanted to make maximum use of my skills required in the dataset to predict the sale prices.
Data source: https://www.kaggle.com/c/house-prices-advancedregression-techniques/data 
Expanatory Data Analysis (EDA): The plots can be seen in the tableau file. In these charts, some significant insights were observed regarding the relationship of Sales Price on some of the factors such as overall quality, neighbourhood, proximity, etc. Based on these insights, the real estate firms can have direction on how and where to make their investments to maximize the revenue and the reduce bad investment costs. However, in a real scenario, there are many more factors that could contribute to the Sales Price of the houses. Adding multiple factors can reduce the legibility of charts and can make it difficult to draw accurate business-related decisions. To solve for this problem, one can use more advanced predictive methodology that can analyse the available data points and provide us a tool based on a model that could predict the house prices based on statistically significant factors.
Required packages on R: tidyverse, dplyr, caret, mice, glmnet
Model Training: Following models were trained and tested using root mean square error (RMSE). Linear Regression: Simple linear regression was applied between Sale Prices and all the other predictors. This model gave a very high adjusted R squared of 0.92. Sale Prices in the testing dataset were predicted after running the model. This model gave a high root mean square error on Kaggle. Log + Interactions: In this case, log was applied to the Sale Prices and the relationship was analysed with the log of predictor variables and its interaction with categorical predictor variables. This model also gave a high root mean square error on Kaggle after predicting the Sale Pricesin the testing dataset. Regularization model (LASSO): This model was applied on the training dataset to estimate the linear relationship between Sale Prices and the predictor variables. This technique is applied to simplify the model and avoid any kind of over-fitting in the data. Random Forest model: This model was applied after utilising the training dataset to build a decision tree. This results in the prediction of the Sale Prices in the testing dataset and gave a reasonably lower root mean square error on Kaggle. 
Model selection: The final selected model to predict the Sale Prices in the testing dataset is "Gradient Boosting method". This model enhanced the accuracy of the predicted Sale Prices. The submission on Kaggle received a score of top 26% (2459 out of 9701). 
