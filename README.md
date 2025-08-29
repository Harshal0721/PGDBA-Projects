# PGDBA-Projects

## House Price Prediction (Regression)
- Predicted Sale Price of houses on a dataset with 1400+ records having 43  categorical and 36  numerical features
- Performed Missing Value Imputation using a novel iterative process with Random Forest Regressors/Classifiers
- Applied an iterative Regression procedure handling Variable Significance, Multicollinearity and Influential Points
- Mitigated the curse of dimensionality using Forward Selection,  Ridge and  Lasso Regression with a best R2 of 0.89

## Early Gallstone Detection (Classification)	
- Predicted early Gallstone disease using a dataset with 300+ records having 7 categorical and 31 numerical features
- Performed feature selection using Mutual Information,  Chi-Square test,  ANOVA test and Likelihood Ratio test
- Trained multiple models like Logistic Regression, XGBoost, Random Forest, Decision Tree with GridSearch tuning
- Achieved Recall of 0.88; analyzed ROC-AUC curves for all models and got feature importance from RF and XGBoost 

## Length of Stay Prediction (NLP/LLM)	
- Predicted length of stay (short/long) using 1000 patient notes from MIMIC-III to support hospital resource planning
- Extracted & standardized clinical entities using Bio-Llama & UMLS; tuned prompts & temperature for better results
- Generated entity embeddings using BioClinicalBERT; fine-tuned XGBoost on embeddings to achieve 0.89 precision

## Pharma Index (Timeseries)	
- Forecasted daily Pharma index prices using 3 years of historical data through SARIMA-GARCH time series modelling
- Applied log transformed differencing; ADF test to check stationarity; validated residuals Ljung-Box and ARCH test
- Fitted GARCH(1,1) on SARIMA residuals to capture volatility and correct heteroscedasticity; achieved MAPE 1.53%

## Customer Segmentation (Clustering)
- Segmented customers using novel  RFMTC  analysis on 4L+ transactions;  engineered 11 features from 8 inputs
- Applied PCA for dimensionality reduction; clustered using K-Means-elbow plot, Agglomerative, DBSCAN methods
- Analysed clusters characteristics using radar plots on engineered features; achieved highest silhouette score 0.42
