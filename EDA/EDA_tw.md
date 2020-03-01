


## Read Data

```{r}
path = "https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/dataset/telco.csv"
library(tidyverse)
raw = read_csv(path);rm(path)
skimr::skim(raw)
```


    ── Variable type: character ──────────────────────────────────────────────────────────────────────────────────────────────────
       skim_variable    n_missing complete_rate   min   max empty n_unique whitespace
     1 customerID               0             1    10    10     0     7043          0
     2 gender                   0             1     4     6     0        2          0
     3 Partner                  0             1     2     3     0        2          0
     4 Dependents               0             1     2     3     0        2          0
     5 PhoneService             0             1     2     3     0        2          0
     6 MultipleLines            0             1     2    16     0        3          0
     7 InternetService          0             1     2    11     0        3          0
     8 OnlineSecurity           0             1     2    19     0        3          0
     9 OnlineBackup             0             1     2    19     0        3          0
    10 DeviceProtection         0             1     2    19     0        3          0
    11 TechSupport              0             1     2    19     0        3          0
    12 StreamingTV              0             1     2    19     0        3          0
    13 StreamingMovies          0             1     2    19     0        3          0
    14 Contract                 0             1     8    14     0        3          0
    15 PaperlessBilling         0             1     2     3     0        2          0
    16 PaymentMethod            0             1    12    25     0        4          0
    17 Churn                    0             1     2     3     0        2          0
    
    ── Variable type: numeric ────────────────────────────────────────────────────────────────────────────────────────────────────
      skim_variable  n_missing complete_rate     mean       sd    p0   p25    p50    p75  p100 hist 
    1 SeniorCitizen          0         1        0.162    0.369   0     0      0      0      1  ▇▁▁▁▂
    2 tenure                 0         1       32.4     24.6     0     9     29     55     72  ▇▃▃▃▆
    3 MonthlyCharges         0         1       64.8     30.1    18.2  35.5   70.4   89.8  119. ▇▅▆▇▅
    4 TotalCharges          11         0.998 2283.    2267.     18.8 401.  1397.  3795.  8685. ▇▂▂▂▁

<hr>
<hr>

## Missing 11 rows. Can we impute?

```{r}
# Imputation
lm(TotalCharges~.,data = dplyr::select(raw,-customerID)) %>% summary
```

    Multiple R-squared:  0.9081,	Adjusted R-squared:  0.9078 
    F-statistic:  3148 on 22 and 7009 DF,  p-value: < 2.2e-16

Looks like it is safe to impute using Multiple Regression. The regression summary output also speaks to collaborative nature of data. There will be lots of insights to extract.

```{r}
test = raw[is.na(raw$TotalCharges),]
train = raw[!is.na(raw$TotalCharges),]
rm(raw);gc() # clear space.
```

```{r}
# Use Recursive Partitioning and Regression to impute
library(rpart)
impute_lm = lm(TotalCharges~.,data = dplyr::select(train,-customerID))
impute_rp = rpart(TotalCharges~.,data = dplyr::select(train,-customerID))
preds_train = data.frame(x1 = predict(impute_lm),x2 = predict(impute_rp),y = train$TotalCharges)
preds_test = data.frame(x1 = predict(impute_lm,test),x2 = predict(impute_rp,test))
# combine and find probability of results being correct.
ob_tests = summary(lm(y~.,preds_train))
relative_coeffs = sapply(1:2,function(i)ob_tests$coefficients[-1,1][i]*median(train$TotalCharges))
(prbs <- abs(relative_coeffs)/sum(abs(relative_coeffs)))
```

           x1        x2 
    0.4217967 0.5782033 

The decision tree is correct 13% more than regression. This new output should probabilistically improve prediction results. Stick the results into database. Furthermore, monthly charges never exceed predicted total charges. This will assure improved outputs.

```{r}
imp_val <- sapply(1:2, function(x) prbs[x]*preds_test[,x]) %>% rowSums()
imp_val <- ifelse(imp_val<test$MonthlyCharges ,test$MonthlyCharges,imp_val)
test$TotalCharges <- imp_val
sum(train$MonthlyCharges>train$MonthlyCharges) # This is proof
train <- rbind(train,test)
rm(imp_val,prbs,relative_coeffs,ob_tests,preds_test,
   preds_train,impute_lm,impute_rp,test);gc()
```

    [1] 0

<hr>
<hr>

## Attrition EDA

What values seem to correlate most to attrition? These correlations can be used to devise a strategy for retention.

```{r}
library(correlationfunnel) # one hot
train %>%
  dplyr::select(-customerID) %>%
  binarize(n_bins = 5, thresh_infreq = 0.01, name_infreq = "OTHER", one_hot = TRUE) %>%
  correlate(Churn__Yes) %>%
  plot_correlation_funnel()
```

![](https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/images/Correlation_Funnel.png)

Some of the attributes of individuals most correlated with leaving are...

- Short-term customers (6 months) 
- fiber optic internet users with no online security/tech support
- Month to Month contracts
- Uses electronic checks to pay

Several attributes for individuals that tend to stay are...

- Those with longer contracts.
- No internet service
- Inclusion of Tech support/Online security
- Credit card users

Some attributes that do not seem to have much correlation with retention are...

- Gender
- Phone Servicce

Although boxplots may provide some handsome information, correlation seems to show enough discrepancy on the data variance. This is enough information that business suggestions may be made to positively impact retention. The information variance shows that although there is some noticeable relationship, it is not extremely high. Some combinations may lead to different results. By performing tests such as logistic regression, random forest, AFT we can decipher the most important variables, hazards, combinations for churn and provide more pertinent and specific suggestions.








