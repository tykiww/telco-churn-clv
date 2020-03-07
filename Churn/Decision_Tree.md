
## Overview

A decision tree, also generalized in statistics as recursive partitioning, is a common analytical tool that lays out possibilities that result in a particular decision. This method will analyze certain major decisions/attributes of customers that result in a customer leaving or staying.

## Analysis

#### Load Modules

```r
# Retrieve Data and Modules
dataset = "https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/dataset/telco.csv"
utilities = "https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/utilities/common.R"
library(tidyverse)
source(utilities)
library(rpart.plot)
library(rpart)
```

#### Read and Impute Data

```r
# Read and Impute Data
raw <- read_csv(dataset)
train <- telco_imputer(raw) %>% as.data.frame
train <- train_test_split(train,.8,15)
test <- train[[2]];train <- train[[1]]
skimr::skim(dataset)
```

    ── Variable type: character ────────────────────────────────────────────────────────────────────────────────────────────────────────────
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
    
    ── Variable type: numeric ──────────────────────────────────────────────────────────────────────────────────────────────────────────────
      skim_variable  n_missing complete_rate     mean       sd    p0   p25    p50    p75  p100 hist 
    1 SeniorCitizen          0             1    0.162    0.369   0     0      0      0      1  ▇▁▁▁▂
    2 tenure                 0             1   32.4     24.6     0     9     29     55     72  ▇▃▃▃▆
    3 MonthlyCharges         0             1   64.8     30.1    18.2  35.5   70.4   89.8  119. ▇▅▆▇▅
    4 TotalCharges           0             1 2280.    2266.     18.8 400.  1395.  3787.  8685. ▇▂▂▂▁

#### Create Decision Tree

```r
rp <- rpart(Churn~.,data = dplyr::select(train,-customerID), method = 'class', model = TRUE)
prune(rp,cp = rp$cptable[which.min(rp$cptable[,"xerror"]),"CP"]) # to avoid overfitting
rpart.plot(rp, extra = 104, fallen.leaves = FALSE)
```

![](https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/images/Decision_Tree.png)

The wonderful aspect of the decision tree is that *the model decides the most 'statistically significant split'* based on the method and keeps the most 'important' values. Fortunately, the model has considered 3 major splits: contract, internet service, and tenure.

In general, the data is split into 74% of those that stay and 26% that leave. The white yes/no boxes indicate the direction of each split. The No/Yes labels at the top of each node declares the general consensus. If majority is a No, it will be labelled no. The probability splits in the middle of the node declares the % split of No to yes. The percentage shown at the bottom show us the total % of the dataset it is using.

1) In the first split, the tree considers the contracts. If the contract is either a one/two year, then you are 93% likely not to have a particular customer leave.

2) In the second split, internet service is observed. Those that have DSL or do not use internet at all tend to stay 72% of the time. 

3) In the final split, longer tenured customers (>=16) are anchored 59% of the time. Alternatively, those that fall beneath the tenure mark are 70% likely to leave.

#### Predictions

```r
rp_pred <- predict(rp,dplyr::select(test,-customerID, -Churn),type = 'class')
mean(rp_pred==test$Churn)
```

    [1] 0.7835344

This is our prediction accuracy. If we were to introduce unseen customers, the decision tree will correctly classify an individual around 78% of the time. 

#### Revenue Analysis

```r
# Compare average(amount)/average(tenure) between groups of interest.
# Demographic is those with lower tenure, lower term contracts, and fiberoptic internet.

rbind(
  rbind(train,test) %>% 
                as_tibble %>%
                subset(!Contract %in% c("One year","Two year")) %>%
                subset(InternetService %in% c("Fiber optic")) %>%
                subset(tenure < 16) %>%
                mutate_if(is.numeric,mean) %>%
                mutate(TotalChargebyTenure = TotalCharges/tenure) %>%
                mutate(MonthChargebyTenure = MonthlyCharges/tenure) %>%
                dplyr::select(.,tenure,MonthlyCharges,TotalCharges,
                              MonthChargebyTenure,TotalChargebyTenure) %>%
                head(1),
  rbind(train,test) %>% 
                as_tibble %>%
                subset(Contract %in% c("One year","Two year")) %>%
                subset(!InternetService %in% c("Fiber optic")) %>%
                subset(tenure >= 16) %>%
                mutate_if(is.numeric,mean) %>%
                mutate(TotalChargebyTenure = TotalCharges/tenure) %>%
                mutate(MonthChargebyTenure = MonthlyCharges/tenure) %>%
                dplyr::select(.,tenure,MonthlyCharges,TotalCharges,
                              MonthChargebyTenure,TotalChargebyTenure) %>%
                head(1)
  ) %>% as.matrix %>% 
      `rownames<-`(c("Leavers","Stayers"))
```

              tenure    MeanMonthly    MeanTotal MonthChargebyTenure TotalChargebyTenure
    Leavers  5.84556       82.61511     495.7652          14.1329673            84.81055
    Stayers 51.11930       47.46743    2531.6631           0.9285618            49.52460

The 'suspect' leavers are more lucrative over their short spans than the stayers. This makes sense because it is common for telco companies to provide discounts for tenure because customers tend to haggle their premiums. *However, this begs the question of whether it is better to keep customers for a longer period of time or to strategize an incentive program to increase market-share and pump out a higher volume of turnover.* Of course, it is always better to have recurring revenue. Nevertheless, there may be an opportunity to linearly maximize revenue by adjusting for the optimal turnover rate.

## Afterthoughts

With an overall retention of 74% most companies would be concerned on how they may keep their customers. The most outright consensus would seem to be: "sign longer contracts!" However, through this analysis we learn that this may not be exactly so. Although a longer contract is the best attribute to retain customers, depending on the company's strategy, we need to focus on the right number of customers to retain.

