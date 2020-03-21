---
Title: Random Forest Churn Analysis
Author: Spencer Allgair
---

## Overview

Similar to techniques used in a decision tree analysis, Random Forest creates many different sets of randomly selected decision trees, hence the name forest, to 'learn'how important each variable should be using a training portion of the data. The model is computationally intensive and requires a large number of observations for acceptable results.

This analysis will make a practical and effective use of the random forest model to decipher the most important variables affecting customer churn.

## Analysis

#### Load libraries

```r
library(tibble)
library(dplyr)
library(randomForest)
```

#### Read and glimpse at data
```r
# Paths for analysis
path <- "https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/dataset/telco.csv"
utilities <-"https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/utilities/common.R"
```

```r
# Load data and module
churn.full <- read.csv(path)
source(utilities)

# Too many variables to do a complete pairs plot, so let's do tables
cbind("Total" = table(churn.full$Churn), 
  "Prop" = prop.table(table(churn.full$Churn)))
```

        Total      Prop
    No   5174 0.7346301
    Yes  1869 0.2653699


```r
# Turn the Yes to TRUE and the No to FALSE
churn.full$Churn <- churn.full$Churn=="Yes"
# Impute NA using imputer tool
churn.full <- telco_imputer(churn.full)
# Take out the customerID column for analysis
churn.full <- churn.full[,-1]
```


#### Train-Test Validation
Now that the data is read and we know what we're dealing with, let's make the training and test datasets.

```r
# Create train (about 25% of data) and test (the rest)
churn.train <- train_test_split(churn.full,.75,42) # training is 75%, testing is 25%
churn.test <- churn.train[[2]];churn.train <- churn.train[[1]]
# Confirm that they are similar
cbind("Train" = prop.table(table(churn.train$Churn)),
      "Test" = prop.table(table(churn.test$Churn)))
```

              Train      Test
    FALSE 0.7364635 0.7291312
    TRUE  0.2635365 0.2708688


Training sets are made as a subset of the total dataset in Random Forest so that the model can calibrate each random tree until the optimum values are set. Typically, 25% of the data is used to train and the other 75% is used to test as a benchmark to see how well the model is calibrated against real, known results.


#### Create the Random Forest Model


For those interested in understanding the parameters 'ntree', 'mtry', and 'nodesize':

- ntree: "Number of trees to grow" - refers to the stability and covariate importance estimates of the model. If you've worked with decision trees before, this is just how many trees are used in the decision. By fiddling with this you can usually affect the accuracy of the predictions, but should generally be used in proportion to the size of your dataset. For example, smaller datasets, like this one, will perform great with a value of 50, while larger datasets should err on the side of 500 or more. Keep in mind that the higher the value, the more memory and longer run time the model will take to run.

- mtry: "Number of variables available for splitting at each tree node") - This parameter is very important to regulate overfitting. Represents how many options/splits are made within each tree. Of course, useful default values exist as a baseline. However, don't be afraid to fiddle with this to fine-tune your model.

- nodesize: "Depth of your trees" - This parameter sets the depth of your trees as the minimum number of values that can be made befor planting smaller trees. Setting a larger value takes less computing power for the model and fewer observations are required. Again, there are baseline default values here (1 for classification and 5 for regression), but this can always be optimized. I decided on a higher value to further fine tune the model.

```r
# Random Forest
out.churn = randomForest(x = churn.train[,-20], y = as.factor(churn.train$Churn),
                          xtest = churn.test[,-20], ytest = as.factor(churn.test$Churn),
                          replace = TRUE,
                          keep.forest = TRUE,
                          ntree = 50,
                          mtry = 5,
                          nodesize = 25)
out.churn
```


                   Type of random forest: classification
                         Number of trees: 50
    No. of variables tried at each split: 5
    
            OOB estimate of  error rate: 20.73%
    Confusion matrix:
          FALSE TRUE class.error
    FALSE  3536  354  0.09100257
    TRUE    741  651  0.53232759
                    Test set error rate: 19.31%
    Confusion matrix:
          FALSE TRUE class.error
    FALSE  1180  104  0.08099688
    TRUE    236  241  0.49475891


The model reports an error rate of ~20%. That means if our model was to predict out of unseen data, we would be around 80% accurate. Our confusion matrix reports a low rate of false positives, but an unfortunate amount of true negatives. *In other words, our model will do a great job predicting those that are truly likely to stay (~90%), but have a more difficult time actually deciphering out of those that are likely to leave (~50%).* This will mean that explanatory details will be more important with this model than the predictions itself.


### Predictions

Visualizing the weight of explanatory variables on response variable:

```r
# Variable importance
round(importance(out.churn), 0)
varImpPlot(out.churn)
```

![](https://raw.githubusercontent.com/tykiww/telco-churn-clv/master/images/varImp_rf.png)

Before making any predictions on any specific observations, we will dive deep into the effects of each explanatory variable on the response variable, churn. 

On the x-axis is the 'MeanDecreaseGini'. This is the weight the inputs have on the result as predicted by Random Forest. Higher numbers mean it is more 'important' in the decision. Just as the ipmortance of exercise is more influential on your health than how many pieces of gum you chew perday. This is enlightening, not only for predictions but to understand which areas to hone in to when you are calculating the biggest bang for your buck.

A few key insights:

- gender, senior citizenship, etc. really dosn't have much to do with churn.

- Higher value MeanDecrease Gini variables are key  variables that can direct management on what to focus on.

- Contract and tenure seem to be the highest importance. However, it is unclear whether rewarding non-tenured clients with a discount on monthly charge would help decrease churn, but this may be studied further with a different model.

Another important thing to mention is the reliability of our results. Due to randomness, we may see slightly different results if the same train-test procedure is not followed. However, our data shows consistency. This will mean that we will not see significant deviance in our outcomes.

#### Demonstrate furture prediction cabability and variability

```r
# Demonstrate future prediction
new.obs1 <- churn.test[500,-20]
churn.test[500,]
levels(new.obs1$gender) <- c("Female", "Male")
predict(out.churn, newdata = new.obs1, type = "prob")
```

    0.842

Prediction is simple. An individual with the following characteristics:
- Gender: Female
- SeniorCitizen: False
- Partner: Yes
- Dependents: No
- Tenure: 1 year
- Phone Service: No
- No multiple Lines
- DSL Internet Service
- No online security
- No online backup
- No device protection
-  Month to month contract
- Streaming TV: yes
... etc.

would likely churn.


## Conclusion

In summary, the random forest does a fantastic job at predicting (~80% accuracy). However, it may still not be the best model to predict a new customer's tendency. However, the model has been a critical in capturing the most important variables in terms of who remains or leaves as a client. In the future, further analysis such as a test for intereactions could be performed for an optimal importance analysis. Although random forests are capable of capturing interactions, but current variable importance measures are still unable to detect them to be 'interactions'. In most cases, interactions are masked by marginal effects and interactions cannot be differentiated from marginal effects.

