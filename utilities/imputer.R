########
#
# Telco Imputer Funcction.
# Module only used for Telco Analysis.
#
########


telco_imputer <- function(dataset) {
  
  # Load Decision Tree
  require(rpart)
  
  # Locate NA
  test = raw[is.na(dataset$TotalCharges),]
  train = raw[!is.na(dataset$TotalCharges),]
  
  # Predict Total Charges
  impute_lm = lm(TotalCharges~.,data = dplyr::select(train,-customerID))
  impute_rp = rpart(TotalCharges~.,data = dplyr::select(train,-customerID))
  preds_train = data.frame(x1 = predict(impute_lm),x2 = predict(impute_rp),y = train$TotalCharges)
  preds_test = data.frame(x1 = predict(impute_lm,test),x2 = predict(impute_rp,test))
  
  # combine and find probability of each result being correct/incorrect.
  ob_tests = summary(lm(y~.,preds_train))
  relative_coeffs = sapply(1:2,function(i)ob_tests$coefficients[-1,1][i]*median(train$TotalCharges))
  prbs <- abs(relative_coeffs)/sum(abs(relative_coeffs))
  
  # Use Probabilies to weight predictions on 'test'
  imp_val <- sapply(1:2, function(x) prbs[x]*preds_test[,x]) %>% rowSums()
  imp_val <- ifelse(imp_val<test$MonthlyCharges ,test$MonthlyCharges,imp_val)
  test$TotalCharges <- imp_val
  
  return(rbind(train,test))
}
