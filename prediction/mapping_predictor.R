source("libs/diff_acumulator.R")
source("libs/na_ts_merge.R")
source("factorizers.R")

mapping_predict_ge_model <- function(dt, train_period, test_period, target_index, ge_model, N_train = 2000) {
  
  # get the GE model
  feature_extract = ge_model$feature_extract
  learner = ge_model$learner
  
  # get all features (omit the na's)
  all_features = feature_extract(dt)
  all_features = na.omit(all_features)
  
  # get the valid indices for training
  valid_train_indices = index(all_features[train_period,])[!is.na(dt[train_period, target_index])]
  
  train_y = dt[valid_train_indices, target_index]
  
  # for index 2 and 3, the target is the difference
  if ((target_index == 2) || (target_index == 3)) {
    train_y = diff(dt[, target_index])[valid_train_indices]
  } else if (target_index == 1) {
    train_y = factorizer1(dt[valid_train_indices, target_index])
  } else if (target_index == 4) {
    train_y = factorizer4(dt[valid_train_indices, target_index])
  }
  
  train_y = na.omit(train_y)
  train_x = all_features[index(train_y),]
  
  # select a maximum number for training the model
  if (nrow(train_x) > N_train) {
    set.seed(0)
    ind = sort(sample(1:nrow(train_x), N_train))
  } else {
    ind = 1:nrow(train_x)
  }
  
  # learn the model. classifiy for 1 & 4, regression for 2 & 3
  if (target_index == 1 || target_index == 4) {
    mdl = learner(coredata(train_x[ind,]), as.factor(coredata(train_y[ind])))
  } else {
    mdl = learner(coredata(train_x[ind,]), coredata(train_y[ind]))
  }
  
  # for regression task, also predict the one after NA
  absent_pred_indices = which(is.na(dt[test_period, target_index]))
  if (target_index == 1 || target_index == 4) {
    absent_pred_indices = index(dt[test_period,])[absent_pred_indices]
  } else {
    absent_pred_indices = index(dt[test_period,])[unique(c(absent_pred_indices, absent_pred_indices+1))]
  }
  
  pred_x = all_features[absent_pred_indices,]
  pred_y = predict(mdl, pred_x)
  
  if (is.factor(pred_y[1])) {
    pred_y = as.numeric(as.character(pred_y))
  }
  
  pred_y = xts(as.numeric(pred_y), index(pred_x))
  
  # convert difference prediction to actual predictions for 2 and 3
  if (target_index == 2 || target_index == 3) {
    pred_y = diff_accumulator(dt[test_period, target_index], pred_y)
  }
  
  # merge with the observed data
  combined = na.ts.merge(main = pred_y, other = dt[test_period, target_index])
  
  return(combined)
}
