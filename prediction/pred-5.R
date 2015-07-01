library("e1071")
library("gramEvol")
library("Metrics")
library("parallel")
library("memoise")

set.seed(1)

options(mc.cores = 8)

source("load_data.R")
source("libs/lag_window.R")
source("libs/memofitness.R")
source("prediction/mapping_predictor.R")

N_train = 15000

# remove the daylight saving
dt = dt[-index_daylight_saving,]

learn_forecaster <- function(dt, train_period, target_index, ge_model, N_train = 2000) {

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
  if (target_index == 1) {
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
  
  return (mdl)
}

iterative_forecaster <- function(dt, target_period, ge_model1, ge_model2, ge_model3, ge_model4) {

  forecast_nas <- function(ge_model, target_index) {
    # get the GE model
    feature_extract = ge_model$feature_extract
    model = ge_model$model
    
    # get all features (omit the na's)
    all_features = feature_extract(dt)
    all_features = na.omit(all_features)
    
    # get the NA targets
    absent_targets = index(dt[target_period])[is.na(dt[target_period, target_index])]
    
    # which we have features for
    pred_x = na.omit(all_features[absent_targets,])

    # predict
    pred_y = predict(model, pred_x)

    if (is.factor(pred_y[1])) {
      pred_y = as.numeric(as.character(pred_y))
    }
  
    pred_y = xts(as.numeric(pred_y), index(pred_x))
  
    # return raw prediction
    return (pred_y)
  }
  
  while (any(is.na(dt[target_period,]))) {
    print(paste("Remaining NAs:", sum(is.na(dt[target_period,]))))
    
    pred1 = forecast_nas(ge_model1, 1)
    pred2 = forecast_nas(ge_model2, 2)
    pred3 = forecast_nas(ge_model3, 3)
    pred4 = forecast_nas(ge_model4, 4)
    
    dt_merged = dt
    dt_merged[,1] = na.ts.merge(dt_merged[,1], pred1)
    dt_merged[,2] = na.ts.merge(dt_merged[,2], round(pred2, 1))
    dt_merged[,3] = na.ts.merge(dt_merged[,3], round(pred3, 1))
    dt_merged[,4] = na.ts.merge(dt_merged[,4], pred4)
    
    dt = dt_merged  
  }
  
  return (dt)
}

################################################################
# Forwards

print("Learning Forward 1")
load("models/d5f1.RData")
ge_model_f1 = eval(ev5f_1$best$expressions)
ge_model_f1$model = learn_forecaster(dt, sp5, 1, ge_model_f1, N_train)

print("Learning Forward 2")
load("models/d5f2.RData")
ge_model_f2 = eval(ev5f_2$best$expressions)
ge_model_f2$model = learn_forecaster(dt, sp5, 2, ge_model_f2, N_train)

print("Learning Forward 3")
load("models/d5f3.RData")
ge_model_f3 = eval(ev5f_3$best$expressions)
ge_model_f3$model = learn_forecaster(dt, sp5, 3, ge_model_f3, N_train)

print("Learning Forward 4")
load("models/d5f4.RData")
ge_model_f4 = eval(ev5f_4$best$expressions)
ge_model_f4$model = learn_forecaster(dt, sp5, 4, ge_model_f4, N_train)

dt_forward = iterative_forecaster(dt, sp5, ge_model_f1, ge_model_f2, ge_model_f3, ge_model_f4)

################################################################
# Backwards

reverse_dt <- function(dt) {
  dt_in_backwards = dt
  dt_in_backwards[,1] = rev(coredata(dt_in_backwards)[,1])
  dt_in_backwards[,2] = rev(coredata(dt_in_backwards)[,2])
  dt_in_backwards[,3] = rev(coredata(dt_in_backwards)[,3])
  dt_in_backwards[,4] = rev(coredata(dt_in_backwards)[,4])
  return(dt_in_backwards)
}

dt_in_backwards = reverse_dt(dt)

sp5rb = sort(nrow(dt) - (436836:(521836-120)))
sp5b = range(index(dt_in_backwards)[sp5rb])
sp5b = do.call(paste, list(substr(sp5b[[1]], 1, 19), substr(sp5b, 1, 19)[[2]], sep="/"))

print("Learning Backwards 1")
load("models/d5b1.RData")
ge_model_b1 = eval(ev5b_1$best$expressions)
ge_model_b1$model = learn_forecaster(dt_in_backwards, sp5b, 1, ge_model_b1, N_train)

print("Learning Backwards 2")
load("models/d5b2.RData")
ge_model_b2 = eval(ev5b_2$best$expressions)
ge_model_b2$model = learn_forecaster(dt_in_backwards, sp5b, 2, ge_model_b2, N_train)

print("Learning Backwards 3")
load("models/d5b3.RData")
ge_model_b3 = eval(ev5b_3$best$expressions)
ge_model_b3$model = learn_forecaster(dt_in_backwards, sp5b, 3, ge_model_b3, N_train)

print("Learning Backwards 4")
load("models/d5b4.RData")
ge_model_b4 = eval(ev5b_4$best$expressions)
ge_model_b4$model = learn_forecaster(dt_in_backwards, sp5b, 4, ge_model_b4, N_train)

dt_backward = iterative_forecaster(dt_in_backwards, sp5b, ge_model_b1, ge_model_b2, ge_model_b3, ge_model_b4)
dt_backward = reverse_dt(dt_backward)

##################################################################

dt_5 = dt
dt_5[sp5,1] = round(na.ts.double.merge(dt_5[sp5,1], dt_forward[sp5,1], dt_backward[sp5,1]))
dt_5[sp5,2] = round(na.ts.double.merge(dt_5[sp5,2], dt_forward[sp5,2], dt_backward[sp5,2]), 1)
dt_5[sp5,3] = round(na.ts.double.merge(dt_5[sp5,3], dt_forward[sp5,3], dt_backward[sp5,3]), 1)
dt_5[sp5,4] = round(na.ts.double.merge(dt_5[sp5,4], dt_forward[sp5,4], dt_backward[sp5,4]))

#save(dt_5, file="pred-5.RData")
