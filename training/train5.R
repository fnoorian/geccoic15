library("e1071")
library("gramEvol")
library("Metrics")
library("parallel")
library("memoise")

set.seed(0)

options(mc.cores = 8)

source("load_data.R")
source("factorizers.R")
source("libs/lag_window.R")
source("libs/memofitness.R")

# remove the daylight saving
dt = dt[-index_daylight_saving,]
  
model5_grammar <- CreateGrammar(list(
  model_def = grule(list(
    feature_extract = function(dt) {
      cbind(LaggedWindows(dt[,1], p = w, pstr=colnames(dt)[1]),
            LaggedWindows(dt[,2], p = w, pstr=colnames(dt)[2]),
            LaggedWindows(dt[,3], p = w, pstr=colnames(dt)[3]),
            LaggedWindows(dt[,4], p = w, pstr=colnames(dt)[4]))
    },
    
    learner = function(feats, target) {
      svm(feats, target, cost = cost, gamma = gamma * ncol(feats), epsilon = epsilon)
    }
  )),
  
  w = grule(0, 1, 2, 3, 4, 5, 6, 8, 10, 12, 15, 20),
  cost = grule(0.25, 0.5, 1, 2, 4),
  gamma = grule(4, 2, 1, 0.5, 0.25),
  epsilon = grule(0.001, 0.05, 0.1, 0.2, 0.4)
))


cross_val_d5 <- function(model_def, period, target_index, is.reverse=FALSE) {
  # is.reverse is not used. Only to tell memoiser the data is different
  
  print(model_def)
  
  feature_extract = model_def$feature_extract
  learner = model_def$learner
  
  print("Extracting features")
  all_features = feature_extract(dt[period,])
  all_train_test = na.omit(cbind(dt[period, target_index], all_features))
  colnames(all_train_test)[1] = "Target"
    
  # extract all features
  all_x = all_train_test[,-1]
  all_y = all_train_test[,1]
  
  err = NULL
  
  print("Parallel Testing")
  
  N_folds = 8
  #for (i in 0:(N_folds-1)) {
  err = as.numeric(mclapply(0:(N_folds-1), function(i) {
    ind = which( seq_along(all_y) %% N_folds == i)
    # ind = sample(seq_along(all_y), 10000)
    
    train_x = all_x[ind,]
    train_y = all_y[ind]
    
    test_x = all_x[-ind,]
    test_y = all_y[-ind]
    
    print("training model")
    if (target_index == 1) {
      mdl = learner(coredata(train_x), as.factor(factorizer1(train_y)))
    } else if (target_index == 4) {
      mdl = learner(coredata(train_x), as.factor(factorizer4(train_y)))
    } else {
      mdl = learner(coredata(train_x), coredata(train_y))
    }
    
    print("predicting")
    pred = predict(mdl, test_x)
    
    if (is.factor(pred[1])) {
      pred = xts(as.numeric(as.character(pred)), index(test_x))
    } else {
      pred = xts(as.numeric(pred), index(test_x))
    }
    
    e = rmse(pred, test_y)
    #print(paste("RMSE #", i, ":", e))
    e
  }))
  
  print("Err:")
  print(err)
  err = mean(err)
  print(paste("Total:", err))
  
  if (is.na(err)) {
    return(Inf)
  }
  
  return(err)
}

cross_val_d5.m = memo_fitness_function(cross_val_d5)

ev5f_1 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5, 1), monitorFunc = print, iterations = 80)
save(ev5f_1, file="models/d5f1.RData")

ev5f_2 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5, 2), monitorFunc = print, iterations = 80)
save(ev5f_2, file="models/d5f2.RData")

ev5f_3 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5, 3), monitorFunc = print, iterations = 80)
save(ev5f_3, file="models/d5f3.RData")

ev5f_4 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5, 4), monitorFunc = print, iterations = 80)
save(ev5f_4, file="models/d5f4.RData")

# reverse dt
reverse_dt <- function(dt) {
  dt_in_backwards = dt
  dt_in_backwards[,1] = rev(coredata(dt_in_backwards)[,1])
  dt_in_backwards[,2] = rev(coredata(dt_in_backwards)[,2])
  dt_in_backwards[,3] = rev(coredata(dt_in_backwards)[,3])
  dt_in_backwards[,4] = rev(coredata(dt_in_backwards)[,4])
  return(dt_in_backwards)
}

dt = reverse_dt(dt)

sp5rb = sort(nrow(dt) - (436836:(521836-120)))
sp5b = range(index(dt)[sp5rb])
sp5b = do.call(paste, list(substr(sp5b[[1]], 1, 19), substr(sp5b, 1, 19)[[2]], sep="/"))

ev5b_1 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5b, 1, is.reverse=TRUE), monitorFunc = print, iterations = 80)
save(ev5b_1, file="models/d5b1.RData")

ev5b_2 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5b, 2, is.reverse=TRUE), monitorFunc = print, iterations = 80)
save(ev5b_2, file="models/d5b2.RData")

ev5b_3 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5b, 3, is.reverse=TRUE), monitorFunc = print, iterations = 80)
save(ev5b_3, file="models/d5b3.RData")

ev5b_4 = GrammaticalEvolution(model5_grammar, function(expr) cross_val_d5.m(eval(expr), sp5b, 4, is.reverse=TRUE), monitorFunc = print, iterations = 80)
save(ev5b_4, file="models/d5b4.RData")
