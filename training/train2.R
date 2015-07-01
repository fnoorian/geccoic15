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

good_log = function(x) {
  ifelse (abs(x) < 0.05, 0, sign(x) * log(20*abs(x)) )
}

reverse_good_log = function(x) {
  y = ifelse (abs(x) < 0.1, 0, sign(x) * exp(abs(x))/20 )
  y = ifelse(abs(y) > 30,  sign(y) * 30, y)

    return(y)
}

cross_val_d2 <- function(model_def, tx) {
  
  print(model_def)
  
  feature_extract = model_def$feature_extract
  learner = model_def$learner
  
  print("Extracting features")
  all_features = feature_extract(dt)

  # extract all features
  all_x = all_features[tx,]
  all_y = good_log(diff(dt[,2]))[tx]

  valid_indices = index(all_y)[!is.na(all_y)]
  #na_indices = index(dt[tx,])[is.na(dt[tx,1])]

  all_x = all_features[valid_indices,]
  all_y = diff(dt[,2])[valid_indices]
  
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
    mdl = learner(coredata(train_x), coredata(train_y))

    print("predicting")
    pred = predict(mdl, test_x)
    pred = xts(as.numeric(as.character(pred)), index(test_x))

    e = rmse(reverse_good_log(pred), reverse_good_log(test_y))

    #print(paste("RMSE #", i, ":", e))
    e
    #err = c(err, e)
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

model2_grammar <- CreateGrammar(list(
  model_def = grule(list(
    feature_extract = function(dt) {
      cbind(CenteredWindows(dt[,1], p = w, pstr=colnames(dt)[1]),
            CenteredWindows(dt[,3], p = w, pstr=colnames(dt)[3]),
            CenteredWindows(dt[,4], p = w, pstr=colnames(dt)[4]))
    },
    
    learner = function(feats, target) {
      svm(feats, target, cost = cost, gamma = gamma * ncol(feats), epsilon = epsilon)
    }
  )),
  
  w = grule(-1, 0, 1, 2, 3, 4, 5, 10),
  cost = grule(0.25, 0.5, 1, 2, 4),
  gamma = grule(4, 2, 1, 0.5, 0.25),
  epsilon = grule(0.001, 0.05, 0.1, 0.2, 0.4)
))

cross_val_d2.m = memo_fitness_function(cross_val_d2)

ev2 = GrammaticalEvolution(model2_grammar, function(expr) cross_val_d2.m(eval(expr), sp2), monitorFunc = print, iterations = 80)
save(ev2, file="models/d2.RData")
