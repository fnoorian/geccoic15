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

cross_val_d4 <- function(model_def, tx) {
  
  print(model_def)
  
  feature_extract = model_def$feature_extract
  learner = model_def$learner
  
  print("Extracting features")
  all_features = feature_extract(dt)

  valid_indices = index(dt[tx,])[!is.na(dt[tx,4])]
  #na_indices = index(dt[tx,])[is.na(dt[tx,4])]

  # extract all features
  all_x = all_features[valid_indices,]
  all_y = dt[valid_indices,4]

  err = NULL

  print("Parallel Testing")

  N_folds = 16
  #for (i in 0:(N_folds-1)) {
  err = as.numeric(mclapply(0:(N_folds-1), function(i) {
    ind = which( seq_along(all_y) %% N_folds == i)
    # ind = sample(seq_along(all_y), 10000)
    
    train_x = all_x[ind,]
    train_y = all_y[ind]

    test_x = all_x[-ind,]
    test_y = all_y[-ind]

    print("training model")
    mdl = learner(coredata(train_x), as.factor(factorizer4(train_y)))

    print("predicting")
    pred = predict(mdl, test_x)
    pred = xts(as.numeric(as.character(pred)), index(test_x))

    e = rmse(pred, test_y)

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

model4_grammar <- CreateGrammar(list(
  model_def = grule(list(
    feature_extract = function(dt) {
      cbind(CenteredWindows(dt[,1], p = w, pstr=colnames(dt)[1]),
            CenteredWindows(dt[,2], p = w, pstr=colnames(dt)[2]),
            CenteredWindows(dt[,3], p = w, pstr=colnames(dt)[3]))
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

cross_val_d4.m = memo_fitness_function(cross_val_d4)

ev4 = GrammaticalEvolution(model4_grammar, function(expr) cross_val_d4.m(eval(expr), sp4r), monitorFunc = print, iterations = 80)
save(ev4, file="models/d4.RData")

