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

sp1a = "2014-01-24 11:03:00/2014-02-28 12:00:00"
sp1b = "2014-02-28 12:01:00/2014-03-24 11:43:00"
sp1ra = 96832:147289 # 
sp1rb = 147290:181832 # 

sp1a_trainer = "2014-01-15 11:03:00/2014-02-28 12:00:00"

N_Train = 15000
  
print("Period 1a")
load("models/d1a.RData")
ge_model = eval(ev1a$best$expressions)
pred1a = mapping_predict_ge_model(dt, sp1a, sp1a, 1, ge_model, N_Train)

##########################
print("Period 1b")
load("models/d1b.RData")
ge_model = eval(ev1b$best$expressions)
pred1b = mapping_predict_ge_model(dt, sp1b, sp1b, 1, ge_model, N_Train)

##########################
print("Period 2")
load("models/d2.RData")
ge_model = eval(ev2$best$expressions)
pred2 = mapping_predict_ge_model(dt, sp2, sp2, 2, ge_model, N_Train)

##########################
print("Period 3")
load("models/d3.RData")
ge_model = eval(ev3$best$expressions)
pred3 = mapping_predict_ge_model(dt, sp3, sp3, 3, ge_model, N_Train)

##########################
print("Period 4")
load("models/d4.RData")
ge_model = eval(ev4$best$expressions)
pred4 = mapping_predict_ge_model(dt, sp4, sp4, 4, ge_model, N_Train)

##########################
print("Merging:")
dt_1_2_3_4 = dt
dt_1_2_3_4[,1] = na.ts.merge(dt_1_2_3_4[,1], pred1a)
dt_1_2_3_4[,1] = na.ts.merge(dt_1_2_3_4[,1], pred1b)
dt_1_2_3_4[,2] = na.ts.merge(dt_1_2_3_4[,2], round(pred2, 1))
dt_1_2_3_4[,3] = na.ts.merge(dt_1_2_3_4[,3], round(pred3, 1))
dt_1_2_3_4[,4] = na.ts.merge(dt_1_2_3_4[,4], pred4)

#save(dt_1_2_3_4, file="pred-1-2-3-4.RData")
