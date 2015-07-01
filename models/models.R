# NOTE: This directory holds the models as extracted by the GE.
# As the discovery process, initiated by train_all.R, takes about 2 days
# on an Intel Core i7-4790, these models are saved for faster future access.
# Additionaly, the following expressions are extracted from saved objects
# for a more convenient inspection or direct use.

ev1a <- expression(
	list(feature_extract = function(dt) {
      cbind(CenteredWindows(dt[, 2], p = 1, pstr = colnames(dt)[2]), 
            CenteredWindows(dt[, 3], p = 1, pstr = colnames(dt)[3]), 
            CenteredWindows(dt[, 4], p = 3, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.4)
}))

ev1b <- expression(
	list(feature_extract = function(dt) {
      cbind(CenteredWindows(dt[, 2], p = 4, pstr = colnames(dt)[2]), 
            CenteredWindows(dt[, 3], p = 1, pstr = colnames(dt)[3]), 
            CenteredWindows(dt[, 4], p = 0, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.2)
}))

ev2 <- expression(
	list(feature_extract = function(dt) {
      cbind(CenteredWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            CenteredWindows(dt[, 3], p = 1, pstr = colnames(dt)[3]), 
            CenteredWindows(dt[, 4], p = 1, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.1)
}))

ev3 <- expression(
	list(feature_extract = function(dt) {
      cbind(CenteredWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            CenteredWindows(dt[, 2], p = 1, pstr = colnames(dt)[2]), 
            CenteredWindows(dt[, 4], p = 0, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.001)
}))

ev4 <- expression(
	list(feature_extract = function(dt) {
      cbind(CenteredWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            CenteredWindows(dt[, 2], p = 1, pstr = colnames(dt)[2]), 
            CenteredWindows(dt[, 3], p = 1, pstr = colnames(dt)[3]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.001)
}))

ev5b_1 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 1, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 4, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 0, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 2, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.2)
}))

ev5b_2 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 2, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 2, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 0, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 2, gamma = 0.25 * ncol(feats), 
        epsilon = 0.001)
}))

ev5b_3 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 1, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 2, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 1, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.001)
}))

ev5b_4 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 0, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 0, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 4, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.5 * ncol(feats), epsilon = 0.4)
}))

ev5f_1 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 2, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 0, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 0, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 0, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 1 * ncol(feats), epsilon = 0.05)
}))

ev5f_2 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 2, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 1, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 1, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 2, gamma = 0.25 * ncol(feats), 
        epsilon = 0.001)
}))

ev5f_3 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 0, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 1, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 2, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 0, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.001)
}))

ev5f_4 <- expression(
	list(feature_extract = function(dt) {
      cbind(LaggedWindows(dt[, 1], p = 2, pstr = colnames(dt)[1]), 
            LaggedWindows(dt[, 2], p = 1, pstr = colnames(dt)[2]), 
            LaggedWindows(dt[, 3], p = 1, pstr = colnames(dt)[3]), 
            LaggedWindows(dt[, 4], p = 1, pstr = colnames(dt)[4]))
}, learner = function(feats, target) {
    svm(feats, target, cost = 4, gamma = 0.25 * ncol(feats), 
        epsilon = 0.001)
}))
