library(mlr3verse)
search_space <- ps(
  mtry.ratio = p_dbl(lower = 0, upper = 1),
  min.node.size = p_int(lower = 1L, upper = 10L, default = 1L)
)

at.ranger <- AutoTuner$new(
  learner = lrn("classif.ranger"),
  resampling = rsmp("cv", folds = 10),
  measure = msr("classif.acc"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 2000),
  tuner = tnr("random_search")
)

search_space.svm = ps(
  cost = p_dbl(-12, 12, trafo = function(x) 2^x),
  gamma = p_dbl(-12, 12, trafo = function(x) 2^x)
)
at.svm <- AutoTuner$new(
  learner = lrn("classif.svm", predict_type = "prob", type = "C-classification",
                kernel = "radial"),
  resampling = rsmp("cv", folds = 10),
  measure = msr("classif.acc"),
  search_space = search_space.svm,
  terminator = trm("evals", n_evals = 1000),
  tuner = tnr("random_search")
)

search_space <- ps(
  eta = p_dbl(lower = -4, upper = 0, trafo = function(x) 10^x, default = log(0.3)),
  nrounds = p_int(lower = 1, upper = 5000),
  max_depth = p_int(lower = 1, upper = 20, default = 6),
  colsample_bytree = p_dbl(lower = 0.1, upper = 1, default = 1),
  colsample_bylevel = p_dbl(lower = 0.1, upper = 1, default = 1),
  alpha = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x, default = 0),
  lambda = p_dbl(lower = -10, upper = 10, trafo = function(x) 2^x, 
                 special_vals = list(-Inf), default = -Inf),
  subsample = p_dbl(lower = 0.1, upper = 1, default = 1), 
  gamma = p_dbl(lower = 0, upper = 20)
)

at.xgboost <- AutoTuner$new(
  learner = lrn("classif.xgboost"),
  resampling = rsmp("cv", folds = 10),
  measure = msr("classif.acc"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 5000),
  tuner = tnr("random_search")
)

# elastic net
search_space <- ps(
  alpha = p_dbl(lower = 0, upper = 1)
)
at.cv_glmnet <- AutoTuner$new(
  learner = lrn("classif.cv_glmnet", relax = FALSE),
  resampling = rsmp("cv", folds = 10),
  measure = msr("classif.acc"),
  search_space = search_space,
  terminator = trm("evals", n_evals = 500),
  tuner = tnr("random_search")
)