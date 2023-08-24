load("../Data/dataset_classifier_paper.RData")
library(mlr3batchmark)
library(mlr3verse)
library(batchtools)
library(BBmisc)
source("autotuner10.R")

classifier.data$Abbreviation <- as.character(classifier.data$Abbreviation)
classifier.data$Strat <- classifier.data$Hepatotoxicity.cat

################################################################################
##                        GROUP ALOEC, DEG, NrDEG                             ##
##                                                                            ##
################################################################################

# 1st repetition for DEG
grid_deg <- benchmark_grid(
  tasks = task_min_conc_deg,
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)


reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_deg", seed = 3001)
set.seed(13062022)
batchmark(grid_deg, reg = reg, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
batchtools::submitJobs(reg = reg)
waitForJobs(reg = reg)

bmr_cv_deg_wdh <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_deg_wdh, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_20220613.RData")


groups <- c("ALOEC", "min\\.conc\\.deg", "Nrdegs")

classifier.data$Abbreviation <- as.character(classifier.data$Abbreviation)
classifier.data$Strat <- classifier.data$Hepatotoxicity.cat

for(g in groups){
  dat <- classifier.data[, c("Hepatotoxicity.cat", "Abbreviation", "Strat", 
                          grep("EC10|Cmax", colnames(classifier.data), value = TRUE), 
                          grep(g, colnames(classifier.data), value = TRUE)), drop = FALSE]
  g.sub <- gsub("\\.", "_", g, fixed = TRUE)
  assign(paste0("task_", g.sub), as_task_classif(id = paste0("hepatotox_", g.sub), dat, 
                                                 target = "Hepatotoxicity.cat", 
                                                 positive = "1"))
  get(paste0("task_", g.sub))$set_col_roles("Abbreviation", roles = "name")
  get(paste0("task_", g.sub))$set_col_roles("Strat", roles = "stratum")
}



grid_sep <- benchmark_grid(
  tasks = lapply(groups, function(g) get(paste0("task_", gsub("\\.", "_", g, fixed = TRUE)))),
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition for NrDEG and ALOEC:
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_sep", seed = 8302)

batchmark(grid_sep, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)

getJobTable(reg = reg)

bmr_cv_aloec <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_aloec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_20220516.RData")
rm(bmr_cv_aloec)


bmr_cv_nrdegs <- reduceResultsBatchmark(reg = reg, ids = 181:270)
save(bmr_cv_nrdegs, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdegs_20220516.RData")
rm(bmr_cv_nrdegs)

# 2nd repetition for NrDEG, ALOEC and DEG
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_sep_2", seed = 1234)
batchmark(grid_sep, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)

getJobTable(reg = reg)

bmr_cv_aloec <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_aloec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_20220516_1.RData")
rm(bmr_cv_aloec)

bmr_cv_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_conc_deg, file = "Biospyder/Daten/Benchmarks/benchmark_CTB_Vivo_all_cv10_conc_deg_20220516_1.RData")
rm(bmr_cv_conc_deg)


bmr_cv_nrdegs <- reduceResultsBatchmark(reg = reg, ids = 181:270)
save(bmr_cv_nrdegs, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdegs_20220516_1.RData")
rm(bmr_cv_nrdegs)


# 3rd repetition for NrDEG, ALOEC and DEG
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_sep_3", seed = 6090)
batchmark(grid_sep, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)

getJobTable(reg = reg)

bmr_cv_aloec <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_aloec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_20220516_2.RData")
rm(bmr_cv_aloec)

bmr_cv_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_conc_deg, file = "Biospyder/Daten/Benchmarks/benchmark_CTB_Vivo_all_cv10_conc_deg_20220516_2.RData")
rm(bmr_cv_conc_deg)


bmr_cv_nrdegs <- reduceResultsBatchmark(reg = reg, ids = 181:270)
save(bmr_cv_nrdegs, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdegs_20220516_2.RData")
rm(bmr_cv_nrdegs)



################################################################################
##                              Only Cytotoxicity                             ##
##                                                                            ##
################################################################################
task_hepatotox_wo <- as_task_classif(id = "hepatotox", classifier.data[, 1:15], 
                                     target = "Hepatotoxicity.cat", 
                                     positive = "1")
task_hepatotox_wo$set_col_roles("Abbreviation", roles = "name")
task_hepatotox_wo$set_col_roles("Strat", roles = "stratum")

grid_wo <- benchmark_grid(
  task = task_hepatotox_wo,
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "Biospyder/Daten/Registry10_wo", seed =  20919)
set.seed(27042022)
batchmark(grid_wo, reg = reg, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)

bmr_cv_wo <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_wo, file = "Biospyder/Daten/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_20220503.RData")

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "Biospyder/Daten/Registry10_wo_wdh", seed = 3969)
set.seed(07062022)
batchmark(grid_wo, reg = reg, store_models = TRUE)
batchtools::submitJobs(reg = reg)
waitForJobs(reg = reg)
bmr_cv_wo_wdh <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_wo_wdh, file = "Biospyder/Daten/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_20220607.RData")

# 3rd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "Biospyder/Daten/Registry10_wo.3", seed =  1006)
set.seed(10062022)
batchmark(grid_wo, reg = reg, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_wo_wdh_2 <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_wo_wdh_2, file = "Biospyder/Daten/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_20220613.RData")



################################################################################
##                                NrDEG+DEG+ALOEC                             ##
##                                                                            ##
################################################################################
groups <- "ALEC"

classifier.data$Abbreviation <- as.character(classifier.data$Abbreviation)
classifier.data$Strat <- classifier.data$Hepatotoxicity.cat

for(i in seq(along = groups)){
  dat <- classifier.data[, c("Hepatotoxicity.cat", "Abbreviation", "Strat", 
                          grep("EC10|Cmax", colnames(classifier.data), value = TRUE), 
                          grep(paste0(groups[-i], collapse = "|"), colnames(classifier.data), value = TRUE)), drop = FALSE]
  g.sub <- gsub("\\.", "_", groups[i], fixed = TRUE)
  assign(paste0("task_wo_", g.sub), as_task_classif(id = paste0("hepatotox_", g.sub), dat, 
                                                    target = "Hepatotoxicity.cat", 
                                                    positive = "1"))
  get(paste0("task_wo_", g.sub))$set_col_roles("Abbreviation", roles = "name")
  get(paste0("task_wo_", g.sub))$set_col_roles("Strat", roles = "stratum")
}



grid_3 <- benchmark_grid(
  tasks = lapply(groups, function(g) get(paste0("task_wo_", gsub("\\.", "_", g, fixed = TRUE)))),
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_komb3_1", seed = 1339)
batchmark(grid_3, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_wo_alec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_wo_alec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_alec_20220615.RData")
rm(bmr_cv_wo_alec)

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_komb3_2", seed = 150622)
batchmark(grid_3, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_wo_alec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_wo_alec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_alec_20220615_1.RData")
rm(bmr_cv_wo_alec)

# 3rd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_komb3_3", seed = 31209)
batchmark(grid_3, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_wo_alec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_wo_alec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_alec_20220615_2.RData")
rm(bmr_cv_wo_alec)



################################################################################
##                            DEG+ALOEC, NrDEG+DEG                            ##
##                                                                            ##
################################################################################
groups <- data.frame(X1 = c("min\\.conc\\.deg", "min\\.conc\\.deg"),
                     X2 = c("ALOEC", "Nrdegs"))


classifier.data$Abbreviation <- as.character(classifier.data$Abbreviation)
classifier.data$Strat <- classifier.data$Hepatotoxicity.cat

for(i in 1:nrow(groups)){
  dat <- classifier.data[, c("Hepatotoxicity.cat", "Abbreviation", "Strat", 
                          grep("EC10|Cmax", colnames(classifier.data), value = TRUE), 
                          grep(paste0(groups[i, ], collapse = "|"), colnames(classifier.data), value = TRUE)), drop = FALSE]
  g.sub <- paste0(gsub("\\.", "_", groups[i, ], fixed = TRUE), collapse = "_")
  assign(paste0("task_", g.sub), as_task_classif(id = paste0("hepatotox_", g.sub), dat, 
                                                 target = "Hepatotoxicity.cat", 
                                                 positive = "1"))
  get(paste0("task_", g.sub))$set_col_roles("Abbreviation", roles = "name")
  get(paste0("task_", g.sub))$set_col_roles("Strat", roles = "stratum")
}



grid_4 <- benchmark_grid(
  tasks = apply(groups, 1, function(g) get(paste0("task_", paste0(gsub("\\.", "_", g, fixed = TRUE), collapse = "_")))),
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)


# 1st repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_komb2_1", seed = 1433)
batchmark(grid_4, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_aloec_min_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_aloec_min_conc_deg, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_min_conc_deg_20220621.RData")
rm(bmr_cv_aloec_min_conc_deg)
bmr_cv_nrdeg_min_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_nrdeg_min_conc_deg, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_min_conc_deg_20220621.RData")
rm(bmr_cv_nrdeg_min_conc_deg)

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_komb2_2", seed = 210622)
batchmark(grid_4, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_aloec_min_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_aloec_min_conc_deg, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_min_conc_deg_20220621_1.RData")
rm(bmr_cv_aloec_min_conc_deg)
bmr_cv_nrdeg_min_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_nrdeg_min_conc_deg, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_min_conc_deg_20220621_1.RData")
rm(bmr_cv_nrdeg_min_conc_deg)

# 3rd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_komb2_3", seed = 172)
batchmark(grid_4, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_aloec_min_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_aloec_min_conc_deg, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_min_conc_deg_20220621_2.RData")
rm(bmr_cv_aloec_min_conc_deg)

bmr_cv_nrdeg_min_conc_deg <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_nrdeg_min_conc_deg, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_min_conc_deg_20220621_2.RData")
rm(bmr_cv_nrdeg_min_conc_deg)

################################################################################
##                               NrDEG + ALOEC                                ##
##                                                                            ##
################################################################################
dat <- classifier.data[, c("Hepatotoxicity.cat", "Abbreviation", "Strat", 
                        grep("Cmax|EC10|ALOEC|Nrdegs", colnames(classifier.data), value = TRUE)),
                    drop = FALSE]
task_nrdeg_aloec <-  as_task_classif(id = "task_nrdeg_aloec", dat, 
                                     target = "Hepatotoxicity.cat", 
                                     positive = "1")
task_nrdeg_aloec$set_col_roles("Abbreviation", roles = "name")
task_nrdeg_aloec$set_col_roles("Strat", roles = "stratum")

grid_5 <- benchmark_grid(
  tasks = task_nrdeg_aloec,
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_all_nrdeg_aloec", seed = 2002)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)

bmr_cv_all_nrdeg_aloec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_all_nrdeg_aloec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_20221214.RData")
rm(bmr_cv_all_nrdeg_aloec)

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_all_nrdeg_aloec_1", seed = 0220)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_all_nrdeg_aloec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_all_nrdeg_aloec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_20230220_1.RData")
rm(bmr_cv_all_nrdeg_aloec)

# 3rd repetition 
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_all_nrdeg_aloec_2", seed = 2200)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_all_nrdeg_aloec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_all_nrdeg_aloec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_20230220_2.RData")
rm(bmr_cv_all_nrdeg_aloec)


################################################################################
##                               All and ALEC                                 ##
##                                                                            ##
################################################################################
dat <- classifier.data
task_all <- as_task_classif(id = "task_all", dat, target = "Hepatotoxicity.cat", 
                            positive = "1")
task_all$set_col_roles("Abbreviation", roles = "name")
task_all$set_col_roles("Strat", roles = "stratum")

dat <- classifier.data[, grepl("Cmax|EC|ALEC$|Hepatotox|Abbr|Strat", colnames(classifier.data))& ! grepl("ALOEC", colnames(classifier.data))]
task_alec <- as_task_classif(id = "task_alec", dat, target = "Hepatotoxicity.cat", 
                             positive = "1")
task_alec$set_col_roles("Abbreviation", roles = "name")
task_alec$set_col_roles("Strat", roles = "stratum")

grid_5 <- benchmark_grid(
  tasks = list(task_all, task_alec),
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_all_alec_corr", seed = 0203)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_all_corrected <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_all_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_corrected_20230302.RData")
rm(bmr_cv_all_corrected)
bmr_cv_all_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_all_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_alec_corrected_20230302.RData")
rm(bmr_cv_all_alec_corrected)

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_all_alec_corr_2", seed = 3020)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_all_corrected <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_all_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_corrected_20230302_1.RData")
rm(bmr_cv_all_corrected)
bmr_cv_all_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_all_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_alec_corrected_20230302_1.RData")
rm(bmr_cv_all_alec_corrected)

# 3rd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_all_alec_corr_3", seed = 2300)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_all_corrected <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_all_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_corrected_20230302_2.RData")
rm(bmr_cv_all_corrected)
bmr_cv_all_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_all_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_alec_corrected_20230302_2.RData")
rm(bmr_cv_all_alec_corrected)

################################################################################
##    NrDEG+ALEC, DEG+ALEC, ALOEC+ALEC, NrDEG+DEG+ALEC, NrDEG+ALOEC+ALEC      ##
##                                                                            ##
################################################################################
groups <- list(c("Nrdegs", "ALEC"), c("min\\.conc\\.deg", "ALEC"), c("ALOEC", "ALEC"), 
               c("Nrdegs", "min\\.conc\\.deg", "ALEC"), c("Nrdegs", "ALOEC", "ALEC"))

for(i in seq(along = groups)){
  dat <- classifier.data[, c("Hepatotoxicity.cat", "Abbreviation", "Strat", 
                          grep("EC10|Cmax", colnames(classifier.data), value = TRUE), 
                          grep(paste0(groups[[i]], collapse = "|"), colnames(classifier.data), value = TRUE)), drop = FALSE]
  g.sub <- paste0(gsub("\\.", "_", groups[[i]], fixed = TRUE), collapse = "_")
  assign(paste0("task_", g.sub), as_task_classif(id = paste0("hepatotox_", g.sub), dat, 
                                                 target = "Hepatotoxicity.cat", 
                                                 positive = "1"))
  get(paste0("task_", g.sub))$set_col_roles("Abbreviation", roles = "name")
  get(paste0("task_", g.sub))$set_col_roles("Strat", roles = "stratum")
}

grid_5 <- benchmark_grid(
  tasks = sapply(groups, function(g) get(paste0("task_", paste0(gsub("\\.", "_", g, fixed = TRUE), collapse = "_")))),
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition 
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_combs_alec_corr", seed = 0834)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_nrdeg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_nrdeg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_alec_corrected_20230302.RData")
rm(bmr_cv_nrdeg_alec_corrected)
bmr_cv_deg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_deg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_alec_corrected_20230302.RData")
rm(bmr_cv_deg_alec_corrected)
bmr_cv_aloec_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 181:270)
save(bmr_cv_aloec_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_alec_corrected_20230302.RData")
rm(bmr_cv_aloec_alec_corrected)
bmr_cv_nrdeg_deg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 271:360)
save(bmr_cv_nrdeg_deg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_deg_alec_corrected_20230302.RData")
rm(bmr_cv_nrdeg_deg_alec_corrected)
bmr_cv_nrdeg_aloec_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 361:450)
save(bmr_cv_nrdeg_aloec_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_alec_corrected_20230302.RData")
rm(bmr_cv_nrdeg_aloec_alec_corrected)

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_combs_alec_corr_2", seed = 8043)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_nrdeg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_nrdeg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_alec_corrected_20230302_1.RData")
rm(bmr_cv_nrdeg_alec_corrected)
bmr_cv_deg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_deg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_alec_corrected_20230302_1.RData")
rm(bmr_cv_deg_alec_corrected)
bmr_cv_aloec_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 181:270)
save(bmr_cv_aloec_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_alec_corrected_20230302_1.RData")
rm(bmr_cv_aloec_alec_corrected)
bmr_cv_nrdeg_deg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 271:360)
save(bmr_cv_nrdeg_deg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_deg_alec_corrected_20230302_1.RData")
rm(bmr_cv_nrdeg_deg_alec_corrected)
bmr_cv_nrdeg_aloec_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 361:450)
save(bmr_cv_nrdeg_aloec_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nedeg_aloec_alec_corrected_20230302_1.RData")
rm(bmr_cv_nrdeg_aloec_alec_corrected)

# 3rd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_combs_alec_corr_3", seed = 4380)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_nrdeg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 1:90)
save(bmr_cv_nrdeg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_alec_corrected_20230302_2.RData")
rm(bmr_cv_nrdeg_alec_corrected)
bmr_cv_deg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 91:180)
save(bmr_cv_deg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_alec_corrected_20230302_2.RData")
rm(bmr_cv_deg_alec_corrected)
bmr_cv_aloec_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 181:270)
save(bmr_cv_aloec_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_alec_corrected_20230302_2.RData")
rm(bmr_cv_aloec_alec_corrected)
bmr_cv_nrdeg_deg_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 271:360)
save(bmr_cv_nrdeg_deg_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_deg_alec_corrected_20230302_2.RData")
rm(bmr_cv_nrdeg_deg_alec_corrected)
bmr_cv_nrdeg_aloec_alec_corrected <- reduceResultsBatchmark(reg = reg, ids = 361:450)
save(bmr_cv_nrdeg_aloec_alec_corrected, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_alec_corrected_20230302_2.RData")
rm(bmr_cv_nrdeg_aloec_alec_corrected)


################################################################################
##                 NrDEG+DEG+ALOEC+ALEC (Gene Expression)                     ##
##                                                                            ##
################################################################################
groups <- c("Nrdegs", "^min\\.conc\\.deg", "ALOEC", "ALEC")
dat <- classifier.data[, c("Hepatotoxicity.cat", "Abbreviation", "Strat", 
                        grep("Cmax", colnames(classifier.data), value = TRUE), 
                        grep(paste0(groups, collapse = "|"), colnames(classifier.data), value = TRUE)), drop = FALSE]

task_gene <- as_task_classif(id = "hepatotox_gene", dat, 
                             target = "Hepatotoxicity.cat", 
                             positive = "1")
task_gene$set_col_roles("Abbreviation", roles = "name")
task_gene$set_col_roles("Strat", roles = "stratum")

grid_5 <- benchmark_grid(
  tasks = task_gene,
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_gene_corr", seed = 0603)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_gene_expression <- reduceResultsBatchmark()
save(bmr_cv_gene_expression, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_gene_expression_corrected_20230302.RData")
rm(bmr_cv_gene_expression)

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_gene_corr_2", seed = 3060)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_gene_expression <- reduceResultsBatchmark()
save(bmr_cv_gene_expression, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_gene_expression_corrected_20230302_1.RData")
rm(bmr_cv_gene_expression)

# 3rd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_gene_corr_3", seed = 6300)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_gene_expression <- reduceResultsBatchmark()
save(bmr_cv_gene_expression, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_gene_expression_corrected_20230302_2.RData")
rm(bmr_cv_gene_expression)

################################################################################
##                             DEG+ALOEC+ALEC                                 ##
##                                                                            ##
################################################################################
dat <- classifier.data[, c("Hepatotoxicity.cat", "Abbreviation", "Strat", 
                        grep("Cmax|EC10", colnames(classifier.data), value = TRUE), 
                        grep(paste0(groups, collapse = "|"), colnames(classifier.data), value = TRUE)), drop = FALSE]

task_deg_aloec_alec <- as_task_classif(id = "hepatotox_gene", dat, 
                                       target = "Hepatotoxicity.cat", 
                                       positive = "1")
task_deg_aloec_alec$set_col_roles("Abbreviation", roles = "name")
task_deg_aloec_alec$set_col_roles("Strat", roles = "stratum")

grid_5 <- benchmark_grid(
  tasks = task_deg_aloec_alec,
  learner = list(at.ranger, at.svm, at.xgboost, at.cv_glmnet,
                 lrn("classif.ranger"), lrn("classif.svm", predict_type = "prob"),
                 lrn("classif.xgboost"), lrn("classif.cv_glmnet"),
                 lrn("classif.featureless")),
  resampling = rsmp("cv", folds = 10)
)

# 1st repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_deg_aloec_alec_corr", seed = 0803)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_deg_aloec_alec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_deg_aloec_alec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_aloec_alec_corrected_20230302.RData")
rm(bmr_cv_deg_aloec_alec)

# 2nd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_deg_aloec_alec_corr_2", seed = 3080)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_deg_aloec_alec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_deg_aloec_alec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_aloec_alec_corrected_20230302_1.RData")
rm(bmr_cv_deg_aloec_alec)

# 3rd repetition
reg <- batchtools::makeExperimentRegistry(file.dir = "../Data/Registry10_deg_aloec_alec_corr_3", seed = 8300)
batchmark(grid_5, store_models = TRUE)
tab <- unwrap(getJobPars(reg = reg))
runtimes <- c(classif.xgboost.tuned = 17.5 * 60 * 60,  classif.cv_glmnet.tuned = 45 * 60, 
              classif.ranger.tuned = 60 * 60, classif.svm.tuned = 30 * 60, untuned = 10 * 60)
tab$runtime <- unname(runtimes[match(tab$learner_id, names(runtimes), nomatch = 5)])
tab$chunk <- binpack(tab$runtime, 20 * 60 * 60)
tab
batchtools::submitJobs(tab, reg = reg)
waitForJobs(reg = reg)
bmr_cv_deg_aloec_alec <- reduceResultsBatchmark(reg = reg)
save(bmr_cv_deg_aloec_alec, file = "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_aloec_alec_corrected_20230302_2.RData")
rm(bmr_cv_deg_aloec_alec)

################################################################################
# sessionInfo()
# R version 4.1.2 (2021-11-01)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: CentOS Linux 7 (Core)
# 
# Matrix products: default
# BLAS/LAPACK: /cluster/sfw/openblas/xeon_e5_4620_v4/0.3.9/int32/threaded/lib64/libopenblas_haswellp-r0.3.9.so
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C
# [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8
# [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8
# [7] LC_PAPER=en_US.UTF-8       LC_NAME=C
# [9] LC_ADDRESS=C               LC_TELEPHONE=C
# [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base
# 
# other attached packages:
#   [1] BBmisc_1.12         batchtools_0.9.15   mlr3verse_0.2.4
# [4] mlr3_0.13.3         mlr3batchmark_0.1.0
# 
# loaded via a namespace (and not attached):
#   [1] Rcpp_1.0.8.3           paradox_0.9.0          lattice_0.20-45
# [4] listenv_0.8.0          prettyunits_1.1.1      palmerpenguins_0.1.0
# [7] digest_0.6.29          utf8_1.2.2             parallelly_1.31.1
# [10] R6_2.5.1               mlr3measures_0.4.1     backports_1.4.1
# [13] ggplot2_3.3.5          pillar_1.7.0           rlang_1.0.2
# [16] progress_1.2.2         mlr3fselect_0.7.0      uuid_1.1-0
# [19] data.table_1.14.2      distr6_1.6.9           Matrix_1.3-4
# [22] checkmate_2.1.0        splines_4.1.2          ooplah_0.2.0
# [25] mlr3pipelines_0.4.0    munsell_0.5.0          compiler_4.1.2
# [28] set6_0.2.4             pkgconfig_2.0.3        globals_0.14.0
# [31] mlr3tuning_0.13.0      tibble_3.1.6           mlr3data_0.6.0
# [34] lgr_0.4.3              mlr3cluster_0.1.3      mlr3misc_0.10.0
# [37] mlr3tuningspaces_0.2.0 codetools_0.2-18       clusterCrit_1.2.8
# [40] fansi_1.0.3            future_1.25.0          crayon_1.5.1
# [43] withr_2.5.0            rappdirs_0.3.3         grid_4.1.2
# [46] gtable_0.3.0           lifecycle_1.0.1        magrittr_2.0.3
# [49] scales_1.2.0           mlr3learners_0.5.2     mlr3proba_0.4.9
# [52] cli_3.3.0              stringi_1.7.6          mlr3viz_0.5.8
# [55] mlr3filters_0.5.0      dictionar6_0.1.3       ellipsis_0.3.2
# [58] brew_1.0-7             bbotk_0.5.2            vctrs_0.4.1
# [61] tools_4.1.2            glue_1.6.2             hms_1.1.1
# [64] parallel_4.1.2         survival_3.2-13        clue_0.3-60
# [67] colorspace_2.0-3       cluster_2.1.2          base64url_1.4
# [70] param6_0.2.4
