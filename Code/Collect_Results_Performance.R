library(mlr3verse)
perf <- data.frame(Learner = character(51), Group = character(51), 
                  Repetition = numeric(51), Accuracy = numeric(51), 
                  Sensitivity = numeric(51), Specificity = numeric(51), 
                  F1 = numeric(51))
file.list <- c("../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_20220509.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_20220607.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_alec_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_alec_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_20220516.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdegs_20220516.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_alec_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_20220516_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_conc_deg_20220516_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdegs_20220516_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_20220613.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_20220613.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_20220516_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_conc_deg_20220516_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdegs_20220516_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_aloec_alec_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_alec_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_aloec_alec_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_deg_alec_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_aloec_alec_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nedeg_aloec_alec_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_alec_20220615_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_alec_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_deg_alec_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_deg_alec_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_alec_20220615_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_wo_alec_20220615.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_alec_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_alec_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_min_conc_deg_20220621.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_alec_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_alec_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_min_conc_deg_20220621_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_alec_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_alec_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_alec_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_alec_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_min_conc_deg_20220621_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_min_conc_deg_20220621_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_deg_alec_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_min_conc_deg_20220621_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_aloec_min_conc_deg_20220621.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_gene_expression_corrected_20230302_2.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_gene_expression_corrected_20230302_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_gene_expression_corrected_20230302.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_20230220.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_20230220_1.RData",
               "../Data/Benchmarks/benchmark_CTB_Vivo_all_cv10_nrdeg_aloec_20230220_2.RData")
rep <- c(1, 2, 1, 2, 1, 2, 1, 1, 3, 2, 3, 2, 2, 1, 3, 3, 3, 3, 3, 3, 2, 3, 1, 2, 
         3, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 2, 3, 1, 2, 3, 2, 3, 3, 3, 1, 3, 2, 1, 
         1, 2, 3)
group <- c("All", "All", "Cytotox", "Cytotox", "ALEC", "ALEC", "ALOEC", "NrDEG", 
           "ALEC", "ALOEC", "All", "DEG", "NrDEG", "DEG", "Cytotox", "ALOEC", "DEG", 
           "NrDEG", "DEG+ALOEC+ALEC", "NrDEG+ALOEC+ALEC", "DEG+ALOEC+ALEC", 
           "NrDEG+DEG+ALEC", "DEG+ALOEC+ALEC", "NrDEG+ALOEC+ALEC", "NrDEG+DEG+ALOEC", 
           "NrDEG+ALOEC+ALEC", "NrDEG+DEG+ALEC", "NrDEG+DEG+ALEC", "NrDEG+DEG+ALOEC", 
           "NrDEG+DEG+ALOEC", "ALEC+ALOEC", "ALEC+NrDEG", "NrDEG+DEG", "ALEC+ALOEC", 
           "DEG+ALEC", "NrDEG+DEG", "ALEC+NrDEG", "ALEC+NrDEG", "ALEC+DEG", 
           "ALEC+ALOEC", "ALOEC+DEG", "NrDEG+DEG", "ALEC+DEG", "ALOEC+DEG", 
           "ALOEC+DEG", "Gene Expression", "Gene Expression", "Gene Expression", 
           "NrDEG+ALOEC", "NrDEG+ALOEC", "NrDEG+ALOEC")

for(i in seq(along = file.list)) {
  n <- load(file = file.list[i])
  force(n)
  aggr <- get(n)$aggregate(msrs(c("classif.acc", "classif.sensitivity", 
                                "classif.specificity", "classif.fbeta")))
  rows <- (1:9) + (i - 1) * 9
  perf[rows, "Learner"] <- aggr$learner_id
  perf[rows, "Group"] <- group[i]
  perf[rows, "Repetition"] <- rep[i]
  perf[rows, "Accuracy"] <- aggr$classif.acc
  perf[rows, "Sensitivity"] <- aggr$classif.sensitivity
  perf[rows, "Specificity"] <- aggr$classif.specificity
  perf[rows, "F1"] <- aggr$classif.fbeta
  rm(list = n)
}

save(perf, file = "../Data/Classif-Biospyder-Performances_20230810.RData")