load("../Data/dataset_classifier_paper.RData")
library(mlr3verse)
library(extrafont)
library(ggplot2)
library(ggrepel)
# font_import()    
# fonts()

# Calculate mean performances: 
load("../Data/Classif-Biospyder-Performances_20230810.RData")
perf$Group[perf$Group == "ALEC+DEG"] <- "DEG+ALEC"
perf$Group[perf$Group == "ALEC+NrDEG"] <- "NrDEG+ALEC"
perf$Group[perf$Group == "ALOEC+DEG"] <- "DEG+ALOEC"
perf$Group[perf$Group == "ALEC+ALOEC"] <- "ALOEC+ALEC"
table(perf$Group)
all(table(perf$Group) == 27)
mean.perf <- aggregate(Accuracy ~ Learner + Group, data = perf, FUN = mean, na.rm = TRUE)
mean.sens <- aggregate(Sensitivity ~ Learner + Group, data = perf, FUN = mean, na.rm = TRUE)
mean.spec <- aggregate(Specificity ~ Learner + Group, data = perf, FUN = mean, na.rm = TRUE)
mean.F1 <- aggregate(F1 ~ Learner + Group, data = perf, FUN = mean, na.rm = TRUE)
mean.perf <- merge(mean.perf, mean.sens)
mean.perf <- merge(mean.perf, mean.spec)
mean.perf <- merge(mean.perf, mean.F1)

################################################################################
# Plots and Tables for Paper: 
acc <- perf
mean.acc <- mean.perf

# Figure 2: 
par(family="Arial")
acc7 <- acc[acc$Learner == "classif.ranger.tuned"  & 
              (!acc$Group  %in% c("All-EC10-Ratio", "Gene Expression")) & 
              !grepl("old", acc$Group), ]
acc7$Group[acc7$Group == "Cytotox"] <- "None"
acc7$Group <- factor(acc7$Group, levels = c("None", "NrDEG", "DEG", "ALOEC", 
                                            "ALEC", "NrDEG+DEG", "NrDEG+ALOEC",
                                            "NrDEG+ALEC", "DEG+ALOEC", 
                                            "DEG+ALEC", "ALOEC+ALEC", 
                                            "NrDEG+DEG+ALOEC", "NrDEG+DEG+ALEC",
                                            "NrDEG+ALOEC+ALEC", 
                                            "DEG+ALOEC+ALEC", "All"))
mean.acc7 <- mean.acc[mean.acc$Learner == "classif.ranger.tuned" & 
                        (!mean.acc$Group  %in% c("All-EC10-Ratio", "Gene Expression")) & 
                        !grepl("old", mean.acc$Group), ]
mean.acc7$Group[mean.acc7$Group == "Cytotox"] <- "None"
mean.acc7$Group <- factor(mean.acc7$Group, levels = c("None", "NrDEG", "DEG", "ALOEC", 
                                                      "ALEC", "NrDEG+DEG", "NrDEG+ALOEC",
                                                      "NrDEG+ALEC", "DEG+ALOEC", 
                                                      "DEG+ALEC", "ALOEC+ALEC", 
                                                      "NrDEG+DEG+ALOEC", "NrDEG+DEG+ALEC",
                                                      "NrDEG+ALOEC+ALEC", 
                                                      "DEG+ALOEC+ALEC", "All"))
mean.acc7$Observation <- "Mean"
acc7 <- acc7[, -3]
acc7$Observation <- as.character(acc7$Group)
acc7$Observation <- "Individual run"
acc7 <- rbind(acc7, mean.acc7)
acc7$Learner <- gsub("\\.", " ", gsub("classif\\.|cv_", "", acc7$Learner))
colnames(acc7)[2] <- "Features"
fig1 <- ggplot(acc7, aes(Features, Accuracy, colour = Observation)) + geom_point(size = 1.2) + 
  scale_color_manual(values = c("Individual run" = "darkgrey",  "Mean" = "black"), 
                     name = "Accuracy") + theme_bw() + 
  theme(text = element_text(size = 10), legend.position="top", 
    axis.text.x=element_blank(),axis.ticks.x=element_blank(),
    axis.title.x=element_blank()) 

grid.data <- data.frame(Accuracy = rep(1:5, 16), 
                        pch = as.factor(c("Not Included", "Not Included", "Not Included", "Not Included", "Included", 
                                          "Not Included", "Not Included", "Not Included", "Included", "Included", 
                                          "Not Included", "Not Included", "Included", "Not Included", "Included", 
                                          "Not Included", "Included", "Not Included", "Not Included", "Included", 
                                          "Included", "Not Included", "Not Included", "Not Included", "Included", 
                                          "Not Included", "Not Included", "Included", "Included", "Included", 
                                          "Not Included", "Included", "Not Included", "Included", "Included", 
                                          "Included", "Not Included", "Not Included", "Included", "Included", 
                                          "Not Included", "Included", "Included", "Not Included", "Included", 
                                          "Included", "Not Included", "Included", "Not Included", "Included", 
                                          "Included", "Included", "Not Included", "Not Included", "Included",
                                          "Not Included", "Included", "Included", "Included", "Included",
                                          "Included", "Not Included", "Included", "Included", "Included", 
                                          "Included", "Included", "Not Included", "Included", "Included",
                                          "Included", "Included", "Included", "Not Included", "Included",
                                          "Included", "Included", "Included", "Included", "Included")), 
                        Variables = factor(rep(levels(acc7$Features), each = 5), levels = levels(acc7$Features)))
fig2 <- ggplot(grid.data, aes(Variables, Accuracy, shape = pch)) + geom_point(size = 1.5) +
  scale_shape_manual(values = c("Not Included" = 1,  "Included" = 3),
                     name = "Fetures included") +
  scale_y_continuous(breaks = 1:5, labels = rev(c("Cytotox", "NrDEGs", "DEG", "ALOEC", "ALEC")),
                     minor_breaks = 1:5, limits = c(0.75, 5.25)) + theme_bw() + theme(axis.title.y=element_blank(),
                     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                     text = element_text(size = 10), legend.position="none",
                     strip.background = element_blank(),
                     strip.placement = "outside") + xlab("Gene Expression Variables")
library(ggpubr)
# pdf("../Paper/fig/Classif-Biospyder-Accuracies_2023_02_20_ranger-comb-cyt-gen_corrected.pdf", width = 3.5433070866, 
#     height = 4.5)
ggarrange(fig1, fig2, ncol = 1, nrow = 2)
# dev.off()


# Supplementary Figure B.1
acc5 <- acc
acc5$Group <- factor(acc5$Group, levels = c("Gene Expression", 
                                            "Cytotox", "NrDEG", "DEG", "ALOEC", 
                                            "ALEC", "NrDEG+DEG", "NrDEG+ALOEC", 
                                            "NrDEG+ALEC",  "DEG+ALOEC",
                                            "DEG+ALEC", "ALOEC+ALEC", 
                                            "NrDEG+DEG+ALOEC", "NrDEG+DEG+ALEC",
                                            "NrDEG+ALOEC+ALEC",
                                            "DEG+ALOEC+ALEC", "All"))
mean.acc5 <- mean.acc
mean.acc5$Group <- factor(mean.acc5$Group, levels = c("Gene Expression", 
                                                      "Cytotox", "NrDEG", "DEG", "ALOEC", 
                                                      "ALEC", "NrDEG+DEG", "NrDEG+ALOEC", 
                                                      "NrDEG+ALEC",  "DEG+ALOEC",
                                                      "DEG+ALEC", "ALOEC+ALEC", 
                                                      "NrDEG+DEG+ALOEC", "NrDEG+DEG+ALEC",
                                                      "NrDEG+ALOEC+ALEC",
                                                      "DEG+ALOEC+ALEC", "All"))
mean.acc5$Observation <- "Mean"
acc5 <- acc5[, -3]
acc5$Observation <- as.character(acc5$Group)
acc5$Observation <- "Individual run"
acc5 <- rbind(acc5, mean.acc5)
acc5$Learner <- gsub("\\.", " ", gsub("classif\\.|cv_", "", acc5$Learner))


# pdf(file = "../Paper/fig/Classif-Biospyder-Accuracies_2022_12_19_corrected.pdf", height = 6, width = 7.0866141732)
par(family="Arial")
ggplot(acc5, aes(Group, Accuracy, colour = Observation)) + geom_point() + 
  scale_color_manual(values = c("Individual run" = "darkgrey",
                                "Mean" = "black"), 
                     name = "Accuracy") + 
  facet_wrap(.~factor(Learner, levels = tab.mean$Learner[order(-tab.mean$Accuracy_All)], 
                      labels = gsub("\\.", " ", gsub("classif\\.|cv_", "", tab.mean$Learner[order(-tab.mean$Accuracy_All)])))) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     text = element_text(size = 10), legend.position="top") 
# dev.off()

# Figure 1
acc9 <- acc
acc9 <- acc9[acc9$Group %in%  c("Gene Expression", "Cytotox", "All"), ]
acc9$Group <- factor(acc9$Group, levels = c( "Cytotox", "Gene Expression", "All"))
mean.acc9 <- mean.acc
mean.acc9 <- mean.acc9[mean.acc9$Group %in%  c("Gene Expression", "Cytotox", "All"), ]
mean.acc9$Group <- factor(mean.acc9$Group, levels = c("Cytotox", "Gene Expression", "All"))
mean.acc9$Observation <- "Mean"
acc9 <- acc9[, -3]
acc9$Observation <- as.character(acc9$Group)
acc9$Observation <- "Individual run"
acc9 <- rbind(acc9, mean.acc9)
acc9$Learner <- gsub("\\.", " ", gsub("classif\\.|cv_", "", acc9$Learner))
acc9$Learner <- factor(acc9$Learner, levels = c("featureless", "glmnet", "glmnet tuned", 
                                                "svm", "svm tuned", "xgboost", "xgboost tuned", 
                                                "ranger", "ranger tuned"))

# pdf(file = "../Paper/fig/Classif-Biospyder-Accuracies_Cytotox_Gene_All_20230413.pdf", 
#     height = 5, width = 7.0866141732)
par(family="Arial")
ggplot(acc9, aes(Learner, Accuracy, colour = Observation)) + geom_point() + 
  scale_color_manual(values = c("Individual run" = "darkgrey",
                                "Mean" = "black"),
                     name = "Accuracy") + 
  facet_wrap(.~Group) +
  theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                     text = element_text(size = 12), legend.position="top") 
# dev.off()



# Table 2 and Supplementary Table B.4
table.accs <- mean.acc6[order(mean.acc6$Accuracy, decreasing = TRUE), 1:5]
table.accs$Learner <- gsub("\\.", " ", gsub("classif\\.|cv_", "", table.accs$Learner))
colnames(table.accs)[3:5] <- paste0("Mean ", colnames(table.accs)[3:5])
rownames(table.accs) <- NULL
names <- c("Cytotox", "NrDEG", "DEG", "ALOEC", "ALEC")
table.accs.pm <- data.frame(Learner = table.accs$Learner, 
                            Cytotox = ifelse(table.accs$Group == "Gene Expression", "o", "+"), 
                            NrDEG = ifelse(grepl("NrDEG|All|Gene Expression", table.accs$Group), "+", "o"), 
                            DEG = ifelse(grepl("^DEG|\\+DEG|All|Gene Expression", table.accs$Group), "+", "o"), 
                            ALOEC = ifelse(grepl("ALOEC|All|Gene Expression", table.accs$Group), "+", "o"), 
                            ALEC = ifelse(grepl("ALEC|All|Gene Expression", table.accs$Group), "+", "o"), 
                            `Mean Accuracy` = table.accs$`Mean Accuracy`, 
                            `Mean Sensitivity` = table.accs$`Mean Sensitivity`, 
                            `Mean Specificity` = table.accs$`Mean Specificity`)
library(xtable)
print(xtable(table.accs.pm[1:20, ], label = "tab:top20Acc", digits = 3, 
             caption = "Best 20 combinations of variable groups and classification method ordered by mean accuracy over three runs of cross validation. A plus indicates inclusion of the corresponding variable group, a circle exclusion."), 
      booktabs = TRUE)
write.xlsx(table.accs.pm, file = "../Ergebnisse/Mean_Accuracies_corrected_pm.xlsx")
print(xtable(table.accs.pm, label = "tab:top20Acc", digits = 3, 
             caption = "All combinations of variable groups and classification method ordered by mean accuracy over three runs of cross validation. A plus indicates inclusion of the corresponding variable group, a circle exclusion."), 
      booktabs = TRUE)
