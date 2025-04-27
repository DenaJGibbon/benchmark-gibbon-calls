library(tidyr)
library(dplyr)
library(ggpubr)

# BirdNET binary ----------------------------------------------------------

CombinedF1dataBirdNET <- read.csv('data/CombinedF1dataBirdNET_automateddetect_binary.csv')

AUCPlotBirdNETBin <- ggpubr::ggerrorplot(data = CombinedF1dataBirdNET, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

F1Plot <- ggpubr::ggerrorplot(data = CombinedF1dataBirdNET, x = 'Thresholds', y = 'F1', facet.by = 'samples') +
  geom_hline(yintercept = 0.8, linetype = 'dashed', color = 'red') +
  xlab('Confidence') + ylim(0, 1)

PrecRec <- ggpubr::ggerrorplot(data = CombinedF1dataBirdNET, x = 'Precision', y = 'Recall', facet.by = 'samples')

# Display plots
AUCPlotBirdNETBin
F1Plot
PrecRec

# ------------------------- Summary -------------------------
CombinedF1dataBirdNET[which.max(CombinedF1dataBirdNET$F1), ]
subset(CombinedF1dataBirdNET, F1 > 0.65)

CombinedF1dataBirdNET$PerformanceFolder <-
  as.factor(str_split_fixed(CombinedF1dataBirdNET$PerformanceFolder,
                            pattern = '_',n=2)[,1])

MaxF1PlotBirdNET <- CombinedF1dataBirdNET %>%
  group_by(PerformanceFolder,samples) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE))

MaxF1PlotBirdNET$samples <- as.factor(MaxF1PlotBirdNET$samples)

BirdNET <- ggpubr::ggline(data = MaxF1PlotBirdNET, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('BirdNET Binary\nMax F1:', max(MaxF1PlotBirdNET$F1))) +
  xlab('') + ylim(0, 1)

BirdNET

# BirdNET Multi--------------------------------------------------------------
CombinedF1dataBirdNETmulti <- read.csv('data/CombinedF1dataBirdNET_automateddetect_multi.csv')

AUCPlotBirdNETMulti <- ggpubr::ggerrorplot(CombinedF1dataBirdNETmulti, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

F1Plot <- ggpubr::ggerrorplot(CombinedF1dataBirdNETmulti, x = 'Thresholds', y = 'F1', facet.by = 'samples') +
  xlab('Confidence') + ylim(0, 1)

PrecRec <- ggpubr::ggerrorplot(CombinedF1dataBirdNETmulti, x = 'Precision', y = 'Recall', facet.by = 'samples')

# Optional: Save plots or combine --------------------------------------------
# pdf('BirdNETmulti_results.pdf', height = 12, width = 11)
AUCPlotBirdNETMulti
F1Plot + geom_hline(yintercept = 0.8, color = 'red', linetype = 'dashed')
PrecRec
# graphics.off()

# Additional summary plot
ggpubr::ggboxplot(CombinedF1dataBirdNETmulti, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

# Identify best-performing configuration
CombinedF1dataBirdNETmulti[which.max(CombinedF1dataBirdNETmulti$F1), ]

# Max F1 score per sample set
MaxF1PlotBirdNETmulti <- CombinedF1dataBirdNETmulti %>%
  dplyr::group_by(PerformanceFolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE))

MaxF1PlotBirdNETmulti$samples <- as.numeric(str_split_fixed(
  MaxF1PlotBirdNETmulti$PerformanceFolder, pattern = 'samples', n = 2)[, 1])

BirdNETmulti <- ggpubr::ggline(MaxF1PlotBirdNETmulti, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('BirdNET multi \n max F1:', max(MaxF1PlotBirdNETmulti$F1))) +
  ylim(0, 1) + xlab('')

BirdNETmulti

# CNN Binary -------------------------------------------------------------------
CombinedF1data <- read.csv('data/CombinedF1dataCNNbinary.csv')

AUCPlotCNNBinary <- ggerrorplot(CombinedF1data, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

F1Plot <- ggerrorplot(CombinedF1data, x = 'Thresholds', y = 'F1', facet.by = 'samples') +
  ylim(0, 1) + xlab('Probability')

PrecRec <- ggerrorplot(CombinedF1data, x = 'Precision', y = 'Recall', facet.by = 'samples')

# Display plots
AUCPlotCNNBinary

# Best threshold performance summary -----------------------------------------
CombinedF1data[which.max(CombinedF1data$F1),]

MaxF1PlotCNN <- CombinedF1data %>%
  group_by(PerformanceFolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE))

MaxF1PlotCNN$samples <- str_split_fixed(MaxF1PlotCNN$PerformanceFolder, 'samples', 2)[, 1]
MaxF1PlotCNN$samples <- as.factor(str_split_fixed(MaxF1PlotCNN$samples, '_', 2)[, 2])

CNN <- ggline(CombinedF1data, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('CNN Binary \n max F1:', max(CombinedF1data$F1))) +
  ylim(0, 1) + xlab('')

CNN

# Combined plot example (BirdNET assumed to exist) ---------------------------
CombinedPlot <- cowplot::plot_grid(BirdNET, CNN) + xlab('Number of training samples')
ggdraw(add_sub(CombinedPlot, "Number of training samples", vpadding = grid::unit(0, "lines"), y = 6, x = 0.5, vjust = 4.5))

# CNN Multi ----------------------------------------------------------------
CombinedF1dataMulti <- read.csv('data/CombinedF1dataMulti.csv')
AUCPlotCNNMulti <- ggerrorplot(data = CombinedF1dataMulti, x = "samples", y = "auc") +
  xlab('') + ylab('AUC') + ylim(0, 1) + ggtitle("CNN Multi AUC")


F1Plot <- ggerrorplot(data = CombinedF1dataMulti, x = "Thresholds", y = "F1", facet.by = "samples") +
  ylim(0, 1) + xlab("Probability") + geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")

PrecRec <- ggerrorplot(data = CombinedF1dataMulti, x = "Precision", y = "Recall", facet.by = "samples")

# Max F1 summary plot
MaxF1PlotCNN <- CombinedF1dataMulti %>%
  group_by(PerformanceFolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE)) %>%
  mutate(samples = as.numeric(str_split_fixed(str_split_fixed(PerformanceFolder, "samples", 2)[, 1], "_", 2)[, 2]))

CNNmulti <- ggline(MaxF1PlotCNN, x = "samples", y = "F1", add = "mean_se") +
  ggtitle(paste("CNN multi \n max F1:", max(MaxF1PlotCNN$F1))) + ylim(0, 1) + xlab("")

# Final combined plot (with BirdNET placeholders)
CombinedPlot <- cowplot::plot_grid(BirdNET, BirdNETmulti, CNN, CNNmulti) + xlab("Number of training samples")
CombinedPlot <- ggdraw(add_sub(CombinedPlot, "Number of training samples", y = 6, x = 0.5, vjust = 4.5))

AUCPlotCNNMulti <- AUCPlotCNNMulti + ggtitle("CNN Multi")

CombinedPlotAUC <- cowplot::plot_grid(AUCPlotBirdNETBin, AUCPlotBirdNETMulti,AUCPlotCNNBinary, AUCPlotCNNMulti)
CombinedPlotAUC <- ggdraw(add_sub(CombinedPlotAUC, "Number of training samples", y = 6, x = 0.5, vjust = 4.5))
CombinedPlotAUC
pdf("F1andAUC.pdf", height = 14, width = 12)
cowplot::plot_grid(CombinedPlot, CombinedPlotAUC, labels = c("A)", "B)"), label_x = 0.9, nrow = 2)
graphics.off()



# Combine all dataframes --------------------------------------------------
CombinedF1dataBirdNETmulti$model <- 'BirdNET multi'
CombinedF1dataBirdNET$model <- 'BirdNET binary'
CombinedF1data$model <- 'ResNet50 binary'
CombinedF1dataMulti$model <- 'ResNet50 multi'

CombinedRandomizationDF <- rbind.data.frame(CombinedF1dataBirdNETmulti,
                 CombinedF1dataBirdNET,CombinedF1data,
                 CombinedF1dataMulti)

write.csv( CombinedRandomizationDF,'data/CombinedRandomizationAutomatedDetection.csv', row.names = F)

CombinedRandomizationDF <- read.csv('data/CombinedRandomizationAutomatedDetection.csv')
CombinedRandomizationDF$AUC <- CombinedRandomizationDF$auc
CombinedRandomizationDFSub <- CombinedRandomizationDF[,c('model', 'F1','AUC','samples')]


MaxCombinedRandomizationDF <- CombinedRandomizationDFSub %>%
  dplyr::group_by(model,samples,AUC) %>%
  dplyr::summarise(F1 = max(F1, na.rm=TRUE))

CombinedRandomizationDFlong<- MaxCombinedRandomizationDF %>%
  select(model,F1, AUC, samples) %>%
  pivot_longer(cols = c(F1, AUC), names_to = "metric", values_to = "value")

CombinedRandomizationDFlong$value <- as.numeric(CombinedRandomizationDFlong$value)
CombinedRandomizationDFlong$samples <- as.numeric(CombinedRandomizationDFlong$samples)


ggpubr::ggline(data=CombinedRandomizationDFlong,
               x='samples',y='value',color='model', add = "mean_se",facet.by = 'metric')+
  ylim(0,1)+xlab('Number of samples')+ylab('Metric')+
  scale_color_manual(values= c("#0080FF", "#00FFFF", "#80FF80", "#FF8000") )




