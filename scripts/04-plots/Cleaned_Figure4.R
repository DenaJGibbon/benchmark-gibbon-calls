# Load required libraries
library(tidyr)
library(dplyr)
library(ggpubr)
library(cowplot)
library(stringr)

# BirdNET Binary Model Evaluation ----------------------------

# Load BirdNET binary performance data
CombinedF1dataBirdNET <- read.csv('data/CombinedF1dataBirdNET_automateddetect_binary.csv')

# Plot AUC for BirdNET binary model
AUCPlotBirdNETBin <- ggpubr::ggerrorplot(data = CombinedF1dataBirdNET, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

# Plot F1 score for BirdNET binary model across thresholds
F1Plot <- ggpubr::ggerrorplot(data = CombinedF1dataBirdNET, x = 'Thresholds', y = 'F1', facet.by = 'samples') +
  geom_hline(yintercept = 0.8, linetype = 'dashed', color = 'red') +
  xlab('Confidence') + ylim(0, 1)

# Plot Precision vs Recall for BirdNET binary model
PrecRec <- ggpubr::ggerrorplot(data = CombinedF1dataBirdNET, x = 'Precision', y = 'Recall', facet.by = 'samples')

# Display the plots
AUCPlotBirdNETBin
F1Plot
PrecRec

# Summary statistics for BirdNET binary model
CombinedF1dataBirdNET[which.max(CombinedF1dataBirdNET$F1), ]  # Best F1 score
subset(CombinedF1dataBirdNET, F1 > 0.65)  # Filter for F1 > 0.65

# Prepare PerformanceFolder data for plotting
CombinedF1dataBirdNET$PerformanceFolder <- as.factor(str_split_fixed(CombinedF1dataBirdNET$PerformanceFolder, pattern = '_', n = 2)[, 1])

# Max F1 per Performance Folder
MaxF1PlotBirdNET <- CombinedF1dataBirdNET %>%
  group_by(PerformanceFolder, samples) %>%
  summarise(F1 = max(F1, na.rm = TRUE))

# Plot maximum F1 scores for BirdNET binary model
MaxF1PlotBirdNET$samples <- as.factor(MaxF1PlotBirdNET$samples)
BirdNET <- ggpubr::ggline(data = MaxF1PlotBirdNET, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('BirdNET Binary\nMax F1:', max(MaxF1PlotBirdNET$F1))) +
  xlab('') + ylim(0, 1)

# BirdNET Multi-Class Model Evaluation ------------------------

# Load BirdNET multi-class performance data
CombinedF1dataBirdNETmulti <- read.csv('data/CombinedF1dataBirdNET_automateddetect_multi.csv')

# Plot AUC for BirdNET multi-class model
AUCPlotBirdNETMulti <- ggpubr::ggerrorplot(CombinedF1dataBirdNETmulti, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

# Plot F1 score for BirdNET multi-class model across thresholds
F1Plot <- ggpubr::ggerrorplot(CombinedF1dataBirdNETmulti, x = 'Thresholds', y = 'F1', facet.by = 'samples') +
  xlab('Confidence') + ylim(0, 1)

# Plot Precision vs Recall for BirdNET multi-class model
PrecRec <- ggpubr::ggerrorplot(CombinedF1dataBirdNETmulti, x = 'Precision', y = 'Recall', facet.by = 'samples')

# Display the plots
AUCPlotBirdNETMulti
F1Plot + geom_hline(yintercept = 0.8, color = 'red', linetype = 'dashed')
PrecRec

# Additional summary plot for AUC
ggpubr::ggboxplot(CombinedF1dataBirdNETmulti, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

# Identify best-performing configuration for BirdNET multi-class
CombinedF1dataBirdNETmulti[which.max(CombinedF1dataBirdNETmulti$F1), ]

# Max F1 per sample set for BirdNET multi-class
MaxF1PlotBirdNETmulti <- CombinedF1dataBirdNETmulti %>%
  group_by(PerformanceFolder) %>%
  summarise(F1 = max(F1, na.rm = TRUE))

# Prepare and plot BirdNET multi-class max F1 scores
MaxF1PlotBirdNETmulti$samples <- as.numeric(str_split_fixed(MaxF1PlotBirdNETmulti$PerformanceFolder, pattern = 'samples', n = 2)[, 1])
BirdNETmulti <- ggpubr::ggline(MaxF1PlotBirdNETmulti, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('BirdNET multi \n max F1:', max(MaxF1PlotBirdNETmulti$F1))) +
  ylim(0, 1) + xlab('')

# CNN Binary Model Evaluation -----------------------------------

# Load CNN binary performance data
CombinedF1data <- read.csv('data/CombinedF1dataCNNbinary.csv')

# Plot AUC for CNN binary model
AUCPlotCNNBinary <- ggerrorplot(CombinedF1data, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

# Plot F1 score for CNN binary model across thresholds
F1Plot <- ggerrorplot(CombinedF1data, x = 'Thresholds', y = 'F1', facet.by = 'samples') +
  ylim(0, 1) + xlab('Probability')

# Plot Precision vs Recall for CNN binary model
PrecRec <- ggerrorplot(CombinedF1data, x = 'Precision', y = 'Recall', facet.by = 'samples')

# Display the AUC plot for CNN binary model
AUCPlotCNNBinary

# Best threshold performance summary for CNN binary model
CombinedF1data[which.max(CombinedF1data$F1),]

# Max F1 per Performance Folder for CNN binary model
MaxF1PlotCNN <- CombinedF1data %>%
  group_by(PerformanceFolder) %>%
  summarise(F1 = max(F1, na.rm = TRUE))

# Prepare and plot CNN binary model F1 scores
MaxF1PlotCNN$samples <- str_split_fixed(MaxF1PlotCNN$PerformanceFolder, 'samples', 2)[, 1]
MaxF1PlotCNN$samples <- as.factor(str_split_fixed(MaxF1PlotCNN$samples, '_', 2)[, 2])
CNN <- ggline(CombinedF1data, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('CNN Binary \n max F1:', max(CombinedF1data$F1))) +
  ylim(0, 1) + xlab('')

# CNN Multi-Class Model Evaluation --------------------------------

# Load CNN multi-class performance data
CombinedF1dataMulti <- read.csv('data/CombinedF1dataMulti.csv')

# Plot AUC for CNN multi-class model
AUCPlotCNNMulti <- ggerrorplot(data = CombinedF1dataMulti, x = "samples", y = "auc") +
  xlab('') + ylab('AUC') + ylim(0, 1) + ggtitle("CNN Multi AUC")

# Plot F1 score for CNN multi-class model across thresholds
F1Plot <- ggerrorplot(data = CombinedF1dataMulti, x = "Thresholds", y = "F1", facet.by = "samples") +
  ylim(0, 1) + xlab("Probability") + geom_hline(yintercept = 0.8, color = "red", linetype = "dashed")

# Plot Precision vs Recall for CNN multi-class model
PrecRec <- ggerrorplot(data = CombinedF1dataMulti, x = "Precision", y = "Recall", facet.by = "samples")

# Max F1 summary plot for CNN multi-class model
MaxF1PlotCNN <- CombinedF1dataMulti %>%
  group_by(PerformanceFolder) %>%
  summarise(F1 = max(F1, na.rm = TRUE)) %>%
  mutate(samples = as.numeric(str_split_fixed(str_split_fixed(PerformanceFolder, "samples", 2)[, 1], "_", 2)[, 2]))

# Plot CNN multi-class max F1 scores
CNNmulti <- ggline(MaxF1PlotCNN, x = "samples", y = "F1", add = "mean_se") +
  ggtitle(paste("CNN multi \n max F1:", max(MaxF1PlotCNN$F1))) + ylim(0, 1) + xlab("")

# Final combined plot for BirdNET and CNN models
CombinedPlot <- cowplot::plot_grid(BirdNET, BirdNETmulti, CNN, CNNmulti) + xlab("Number of training samples")
CombinedPlot <- ggdraw(add_sub(CombinedPlot, "Number of training samples", y = 6, x = 0.5, vjust = 4.5))

# AUC Plot for combined models
CombinedPlotAUC <- cowplot::plot_grid(AUCPlotBirdNETBin, AUCPlotBirdNETMulti, AUCPlotCNNBinary, AUCPlotCNNMulti)
CombinedPlotAUC <- ggdraw(add_sub(CombinedPlotAUC, "Number of training samples", y = 6, x = 0.5, vjust = 4.5))

# Save combined plots to a PDF
pdf("F1andAUC.pdf", height = 14, width = 12)
cowplot::plot_grid(CombinedPlot, CombinedPlotAUC, labels = c("A)", "B)"), label_x = 0.9, nrow = 2)
graphics.off()

# Combine all dataframes for final summary -------------------------
CombinedF1dataBirdNETmulti$model <- 'BirdNET multi'
CombinedF1dataBirdNET$model <- 'BirdNET binary'
CombinedF1data$model <- 'ResNet50 binary'
CombinedF1dataMulti$model <- 'ResNet50 multi'

# Combine all datasets into one
CombinedRandomizationDF <- rbind.data.frame(CombinedF1dataBirdNETmulti,
                                            CombinedF1dataBirdNET, CombinedF1data,
                                            CombinedF1dataMulti)

# Save the combined dataframe to CSV
write.csv(CombinedRandomizationDF, 'data/CombinedRandomizationAutomatedDetection.csv', row.names = FALSE)

# Process the combined data and generate plots ----------------------
CombinedRandomizationDF <- read.csv('data/CombinedRandomizationAutomatedDetection.csv')
CombinedRandomizationDF$AUC <- CombinedRandomizationDF$auc
CombinedRandomizationDFSub <- CombinedRandomizationDF[, c('model', 'F1', 'AUC', 'samples')]

# Max F1 score per model and sample
MaxCombinedRandomizationDF <- CombinedRandomizationDFSub %>%
  dplyr::group_by(model, samples, AUC) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE))

# Reshape data for long format
CombinedRandomizationDFlong <- MaxCombinedRandomizationDF %>%
  select(model, F1, AUC, samples) %>%
  pivot_longer(cols = c(F1, AUC), names_to = "metric", values_to = "value")

# Ensure the value column is numeric
CombinedRandomizationDFlong$value <- as.numeric(CombinedRandomizationDFlong$value)
CombinedRandomizationDFlong$samples <- as.numeric(CombinedRandomizationDFlong$samples)

# Plot F1 and AUC metrics for different models
ggpubr::ggline(data = CombinedRandomizationDFlong,
               x = 'samples', y = 'value', color = 'model', add = "mean_se", facet.by = 'metric') +
  ylim(0, 1) + xlab('Number of samples') + ylab('Metric') +
  scale_color_manual(values = c("#0080FF", "#00FFFF", "#80FF80", "#FF8000"))
