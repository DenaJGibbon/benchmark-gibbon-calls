# Load required libraries
library(tidyr)
library(dplyr)
library(ggpubr)
library(cowplot)
library(stringr)

# ------------------------- Helper Functions -------------------------
# Function to create AUC plots
create_auc_plot <- function(data, x, y, title = NULL) {
  ggerrorplot(data, x = x, y = y) +
    xlab('') + ylab('AUC') + ylim(0, 1) + ggtitle(title)
}

# Function to create F1 plots
create_f1_plot <- function(data, x, y, facet_by, title = NULL) {
  ggerrorplot(data, x = x, y = y, facet.by = facet_by) +
    geom_hline(yintercept = 0.8, linetype = 'dashed', color = 'red') +
    xlab('Confidence') + ylim(0, 1) + ggtitle(title)
}

# Function to create Precision-Recall plots
create_prec_rec_plot <- function(data, x, y, facet_by) {
  ggerrorplot(data, x = x, y = y, facet.by = facet_by)
}

# ------------------------- BirdNET Binary -------------------------
CombinedF1dataBirdNET <- read.csv('data/CombinedF1dataBirdNET_automateddetect_binary.csv') %>%
  mutate(samples = as.numeric(samples)) # Ensure 'samples' is numeric

# Create plots
AUCPlotBirdNETBin <- create_auc_plot(CombinedF1dataBirdNET, 'samples', 'auc', "BirdNET Binary AUC")
F1PlotBirdNETBin <- create_f1_plot(CombinedF1dataBirdNET, 'Thresholds', 'F1', 'samples', "BirdNET Binary F1")
PrecRecBirdNETBin <- create_prec_rec_plot(CombinedF1dataBirdNET, 'Precision', 'Recall', 'samples')

# Max F1 summary
MaxF1PlotBirdNET <- CombinedF1dataBirdNET %>%
  group_by(PerformanceFolder, samples) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE)) %>%
  mutate(samples = as.factor(samples))

BirdNET <- ggline(MaxF1PlotBirdNET, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('BirdNET Binary\nMax F1:', max(MaxF1PlotBirdNET$F1))) +
  xlab('') + ylim(0, 1)

# ------------------------- BirdNET Multi -------------------------
CombinedF1dataBirdNETmulti <- read.csv('data/CombinedF1dataBirdNET_automateddetect_multi.csv') %>%
  mutate(samples = as.numeric(samples)) # Ensure 'samples' is numeric

# Create plots
AUCPlotBirdNETMulti <- create_auc_plot(CombinedF1dataBirdNETmulti, 'samples', 'auc', "BirdNET Multi AUC")
F1PlotBirdNETMulti <- create_f1_plot(CombinedF1dataBirdNETmulti, 'Thresholds', 'F1', 'samples', "BirdNET Multi F1")
PrecRecBirdNETMulti <- create_prec_rec_plot(CombinedF1dataBirdNETmulti, 'Precision', 'Recall', 'samples')

# Max F1 summary
MaxF1PlotBirdNETmulti <- CombinedF1dataBirdNETmulti %>%
  group_by(PerformanceFolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE)) %>%
  mutate(samples = as.numeric(str_extract(PerformanceFolder, "\\d+")))

BirdNETmulti <- ggline(MaxF1PlotBirdNETmulti, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('BirdNET Multi\nMax F1:', max(MaxF1PlotBirdNETmulti$F1))) +
  xlab('') + ylim(0, 1)

# ------------------------- CNN Binary -------------------------
CombinedF1data <- read.csv('data/CombinedF1dataCNNbinary.csv') %>%
  mutate(samples = as.numeric(samples)) # Ensure 'samples' is numeric

# Create plots
AUCPlotCNNBinary <- create_auc_plot(CombinedF1data, 'samples', 'auc', "CNN Binary AUC")
F1PlotCNNBinary <- create_f1_plot(CombinedF1data, 'Thresholds', 'F1', 'samples', "CNN Binary F1")
PrecRecCNNBinary <- create_prec_rec_plot(CombinedF1data, 'Precision', 'Recall', 'samples')

# Max F1 summary
MaxF1PlotCNN <- CombinedF1data %>%
  group_by(PerformanceFolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE)) %>%
  mutate(samples = as.factor(str_extract(PerformanceFolder, "\\d+")))

CNN <- ggline(MaxF1PlotCNN, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('CNN Binary\nMax F1:', max(MaxF1PlotCNN$F1))) +
  xlab('') + ylim(0, 1)

# ------------------------- CNN Multi -------------------------
CombinedF1dataMulti <- read.csv('data/CombinedF1dataMulti.csv') %>%
  mutate(samples = as.numeric(samples)) # Ensure 'samples' is numeric

# Create plots
AUCPlotCNNMulti <- create_auc_plot(CombinedF1dataMulti, 'samples', 'auc', "CNN Multi AUC")
F1PlotCNNMulti <- create_f1_plot(CombinedF1dataMulti, 'Thresholds', 'F1', 'samples', "CNN Multi F1")
PrecRecCNNMulti <- create_prec_rec_plot(CombinedF1dataMulti, 'Precision', 'Recall', 'samples')

# Max F1 summary
MaxF1PlotCNNMulti <- CombinedF1dataMulti %>%
  group_by(PerformanceFolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE)) %>%
  mutate(samples = as.numeric(str_extract(PerformanceFolder, "\\d+")))

CNNmulti <- ggline(MaxF1PlotCNNMulti, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('CNN Multi\nMax F1:', max(MaxF1PlotCNNMulti$F1))) +
  xlab('') + ylim(0, 1)

# ------------------------- Combined Plots -------------------------
# Combine F1 plots
CombinedPlotF1 <- cowplot::plot_grid(BirdNET, BirdNETmulti, CNN, CNNmulti, labels = c("A", "B", "C", "D"))

# Combine AUC plots
CombinedPlotAUC <- cowplot::plot_grid(AUCPlotBirdNETBin, AUCPlotBirdNETMulti, AUCPlotCNNBinary, AUCPlotCNNMulti, labels = c("E", "F", "G", "H"))

# Save combined plots
pdf("F1andAUC_Cleaned.pdf", height = 14, width = 12)
cowplot::plot_grid(CombinedPlotF1, CombinedPlotAUC, labels = c("1", "2"), nrow = 2)
graphics.off()

# ------------------------- Combined Data Summary -------------------------
CombinedF1dataBirdNETmulti$model <- 'BirdNET Multi'
CombinedF1dataBirdNET$model <- 'BirdNET Binary'
CombinedF1data$model <- 'CNN Binary'
CombinedF1dataMulti$model <- 'CNN Multi'

CombinedRandomizationDF <- bind_rows(CombinedF1dataBirdNETmulti, CombinedF1dataBirdNET, CombinedF1data, CombinedF1dataMulti)

#write.csv(CombinedRandomizationDF, 'data/CombinedRandomizationAutomatedDetection_Cleaned.csv', row.names = FALSE)

# Create summary plot
CombinedRandomizationDFlong <- CombinedRandomizationDF %>%
  pivot_longer(cols = c(F1, auc), names_to = "metric", values_to = "value") %>%
  mutate(samples = as.numeric(samples)) # Ensure 'samples' is numeric

ggpubr::ggline(data = CombinedRandomizationDFlong, x = 'samples', y = 'value', color = 'model', add = "mean_se", facet.by = 'metric') +
  ylim(0, 1) + xlab('Number of Samples') + ylab('Metric') +
  scale_color_manual(values = c("#0080FF", "#00FFFF", "#80FF80", "#FF8000"))
