# Load necessary libraries
library(stringr)      # For string manipulation
library(cowplot)      # For arranging plots
library(ggpubr)       # For generating plots

# Read in processed BirdNET results ---------------------------------------
MaxF1PlotBirdNETbinary <- read.csv('data/MaxF1PlotBirdNETbinary_generalizability.csv')
MaxF1PlotBirdNETmulti <- read.csv('data/MaxF1PlotBirdNETmulti_generalizability.csv')

# ------------------------ Process Binary CNN Model ------------------------

# Step 1: Read model folders for binary CNN and aggregate data
ModelFolders <- list.files('results/gibbonNetR/randomization/', full.names = TRUE)

CombinedDFclipsbinaryCNN <- data.frame()
for (a in 1:length(ModelFolders)) {
  print(a)

  # Get CSV file paths in the folder
  Temp.CSV <- list.files(list.files(ModelFolders[a], full.names = T)[4], full.names = T)

  # Read data and extract sample information from file name
  TempVals <- read.csv(Temp.CSV)
  TempName <- basename(Temp.CSV)
  TempVals$samples <- as.numeric(str_split_fixed(TempName, 'samples', n = 2)[, 1])

  # Append to the combined data frame
  CombinedDFclipsbinaryCNN <- rbind.data.frame(CombinedDFclipsbinaryCNN, TempVals)
}

# Step 2: Clean up the data
CombinedDFclipsbinaryCNN <- na.omit(CombinedDFclipsbinaryCNN)
CombinedDFclipsbinaryCNN$Precision <- round(CombinedDFclipsbinaryCNN$Precision, 1)
CombinedDFclipsbinaryCNN$Recall <- round(CombinedDFclipsbinaryCNN$Recall, 2)
CombinedDFclipsbinaryCNN$F1 <- as.numeric(round(CombinedDFclipsbinaryCNN$F1, 2))

# Step 3: Aggregate data to find the maximum F1 score per training set
MaxF1PlotCNNBinary <- CombinedDFclipsbinaryCNN %>%
  dplyr::group_by(Training.Data) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE), AUC = max(AUC, na.rm = TRUE))

# Extract sample size from the 'Training.Data' column
MaxF1PlotCNNBinary$samples <- as.numeric(str_split_fixed(MaxF1PlotCNNBinary$Training.Data, 'samples', n = 2)[, 1])

# Step 4: Plot the F1 score for ResNet50 binary model
F1PlotBinaryCNN <- ggpubr::ggline(data = MaxF1PlotCNNBinary, x = 'samples', y = 'F1', add = "mean_se") +
  ylim(0, 1) + ggtitle('ResNet50 Binary')

# ---------------------- Process Multi-Class CNN Model ------------------------

# Step 5: Read model folders for multi-class CNN and aggregate data
ModelFolders <- list.files('results/gibbonNetR/randomization_multi', full.names = TRUE)

CombinedDFclipsmultiCNN <- data.frame()
for (a in 1:length(ModelFolders)) {
  print(a)

  # Get CSV file paths in the folder
  Temp.CSV <- list.files(list.files(ModelFolders[a], full.names = T)[4], full.names = T)

  # Read data and extract sample information from file name
  TempVals <- read.csv(Temp.CSV)
  TempName <- basename(Temp.CSV)
  TempVals$samples <- as.numeric(str_split_fixed(TempName, 'samples', n = 2)[, 1])

  # Append to the combined data frame
  CombinedDFclipsmultiCNN <- rbind.data.frame(CombinedDFclipsmultiCNN, TempVals)
}

# Step 6: Clean up the data
CombinedDFclipsmultiCNN <- na.omit(CombinedDFclipsmultiCNN)
CombinedDFclipsmultiCNN$Precision <- round(CombinedDFclipsmultiCNN$Precision, 1)
CombinedDFclipsmultiCNN$Recall <- round(CombinedDFclipsmultiCNN$Recall, 2)
CombinedDFclipsmultiCNN$F1 <- as.numeric(round(CombinedDFclipsmultiCNN$F1, 2))

# Step 7: Aggregate data to find the maximum F1 score per training set for multi-class CNN
MaxF1PlotCNNMulti <- CombinedDFclipsmultiCNN %>%
  dplyr::group_by(Training.Data) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE), AUC = max(AUC, na.rm = TRUE))

# Extract sample size from the 'Training.Data' column
MaxF1PlotCNNMulti$samples <- as.numeric(str_split_fixed(MaxF1PlotCNNMulti$Training.Data, 'samples', n = 2)[, 1])

# Step 8: Plot the F1 score for ResNet50 multi-class model
F1PlotMultiCNN <- ggpubr::ggline(data = MaxF1PlotCNNMulti, x = 'samples', y = 'F1', add = "mean_se") +
  ylim(0, 1) + ggtitle('ResNet50 Multi')

# -------------------- Process BirdNET Binary and Multi ----------------------

# Add model labels to the data
MaxF1PlotBirdNETbinary$Model <- 'BirdNET Binary'
MaxF1PlotBirdNETmulti$Model <- 'BirdNET Multi'
MaxF1PlotCNNMulti$Model <- 'ResNet50 Multi'
MaxF1PlotCNNBinary$Model <- 'ResNet50 Binary'

# Ensure consistent column names across datasets
colnames(MaxF1PlotCNNBinary) <- c("performancefolder", "F1", "AUC", "samples", "Model")
colnames(MaxF1PlotCNNMulti) <- c("performancefolder", "F1", "AUC", "samples", "Model")

# Step 9: Combine all model results into a single data frame
CombinedPerformanceDF <- rbind.data.frame(MaxF1PlotCNNMulti, MaxF1PlotCNNBinary,
                                          MaxF1PlotBirdNETbinary, MaxF1PlotBirdNETmulti)

# ---------------------------- Plot F1 and AUC -----------------------------

# F1 plot
F1Plot <- ggpubr::ggline(data = CombinedPerformanceDF, x = 'samples', y = 'F1', color = 'Model',
                         add = "mean_se") +
  ylim(0, 1) +
  xlab('') + ylab('MAX F1') +
  scale_color_manual(values = c("#0080FF", "#00FFFF", "#80FF80", "#FF8000"))

# AUC plot
AUCPlot <- ggpubr::ggline(data = CombinedPerformanceDF, x = 'samples', y = 'AUC', color = 'Model',
                          add = "mean_se") +
  ylim(0.5, 1) +
  xlab('Number of training samples') +
  scale_color_manual(values = c("#0080FF", "#00FFFF", "#80FF80", "#FF8000"))+guides(color="none")

# -------------------- Combine F1 and AUC Plots ----------------------
pdf('results/Figure5-performance-metrics.pdf',width=10)

# Combine the plots into one figure
CombinedPlot <- plot_grid(F1Plot, AUCPlot, labels = c("A)", "B)"), label_x =.95, label_y = 1, ncol = 1)

# Display the combined plot
print(CombinedPlot)
graphics.off()

