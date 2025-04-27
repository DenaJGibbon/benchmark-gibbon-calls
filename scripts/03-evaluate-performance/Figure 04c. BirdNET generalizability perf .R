# Load necessary libraries
library(ggpubr)        # For generating plots
library(caret)         # For confusion matrix calculation
library(stringr)       # For string manipulation
library(ROCR)          # For AUC calculations

# ---------------------- BirdNET Binary Model Performance --------------------

# Step 1: Load file paths for BirdNET binary detections
ClipDetections <- list.files('results/BirdNETbinary_testgeneralizability', recursive = TRUE, full.names = TRUE)
ClipDetectionsShort <- dirname(ClipDetections)  # Get the directories for each clip

# Initialize an empty data frame to store results
BirdNETBinaryPerformanceDF <- data.frame()

# Step 2: Loop through each file and extract relevant information
for (a in 1:length(ClipDetections)) {
  TempDF <- read.delim(ClipDetections[a])  # Read the current file

  # Filter the data for relevant species (gibbons)
  TempDF <- subset(TempDF, Common.Name == 'CrestedGibbons' | Common.Name == 'gibbon' | Common.Name == 'Gibbons')

  # Check if the data is not empty
  if (nrow(TempDF) > 0) {
    # Extract the highest confidence for the clip
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence), ]  # Select the row with the highest confidence
    ActualLabel <- basename(ClipDetectionsShort[a])
  } else {
    # Handle cases with no gibbon detections (set confidence to 1 - max confidence)
    Confidence <- 1 - max(TempDF$Confidence)
    ActualLabel <- basename(ClipDetectionsShort[a])
  }

  # Create a row of results for this clip
  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <- ClipDetectionsShort[a]
  TempRow$BirdNETBinaryBinary <- ifelse(TempRow$Confidence <= 0.5, 'noise', 'gibbon')

  # Extract the sample and performance folder information from the file path
  TempRow$samples <- str_split_fixed(ClipDetectionsShort[a], 'samples', n = 2)[, 1]
  TempRow$samples <- basename(TempRow$samples)
  TempRow$PerformanceFolder <- str_split_fixed(ClipDetectionsShort[a], pattern = '/', n = 7)[, 3]

  # Append the row to the main data frame
  BirdNETBinaryPerformanceDF <- rbind.data.frame(BirdNETBinaryPerformanceDF, TempRow)
}

# Step 3: Generate confusion matrix for BirdNET binary model performance
caretConf <- caret::confusionMatrix(
  as.factor(BirdNETBinaryPerformanceDF$BirdNETBinaryBinary),
  as.factor(BirdNETBinaryPerformanceDF$ActualLabel),
  mode = 'everything'
)
print(caretConf)

# ------------------- Performance Analysis per Sample ---------------------

# Step 4: Initialize unique sample folders
uniquesamples <- unique(BirdNETBinaryPerformanceDF$PerformanceFolder)
SampleSizeCrestedGibbonBirdNETBinary <- data.frame()

# Step 5: Loop through each unique sample and calculate metrics for different thresholds
for (z in 1:length(uniquesamples)) {
  Thresholds <- seq(0.1, 1, 0.1)  # Define confidence thresholds

  # Filter data for the current sample folder
  BirdNETBinaryPerformanceDFtemp <- subset(BirdNETBinaryPerformanceDF, PerformanceFolder == uniquesamples[z])

  # Define the binary labels (gibbon = 1, noise = 0)
  BirdNETBinaryPerformanceDFtemp$BinaryLabel <- ifelse(BirdNETBinaryPerformanceDFtemp$ActualLabel == 'gibbon', 1, 0)

  # Perform AUC calculation
  pred <- ROCR::prediction(predictions = as.numeric(BirdNETBinaryPerformanceDFtemp$Confidence),
                           labels = as.factor(BirdNETBinaryPerformanceDFtemp$BinaryLabel))
  auc <- ROCR::performance(pred, "auc")
  binaryauc <- round(as.numeric(auc@y.values), 2)

  # Step 6: Evaluate model performance for each threshold
  for (a in 1:length(Thresholds)) {
    # Assign predicted class based on the current threshold
    TopModelDetectionDF_single <- BirdNETBinaryPerformanceDFtemp
    TopModelDetectionDF_single$PredictedClass <- ifelse(TopModelDetectionDF_single$Confidence <= Thresholds[a], 'noise', 'gibbon')

    # Generate confusion matrix for each threshold
    caretConf <- caret::confusionMatrix(
      as.factor(TopModelDetectionDF_single$PredictedClass),
      as.factor(TopModelDetectionDF_single$ActualLabel),
      mode = 'everything'
    )

    # Extract F1 score, Precision, and Recall from confusion matrix
    F1 <- caretConf$byClass[7]
    Precision <- caretConf$byClass[5]
    Recall <- caretConf$byClass[6]

    # Store the results for this threshold
    TempF1Row <- cbind.data.frame(F1, Precision, Recall)
    TempF1Row$Thresholds <- Thresholds[a]
    TempF1Row$samples <- uniquesamples[z]
    TempF1Row$performancefolder <- uniquesamples[z]
    TempF1Row$AUC <- binaryauc

    # Append the row to the final results data frame
    SampleSizeCrestedGibbonBirdNETBinary <- rbind.data.frame(SampleSizeCrestedGibbonBirdNETBinary, TempF1Row)
  }
}

# Step 7: Clean and prepare the final data frame for plotting
SampleSizeCrestedGibbonBirdNETBinary <- na.omit(SampleSizeCrestedGibbonBirdNETBinary)
SampleSizeCrestedGibbonBirdNETBinary$Precision <- round(SampleSizeCrestedGibbonBirdNETBinary$Precision, 1)
SampleSizeCrestedGibbonBirdNETBinary$Recall <- round(SampleSizeCrestedGibbonBirdNETBinary$Recall, 2)
SampleSizeCrestedGibbonBirdNETBinary$F1 <- as.numeric(round(SampleSizeCrestedGibbonBirdNETBinary$F1, 2))

# Step 8: Find maximum F1 score for each performance folder
MaxF1PlotBirdNETbinary <- SampleSizeCrestedGibbonBirdNETBinary %>%
  dplyr::group_by(performancefolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE), AUC = max(AUC, na.rm = TRUE))

# Extract the sample size
MaxF1PlotBirdNETbinary$samples <- as.numeric(str_split_fixed(MaxF1PlotBirdNETbinary$performancefolder, pattern = 'samples', n = 2)[, 1])

# Plot F1 score for BirdNET binary model
ggpubr::ggline(data = MaxF1PlotBirdNETbinary, x = 'samples', y = 'F1', add = "mean_se") +
  ylim(0, 1) + ggtitle('BirdNET binary')

write.csv(MaxF1PlotBirdNETbinary,'data/MaxF1PlotBirdNETbinary_generalizability.csv',row.names = F)

# ---------------------- BirdNET Multi-Class Model Performance ----------------

# Step 1: Load file paths for BirdNET multi-class detections
ClipDetections <- list.files('results/BirdNETmulti_testgeneralizability/', recursive = TRUE, full.names = TRUE)
ClipDetectionsShort <- dirname(ClipDetections)  # Get the directories for each clip

# Initialize an empty data frame to store results
BirdNETmultiPerformanceDF <- data.frame()

# Step 2: Loop through each file, extract relevant information, and aggregate results
for (a in 1:length(ClipDetections)) {
  TempDF <- read.delim(ClipDetections[a])  # Read the current file

  # Filter data for relevant species (Crested Gibbon, Gibbon, etc.)
  TempDF <- subset(TempDF, Common.Name == 'CrestedGibbon' | Common.Name == 'CrestedGibbons' | Common.Name == 'gibbon' | Common.Name == 'Gibbons')

  # If there are detections, select the row with the highest confidence
  if (nrow(TempDF) > 0) {
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence), ]  # Select the row with the highest confidence
    ActualLabel <- basename(ClipDetectionsShort[a])
  } else {
    # Handle cases with no gibbon detections (set confidence to 1 - max confidence)
    TempDF <- read.delim(ClipDetections[a])
    TempDF <- subset(TempDF, Common.Name == 'nocall')

    if (nrow(TempDF) > 0) {
      Confidence <- 1 - TempDF$Confidence
      ActualLabel <- basename(ClipDetectionsShort[a])
    } else {
      Confidence <- 0
      ActualLabel <- basename(ClipDetectionsShort[a])
    }
  }

  # Create a row of results for this clip
  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <- ClipDetectionsShort[a]
  TempRow$BirdNETmultimulti <- ifelse(TempRow$Confidence <= 0.5, 'noise', 'CrestedGibbon')

  # Extract sample and performance folder information from the file path
  TempRow$samples <- str_split_fixed(ClipDetectionsShort[a], 'samples', n = 2)[, 1]
  TempRow$samples <- basename(TempRow$samples)
  TempRow$PerformanceFolder <- str_split_fixed(ClipDetectionsShort[a], pattern = '/', n = 7)[, 4]

  # Append the row to the multi-class data frame
  BirdNETmultiPerformanceDF <- rbind.data.frame(BirdNETmultiPerformanceDF, TempRow)
}

# Step 3: Generate confusion matrix for BirdNET multi-class model performance
caretConf <- caret::confusionMatrix(
  as.factor(BirdNETmultiPerformanceDF$BirdNETmultimulti),
  as.factor(BirdNETmultiPerformanceDF$ActualLabel),
  mode = 'everything'
)
print(caretConf)  # Display the confusion matrix

# --------------------- Performance Analysis per Sample ---------------------

# Step 4: Initialize unique sample folders
uniquesamples <- unique(BirdNETmultiPerformanceDF$PerformanceFolder)
SampleSizeCrestedGibbonBirdNETmulti <- data.frame()

# Step 5: Loop through each unique sample and calculate metrics for different thresholds
for (z in 1:length(uniquesamples)) {
  Thresholds <- seq(0.1, 1, 0.1)  # Define confidence thresholds

  # Filter data for the current sample folder
  BirdNETmultiPerformanceDFtemp <- subset(BirdNETmultiPerformanceDF, PerformanceFolder == uniquesamples[z])

  # Define binary labels for Crested Gibbon (1) and noise (0)
  BirdNETmultiPerformanceDFtemp$BinaryLabel <- ifelse(BirdNETmultiPerformanceDFtemp$ActualLabel == 'CrestedGibbon', 1, 0)

  # Perform AUC calculation
  pred <- ROCR::prediction(predictions = as.numeric(BirdNETmultiPerformanceDFtemp$Confidence),
                           labels = as.factor(BirdNETmultiPerformanceDFtemp$BinaryLabel))
  auc <- ROCR::performance(pred, "auc")
  multiauc <- round(as.numeric(auc@y.values), 2)

  # Step 6: Evaluate model performance for each threshold
  for (a in 1:length(Thresholds)) {
    # Assign predicted class based on the current threshold
    TopModelDetectionDF_single <- BirdNETmultiPerformanceDFtemp
    TopModelDetectionDF_single$PredictedClass <- ifelse(TopModelDetectionDF_single$Confidence <= Thresholds[a], 'noise', 'CrestedGibbon')

    # Generate confusion matrix for each threshold
    caretConf <- caret::confusionMatrix(
      as.factor(TopModelDetectionDF_single$PredictedClass),
      as.factor(TopModelDetectionDF_single$ActualLabel),
      mode = 'everything'
    )

    # Extract F1 score, Precision, and Recall from confusion matrix
    F1 <- caretConf$byClass[7]
    Precision <- caretConf$byClass[5]
    Recall <- caretConf$byClass[6]

    # Store the results for this threshold
    TempF1Row <- cbind.data.frame(F1, Precision, Recall)
    TempF1Row$Thresholds <- Thresholds[a]
    TempF1Row$samples <- uniquesamples[z]
    TempF1Row$performancefolder <- uniquesamples[z]
    TempF1Row$AUC <- multiauc

    # Append the row to the final results data frame
    SampleSizeCrestedGibbonBirdNETmulti <- rbind.data.frame(SampleSizeCrestedGibbonBirdNETmulti, TempF1Row)
  }
}

# Step 7: Clean and prepare the final data frame for plotting
SampleSizeCrestedGibbonBirdNETmulti <- na.omit(SampleSizeCrestedGibbonBirdNETmulti)
SampleSizeCrestedGibbonBirdNETmulti$Precision <- round(SampleSizeCrestedGibbonBirdNETmulti$Precision, 1)
SampleSizeCrestedGibbonBirdNETmulti$Recall <- round(SampleSizeCrestedGibbonBirdNETmulti$Recall, 2)
SampleSizeCrestedGibbonBirdNETmulti$F1 <- as.numeric(round(SampleSizeCrestedGibbonBirdNETmulti$F1, 2))

# Step 8: Find maximum F1 score for each performance folder
MaxF1PlotBirdNETmulti <- SampleSizeCrestedGibbonBirdNETmulti %>%
  dplyr::group_by(performancefolder) %>%
  dplyr::summarise(F1 = max(F1, na.rm = TRUE), AUC = max(AUC, na.rm = TRUE))

# Extract the sample size
MaxF1PlotBirdNETmulti$samples <- as.numeric(str_split_fixed(MaxF1PlotBirdNETmulti$performancefolder, pattern = 'samples', n = 2)[, 1])

# Round the F1 score for display
MaxF1PlotBirdNETmulti$F1 <- as.numeric(round(MaxF1PlotBirdNETmulti$F1, 2))

# Step 9: Plot F1 score for BirdNET multi-class model
ggpubr::ggline(data = MaxF1PlotBirdNETmulti, x = 'samples', y = 'F1', add = "mean_se") +
  ylim(0, 1) + ggtitle('BirdNET Multi')

write.csv(MaxF1PlotBirdNETmulti,'data/MaxF1PlotBirdNETmulti_generalizability.csv',row.names = F)
