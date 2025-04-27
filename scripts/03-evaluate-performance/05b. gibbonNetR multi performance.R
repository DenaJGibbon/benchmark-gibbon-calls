# Load necessary libraries
library(stringr)
library(caret)
library(ggpubr)
library(dplyr)
library(data.table)
library(ggplot2)
library(ROCR)
library(pROC)
library(plyr)
library(cowplot)

# Parameters and Paths
detect.class <- 'CrestedGibbons'
PerformanceFolders <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR/randomization_multi_results', full.names = TRUE)
TestDataSet <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_testdata_1hr_annotations/', full.names = TRUE)
start.time.buffer <- 6
end.time.buffer <- 3
CombinedF1dataMulti <- data.frame()

# Loop over model output folders
for (z in seq_along(PerformanceFolders)) {
  tryCatch({
    message(sprintf("Processing %d out of %d", z, length(PerformanceFolders)))
    TopModelresults <- list.files(file.path(PerformanceFolders[z], "Selections"), full.names = TRUE)
    TopModelDetectionDF <- data.frame()

    # Loop over detection results
    for (result_file in TopModelresults) {
      TempTopModelTable <- read.delim2(result_file)
      ShortName <- str_split_fixed(basename(result_file), pattern = ".wav", n = 2)[, 1]
      ShortName <- str_split_fixed(ShortName, pattern = "__", n = 2)[, 2]

      testDataIndex <- which(str_detect(TestDataSet, ShortName))

      if (length(testDataIndex) > 0) {
        TestDataTable <- read.delim2(TestDataSet[testDataIndex])
        TestDataTable$Class <- detect.class
        TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
        TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))

        DetectionList <- list()

        for (c in 1:nrow(TempTopModelTable)) {
          TempRow <- TempTopModelTable[c, ]
          if (!is.na(TempRow$Begin.Time..s.)) {
            TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
            TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)
            TimeBetween <- between(TempRow$Begin.Time..s., TestDataTable$Begin.Time..s. - start.time.buffer, TestDataTable$End.Time..s. + end.time.buffer)
            matched_detections <- TestDataTable[TimeBetween, ]

            TempRow$Class <- if (nrow(matched_detections) > 0) detect.class else "noise"
            if (nrow(matched_detections) > 0) DetectionList[[length(unlist(DetectionList)) + 1]] <- which(TimeBetween)
            TempRow$Detections <- ShortName
            TopModelDetectionDF <- rbind(TopModelDetectionDF, TempRow)
          }
        }

        # Add missed detections as noise
        missed_indices <- setdiff(seq_len(nrow(TestDataTable)), unlist(DetectionList))
        if (length(missed_indices) > 0) {
          missed_detections <- TestDataTable[missed_indices, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
          missed_detections$Probability <- 0
          missed_detections$Class <- detect.class
          missed_detections$Detections <- ShortName
          TopModelDetectionDF <- rbind(TopModelDetectionDF, missed_detections)
        }
      }
    }

    TopModelDetectionDF$Class <- factor(TopModelDetectionDF$Class)
    TopModelDetectionDF$Probability <- as.numeric(TopModelDetectionDF$Probability)

    # Evaluate across thresholds
    Thresholds <- seq(0.1, 0.9, 0.1)
    BestF1data <- data.frame()

    for (thresh in Thresholds) {
      tryCatch({
        TopModelDetectionDF$PredictedClass <- ifelse(TopModelDetectionDF$Probability <= thresh, "noise", detect.class)
        conf <- caret::confusionMatrix(factor(TopModelDetectionDF$PredictedClass),
                                       factor(TopModelDetectionDF$Class),
                                       positive = detect.class, mode = "everything")

        F1 <- conf$byClass["F1"]
        Precision <- conf$byClass["Precision"]
        Recall <- conf$byClass["Recall"]
        FP <- conf$table[1, 2]
        TN <- sum(conf$table[2, ])
        FPR <- FP / (FP + TN)

        row <- data.frame(F1 = F1, Precision = Precision, Recall = Recall, FPR = FPR, Thresholds = thresh)
        BestF1data <- rbind(BestF1data, row)
      }, error = function(e) message("Threshold eval error: ", e$message))
    }

    BestF1data$PerformanceFolder <- basename(PerformanceFolders[z])
    BestF1data$auc <- auc(roc(response = TopModelDetectionDF$Class,
                              predictor = TopModelDetectionDF$Probability,
                              levels = c("noise", detect.class), direction = "<"))

    CombinedF1dataMulti <- rbind(CombinedF1dataMulti, BestF1data)
  }, error = function(e) message("Main loop error: ", e$message))
}

# Clean and prep Combined results
CombinedF1dataMulti <- na.omit(CombinedF1dataMulti)
CombinedF1dataMulti$samples <- as.numeric(str_split_fixed(str_split_fixed(CombinedF1dataMulti$PerformanceFolder, "samples", 2)[, 1], "_", 2)[, 2])

CombinedF1dataMulti <- CombinedF1dataMulti %>%
  mutate(Precision = round(Precision, 1),
         Recall = round(Recall, 1),
         F1 = round(F1, 1))


write.csv(CombinedF1dataMulti,'data/CombinedF1dataMulti.csv',row.names = F)
