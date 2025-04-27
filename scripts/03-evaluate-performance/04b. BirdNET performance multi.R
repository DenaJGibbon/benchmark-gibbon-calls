# Load necessary libraries ---------------------------------------------------
library(stringr)      # String manipulation
library(caret)        # Machine learning and evaluation
library(ggpubr)       # Visualization
library(dplyr)        # Data manipulation
library(data.table)   # Efficient table operations
library(ggplot2)
library(ROCR)         # ROC/AUC calculations
library(pROC)
library(plyr)

# Set detection class and file paths -----------------------------------------
detect.class <- 'CrestedGibbon'

PerformanceFolders <- list.files(
  '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/BirdNETmulti/',
  full.names = TRUE
)

TestDataSet <- list.files(
  '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_testdata_1hr_annotations/',
  full.names = TRUE
)

start.time.buffer <- 6
end.time.buffer <- 3
CombinedF1dataBirdNETmulti <- data.frame()

# Iterate over performance folders -------------------------------------------
for (z in seq_along(PerformanceFolders)) {
  cat(sprintf("Processing %d out of %d\n", z, length(PerformanceFolders)))

  TopModelresults <- list.files(
    PerformanceFolders[[z]],
    pattern = 'BirdNET.selection.table',
    full.names = TRUE
  )

  TopModelDetectionDF <- data.frame()

  for (a in seq_along(TopModelresults)) {
    TempTopModelTable <- read.delim2(TopModelresults[a])[ , -c(4,5)]
    TempTopModelTable <- subset(TempTopModelTable, Common.Name %in% c(detect.class, 'CrestedGibbons'))

    ShortName <- str_split_fixed(basename(TopModelresults[a]), pattern = '.BirdNET', n = 2)[, 1]
    testDataIndex <- which(str_detect(TestDataSet, ShortName))

    if (length(testDataIndex) > 0) {
      TestDataTable <- read.delim2(TestDataSet[testDataIndex])
      TestDataTable$Class <- detect.class
      TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
      TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))

      DetectionList <- list()

      for (c in seq_len(nrow(TempTopModelTable))) {
        TempRow <- TempTopModelTable[c,]

        if (!is.na(TempRow$Begin.Time..s.)) {
          TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
          TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)

          TimeBetween <- data.table::between(
            TempRow$Begin.Time..s.,
            TestDataTable$Begin.Time..s. - start.time.buffer,
            TestDataTable$End.Time..s. + end.time.buffer
          )

          matched_detections <- TestDataTable[TimeBetween, ]

          if (nrow(matched_detections) > 0) {
            TempRow$Class <- detect.class
            DetectionList[[length(unlist(DetectionList)) + 1]] <- which(TimeBetween == TRUE)
          } else {
            TempRow$Class <- 'noise'
          }

          TopModelDetectionDF <- rbind(TopModelDetectionDF, TempRow)
        }
      }

      # Handle missed detections
      missed_detections <- NULL

      if (length(unlist(DetectionList)) > 0 &&
          length(unlist(DetectionList)) < nrow(TestDataTable)) {
        missed_detections <- TestDataTable[-unlist(DetectionList), ]
      } else if (length(unlist(DetectionList)) == 0) {
        missed_detections <- TestDataTable
      }

      if (!is.null(missed_detections)) {
        missed_detections <- missed_detections[, c("Selection", "View", "Channel",
                                                   "Begin.Time..s.", "End.Time..s.",
                                                   "Low.Freq..Hz.", "High.Freq..Hz.")]
        missed_detections$Confidence <- 0
        missed_detections$Species.Code <- detect.class
        missed_detections$Common.Name <- detect.class
        missed_detections$Class <- detect.class

        TopModelDetectionDF <- rbind(TopModelDetectionDF, missed_detections)
      }
    }
  }

  TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)

  # Threshold evaluation -----------------------------------------------------
  Thresholds <- seq(0.1, 0.9, 0.1)
  BestF1data.framecrestedmulti <- data.frame()

  for (a in seq_along(Thresholds)) {
    df <- TopModelDetectionDF
    df$PredictedClass <- ifelse(df$Confidence <= Thresholds[a], 'noise', detect.class)

    caretConf <- caret::confusionMatrix(
      as.factor(df$PredictedClass),
      as.factor(df$Class),
      positive = detect.class,
      mode = 'everything'
    )

    F1 <- caretConf$byClass[7]
    Precision <- caretConf$byClass[5]
    Recall <- caretConf$byClass[6]
    FP <- caretConf$table[1, 2]
    TN <- sum(caretConf$table[, 2])
    FPR <- FP / (FP + TN)

    TempF1Row <- data.frame(F1, Precision, Recall, FPR, Thresholds = Thresholds[a])
    BestF1data.framecrestedmulti <- rbind(BestF1data.framecrestedmulti, TempF1Row)
  }

  BestF1data.framecrestedmulti$PerformanceFolder <- basename(PerformanceFolders[[z]])

  # AUC Calculation ----------------------------------------------------------
  TopModelDetectionDF$BinaryLabel <- ifelse(TopModelDetectionDF$Class == detect.class, 1, 0)
  pred <- ROCR::prediction(predictions = as.numeric(TopModelDetectionDF$Confidence),
                           labels = as.factor(TopModelDetectionDF$BinaryLabel))
  auc <- ROCR::performance(pred, "auc")
  auc <- round(as.numeric(auc@y.values), 2)
  BestF1data.framecrestedmulti$auc <- auc

  CombinedF1dataBirdNETmulti <- rbind(CombinedF1dataBirdNETmulti, BestF1data.framecrestedmulti)
}

# Clean and format results ---------------------------------------------------
CombinedF1dataBirdNETmulti <- na.omit(CombinedF1dataBirdNETmulti)
CombinedF1dataBirdNETmulti$samples <- as.numeric(str_split_fixed(
  CombinedF1dataBirdNETmulti$PerformanceFolder, pattern = 'samples', n = 2)[, 1])

CombinedF1dataBirdNETmulti <- CombinedF1dataBirdNETmulti %>%
  mutate(Precision = round(Precision, 1),
         Recall = round(Recall, 1),
         F1 = round(F1, 1))

write.csv(CombinedF1dataBirdNETmulti,'data/CombinedF1dataBirdNET_automateddetect_multi.csv',row.names = F)


