# ------------------------- Load libraries -------------------------
library(stringr)
library(caret)
library(ggpubr)
library(dplyr)
library(data.table)
library(ggplot2)
library(ROCR)
library(pROC)
library(plyr)

# ------------------------- Setup -------------------------
detect.class <- 'gibbon'
start.time.buffer <- 12
end.time.buffer <- 3

PerformanceFolders <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/BirdNETbinary/',
                                 full.names = TRUE)

TestDataSet <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_testdata_1hr_annotations/',
                          full.names = TRUE)

CombinedF1dataBirdNET <- data.frame()

# ------------------------- Process each model folder -------------------------
for (z in seq_along(PerformanceFolders)) {
  message(sprintf("Processing %d out of %d", z, length(PerformanceFolders)))

  TopModelresults <- list.files(PerformanceFolders[[z]], pattern = 'BirdNET.selection.table', full.names = TRUE)
  TopModelDetectionDF <- data.frame()

  for (a in seq_along(TopModelresults)) {
    TempTopModelTable <- read.delim2(TopModelresults[a])
    TempTopModelTable <- TempTopModelTable[, -c(4, 5)]
    TempTopModelTable <- subset(TempTopModelTable, Common.Name == detect.class)

    ShortName <- str_split_fixed(basename(TopModelresults[a]), '.BirdNET', 2)[, 1]
    testDataIndex <- which(str_detect(TestDataSet, ShortName))

    if (length(testDataIndex) > 0) {
      TestDataTable <- read.delim2(TestDataSet[testDataIndex])
      TestDataTable <- TestDataTable %>%
        mutate(Class = detect.class,
               Begin.Time..s. = round(as.numeric(Begin.Time..s.)),
               End.Time..s. = round(as.numeric(End.Time..s.)))

      DetectionList <- list()

      for (c in 1:nrow(TempTopModelTable)) {
        TempRow <- TempTopModelTable[c, ]

        if (!is.na(TempRow$Begin.Time..s.)) {
          TempRow$Begin.Time..s. <- as.numeric(TempRow$Begin.Time..s.)
          TempRow$End.Time..s. <- as.numeric(TempRow$End.Time..s.)

          TimeBetween <- data.table::between(TempRow$Begin.Time..s.,
                                             TestDataTable$Begin.Time..s. - start.time.buffer,
                                             TestDataTable$End.Time..s. + end.time.buffer)

          matched <- TestDataTable[TimeBetween, ]

          TempRow$Class <- if (nrow(matched) > 0) {
            DetectionList[[length(unlist(DetectionList)) + 1]] <- which(TimeBetween)
            detect.class
          } else {
            'noise'
          }

          TopModelDetectionDF <- rbind(TopModelDetectionDF, TempRow)
        }
      }

      # Add missed detections
      if (length(unlist(DetectionList)) < nrow(TestDataTable)) {
        missed_detections <- TestDataTable[-unlist(DetectionList), ]
        missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.",
                                                   "Low.Freq..Hz.", "High.Freq..Hz.")]
        missed_detections <- missed_detections %>%
          mutate(Confidence = 0,
                 Species.Code = detect.class,
                 Common.Name = detect.class,
                 Class = detect.class)
        TopModelDetectionDF <- rbind(TopModelDetectionDF, missed_detections)
      }
    }
  }

  TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)

  # ------------------------- Evaluate thresholds -------------------------
  Thresholds <- seq(0.1, 0.9, 0.1)
  BestF1data <- data.frame()

  for (threshold in Thresholds) {
    TopModelDetectionDF_single <- TopModelDetectionDF %>%
      mutate(PredictedClass = ifelse(Confidence <= threshold, 'noise', detect.class))

    caretConf <- caret::confusionMatrix(
      as.factor(TopModelDetectionDF_single$PredictedClass),
      as.factor(TopModelDetectionDF_single$Class),
      positive = detect.class,
      mode = 'everything')

    F1 <- caretConf$byClass['F1']
    Precision <- caretConf$byClass['Precision']
    Recall <- caretConf$byClass['Recall']
    FP <- caretConf$table[1, 2]
    TN <- sum(caretConf$table[, 2])
    FPR <- FP / (FP + TN)

    BestF1data <- rbind(BestF1data, data.frame(F1, Precision, Recall, FPR, Thresholds = threshold))
  }

  BestF1data$PerformanceFolder <- basename(PerformanceFolders[[z]])

  # ------------------------- Calculate AUC -------------------------
  TopModelDetectionDF$BinaryLabel <- ifelse(TopModelDetectionDF$Class == detect.class, 1, 0)
  pred <- ROCR::prediction(predictions = as.numeric(TopModelDetectionDF$Confidence),
                           labels = as.factor(TopModelDetectionDF$BinaryLabel))
  auc <- ROCR::performance(pred, "auc")
  auc <- round(as.numeric(auc@y.values), 2)
  BestF1data$auc <- auc

  CombinedF1dataBirdNET <- rbind(CombinedF1dataBirdNET, BestF1data)
}

# ------------------------- Clean & summarize -------------------------
CombinedF1dataBirdNET <- na.omit(CombinedF1dataBirdNET)

CombinedF1dataBirdNET$samples <- as.numeric(str_split_fixed(CombinedF1dataBirdNET$PerformanceFolder, 'samples', 2)[, 1])

CombinedF1dataBirdNET <- CombinedF1dataBirdNET %>%
  mutate(Precision = round(Precision, 1),
         Recall = round(Recall, 1),
         F1 = round(F1, 1))
write.csv(CombinedF1dataBirdNET,'data/CombinedF1dataBirdNET_automateddetect_binary.csv',row.names = FALSE)

# ------------------------- Plotting -------------------------
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
