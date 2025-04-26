# Load necessary libraries ---------------------------------------------------
library(stringr)
library(caret)
library(ggpubr)
library(dplyr)
library(data.table)
library(ggplot2)
library(ROCR)
library(pROC)
library(plyr)

# Define variables -----------------------------------------------------------
detect.class <- 'gibbon'
start.time.buffer <- 6
end.time.buffer <- 3

PerformanceFolders <- list.files(
  '/Volumes/DJC Files/JahooTestDataPerformancegibbonNetR/',
  full.names = TRUE
)

TestDataSet <- list.files(
  '/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/AnnotatedFiles',
  full.names = TRUE
)

CombinedF1data <- data.frame()

# Loop through performance folders -------------------------------------------
for (z in seq_along(PerformanceFolders)) {
  tryCatch({

    print(paste('Processing', z, 'out of', length(PerformanceFolders)))

    TopModelresults <- list.files(
      paste0(PerformanceFolders[[z]], '/Selections/'),
      full.names = TRUE
    )

    TopModelDetectionDF <- data.frame()

    for (a in seq_along(TopModelresults)) {

      TempTopModelTable <- read.delim2(TopModelresults[a])
      ShortName <- str_split_fixed(basename(TopModelresults[a]), pattern = '.wav', n = 2)[, 1]
      ShortName <- str_split_fixed(ShortName, pattern = '__', n = 2)[, 2]

      testDataIndex <- which(str_detect(TestDataSet, ShortName))

      if (length(testDataIndex) > 0) {
        TestDataTable <- read.delim2(TestDataSet[testDataIndex])
        TestDataTable$Class <- detect.class
        TestDataTable$Begin.Time..s. <- round(as.numeric(TestDataTable$Begin.Time..s.))
        TestDataTable$End.Time..s. <- round(as.numeric(TestDataTable$End.Time..s.))

        DetectionList <- list()

        for (c in 1:nrow(TempTopModelTable)) {
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

            TempRow$Detections <- ShortName
            TopModelDetectionDF <- rbind(TopModelDetectionDF, TempRow)
          }
        }

        if (length(unlist(DetectionList)) > 0 && length(unlist(DetectionList)) < nrow(TestDataTable)) {
          missed_detections <- TestDataTable[-unlist(DetectionList), ]
        } else if (length(unlist(DetectionList)) == 0) {
          missed_detections <- TestDataTable
        }

        if (exists("missed_detections")) {
          missed_detections <- missed_detections[, c("Selection", "View", "Channel", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")]
          missed_detections$Probability <- 0
          missed_detections$Class <- detect.class
          missed_detections$Detections <- ShortName
          TopModelDetectionDF <- rbind(TopModelDetectionDF, missed_detections)
        }
      }
    }

    # Evaluation loop over thresholds ------------------------------------------
    TopModelDetectionDF$Class <- as.factor(TopModelDetectionDF$Class)
    TopModelDetectionDF$Probability <- as.numeric(TopModelDetectionDF$Probability)
    Thresholds <- seq(0.1, 0.9, 0.1)
    BestF1data <- data.frame()

    for (a in seq_along(Thresholds)) {
      tryCatch({
        DF <- TopModelDetectionDF
        DF$PredictedClass <- ifelse(DF$Probability <= Thresholds[a], 'noise', detect.class)

        caretConf <- confusionMatrix(
          as.factor(DF$PredictedClass),
          as.factor(DF$Class),
          positive = detect.class,
          mode = 'everything'
        )

        F1 <- caretConf$byClass[7]
        Precision <- caretConf$byClass[5]
        Recall <- caretConf$byClass[6]
        FP <- caretConf$table[1, 2]
        TN <- sum(caretConf$table[2, ])
        FPR <- FP / (FP + TN)

        row <- cbind.data.frame(F1, Precision, Recall, FPR, Thresholds = Thresholds[a])
        BestF1data <- rbind(BestF1data, row)

      }, error = function(e) {
        cat("ERROR (Threshold loop):", conditionMessage(e), "\n")
      })
    }

    BestF1data$PerformanceFolder <- basename(PerformanceFolders[[z]])

    roc_score <- auc(
      roc(response = TopModelDetectionDF$Class,
          predictor = as.numeric(TopModelDetectionDF$Probability),
          levels = c("noise", "gibbon"), direction = "<")
    )

    BestF1data$auc <- as.numeric(roc_score)

    CombinedF1data <- rbind(CombinedF1data, BestF1data)

  }, error = function(e) {
    cat("ERROR (Outer loop):", conditionMessage(e), "\n")
  })
}

# Final data prep ------------------------------------------------------------
CombinedF1data <- na.omit(CombinedF1data)
CombinedF1data$samples <- str_split_fixed(CombinedF1data$PerformanceFolder, 'samples', 2)[, 1]
CombinedF1data$samples <- as.numeric(str_split_fixed(CombinedF1data$samples, '_', 2)[, 2])
CombinedF1data$Precision <- round(CombinedF1data$Precision, 1)
CombinedF1data$Recall <- round(CombinedF1data$Recall, 2)
CombinedF1data$F1 <- round(CombinedF1data$F1, 2)

# Plotting -------------------------------------------------------------------
AUCPlotCNNBinary <- ggerrorplot(CombinedF1data, x = 'samples', y = 'auc') +
  xlab('') + ylab('AUC') + ylim(0, 1)

F1Plot <- ggerrorplot(CombinedF1data, x = 'Thresholds', y = 'F1', facet.by = 'samples') +
  ylim(0, 1) + xlab('Probability')

PrecRec <- ggerrorplot(CombinedF1data, x = 'Precision', y = 'Recall', facet.by = 'samples')

# Display plots
AUCPlotCNNBinary
F1Plot + geom_hline(yintercept = 0.8, color = 'red', linetype = 'dashed')
PrecRec

# Best threshold performance summary -----------------------------------------
CombinedF1data[which.max(CombinedF1data$F1),]

MaxF1PlotCNN <- CombinedF1data %>%
  group_by(PerformanceFolder) %>%
  summarise(F1 = max(F1, na.rm = TRUE))

MaxF1PlotCNN$samples <- str_split_fixed(MaxF1PlotCNN$PerformanceFolder, 'samples', 2)[, 1]
MaxF1PlotCNN$samples <- as.numeric(str_split_fixed(MaxF1PlotCNN$samples, '_', 2)[, 2])

CNN <- ggline(CombinedF1data, x = 'samples', y = 'F1', add = "mean_se") +
  ggtitle(paste('CNN Binary \n max F1:', max(CombinedF1data$F1))) +
  ylim(0, 1) + xlab('')

# Combined plot example (BirdNET assumed to exist) ---------------------------
CombinedPlot <- cowplot::plot_grid(BirdNET, CNN) + xlab('Number of training samples')
ggdraw(add_sub(CombinedPlot, "Number of training samples", vpadding = grid::unit(0, "lines"), y = 6, x = 0.5, vjust = 4.5))
