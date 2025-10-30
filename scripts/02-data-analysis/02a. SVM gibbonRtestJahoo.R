# Load required libraries
library(e1071)      # For SVM (Support Vector Machines)
library(caret)      # For confusion matrix calculation
library(ggplot2)    # For plotting
library(pROC)       # For AUC calculation
library(gibbonR) # For MFCCs


# SVM binary: MFCCs -------------------------------------------------------

# JahooMFCCDF <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations/Gibbons/',
#                             min.freq = 400,
#                             max.freq = 3000,
#                             n.windows = 9,
#                             num.cep = 12,
#                             win.avg = 'standard',
#                             win.hop.time = 0.25)
#
#
# JahooMFCCDFNoise <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations/Noise/',
#                                  min.freq = 400,
#                                  max.freq = 3000,
#                                  n.windows = 9,
#                                  num.cep = 12,
#                                  win.avg = 'standard',
#                                  win.hop.time = 0.25)
#
#
#
# JahooMFCCDFTrain <- rbind.data.frame(JahooMFCCDF,JahooMFCCDFNoise)
#
# write.csv(JahooMFCCDFTrain,'data/JahooMFCCDFTrain.csv',row.names = F)


# # SVM Multi MFCCs ---------------------------------------------------------
# JahooMFCCDFCrested <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum/CrestedGibbons',
#                                    min.freq = 400,
#                                    max.freq = 3000,
#                                    n.windows = 9,
#                                    num.cep = 12,
#                                    win.avg = 'standard',
#                                    win.hop.time = 0.25)
#
# JahooMFCCDFCrested$class <- 'CrestedGibbons'
#
# JahooMFCCDFGrey<- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum/GreyGibbons',
#                                min.freq = 400,
#                                max.freq = 3000,
#                                n.windows = 9,
#                                num.cep = 12,
#                                win.avg = 'standard',
#                                win.hop.time = 0.25)
#
# JahooMFCCDFGrey$class <- 'GreyGibbons'
#
# JahooMFCCDFNoise <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum/Noise',
#                                  min.freq = 400,
#                                  max.freq = 3000,
#                                  n.windows = 9,
#                                  num.cep = 12,
#                                  win.avg = 'standard',
#                                  win.hop.time = 0.25)
#
#
#
# JahooMFCCDFTrain <- rbind.data.frame(JahooMFCCDFCrested,JahooMFCCDFGrey,JahooMFCCDFNoise)
#
# #write.csv(JahooMFCCDFTrain,'data/JahooMFCCDFTrainMulti.csv', row.names = F)
# SVM test: MFCCs ---------------------------------------------------------
# JahooTestMFCCDF <- MFCCFunction(input.dir='data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsWavs/CrestedGibbons/',
#                                 min.freq = 400,
#                                 max.freq = 3000,
#                                 n.windows = 9,
#                                 num.cep = 12,
#                                 win.avg = 'standard',
#                                 win.hop.time = 0.25)
#
#
# JahooMFCCDFTestNoise <- MFCCFunction(input.dir='data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsWavs/Noise/',
#                                      min.freq = 400,
#                                      max.freq = 3000,
#                                      n.windows = 9,
#                                      num.cep = 12,
#                                      win.avg = 'standard',
#                                      win.hop.time = 0.25)
#
# JahooMFCCDFTest <- rbind.data.frame(JahooTestMFCCDF,JahooMFCCDFTestNoise)
#write.csv(JahooMFCCDFTest,'data/JahooMFCCDFTest.csv',row.names = F)

# SVM binary: train model -------------------------------------------------
# Step 1: Load and Prepare Training Data
JahooMFCCDFTrain <- read.csv('data/JahooMFCCDFTrain.csv')  # Load pre-processed MFCC features for training
JahooMFCCDFTrain$class <- as.factor(JahooMFCCDFTrain$class)  # Convert class labels to factor

# Step 2: Train the SVM Model using the Training Data
ml.model.svm.jahoo <- e1071::svm(
  JahooMFCCDFTrain[, 2: (ncol(JahooMFCCDFTrain) - 1)],  # Use all features except the class label
  JahooMFCCDFTrain$class,  # Target class variable (CrestedGibbons or Noise)
  kernel = "radial",       # Radial basis function kernel for SVM
  cross = 20,             # 20-fold cross-validation
  probability = TRUE      # Enable probability estimates for predictions
)

# Print model's total accuracy
ml.model.svm.jahoo$tot.accuracy

# Step 3: Load and Prepare Test Data
JahooMFCCDFTest <- read.csv('data/JahooMFCCDFTest.csv')  # Load pre-processed MFCC features for testing
JahooMFCCDFTest$class <- as.factor(JahooMFCCDFTest$class)  # Convert class labels to factor
table(JahooMFCCDFTest$class)  # Check class distribution in test data

# Preview the test data
head(JahooMFCCDFTest)

# Step 4: Generate Predictions on Test Data using the trained SVM Model
TestPredictions <- predict(ml.model.svm.jahoo, JahooMFCCDFTest[, 2: (ncol(JahooMFCCDFTest) - 1)], probability = TRUE)
TestPredictionsProb <- as.data.frame(attr(TestPredictions, "probabilities"))  # Extract probability predictions
TestPredictionsProb$ActualLabel <- JahooMFCCDFTest$class  # Add actual class labels to the results

# Step 5: Define Confidence Thresholds for Detection
Thresholds <- seq(0.1, 1, 0.1)

# Step 6: Create an empty data frame to store evaluation results
BestF1data.frameCrestedGibbonSVMBinary <- data.frame()

# Step 7: Loop through each threshold and evaluate model performance
for (a in 1:length(Thresholds)) {
  # Filter the predictions based on the current confidence threshold
  TopModelDetectionDF_single <- TestPredictionsProb
  TopModelDetectionDF_single$PredictedClass <- ifelse(TopModelDetectionDF_single[, 1] <= Thresholds[a], 'Noise', 'CrestedGibbons')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything'
  )

  # Extract F1 score, Precision, and Recall from confusion matrix
  F1 <- caretConf$byClass[7]  # F1 score
  Precision <- caretConf$byClass[5]  # Precision
  Recall <- caretConf$byClass[6]  # Recall

  # Store the results for each threshold
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonSVMBinary <- rbind.data.frame(BestF1data.frameCrestedGibbonSVMBinary, TempF1Row)
}

# Step 8: Calculate AUC (Area Under the Curve)
AUCSVMBinary <- auc(roc(response = TestPredictionsProb$ActualLabel,
                        predictor = TestPredictionsProb[, 1],
                        levels = c("Noise", "CrestedGibbons"), direction = "<"))
AUCSVMBinary <- round(as.numeric(AUCSVMBinary), 2)

pos <- TestPredictionsProb[, 1][TestPredictionsProb$ActualLabel == "CrestedGibbons"]
neg <- TestPredictionsProb[, 1][TestPredictionsProb$ActualLabel == "Noise"]

AUCSVMBinary_prec <- round(pr.curve(scores.class0 = pos, scores.class1 = neg)$auc.integral, 2)

# Add AUC to the results data frame
BestF1data.frameCrestedGibbonSVMBinary$AUC <- AUCSVMBinary
BestF1data.frameCrestedGibbonSVMBinary$AUC_prec <- AUCSVMBinary_prec

# Step 9: View the best F1 score and corresponding threshold
BestF1data.frameCrestedGibbonSVMBinary
max(na.omit(BestF1data.frameCrestedGibbonSVMBinary$F1))
BestF1data.frameCrestedGibbonSVMBinary[which.max(na.omit(BestF1data.frameCrestedGibbonSVMBinary$F1)),]

# Step 10: Save the results to a CSV file
write.csv(BestF1data.frameCrestedGibbonSVMBinary, 'data/BestF1data.frameCrestedGibbonSVMBinary_v1.csv', row.names = FALSE)

# Step 11: Plot F1 score, Precision, and Recall against confidence thresholds
MaxF1SVM <- round(max(na.omit(BestF1data.frameCrestedGibbonSVMBinary$F1)), 2)
CrestedGibbonSVMBinaryPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVMBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (SVM + MFCC) \n", 'Max F1=', MaxF1SVM),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(color  = "Metric", linetype = "Metric", shape = "Metric") +
  ylim(0, 1)

# Step 12: Display the plot
CrestedGibbonSVMBinaryPlot


# SVM Multi train model ---------------------------------------------------
# Step 1: Load and Prepare the Multi-Class Training Data
JahooMFCCDFTrain <- read.csv('data/JahooMFCCDFTrainMulti.csv')  # Load the multi-class MFCC features for training
JahooMFCCDFTrain$class <- as.factor(JahooMFCCDFTrain$class)  # Convert class labels to factor (e.g., CrestedGibbons, Noise)

# Step 2: Train the SVM Model using the Training Data
ml.model.svm.jahoo <- e1071::svm(
  JahooMFCCDFTrain[, 2: (ncol(JahooMFCCDFTrain) - 1)],  # Use all features except the class label column
  JahooMFCCDFTrain$class,  # Target class variable (e.g., CrestedGibbons or Noise)
  kernel = "radial",       # Radial basis function kernel for SVM
  cross = 20,             # 20-fold cross-validation
  probability = TRUE      # Enable probability estimates for predictions
)

# Print model's total accuracy
ml.model.svm.jahoo$tot.accuracy

# Step 3: Load and Prepare the Multi-Class Test Data
JahooMFCCDFTest <- read.csv('data/JahooMFCCDFTest.csv')  # Load pre-processed MFCC features for testing
JahooMFCCDFTest$class <- as.factor(JahooMFCCDFTest$class)  # Convert class labels to factor
table(JahooMFCCDFTest$class)  # Check class distribution in the test data

# Preview the test data
head(JahooMFCCDFTest)

# Step 4: Generate Predictions on Test Data using the trained SVM Model
TestPredictions <- predict(ml.model.svm.jahoo, JahooMFCCDFTest[, 2: (ncol(JahooMFCCDFTest) - 1)], probability = TRUE)
TestPredictionsProb <- as.data.frame(attr(TestPredictions, "probabilities"))  # Extract probability predictions
TestPredictionsProb$ActualLabel <- JahooMFCCDFTest$class  # Add actual class labels to the results

# Step 5: Define Confidence Thresholds for Detection
Thresholds <- seq(0.1, 1, 0.1)

# Step 6: Create an empty data frame to store evaluation results
BestF1data.frameCrestedGibbonSVMMulti <- data.frame()

# Step 7: Loop through each threshold and evaluate model performance
for (a in 1:length(Thresholds)) {
  # Filter the predictions based on the current confidence threshold
  TopModelDetectionDF_single <- TestPredictionsProb
  TopModelDetectionDF_single$PredictedClass <- ifelse(TopModelDetectionDF_single[, 1] <= Thresholds[a], 'Noise', 'CrestedGibbons')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything'
  )

  # Extract F1 score, Precision, and Recall from confusion matrix
  F1 <- caretConf$byClass[7]  # F1 score
  Precision <- caretConf$byClass[5]  # Precision
  Recall <- caretConf$byClass[6]  # Recall

  # Store the results for each threshold
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonSVMMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonSVMMulti, TempF1Row)
}

# Step 8: Calculate AUC (Area Under the Curve)
AUCSVMMulti <- auc(roc(response = TestPredictionsProb$ActualLabel,
                       predictor = TestPredictionsProb[, 1],
                       levels = c("Noise", "CrestedGibbons"), direction = "<"))
AUCSVMMulti <- round(as.numeric(AUCSVMMulti), 2)

pos <- TestPredictionsProb[, 1][TestPredictionsProb$ActualLabel == "CrestedGibbons"]
neg <- TestPredictionsProb[, 1][TestPredictionsProb$ActualLabel == "Noise"]

AUCSVMMulti_prec <- round(pr.curve(scores.class0 = pos, scores.class1 = neg)$auc.integral, 2)

# Add AUC to the results data frame
BestF1data.frameCrestedGibbonSVMMulti$AUC <- AUCSVMMulti
# Add AUC to the results data frame
BestF1data.frameCrestedGibbonSVMMulti$AUCSVMMulti_prec <- AUCSVMMulti_prec

# Step 9: View the best F1 score and corresponding threshold
BestF1data.frameCrestedGibbonSVMMulti  # View all results
max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1))  # Maximum F1 score
BestF1data.frameCrestedGibbonSVMMulti[which.max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),]  # Best F1 score and threshold

# Step 10: Save the results to a CSV file
write.csv(BestF1data.frameCrestedGibbonSVMMulti, 'data/BestF1data.frameCrestedGibbonSVMMulti_v1.csv')  # Save results

# Step 11: Plot F1 score, Precision, and Recall against confidence thresholds
MaxF1SVM <- round(max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)), 2)
CrestedGibbonSVMMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVMMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (SVM + MFCC) \n", 'Max F1=', MaxF1SVM),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(color  = "Metric", linetype = "Metric", shape = "Metric") +
  ylim(0, 1)

# Step 12: Display the plot
CrestedGibbonSVMMultiPlot

