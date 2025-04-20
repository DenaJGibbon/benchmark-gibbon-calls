library(gibbonR)
library(ggplot2)

JahooMFCCDF <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations/Gibbons/',
                            min.freq = 400,
                            max.freq = 3000,
                            n.windows = 9,
                            num.cep = 12,
                            win.avg = 'standard',
                            win.hop.time = 0.25)


JahooMFCCDFNoise <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations/Noise/',
                                 min.freq = 400,
                                 max.freq = 3000,
                                 n.windows = 9,
                                 num.cep = 12,
                                 win.avg = 'standard',
                                 win.hop.time = 0.25)



JahooMFCCDFTrain <- rbind.data.frame(JahooMFCCDF,JahooMFCCDFNoise)

JahooMFCCDFTrain$class <- as.factor(JahooMFCCDFTrain$class )

#write.csv(JahooMFCCDFTrain,'data/JahooMFCCDFTrain.csv')

ml.model.svm.jahoo <-
  e1071::svm(
    JahooMFCCDFTrain[, 2: (ncol(JahooMFCCDFTrain) -1)],
    JahooMFCCDFTrain$class,
    kernel = "radial",
    #gamma = tune.rad$best.parameters$gamma,
    #cost = tune.rad$best.parameters$cost,
    cross = 20,
    probability = TRUE
  )

ml.model.svm.jahoo$tot.accuracy


JahooTestMFCCDF <- MFCCFunction(input.dir='data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsWavs/CrestedGibbons/',
                                min.freq = 400,
                                max.freq = 3000,
                                n.windows = 9,
                                num.cep = 12,
                                win.avg = 'standard',
                                win.hop.time = 0.25)


JahooMFCCDFTestNoise <- MFCCFunction(input.dir='data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsWavs/Noise/',
                                     min.freq = 400,
                                     max.freq = 3000,
                                     n.windows = 9,
                                     num.cep = 12,
                                     win.avg = 'standard',
                                     win.hop.time = 0.25)

JahooMFCCDFTest <- rbind.data.frame(JahooTestMFCCDF,JahooMFCCDFTestNoise)

JahooMFCCDFTest$class <- as.factor(JahooMFCCDFTest$class )
table(JahooMFCCDFTest$class)

head(JahooMFCCDFTest)

TestPredictions <- predict( ml.model.svm.jahoo,JahooMFCCDFTest[, 2: (ncol(JahooMFCCDFTest) -1)], probability = TRUE)
TestPredictionsProb <- as.data.frame(attr(TestPredictions, "probabilities"))
TestPredictionsProb$ActualLabel <-JahooMFCCDFTest$class


# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonSVMMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-TestPredictionsProb

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single[,1]  <=Thresholds[a], 'Noise','CrestedGibbons')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything')


  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameCrestedGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonSVMMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonSVMMulti, TempF1Row)
}


BestF1data.frameCrestedGibbonSVMMulti
max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1))
BestF1data.frameCrestedGibbonSVMMulti[which.max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),]


MaxF1SVM <- round(max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),2)
# Metric plot
CrestedGibbonSVMMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVMMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (SVM + MFCC) \n",'Max F1=', MaxF1SVM),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMMultiPlot

# Multi-class -------------------------------------------------------------
JahooMFCCDFCrested <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum/CrestedGibbons',
                            min.freq = 400,
                            max.freq = 3000,
                            n.windows = 9,
                            num.cep = 12,
                            win.avg = 'standard',
                            win.hop.time = 0.25)

JahooMFCCDFCrested$class <- 'CrestedGibbons'

JahooMFCCDFGrey<- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum/GreyGibbons',
                                   min.freq = 400,
                                   max.freq = 3000,
                                   n.windows = 9,
                                   num.cep = 12,
                                   win.avg = 'standard',
                                   win.hop.time = 0.25)

JahooMFCCDFGrey$class <- 'GreyGibbons'

JahooMFCCDFNoise <- MFCCFunction(input.dir='data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum/Noise',
                                 min.freq = 400,
                                 max.freq = 3000,
                                 n.windows = 9,
                                 num.cep = 12,
                                 win.avg = 'standard',
                                 win.hop.time = 0.25)



JahooMFCCDFTrain <- rbind.data.frame(JahooMFCCDFCrested,JahooMFCCDFGrey,JahooMFCCDFNoise)

#write.csv(JahooMFCCDFTrain,'data/JahooMFCCDFTrainMulti.csv')

JahooMFCCDFTrain$class <- as.factor(JahooMFCCDFTrain$class )

ml.model.svm.jahoo <-
  e1071::svm(
    JahooMFCCDFTrain[, 2: (ncol(JahooMFCCDFTrain) -1)],
    JahooMFCCDFTrain$class,
    kernel = "radial",
    # gamma = tune.rad$best.parameters$gamma,
    # cost = tune.rad$best.parameters$cost,
    cross = 20,
    probability = TRUE
  )

ml.model.svm.jahoo$tot.accuracy


JahooTestMFCCDF <- MFCCFunction(input.dir='data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsWavs/CrestedGibbons/',
                                min.freq = 400,
                                max.freq = 3000,
                                n.windows = 9,
                                num.cep = 12,
                                win.avg = 'standard',
                                win.hop.time = 0.25)


JahooMFCCDFTestNoise <- MFCCFunction(input.dir='data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsWavs/Noise/',
                                     min.freq = 400,
                                     max.freq = 3000,
                                     n.windows = 9,
                                     num.cep = 12,
                                     win.avg = 'standard',
                                     win.hop.time = 0.25)

JahooMFCCDFTest <- rbind.data.frame(JahooTestMFCCDF,JahooMFCCDFTestNoise)
#write.csv(JahooMFCCDFTest,'data/JahooMFCCDFTest.csv',row.names = F)
JahooMFCCDFTest$class <- as.factor(JahooMFCCDFTest$class )
table(JahooMFCCDFTest$class)

head(JahooMFCCDFTest)

TestPredictions <- predict( ml.model.svm.jahoo,JahooMFCCDFTest[, 2: (ncol(JahooMFCCDFTest) -1)], probability = TRUE)
TestPredictionsProb <- as.data.frame(attr(TestPredictions, "probabilities"))
TestPredictionsProb$ActualLabel <-JahooMFCCDFTest$class


# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.1)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonSVMMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-TestPredictionsProb

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single[,1]  <=Thresholds[a], 'Noise','CrestedGibbons')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    mode = 'everything')


  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameCrestedGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonSVMMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonSVMMulti, TempF1Row)
}


BestF1data.frameCrestedGibbonSVMMulti
max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1))
BestF1data.frameCrestedGibbonSVMMulti[which.max(na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),]

#write.csv(BestF1data.frameCrestedGibbonSVMMulti,'data/BestF1data.frameCrestedGibbonSVMMulti.csv')
