library(ggpubr)
library(ROCR)


# BirdNET Multi -----------------------------------------------------------

ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonAddDanum',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonAddDanum/',
                                  recursive = T,full.names = F)

BirdNETMultiPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])

  TempDF <-  subset(TempDF,Common.Name=='CrestedGibbons')
  # Find the highest confidence for each clip
  if(nrow(TempDF) > 0){
    Confidence <- max(TempDF$Confidence)
    TempDF <- TempDF[which.max(TempDF$Confidence),]
    ActualLabel <- dirname(ClipDetectionsShort[a])
  } else{

    Confidence <- 0

    ActualLabel <- dirname(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Confidence, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$BirdNETMultiBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','CrestedGibbons')
  BirdNETMultiPerformanceDF <- rbind.data.frame(BirdNETMultiPerformanceDF,TempRow)
}


caretConf <- caret::confusionMatrix(
  as.factor(BirdNETMultiPerformanceDF$BirdNETMultiBinary),
  as.factor(BirdNETMultiPerformanceDF$ActualLabel),
  mode = 'everything',positive = 'CrestedGibbons')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNETMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETMultiPerformanceDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','CrestedGibbons')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    positive = 'CrestedGibbons',
    mode = 'everything')


  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonBirdNETMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNETMulti, TempF1Row)
}

BestF1data.frameCrestedGibbonBirdNETMulti
max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1))
BestF1data.frameCrestedGibbonBirdNETMulti[which.max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1)),]

BirdNETMultiPerformanceDF$BinaryLabel <-
  ifelse(BirdNETMultiPerformanceDF$ActualLabel=='CrestedGibbons',1,0)

ROCRpredBirdNETMulti <- ROCR::prediction(predictions = as.numeric(BirdNETMultiPerformanceDF$Confidence),
                             labels = as.factor(BirdNETMultiPerformanceDF$BinaryLabel))
BirdNETMulti_AUC <- ROCR::performance(ROCRpredBirdNETMulti, "auc")
BirdNETMulti_AUC <- round(as.numeric(BirdNETMulti_AUC@y.values),2)
BirdNETMulti_AUC

MaxF1BirdNETMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1)),2)
# Metric plot
CrestedGibbonBirdNETMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNETMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (BirdNET Multi) \n",'Max F1=', MaxF1BirdNETMulti,'AUC=', BirdNETMulti_AUC),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETMultiPlot


# BirdNET binary ----------------------------------------------------------


ClipDetections <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonIgnoreWindows/',
           recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Volumes/DJC Files/MultiSpeciesTransferLearning/WideArrayEvaluation/Jahoo/BirdNETComparisonIgnoreWindows/',
                             recursive = T,full.names = F)


BirdNETPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

 TempDF <- read.delim(ClipDetections[a])

 TempDF <-  subset(TempDF,Common.Name=='Gibbons')
 # Find the highest confidence for each clip
 if(nrow(TempDF) > 0){
 Confidence <- max(TempDF$Confidence)
 TempDF <- TempDF[which.max(TempDF$Confidence),]
 ActualLabel <- dirname(ClipDetectionsShort[a])
 } else{

   Confidence <- 0

   ActualLabel <- dirname(ClipDetectionsShort[a])
 }

 TempRow <- cbind.data.frame(Confidence, ActualLabel)
 TempRow$FileName <-ClipDetectionsShort[a]
 TempRow$BirdNETBinary <- ifelse(TempRow$Confidence <= 0.5, 'Noise','CrestedGibbons')
 BirdNETPerformanceDF <- rbind.data.frame(BirdNETPerformanceDF,TempRow)
}

#subset(BirdNETPerformanceDF,ActualLabel=='Noise'& BirdNETBinary=='CrestedGibbons')

caretConf <- caret::confusionMatrix(
  as.factor(BirdNETPerformanceDF$BirdNETBinary),
  as.factor(BirdNETPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of confidence Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonBirdNET <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the confidence threshold
  TopModelDetectionDF_single <-BirdNETPerformanceDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Confidence  <=Thresholds[a], 'Noise','CrestedGibbons')

  # Calculate confusion matrix using caret package
  caretConf <- caret::confusionMatrix(
    as.factor(TopModelDetectionDF_single$PredictedClass),
    as.factor(TopModelDetectionDF_single$ActualLabel),
    positive = 'CrestedGibbons',
    mode = 'everything')


  # Extract F1 score, Precision, and Recall from the confusion matrix
  F1 <- caretConf$byClass[7]
  Precision <- caretConf$byClass[5]
  Recall <- caretConf$byClass[6]
  FP <- caretConf$table[1,2]
  # TN <- caretConf$table[2,2]+JahooAdj
  # FPR <-  FP / (FP + TN)
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonBirdNET <- rbind.data.frame(BestF1data.frameCrestedGibbonBirdNET, TempF1Row)
}

BestF1data.frameCrestedGibbonBirdNET
max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1))
BestF1data.frameCrestedGibbonBirdNET[which.max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1)),]

BirdNETPerformanceDF$BinaryLabel <-
  ifelse(BirdNETPerformanceDF$ActualLabel=='CrestedGibbons',1,0)

ROCRpredBirdNETBinary <- ROCR::prediction(predictions = as.numeric(BirdNETPerformanceDF$Confidence),
                                         labels = as.factor(BirdNETPerformanceDF$BinaryLabel))
BirdNETBinary_AUC <- ROCR::performance(ROCRpredBirdNETBinary, "auc")
BirdNETBinary_AUC <- round(as.numeric(BirdNETBinary_AUC@y.values),2)
BirdNETBinary_AUC

MaxF1BirdNET <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1)),2)
# Metric plot
CrestedGibbonBirdNETPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNET, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (BirdNET Binary) \n",'Max F1=', MaxF1BirdNET,'AUC=', BirdNETBinary_AUC),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETPlot

# Binary gibbonNetR ------------------------------------------------

CrestedTopBinary <- read.csv('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR/jahoo_binary_jitter_3_jahoo_binary_jitter_binary_unfrozen_TRUE_/performance_tables/jahoo_binary_jitter_5_resnet50_CNNDF.csv')
MaxF1Binary <- round(max(na.omit(CrestedTopBinary$F1)),2)
AUCBinCNN <- round(CrestedTopBinary$AUC[1],2)

CrestedGibbonCNNBinary <- ggplot(data = CrestedTopBinary, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (ResNet50 Binary) \n",'Max F1=', MaxF1Binary,'AUC=',AUCBinCNN),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)


# Multiclass gibbonNetR ---------------------------------------------------

CrestedTopMulti <- read.csv('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR/jahoo_multi_jitter_3_jahoo_multi_jitter_multi_unfrozen_TRUE_/performance_tables_multi/jahoo_multi_jitter_5_resnet50_TransferLearningCNNDFMultiThreshold.csv')
MaxF1Multi <- round(max(na.omit(CrestedTopMulti$F1)),2)
AUCMultiCNN <- round(CrestedTopMulti$AUC[1],2)

CrestedGibbonCNNMulti <- ggplot(data = CrestedTopMulti, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (ResNet50 Multi) \n",'Max F1=', MaxF1Multi, 'AUC=',AUCMultiCNN),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)


cowplot::plot_grid(CrestedGibbonCNNBinary,CrestedGibbonCNNMulti,CrestedGibbonBirdNETPlot,CrestedGibbonBirdNETMultiPlot)


BestF1data.frameCrestedGibbonSVM <- read.csv('data/BestF1data.frameCrestedGibbonSVM.csv')
MaxF1SVM <- round(max( na.omit(BestF1data.frameCrestedGibbonSVM$F1)),2)

# Metric plot
CrestedGibbonSVMPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVM, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons Binary (SVM + MFCC) \n",'Max F1=', MaxF1SVM),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMPlot


BestF1data.frameCrestedGibbonSVMMulti <- read.csv('data/BestF1data.frameCrestedGibbonSVMMulti.csv')
MaxF1SVMMulti <- round(max( na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),2)

# Metric plot
CrestedGibbonSVMMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVMMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons Multi (SVM + MFCC) \n",'Max F1=', MaxF1SVMMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMMultiPlot


# Koogu binary  -----------------------------------------------------------
ClipDetections <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections/',
                                  recursive = T,full.names = F)

KooguBinaryPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])

  TempDF <-  subset(TempDF,Tags=='Gibbons')
  # Find the highest Score for each clip
  if(nrow(TempDF) > 0){
    Score <- max(TempDF$Score)
    TempDF <- TempDF[which.max(TempDF$Score),]
    ActualLabel <-dirname(ClipDetectionsShort[a])
  } else{

    Score <- 0

    ActualLabel <- dirname(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Score, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$KooguBinaryBinary <- ifelse(TempRow$Score <= 0.5, 'Noise','Gibbons')
  KooguBinaryPerformanceDF <- rbind.data.frame(KooguBinaryPerformanceDF,TempRow)
}

head(KooguBinaryPerformanceDF)

KooguBinaryPerformanceDF$ActualLabel <- revalue(KooguBinaryPerformanceDF$ActualLabel, c(CrestedGibbons = "Gibbons"))

caretConf <- caret::confusionMatrix(
  as.factor(KooguBinaryPerformanceDF$KooguBinaryBinary),
  as.factor(KooguBinaryPerformanceDF$ActualLabel),
  mode = 'everything')
caretConf

# Define a vector of Score Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonKooguBinary <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the Score threshold
  TopModelDetectionDF_single <-KooguBinaryPerformanceDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Score  <=Thresholds[a], 'Noise','Gibbons')

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
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonKooguBinary <- rbind.data.frame(BestF1data.frameCrestedGibbonKooguBinary, TempF1Row)
}

BestF1data.frameCrestedGibbonKooguBinary
max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1))
BestF1data.frameCrestedGibbonKooguBinary[which.max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1)),]

KooguBinaryPerformanceDF$BinaryLabel <-
  ifelse(KooguBinaryPerformanceDF$ActualLabel=='Gibbons',1,0)

ROCRpredKooguBinary <- ROCR::prediction(predictions = as.numeric(KooguBinaryPerformanceDF$Score),
                                         labels = as.factor(KooguBinaryPerformanceDF$BinaryLabel))
KooguBinary_AUC <- ROCR::performance(ROCRpredKooguBinary, "auc")
KooguBinary_AUC <- round(as.numeric(KooguBinary_AUC@y.values),2)
KooguBinary_AUC


MaxF1KooguBinary <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1)),2)
# Metric plot
CrestedGibbonKooguBinaryPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (Koogu Binary) \n",'Max F1=', MaxF1KooguBinary, 'AUC=',KooguBinary_AUC),
       x = "Score",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonKooguBinaryPlot

# Koogu gibbon multi ----------------------------------------------------

ClipDetections <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections_multi',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('/Users/denaclink/Desktop/VSCodeRepos/BEANS/detections_multi/',
                                  recursive = T,full.names = F)

KooguMultiPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

  TempDF <- read.delim(ClipDetections[a])

  TempDF <-  subset(TempDF,Tags=='CrestedGibbons')
  # Find the highest Score for each clip
  if(nrow(TempDF) > 0){
    Score <- max(TempDF$Score)
    TempDF <- TempDF[which.max(TempDF$Score),]
    ActualLabel <-dirname(ClipDetectionsShort[a])
  } else{

    Score <- 0

    ActualLabel <- dirname(ClipDetectionsShort[a])
  }

  TempRow <- cbind.data.frame(Score, ActualLabel)
  TempRow$FileName <-ClipDetectionsShort[a]
  TempRow$KooguMultiMulti <- ifelse(TempRow$Score <= 0.5, 'Noise','CrestedGibbons')
  KooguMultiPerformanceDF <- rbind.data.frame(KooguMultiPerformanceDF,TempRow)
}

tail(KooguMultiPerformanceDF)


caretConf <- caret::confusionMatrix(
  as.factor(KooguMultiPerformanceDF$KooguMultiMulti),
  as.factor(KooguMultiPerformanceDF$ActualLabel),
  mode = 'everything')

caretConf

# Define a vector of Score Thresholds
Thresholds <- seq(0.1,1,0.005)

# Create an empty data frame to store results
BestF1data.frameCrestedGibbonKooguMulti <- data.frame()

# Loop through each threshold value
for(a in 1:length(Thresholds)){

  # Filter the subset based on the Score threshold
  TopModelDetectionDF_single <-KooguMultiPerformanceDF

  TopModelDetectionDF_single$PredictedClass <-
    ifelse(TopModelDetectionDF_single$Score  <=Thresholds[a], 'Noise','CrestedGibbons')

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
  # # Create a row for the result and add it to the BestF1data.frameGreyGibbon
  #TrainingData <- training_data_type
  TempF1Row <- cbind.data.frame(F1, Precision, Recall)#,FPR
  TempF1Row$Thresholds <- Thresholds[a]
  BestF1data.frameCrestedGibbonKooguMulti <- rbind.data.frame(BestF1data.frameCrestedGibbonKooguMulti, TempF1Row)
}

BestF1data.frameCrestedGibbonKooguMulti
max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1))
BestF1data.frameCrestedGibbonKooguMulti[which.max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1)),]

KooguMultiPerformanceDF$MultiLabel <-
  ifelse(KooguMultiPerformanceDF$ActualLabel=='CrestedGibbons',1,0)

ROCRpredKooguMulti <- ROCR::prediction(predictions = as.numeric(KooguMultiPerformanceDF$Score),
                                        labels = as.factor(KooguMultiPerformanceDF$MultiLabel))
KooguMulti_AUC <- ROCR::performance(ROCRpredKooguMulti, "auc")
KooguMulti_AUC <- round(as.numeric(KooguMulti_AUC@y.values),2)
KooguMulti_AUC


MaxF1KooguMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1)),2)
# Metric plot
CrestedGibbonKooguMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Crested Gibbons (Koogu Multi) \n",'Max F1=', MaxF1KooguMulti, 'AUC'=KooguMulti_AUC),
       x = "Score",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonKooguMultiPlot

cowplot::plot_grid(#CrestedGibbonSVMPlot, CrestedGibbonSVMMultiPlot,
                   CrestedGibbonKooguBinaryPlot,CrestedGibbonKooguMultiPlot,
                   CrestedGibbonCNNBinary,CrestedGibbonCNNMulti,
                   CrestedGibbonBirdNETPlot,CrestedGibbonBirdNETMultiPlot,
                   nrow=4, labels=c('A)','B)','C)','D)','E)','F)','G)','H)'),
                   label_x = 0.9)

