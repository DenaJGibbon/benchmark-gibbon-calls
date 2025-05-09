library(ggpubr)
library(pROC)
library(plyr); library(dplyr)

# BirdNET multi -----------------------------------------------------------

ClipDetections <- list.files('results/BirdNET/multi/',
                             recursive = T,full.names = T)

ClipDetectionsShort <-  list.files('results/BirdNET/multi/',
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
  mode = 'everything')
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


AUCBirdNETMulti <- auc(roc(response = BirdNETMultiPerformanceDF$ActualLabel,
                          predictor = BirdNETMultiPerformanceDF$Confidence,
                          levels = c("Noise", "CrestedGibbons"), direction = "<"))

AUCBirdNETMulti <- round(as.numeric(AUCBirdNETMulti),2)

MaxF1BirdNETMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNETMulti$F1)),2)
# Metric plot
CrestedGibbonBirdNETMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNETMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("BirdNET Multi \n",'Max F1=', MaxF1BirdNETMulti,
                     "AUC=",AUCBirdNETMulti),
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

ClipDetections <- list.files('results/BirdNET/binary/',
           recursive = T,full.names = T)

ClipDetectionsShort <- list.files('results/BirdNET/binary/',
                             recursive = T,full.names = F)


BirdNETPerformanceDF <- data.frame()

for(a in 1: length(ClipDetections)){

 TempDF <- read.delim(ClipDetections[a])

 TempDF <-  subset(TempDF,Common.Name=='gibbon')
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

AUCBirdNETBinary <- auc(roc(response = BirdNETPerformanceDF$ActualLabel,
                           predictor = BirdNETPerformanceDF$Confidence,
                           levels = c("Noise", "CrestedGibbons"), direction = "<"))

AUCBirdNETBinary <- round(as.numeric(AUCBirdNETBinary),2)


MaxF1BirdNET <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1)),2)
# Metric plot
CrestedGibbonBirdNETPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNET, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("BirdNET Binary \n",'Max F1=', MaxF1BirdNET,
                     "AUC=",AUCBirdNETBinary),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonBirdNETPlot


# CNN Binary --------------------------------------------------------------

CrestedTopBinary <- read.csv('results/gibbonNetR/jahoo_binary_jitter_3_jahoo_binary_jitter_binary_unfrozen_TRUE_/performance_tables/jahoo_binary_jitter_5_resnet50_CNNDF.csv')
MaxF1Binary <- round(max(na.omit(CrestedTopBinary$F1)),2)
AUCBinary <- round(max(na.omit(CrestedTopBinary$AUC)),2)

CrestedGibbonCNNBinary <- ggplot(data = CrestedTopBinary, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("ResNet50 Binary \n",'Max F1=', MaxF1Binary,
                     "AUC=",AUCBinary),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)
CrestedGibbonCNNBinary


# CNN Multi ---------------------------------------------------------------

CrestedTopMulti <- read.csv('results/gibbonNetR/jahoo_multi_jitter_3_jahoo_multi_jitter_multi_unfrozen_TRUE_/performance_tables_multi/jahoo_multi_jitter_5_resnet50_TransferLearningCNNDFMultiThreshold.csv')
MaxF1Multi <- round(max(na.omit(CrestedTopMulti$F1)),2)
AUCMulti <- round(max(na.omit(CrestedTopMulti$AUC)),2)

CrestedGibbonCNNMulti <- ggplot(data = CrestedTopMulti, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("ResNet50 Multi \n",'Max F1=', MaxF1Multi,
                     "AUC=",AUCMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)


cowplot::plot_grid(CrestedGibbonCNNBinary,CrestedGibbonCNNMulti,CrestedGibbonBirdNETPlot,
                   CrestedGibbonBirdNETMultiPlot)


# SVM Binary --------------------------------------------------------------

BestF1data.frameCrestedGibbonSVM <- read.csv('data/BestF1data.frameCrestedGibbonSVMBinary.csv')
MaxF1SVM <- round(max( na.omit(BestF1data.frameCrestedGibbonSVM$F1)),2)
AUCSVMBin <- round(max( na.omit(BestF1data.frameCrestedGibbonSVM$AUC)),2)

# Metric plot
CrestedGibbonSVMPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVM, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("SVM + MFCC Binary \n",'Max F1=', MaxF1SVM,
                     "AUC=",AUCSVMBin),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMPlot


# SVM Multi ---------------------------------------------------------------

BestF1data.frameCrestedGibbonSVMMulti <- read.csv('data/BestF1data.frameCrestedGibbonSVMMulti.csv')
MaxF1SVMMulti <- round(max( na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),2)
AUCSVMMulti <- round(max( na.omit(BestF1data.frameCrestedGibbonSVMMulti$AUC)),2)

# Metric plot
CrestedGibbonSVMMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVMMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("SVM + MFCC Multi\n",'Max F1=', MaxF1SVMMulti,
                     "AUC=", AUCSVMMulti),
       x = "Confidence",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonSVMMultiPlot


# Koogu binary -------------------------------------------------------------------

ClipDetections <- list.files('results/koogu/detections',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('results/koogu/detections/',
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

AUCKooguBinary <- auc(roc(response = KooguBinaryPerformanceDF$ActualLabel,
                            predictor = KooguBinaryPerformanceDF$Score,
                            levels = c("Noise", "Gibbons"), direction = "<"))

AUCKooguBinary <- round(as.numeric(AUCKooguBinary),2)

MaxF1KooguBinary <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1)),2)
# Metric plot
CrestedGibbonKooguBinaryPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Koogu Binary \n",'Max F1=', MaxF1KooguBinary,
                     "AUC=",AUCKooguBinary),
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

ClipDetections <- list.files('results/koogu/detections_multi',
                             recursive = T,full.names = T)

ClipDetectionsShort <- list.files('results/koogu/detections_multi/',
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

AUCKooguMulti <- auc(roc(response = KooguMultiPerformanceDF$ActualLabel,
                          predictor = KooguMultiPerformanceDF$Score,
                          levels = c("Noise", "CrestedGibbons"), direction = "<"))

AUCKooguMulti <- round(as.numeric(AUCKooguMulti),2)


MaxF1KooguMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1)),2)
# Metric plot
CrestedGibbonKooguMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Koogu Multi \n",'Max F1=', MaxF1KooguMulti,
                     "AUC=",AUCKooguMulti),
       x = "Score",
       y = "Values") +
  scale_color_manual(values = c("F1" = "blue", "Precision" = "red", "Recall" = "green"),
                     labels = c("F1", "Precision", "Recall")) +
  scale_linetype_manual(values = c("F1" = "dashed", "Precision" = "dotted", "Recall" = "solid")) +
  theme_minimal() +
  theme(legend.title = element_blank())+
  labs(color  = "Guide name", linetype = "Guide name", shape = "Guide name")+ylim(0,1)

CrestedGibbonKooguMultiPlot


# Plot results  -----------------------------------------------------------
pdf('results/Figure3-performance-metrics.pdf')
cowplot::plot_grid(CrestedGibbonSVMPlot, CrestedGibbonSVMMultiPlot,
                   CrestedGibbonKooguBinaryPlot,CrestedGibbonKooguMultiPlot,
                   CrestedGibbonCNNBinary,CrestedGibbonCNNMulti,
                   CrestedGibbonBirdNETPlot,CrestedGibbonBirdNETMultiPlot,
                   nrow=4, labels=c('A)','B)','C)','D)','E)','F)','G)','H)'),
                   label_x = 0.9)
graphics.off()

