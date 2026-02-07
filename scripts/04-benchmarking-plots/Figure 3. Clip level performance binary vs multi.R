library(ggpubr)
library(pROC)
library(plyr)
library(dplyr)
library(PRROC)

# NOTE: you will want to copy the files from Zenodo into your directory, or use
# setwd() /Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo
setwd('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/')
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

AUCBirdNETMulti_prec  <- round(pr.curve(
  scores.class0 = BirdNETMultiPerformanceDF$Confidence[BirdNETMultiPerformanceDF$ActualLabel == "CrestedGibbons"],
  scores.class1 = BirdNETMultiPerformanceDF$Confidence[BirdNETMultiPerformanceDF$ActualLabel == "Noise"]
)$auc.integral, 2)

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
                     "AUC-ROC=",AUCBirdNETMulti),
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

AUCBirdNETBinary_prec <- round(pr.curve(
  scores.class0 = BirdNETPerformanceDF$Confidence[BirdNETPerformanceDF$ActualLabel == "CrestedGibbons"],
  scores.class1 = BirdNETPerformanceDF$Confidence[BirdNETPerformanceDF$ActualLabel == "Noise"]
)$auc.integral, 2)

AUCBirdNETBinary <- round(as.numeric(auc(
  roc(response = BirdNETPerformanceDF$ActualLabel,
      predictor = BirdNETPerformanceDF$Confidence,
      levels = c("Noise", "CrestedGibbons"),
      direction = "<"))), 2)



MaxF1BirdNET <- round(max(na.omit(BestF1data.frameCrestedGibbonBirdNET$F1)),2)
# Metric plot
CrestedGibbonBirdNETPlot <- ggplot(data = BestF1data.frameCrestedGibbonBirdNET, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("BirdNET Binary \n",'Max F1=', MaxF1BirdNET,
                     "AUC-ROC=",AUCBirdNETBinary),
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

# Read data
CrestedTopBinary_preds <- read.csv("results/gibbonNetR/jahoo_binary_jitter_3_jahoo_binary_jitter_binary_unfrozen_TRUE_/_jahoo_binary_jitter_5_resnet50_output_TrainedModel_testdata.csv")

# Ensure labels are correct factors
CrestedTopBinary_preds$ActualClass <- factor(CrestedTopBinary_preds$ActualClass, levels = c("Noise", "Gibbons"))

# --- PR AUC ---
pos <- CrestedTopBinary_preds$Probability[CrestedTopBinary_preds$ActualClass == "Gibbons"]
neg <- CrestedTopBinary_preds$Probability[CrestedTopBinary_preds$ActualClass == "Noise"]

AUCBinary_prec  <- round(pr.curve(scores.class0 = pos, scores.class1 = neg)$auc.integral, 3)
AUCBinary

CrestedGibbonCNNBinary <- ggplot(data = CrestedTopBinary, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("ResNet50 Binary \n",'Max F1=', MaxF1Binary,
                     "AUC-ROC=",AUCBinary),
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

CrestedTopMulti_preds <- read.csv('results/gibbonNetR/jahoo_multi_jitter_3_jahoo_multi_jitter_multi_unfrozen_TRUE_/_jahoo_multi_jitter_5_resnet50_output_TrainedModel_testdata.csv')

# positives = CrestedGibbons; negatives = (Noise or GreyGibbons)
pos <- CrestedTopMulti_preds$CrestedGibbons[CrestedTopMulti_preds$ActualClass == "CrestedGibbons"]
neg <- CrestedTopMulti_preds$CrestedGibbons[CrestedTopMulti_preds$ActualClass != "CrestedGibbons"]

# drop non-finite just in case
pos <- pos[is.finite(pos)]; neg <- neg[is.finite(neg)]

AUCMulti_prec <- round(pr.curve(scores.class0 = pos, scores.class1 = neg)$auc.integral, 3)
AUCMulti_prec

AUCMulti

CrestedGibbonCNNMulti <- ggplot(data = CrestedTopMulti, aes(x = Threshold)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("ResNet50 Multi \n",'Max F1=', MaxF1Multi,
                     "AUC-ROC=",AUCMulti),
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

BestF1data.frameCrestedGibbonSVM <- read.csv('data/BestF1data.frameCrestedGibbonSVMBinary_v1.csv')
MaxF1SVM <- round(max( na.omit(BestF1data.frameCrestedGibbonSVM$F1)),2)
AUCSVMBin <- round(max( na.omit(BestF1data.frameCrestedGibbonSVM$AUC)),2)
AUCSVMBin_prec <- round(max( na.omit(BestF1data.frameCrestedGibbonSVM$AUC_prec)),2)

# Metric plot
CrestedGibbonSVMPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVM, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("SVM + MFCC Binary \n",'Max F1=', MaxF1SVM,
                     "AUC-ROC=",AUCSVMBin),
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

BestF1data.frameCrestedGibbonSVMMulti <- read.csv('data/BestF1data.frameCrestedGibbonSVMMulti_v1.csv')
MaxF1SVMMulti <- round(max( na.omit(BestF1data.frameCrestedGibbonSVMMulti$F1)),2)
AUCSVMMulti <- round(max( na.omit(BestF1data.frameCrestedGibbonSVMMulti$AUC)),2)
AUCSVMMulti_prec <- round(max( na.omit(BestF1data.frameCrestedGibbonSVMMulti$AUCSVMMulti_prec)),2)

# Metric plot
CrestedGibbonSVMMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonSVMMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("SVM + MFCC Multi\n",'Max F1=', MaxF1SVMMulti,
                     "AUC-ROC=", AUCSVMMulti),
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

ClipDetections <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/koogu_updated/binary/detections/',
                             recursive = T,full.names = T, pattern = '.txt')

ClipDetectionsShort <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/koogu_updated/binary/detections/',
                                  recursive = T,full.names = F, pattern = '.txt')

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

pos <- KooguBinaryPerformanceDF$Score[KooguBinaryPerformanceDF$ActualLabel == "Gibbons"]
neg <- KooguBinaryPerformanceDF$Score[KooguBinaryPerformanceDF$ActualLabel == "Noise"]

AUCKooguBinary_prec <- round(pr.curve(scores.class0 = pos, scores.class1 = neg)$auc.integral, 2)


MaxF1KooguBinary <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguBinary$F1)),2)
# Metric plot
CrestedGibbonKooguBinaryPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguBinary, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Koogu Binary \n",'Max F1=', MaxF1KooguBinary,
                     "AUC-ROC=",AUCKooguBinary),
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

ClipDetections <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/koogu_updated/multi/detections/',
                             recursive = T,full.names = T,pattern = '.txt')

ClipDetectionsShort <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/koogu_updated/multi/detections/',
                                  recursive = T,full.names = F,pattern = '.txt')

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

AUCKooguMulti_prec <- round(pr.curve(
  scores.class0 = KooguMultiPerformanceDF$Score[KooguMultiPerformanceDF$ActualLabel == "CrestedGibbons"],
  scores.class1 = KooguMultiPerformanceDF$Score[KooguMultiPerformanceDF$ActualLabel == "Noise"]
)$auc.integral, 2)


AUCKooguMulti <- round(as.numeric(AUCKooguMulti),2)


MaxF1KooguMulti <- round(max(na.omit(BestF1data.frameCrestedGibbonKooguMulti$F1)),2)
# Metric plot
CrestedGibbonKooguMultiPlot <- ggplot(data = BestF1data.frameCrestedGibbonKooguMulti, aes(x = Thresholds)) +
  geom_line(aes(y = F1, color = "F1", linetype = "F1")) +
  geom_line(aes(y = Precision, color = "Precision", linetype = "Precision")) +
  geom_line(aes(y = Recall, color = "Recall", linetype = "Recall")) +
  labs(title = paste("Koogu Multi \n",'Max F1=', MaxF1KooguMulti,
                     "AUC-ROC=",sprintf("%.2f", AUCKooguMulti)
),
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
                   nrow=4, labels=c('A)','B)','C)','D)','E)','F)','G)','H)'))
graphics.off()


# Create table ------------------------------------------------------------

# ---- Create concise summary table with ROC + PR AUC ------------------------
library(dplyr)
library(knitr)
library(readr)

.best_row <- function(df, thr_col = "Thresholds") {
  df %>%
    filter(is.finite(F1)) %>%
    slice_max(order_by = F1, n = 1, with_ties = FALSE) %>%
    mutate(Threshold = .data[[thr_col]])
}

# Build per-model summaries
bn_bin_best <- .best_row(BestF1data.frameCrestedGibbonBirdNET, thr_col = "Thresholds") %>%
  transmute(Model = "BirdNET", Task = "Binary",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCBirdNETBinary,
            AUC_PR  = AUCBirdNETBinary_prec)

bn_multi_best <- .best_row(BestF1data.frameCrestedGibbonBirdNETMulti, thr_col = "Thresholds") %>%
  transmute(Model = "BirdNET", Task = "Multi",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCBirdNETMulti,
            AUC_PR  = AUCBirdNETMulti_prec)

cnn_bin_best <- .best_row(CrestedTopBinary, thr_col = "Threshold") %>%
  transmute(Model = "ResNet50 (gibbonNetR)", Task = "Binary",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCBinary,
            AUC_PR  = AUCBinary_prec)

cnn_multi_best <- .best_row(CrestedTopMulti, thr_col = "Threshold") %>%
  transmute(Model = "ResNet50 (gibbonNetR)", Task = "Multi",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCMulti,
            AUC_PR  = AUCMulti_prec)

svm_bin_best <- .best_row(BestF1data.frameCrestedGibbonSVM, thr_col = "Thresholds") %>%
  transmute(Model = "SVM + MFCC", Task = "Binary",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCSVMBin,
            AUC_PR  = AUCSVMBin_prec)

svm_multi_best <- .best_row(BestF1data.frameCrestedGibbonSVMMulti, thr_col = "Thresholds") %>%
  transmute(Model = "SVM + MFCC", Task = "Multi",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCSVMMulti,
            AUC_PR  = AUCSVMMulti_prec)

koogu_bin_best <- .best_row(BestF1data.frameCrestedGibbonKooguBinary, thr_col = "Thresholds") %>%
  transmute(Model = "Koogu", Task = "Binary",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCKooguBinary,
            AUC_PR  = AUCKooguBinary_prec)

koogu_multi_best <- .best_row(BestF1data.frameCrestedGibbonKooguMulti, thr_col = "Thresholds") %>%
  transmute(Model = "Koogu", Task = "Multi",
            Threshold, Precision, Recall, F1,
            AUC_ROC = AUCKooguMulti,
            AUC_PR  = AUCKooguMulti_prec)

# Combine, round, and order
BestMetricsTable <- bind_rows(
  bn_bin_best, bn_multi_best,
  cnn_bin_best, cnn_multi_best,
  svm_bin_best, svm_multi_best,
  koogu_bin_best, koogu_multi_best
) %>%
  mutate(across(c(Threshold, Precision, Recall, F1, AUC_ROC, AUC_PR),
                ~ round(as.numeric(.), 2))) %>%
  arrange(Task, desc(F1), desc(AUC_PR))

# Save + print
write_csv(BestMetricsTable, "results/Table2-best-metrics-with-PR.csv")
kable(BestMetricsTable, align = "lccccccc",
      caption = "Best threshold metrics per model (Precision, Recall, F1 at max-F1 threshold; ROC-AUC and PR-AUC).")

print(BestMetricsTable)
