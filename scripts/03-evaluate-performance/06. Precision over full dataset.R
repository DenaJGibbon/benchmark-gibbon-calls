library(stringr)
library(dplyr)
library(lubridate)
library(suncalc)
library(ggpubr)


# Precision calculation ---------------------------------------------------
ManuallyVerifiedDetects <-
  '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/manually_verified_detections/CrestedGibbons/Positive/'

ManuallyVerifiedNames <-
  basename(list.files((ManuallyVerifiedDetects), recursive = T))

ManuallyVerifiedFolder <-
  (list.files((ManuallyVerifiedDetects), recursive = T))

ManuallyVerifiedNamesSplit <- str_split_fixed(ManuallyVerifiedNames,pattern = '_',n=9)

# Extract matches
ID   <- ManuallyVerifiedNamesSplit[,5]
Date <- ManuallyVerifiedNamesSplit[,6]
Time <- ManuallyVerifiedNamesSplit[,7]

ManuallyVerifiedNamesWav <- paste(ID, Date, Time,sep='_')

TrainingData <-
  '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_trainingdata_sorted_detections/'

TrainingNames <-
  basename(list.files((TrainingData), recursive = T))

TrainingNamesSplit <- str_split_fixed(TrainingNames,pattern = '_',n=6)

# Extract matches
ID   <- TrainingNamesSplit[,1]
Date <- TrainingNamesSplit[,2]
Time <- TrainingNamesSplit[,3]

TrainingNamesWav <- paste(ID, Date, Time,sep='_')

WavsToKeep <-
  ManuallyVerifiedFolder[!(ManuallyVerifiedNamesWav %in% TrainingNamesWav)]

TP <- length(which(str_detect(WavsToKeep,'Positive')))
FP <- length(which(str_detect(WavsToKeep,'Negative')))

TP / (TP+FP)


