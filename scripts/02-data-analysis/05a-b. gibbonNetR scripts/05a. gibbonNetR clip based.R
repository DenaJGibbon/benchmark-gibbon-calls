library(gibbonNetR)

# A. Train multiclass model ------------------------------------------------------------
Wavdir <-
  'data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum/'

OutputDir <- 'data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum_images/'

dir.create(OutputDir,recursive = TRUE)

# Create spectrograms -----------------------------------------------------

spectrogram_images(
  trainingBasePath = Wavdir,
  outputBasePath = OutputDir,
  minfreq.khz = 0.5,
  maxfreq.khz = 3.0,
  splits = c(0.7, 0.3, 0), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 'NA'
)


# Script to train the multiclass model -----------------------------------------------
input.data.path <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum_images'
test.data.path <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindows'
output.dir <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR'
trainingfolder.short <- 'jahoo_multi_jitter'

gibbonNetR::train_CNN_multi(input.data.path=input.data.path,
                            architecture = 'resnet50',
                            learning_rate = 0.001,
                            test.data=test.data.path,
                            unfreeze.param = TRUE,
                            epoch.iterations=5,
                            brightness = 1,
                            contrast = 1 ,
                            saturation = 1,
                            #list.thresholds = seq(0, 1, .1),
                            save.model = TRUE,
                            early.stop = "yes",
                            output.base.path = paste(output.dir,trainingfolder.short,'_',d,sep=''),
                            trainingfolder= 'jahoo_multi_jitter',
                            noise.category = "Noise")

# B. Train binary model ------------------------------------------------------------
WavdirBin <-
  'data/AcousticData/Jahoo_trainingdata_manual_annotations/'

OutputDirBin <- 'data/AcousticData/Jahoo_trainingdata_manual_annotations_images/'

dir.create(OutputDirBin,recursive = TRUE)

# Create spectrograms -----------------------------------------------------

spectrogram_images(
  trainingBasePath = WavdirBin,
  outputBasePath = OutputDirBin,
  minfreq.khz = 0.5,
  maxfreq.khz = 3.0,
  splits = c(0.7, 0.3, 0), # Assign proportion to training, validation, or test folders
  new.sampleratehz = 'NA'
)


# Script to train the binary model -----------------------------------------------
input.data.path.bin <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_trainingdata_manual_annotations_images/'
test.data.path.bin <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsBinLabels/'
output.dir.bin <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR/'
trainingfolder.short.bin <- 'jahoo_binary_jitter'

gibbonNetR::train_CNN_binary(input.data.path=input.data.path.bin,
                            architecture = 'resnet50',
                            learning_rate = 0.001,
                            test.data=test.data.path.bin,
                            unfreeze.param = TRUE,
                            epoch.iterations=5,
                            brightness = 1,
                            contrast = 1 ,
                            saturation = 1,
                            #list.thresholds = seq(0, 1, .1),
                            save.model = TRUE,
                            early.stop = "yes",
                            output.base.path = paste(output.dir.bin,trainingfolder.short.bin,'_',d,sep=''),
                            trainingfolder= trainingfolder.short.bin,
                            positive.class = "Gibbons",
                            negative.class = "Noise")

# # Evaluate the performance of the trained models using the test images
# trained_models_dir <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR/jahoo_binary_jitter_3_jahoo_binary_jitter_binary_unfrozen_TRUE_'
# image_data_dir <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsBinLabels/'
# output_dir <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR/jahoo_binary_jitter_3_jahoo_binary_jitter_binary_unfrozen_TRUE_/performance_tables'
#
# evaluate_trainedmodel_performance(
#   trained_models_dir = trained_models_dir,
#   image_data_dir = image_data_dir,
#   output_dir = output_dir,
#   positive.class = "Gibbons", # Label for positive class
#   negative.class = "Noise" # Label for negative class
# )
#

