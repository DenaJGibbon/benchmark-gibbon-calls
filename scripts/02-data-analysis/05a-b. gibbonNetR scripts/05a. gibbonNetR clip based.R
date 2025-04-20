library(gibbonNetR)



# Link to data ------------------------------------------------------------

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
  new.sampleratehz = 16000
)

library(gibbonNetR)
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

