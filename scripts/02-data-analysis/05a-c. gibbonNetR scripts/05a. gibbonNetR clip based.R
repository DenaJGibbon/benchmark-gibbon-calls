library(gibbonNetR)

# === Define all file paths at the top ============================================

# Base directory (change this to update all paths)
base.dir <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo'

# Multiclass
wavdir.multi <- file.path('data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum')
outputdir.multi <- file.path('data/AcousticData/Jahoo_trainingdata_manual_annotations_addDanum_images')
input.data.path.multi <- file.path(base.dir, outputdir.multi)
test.data.path.multi <- file.path(base.dir, 'data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindows')
results.dir.multi <- file.path(base.dir, 'results/gibbonNetR')
trainingfolder.short.multi <- 'jahoo_multi_jitter'

# Binary
wavdir.bin <- file.path('data/AcousticData/Jahoo_trainingdata_manual_annotations')
outputdir.bin <- file.path('data/AcousticData/Jahoo_trainingdata_manual_annotations_images')
input.data.path.bin <- file.path(base.dir, outputdir.bin)
test.data.path.bin <- file.path(base.dir, 'data/AcousticData/Jahoo_testdata_clips/ImagesIgnoreWindowsBinLabels')
results.dir.bin <- file.path(base.dir, 'results/gibbonNetR')
trainingfolder.short.bin <- 'jahoo_binary_jitter'

# Example dynamic ID
d <- 3  # <-- change as needed

# === A. Train multiclass model ===================================================

dir.create(outputdir.multi, recursive = TRUE, showWarnings = FALSE)

spectrogram_images(
  trainingBasePath = wavdir.multi,
  outputBasePath = outputdir.multi,
  minfreq.khz = 0.5,
  maxfreq.khz = 3.0,
  splits = c(0.7, 0.3, 0),
  new.sampleratehz = 'NA'
)

gibbonNetR::train_CNN_multi(
  input.data.path = input.data.path.multi,
  architecture = 'resnet50',
  learning_rate = 0.001,
  test.data = test.data.path.multi,
  unfreeze.param = TRUE,
  epoch.iterations = 5,
  brightness = 1,
  contrast = 1,
  saturation = 1,
  save.model = TRUE,
  early.stop = "yes",
  output.base.path = paste0(results.dir.multi, '/', trainingfolder.short.multi),
  trainingfolder = trainingfolder.short.multi,
  noise.category = "Noise"
)

# === B. Train binary model ========================================================

dir.create(outputdir.bin, recursive = TRUE, showWarnings = FALSE)

spectrogram_images(
  trainingBasePath = wavdir.bin,
  outputBasePath = outputdir.bin,
  minfreq.khz = 0.5,
  maxfreq.khz = 3.0,
  splits = c(0.7, 0.3, 0),
  new.sampleratehz = 'NA'
)

gibbonNetR::train_CNN_binary(
  input.data.path = input.data.path.bin,
  architecture = 'resnet50',
  learning_rate = 0.001,
  test.data = test.data.path.bin,
  unfreeze.param = TRUE,
  epoch.iterations = 5,
  brightness = 1,
  contrast = 1,
  saturation = 1,
  save.model = TRUE,
  early.stop = "yes",
  output.base.path = paste0(results.dir.bin, '/', trainingfolder.short.bin),
  trainingfolder = trainingfolder.short.bin,
  positive.class = "Gibbons",
  negative.class = "Noise"
)
