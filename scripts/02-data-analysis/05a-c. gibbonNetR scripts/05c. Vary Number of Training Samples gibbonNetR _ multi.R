# Load necessary packages and functions -----------------------------------
library(stringr)
library(gibbonNetR)

# ---- SET BASE DIRECTORY HERE ----
# This is the only line you need to edit to reconfigure the script
base_dir <- "/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/"


# Define all other paths relative to base_dir -----------------------------
input_clips_dir <- file.path(base_dir, "randomization/JahooGibbonClipsRandomMulti")
spectrogram_output_dir <- file.path(base_dir, "/randomization/JahooGibbonClipsRandomMultiImages/")
test_data_path <- file.path(base_dir, "data/AcousticData/Dakrong_testdata_images_multi/")
model_output_dir <- file.path(base_dir, "results/gibbonNetR/randomization_multi/")
sound_files_dir <- file.path(base_dir, "data/AcousticData/Jahoo_testdata_1hr_files/")
deployment_output_base <- file.path(base_dir, "results/gibbonNetR/randomization_multi_results")

# Create spectrograms -----------------------------------------------------
# NOTE: Only need to run this one time
# TempFolder <- list.files(input_clips_dir, full.names = TRUE)
#
# for (a in 1:length(TempFolder)) {
#   TempPath <- basename(TempFolder[a])
#
#   spectrogram_images(
#     trainingBasePath = TempFolder[a],
#     outputBasePath = file.path(spectrogram_output_dir, TempPath),
#     minfreq.khz = 0.5,
#     maxfreq.khz = 3.0,
#     splits = c(0.7, 0.3, 0),
#     new.sampleratehz = 16000
#   )
# }

# Train CNNs --------------------------------------------------------------

ListRandomFolders <- list.files(spectrogram_output_dir, full.names = TRUE)

for (b in c(1:length(ListRandomFolders))) {
  input.data.path <- ListRandomFolders[b]
  trainingfolder.short <- basename(input.data.path)
  Nsamplenumeric <- as.numeric(str_split_fixed(trainingfolder.short, 'samples', 2)[, 1])
  batch_size <- round(Nsamplenumeric * 0.3, 0)
  epoch.iterations <- c(5)

  gibbonNetR::train_CNN_multi(
    input.data.path = input.data.path,
    architecture = 'resnet50',
    learning_rate = 0.001,
    test.data = test_data_path,
    batch_size = batch_size,
    class_weights = c(0.33, 0.33, 0.33),
    unfreeze.param = TRUE,
    epoch.iterations = epoch.iterations,
    brightness = 1,
    contrast = 1,
    saturation = 1,
    save.model = TRUE,
    early.stop = "yes",
    output.base.path = model_output_dir,
    trainingfolder = trainingfolder.short,
    noise.category = "noise"
  )
}

# Deploy trained models ---------------------------------------------------

ModelPath <- list.files(model_output_dir, full.names = TRUE, recursive = TRUE)
ModelList <- ModelPath[str_detect(ModelPath, ".pt")]

for (k in 1:length(ModelList)) {
  tryCatch({
    print(k)
    model_path <- ModelList[k]
    model_name <- str_split_fixed(basename(model_path), "_model", n = 2)[, 1]
    output_folder <- file.path(deployment_output_base, model_name)

    deploy_CNN_multi(
      clip_duration = 12,
      architecture = 'resnet50',
      output_folder = file.path(output_folder, "Images/"),
      output_folder_selections = file.path(output_folder, "Selections/"),
      output_folder_wav = file.path(output_folder, "Wavs/"),
      detect_pattern = NA,
      top_model_path = model_path,
      path_to_files = sound_files_dir,
      downsample_rate = 'NA',
      save_wav = FALSE,
      class_names = c('CrestedGibbon', 'GreyGibbon', 'noise'),
      noise_category = 'noise',
      single_class = TRUE,
      single_class_category = 'CrestedGibbon',
      threshold = 0.1,
      max_freq_khz = 3
    )
  }, error = function(e) {
    cat("ERROR:", conditionMessage(e), "\n")
  })
}


# Add full training samples -----------------------------------------------
input.data.path <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/randomization/JahooGibbonClipsRandomMultiImages/213samples_1'
trainingfolder.short <- basename(input.data.path)
Nsamplenumeric <- as.numeric(str_split_fixed(trainingfolder.short, 'samples', 2)[, 1])
batch_size <- round(Nsamplenumeric * 0.3, 0)
epoch.iterations <- c(5)

gibbonNetR::train_CNN_multi(
  input.data.path = input.data.path,
  architecture = 'resnet50',
  learning_rate = 0.001,
  test.data = test_data_path,
  batch_size = batch_size,
  class_weights = c(0.33, 0.33, 0.33),
  unfreeze.param = TRUE,
  epoch.iterations = epoch.iterations,
  brightness = 1,
  contrast = 1,
  saturation = 1,
  save.model = TRUE,
  early.stop = "yes",
  output.base.path = model_output_dir,
  trainingfolder = trainingfolder.short,
  noise.category = "noise"
)


model_path <- list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/results/gibbonNetR/randomization_multi/_213samples_1_multi_unfrozen_TRUE_',
           pattern = '.pt',full.names = TRUE)

model_name <- str_split_fixed(basename(model_path), "_model", n = 2)[, 1]
output_folder <- file.path(deployment_output_base, model_name)

deploy_CNN_multi(
  clip_duration = 12,
  architecture = 'resnet50',
  output_folder = file.path(output_folder, "Images/"),
  output_folder_selections = file.path(output_folder, "Selections/"),
  output_folder_wav = file.path(output_folder, "Wavs/"),
  detect_pattern = NA,
  top_model_path = model_path,
  path_to_files = sound_files_dir,
  downsample_rate = 'NA',
  save_wav = FALSE,
  class_names = c('CrestedGibbon', 'GreyGibbon', 'noise'),
  noise_category = 'noise',
  single_class = TRUE,
  single_class_category = 'CrestedGibbon',
  threshold = 0.1,
  max_freq_khz = 3
)
