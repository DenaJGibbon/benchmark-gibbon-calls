library(gibbonNetR)
library(stringr)

# ------------------------ #
#        BASE PATH         #
# ------------------------ #

base_dir <- "/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo"

# ------------------------ #
#        PATH SETUP        #
# ------------------------ #

input_clips_path <- file.path(base_dir, "randomization/JahooGibbonClipsRandom")
spectrogram_output_path <- "/Volumes/DJC Files/JahooClipsRandomImages/"  # not under base_dir
image_training_path <- file.path(base_dir, "randomization/JahooGibbonClipsRandomImages")
image_test_path <- file.path(base_dir, "data/AcousticData/Dakrong_testdata_images/")
training_output_path <- file.path(base_dir, "results/gibbonNetR/randomization_binary")
deployment_output_base <- file.path(base_dir, "results/gibbonNetR/randomization_binary_results")
deployment_input_wavs <- file.path(base_dir, "data/AcousticData/Jahoo_testdata_1hr_files")

# ------------------------ #
#  SPECTROGRAM GENERATION  #
# (optional - run once)    #
# ------------------------ #

# spectro_folders <- list.files(input_clips_path, full.names = TRUE)
# for (i in seq_along(spectro_folders)) {
#   folder_name <- basename(spectro_folders[i])
#   output_path <- file.path(spectrogram_output_path, folder_name)
#   spectrogram_images(
#     trainingBasePath = spectro_folders[i],
#     outputBasePath = output_path,
#     minfreq.khz = 0.5,
#     maxfreq.khz = 3.0,
#     splits = c(0.7, 0.3, 0),
#     new.sampleratehz = 'NA'
#   )
# }

# ------------------------ #
#     TRAINING MODELS      #
# ------------------------ #

train_folders <- list.files(image_training_path, full.names = TRUE)

for (i in seq_along(train_folders)) {
  train_folder <- train_folders[i]
  training_name <- basename(train_folder)
  n_samples <- as.numeric(str_split_fixed(training_name, 'samples', 2)[,1])
  batch_size <- round(n_samples * 0.3)

  train_CNN_binary(
    input.data.path = train_folder,
    noise.weight = 0.5,
    architecture = 'resnet50',
    save.model = TRUE,
    learning_rate = 0.001,
    test.data = image_test_path,
    unfreeze.param = TRUE,
    batch_size = batch_size,
    brightness = 1,
    contrast = 1,
    saturation = 1,
    epoch.iterations = c(5),
    list.thresholds = seq(0, 1, 0.1),
    early.stop = "yes",
    output.base.path = training_output_path,
    trainingfolder = training_name,
    positive.class = "gibbon",
    negative.class = "noise"
  )
}

# ------------------------ #
#   DEPLOYING MODELS       #
# ------------------------ #

model_files <- list.files(training_output_path, pattern = "\\.pt$", recursive = TRUE, full.names = TRUE)

for (k in seq_along(model_files)) {
  model_path <- model_files[k]
  model_name <- str_split_fixed(basename(model_path), "_model", 2)[,1]
  output_folder <- file.path(deployment_output_base, model_name)

  tryCatch({
    deploy_CNN_binary(
      clip_duration = 12,
      architecture = 'resnet50',
      output_folder = file.path(output_folder, "Images"),
      output_folder_selections = file.path(output_folder, "Selections"),
      output_folder_wav = file.path(output_folder, "Wavs"),
      detect_pattern = NA,
      top_model_path = model_path,
      path_to_files = deployment_input_wavs,
      downsample_rate = 'NA',
      threshold = 0.1,
      save_wav = FALSE,
      positive.class = 'gibbon',
      negative.class = 'noise',
      max_freq_khz = 3
    )
  }, error = function(e) {
    cat("Error in model", model_name, ":", conditionMessage(e), "\n")
  })
}
