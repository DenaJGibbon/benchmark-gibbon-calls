# Load the required package
library(gibbonNetR)

# ------------------ Generate spectrograms for binary model training ------------------

# Define the folders containing randomized gibbon clips (e.g., from Jahoo site)
RandomizationFolders <- list.files('randomization/JahooGibbonClipsRandom/', full.names = TRUE)

# Set the output directory for spectrogram images
ImageOutputDir <- 'randomization/JahooGibbonClipsRandomImages/'

# Loop through each randomized folder and generate spectrograms
for (a in 1:length(RandomizationFolders)) {
  print(a)

  # Define the subdirectory where spectrograms will be saved
  ImageDir <- paste(ImageOutputDir, basename(RandomizationFolders[a]), sep = '')

  # Create spectrogram images from WAV clips
  gibbonNetR::spectrogram_images(
    trainingBasePath = RandomizationFolders[a],  # Input folder of clips
    outputBasePath   = ImageDir,                 # Output folder for images
    splits           = c(0.7, 0.3, 0),           # Split: 70% training, 30% validation, 0% test
    minfreq.khz      = 0.4,                      # Min frequency of interest in kHz
    maxfreq.khz      = 3,                        # Max frequency of interest in kHz
    new.sampleratehz = 'NA'                      # Keep original sample rate
  )
}

# ------------------ Generate spectrograms for multiclass model training ------------------

# Define folders for multiclass randomized gibbon clips
RandomizationFolders <- list.files(
  '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/randomization/JahooGibbonClipsRandomMulti/',
  full.names = TRUE
)

# Set the output directory for multiclass spectrogram images
ImageOutputDir <- 'randomization/JahooGibbonClipsRandomMultiImages/'

# Loop through each folder and generate spectrograms for multiclass models
for (a in 1:length(RandomizationFolders)) {
  print(a)

  # Define the output path for each set of spectrograms
  ImageDir <- paste(ImageOutputDir, basename(RandomizationFolders[a]), sep = '')

  # Create spectrogram images from WAV clips
  gibbonNetR::spectrogram_images(
    trainingBasePath = RandomizationFolders[a],  # Input folder of clips
    outputBasePath   = ImageDir,                 # Output folder for images
    splits           = c(0.7, 0.3, 0),           # 70% training, 30% validation, 0% test
    minfreq.khz      = 0.4,                      # Min frequency in kHz
    maxfreq.khz      = 3,                        # Max frequency in kHz (higher than binary case)
    new.sampleratehz = 'NA'                      # Retain original sample rate
  )
}
