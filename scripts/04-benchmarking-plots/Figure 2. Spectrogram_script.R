
library(phonTools)
library(tuneR)
library(seewave)

# Note need to read in the function using 'run'

WavPath <- '/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/samples_for_spectrogram/CrestedExample_R1023_20220320_050002.wav'

TempWav <- readWave(WavPath)
TempSpec <- spectrogram (TempWav@left,fs = TempWav@samp.rate,windowlength = 75, quality = F,maxfreq = 5000)

tiff('results/Spectroexamplefullduet.tiff',width = 1600,res=200,height = 800)
plot.spectrogram(TempSpec)
graphics.off()

setwd('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/')
TempWavLQ <- readWave('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/samples_for_spectrogram/R1060_20220830_060002_LQ.wav')
TempWavMQ <- readWave('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/samples_for_spectrogram/Gibbons_M_R1023_20220502_090003_2617.wav')
TempWavHQ <- readWave('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/samples_for_spectrogram/Grp1_H_R1064_20220913_060003_3.wav')
# 2. Downsample each to 16 kHz
TempWavLQ <- resamp(TempWavLQ, f = TempWavLQ@samp.rate, g = 16000, output = "Wave")
TempWavMQ <- resamp(TempWavMQ, f = TempWavMQ@samp.rate, g = 16000, output = "Wave")
TempWavHQ <- resamp(TempWavHQ, f = TempWavHQ@samp.rate, g = 16000, output = "Wave")

TempWavDakrong <- readWave('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/Dakrong_testdata/gibbon/Gibbon_Group 3_R1_20210702_050102_3325.wav')

TempSpecLQ <- spectrogram (TempWavLQ@left,fs = TempWavLQ@samp.rate,windowlength = 75, quality = F)
TempSpecMQ <-spectrogram (TempWavMQ@left,fs = TempWavMQ@samp.rate,windowlength = 75, quality = F)
TempSpecHQ <-spectrogram (TempWavHQ@left,fs = TempWavHQ@samp.rate,windowlength = 75, quality = F)
TempSpecDakrong <-spectrogram (TempWavDakrong@left,fs = TempWavDakrong@samp.rate,windowlength = 75, quality = F)

tiff('results/Spectroexample_updated.tiff',width = 1600,res=200)
par(mfrow=c(1,4))
plot.spectrogram(TempSpecLQ)
plot.spectrogram(TempSpecMQ)
plot.spectrogram(TempSpecHQ)
plot.spectrogram(TempSpecDakrong)
graphics.off()

