
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


TempWavLQ <- readWave('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/samples_for_spectrogram/Grp2_M_R1050_20221116_060002_13.wav' )
TempWavMQ <- readWave('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/samples_for_spectrogram/Gibbons_M_R1023_20220502_090003_2617.wav')
TempWavHQ <- readWave('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/data/AcousticData/samples_for_spectrogram/Grp1_H_R1064_20220913_060003_3.wav')

TempSpecLQ <- spectrogram (TempWavLQ@left,fs = TempWavLQ@samp.rate,windowlength = 75, quality = F)
TempSpecMQ <-spectrogram (TempWavMQ@left,fs = TempWavMQ@samp.rate,windowlength = 75, quality = F)
TempSpecHQ <-spectrogram (TempWavHQ@left,fs = TempWavHQ@samp.rate,windowlength = 75, quality = F)

tiff('results/SpectroexampleLQ1.tiff',width = 1600,res=200)
par(mfrow=c(1,3))
plot.spectrogram(TempSpecLQ)
plot.spectrogram(TempSpecMQ)
plot.spectrogram(TempSpecHQ)
graphics.off()

