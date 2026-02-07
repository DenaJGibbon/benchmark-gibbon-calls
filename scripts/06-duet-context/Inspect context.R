library(stringr)

TPDetections <-
  list.files('/Volumes/DJC Files/Benchmarking_MS_Data/benchmarking_zenodo/manually_verified_detections/CrestedGibbons/Positive')

WavFiles <- sub(".*_(R.*)", "\\1", TPDetections)

length(unique(WavFiles))

library(dplyr)
library(stringr)
library(tibble)

TP_df <- tibble(det_file = TPDetections) %>%
  mutate(
    parts = str_split(det_file, "_"),
    Confidence = as.numeric(map_chr(parts, 1)),
    StartSec   = as.numeric(map_chr(parts, 3)),
    EndSec     = as.numeric(map_chr(parts, 4)),
    WavFile    = sub(".*_(R.*)", "\\1", det_file),
    MidSec     = (StartSec + EndSec) / 2
  )

wav_counts <- TP_df %>%
  count(WavFile, name = "n_calls")

multi_call_wavs <- wav_counts %>%
  filter(n_calls > 1)

middle_calls_df <- TP_df %>%
  inner_join(multi_call_wavs, by = "WavFile") %>%
  arrange(WavFile, MidSec) %>%
  group_by(WavFile) %>%
  slice(ceiling(n() / 2)) %>%
  ungroup() %>%
  select(WavFile, MidSec, StartSec, EndSec, n_calls)

middle_calls_df

set.seed(123)

inspect_df <- middle_calls_df %>%
  sample_n(200)

write.csv(inspect_df,'/Users/denaclink/Desktop/RStudioProjects/benchmark-gibbon-calls/inspect_df.csv')
