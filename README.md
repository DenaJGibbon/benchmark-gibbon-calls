
<!-- README.md is generated from README.Rmd. Please edit that file -->

# benchmark-gibbon-calls

<!-- badges: start -->
<!-- badges: end -->

## Overview

This repository provides a comparative analysis of the BirdNET algorithm
performance to that of Koogu, ‘gibbonNetR’, and a traditional ML
approach that combines MFCC features with support vector machines. The
code provided compares the different algorithms across various datasets
and configurations.

## Data availabilty

Data needed to run the analyses can be found on Zenodo:
<https://zenodo.org/records/12706803>.

## Overview

This repository benchmarks various machine learning models for detecting
and classifying gibbon calls in audio recordings. The workflow includes
data preparation, model training, performance evaluation, and
visualization of results.

## Repository Structure

    benchmark-gibbon-calls/
    ├── data/                     # Input data (audio files, annotations, etc.)
    ├── manually_verified_detections/ # Manually verified detection results
    ├── randomization/            # Randomized subsets of training data
    ├── results/                  # Trained models and evaluation results

    ~~~ Above files are on hosted Zenodo  ~~~ 

    ├── scripts/                  # R and Python scripts for the workflow
    │   ├── 01-data-preparation/  # Scripts for preparing training data
    │   ├── 02-data-analysis/     # Scripts for training models
    │   ├── 03-evaluate-performance/ # Scripts for evaluating models
    │   ├── 04-benchmarking-plots/ # Scripts for generating visualizations
    │   └── 05-spatial-temporal-variation-plots/ # Plots for verified detections
    ├── README.md                 # Project documentation

## Required Folders

The following folders must be downloaded from Zenodo and placed in the
project directory:  
1. **`data/`**: Contains input data such as audio files and
annotations.  
2. **`manually_verified_detections/`**: Contains manually verified
detection results for evaluation.

3.  **`randomization/`**: Contains randomized subsets of training data.

4.  **`results/`**: Contains trained models, evaluation results, and
    generated plots.

## Scripts Overview

The repository contains scripts organized into subdirectories, each
focusing on specific tasks related to gibbon call data preparation,
analysis, and evaluation.

### 1. `01-data-preparation/`

- **`01a. Randomization of training samples.R`**: Randomly samples
  `.wav` files from gibbon and noise datasets to create subsets for
  training and testing.
- **`01b. Randomization of training samples multi.R`**: Extends the
  randomization process to handle the multiclass data.
- **`01c. Create Images for Random Subsets.R`**: Generates spectrogram
  images for the randomized subsets.

### 2. `02-data-analysis/`

- **`02a. SVM gibbonRtestJahoo.R`**: Implements Support Vector Machine
  (SVM) models for binary and multiclass classification.
- **`03a-c. BirdNET scripts/`**: Scripts for training and deploying
  BirdNET models for binary and multiclass classification.
  - Includes automated detection and generalizability testing.
- **`04a-c. Koogu scripts/`**: Python-based scripts for training and
  deploying Koogu models for gibbon call classification.
- **`05a-c. gibbonNetR scripts/`**: R-based scripts for training and
  deploying gibbonNetR models, including experiments with varying
  training sample sizes.

### 3. `03-evaluate-performance/`

- **`04a. BirdNET performance binary.R`**: Evaluates the performance of
  BirdNET binary classification models.
- **`04b. BirdNET performance multi.R`**: Evaluates BirdNET multiclass
  models.
- **`04c. BirdNET generalizability perf.R`**: Tests the generalizability
  of BirdNET models across datasets.
- **`05a. gibbonNetR performance.R`**: Evaluates the performance of
  gibbonNetR binary models.
- **`05b. gibbonNetR multi performance.R`**: Evaluates gibbonNetR
  multiclass models.

### 4. `04-benchmarking plots/`

- Scripts for generating benchmarking plots to visualize model
  performance metrics.

### 5. `05-spatial and temporal variation plots/`

- **`Figure 6 and 7. Temporal and spatial variation.R`**: Creates
  visualizations of temporal and spatial variation in detections.
- **`Figure 1 OSM. Monthly plots.R`**: Generates monthly call density
  plots and animates them as a GIF.

## Installation

1.  **Clone the Repository**:

    ``` bash
    git clone https://github.com/your-repo/benchmark-gibbon-calls.git
    cd benchmark-gibbon-calls
    ```

2.  **Download Required Folders**:

    - Download the following folders from Zenodo:
      - `data/`
      - `manually_verified_detections/`
      - `randomization/`
      - `results/`
    - Place these folders in the root directory of the repository.

3.  **Install Dependencies**:

    - **R Packages**: Install the required R packages using the
      following command:

      ``` r
      install.packages(c("tidyr", "dplyr", "ggpubr", "cowplot", "stringr", "caret", "pROC"))
      ```

    - **Python Dependencies**:

For Koogu instructions refer to:
<https://shyamblast.github.io/Koogu/en/stable/>

For BirdNET installation instructions refer to:
<https://birdnet-team.github.io/BirdNET-Analyzer/>

4.  **Set Up Environment**: Ensure all paths in the scripts are updated
    to match your local directory structure.

## How to Run

1.  **Prepare Data**:
    - Ensure the `data/` directory from Zenodo is in your project.
    - Run `01a. Randomization of training samples.R` to create
      randomized subsets.
2.  **Train Models**:
    - Run the training scripts for `gibbonNetR`, `BirdNET`, and `Koogu`
      models.
3.  **Evaluate Models**:
    - Use the evaluation scripts to calculate performance metrics.
4.  **Visualize Results**:
    - 04-benchmarking plots
    - 05-spatial and temporal variation plots

## Example Outputs

- **Plots**:
  - `results/plots/Figure4-performance-metrics.pdf`
  - `results/plots/Figure3-performance-metrics.pdf`

## Citation

Please cite both if you use these data or methods:

Clink, D., Cross-Jaya, H., Kim, J., Ahmad, A. H., Hong, M., Sala, R.,
Birot, H., Agger, C., Vu, T. T., Thi, H. N., Chi, T. N., & Klinck, H.
(2024). Dataset for “Benchmarking automated detection and classification
approaches for long-term acoustic monitoring of endangered species: a
case study on gibbons from Cambodia” \[Data set\]. Zenodo.
<https://doi.org/10.5281/zenodo.12706803>

Clink DJ, Cross-Jaya H, Kim J, Ahmad AH, Hong M, Sala R, Birot H, Agger
C, Vu TT, Thi HN, Chi TN. Benchmarking automated detection and
classification approaches for long-term acoustic monitoring of
endangered species: a case study on gibbons from Cambodia. bioRxiv.
2024:2024-08.

## Representative figures

![Online Supporting Material 1. A GIF showing the montly change in
gibbon
detections](https://github.com/DenaJGibbon/benchmark-gibbon-calls/blob/main/MonthlyGIFFramesCallDensityOverTimev1.gif)
\## Contact For questions or contributions, please contact the project
maintainer at \[<dena.clink@cornell.edu>\].
