# Load required libraries
library(stringr)  # String manipulation
library(ggpubr)   # ggplot2 extensions for easy plotting
library(gpx)      # Reading GPX files
library(sp)       # Spatial data handling
library(gstat)    # Interpolation and geostatistical modeling
library(matlab)   # Color palettes (e.g., jet.colors)
library(ggplot2)  # Data visualization
library(gridExtra)
library(dplyr)
library(ggplot2)

# Load detection data from a CSV file
DetectionsDF <- read.csv('data/spatial-and-temporal/verified_detections.csv')  # Load the verified detection data from a CSV file
head(DetectionsDF)                                       # Display the first few rows of the dataset
nrow(DetectionsDF)                                       # Display the number of rows in the detection dataset

# Load GPS data and format it
TempFrame <- gpx::read_gpx('data/spatial-and-temporal/Jahootraining_Bioacoustic Units.gpx')  # Read GPS data from a GPX file
TempFrame$waypoints$Name <- str_split_fixed(TempFrame$waypoints$Name, pattern = 'UNIT ', n = 2)[, 2]  # Extract recorder names from the "Name" field
GPSPoints <- data.frame(TempFrame$waypoints$Name, TempFrame$waypoints$Latitude, TempFrame$waypoints$Longitude)  # Create a data frame with the recorder names and coordinates
colnames(GPSPoints) <- c('Recorder', 'Latitude', 'Longitude')  # Rename the columns for clarity
head(GPSPoints)                                                # Display the first few rows of the GPS data

# Merge detection data with GPS data
CombinedDetectionData <- merge(DetectionsDF, GPSPoints, by.x = "Recorder", by.y = "Recorder")  # Merge the detection data with GPS data based on the "Recorder" field
head(CombinedDetectionData)  # Display the first few rows of the combined data

# Remove unnecessary columns (in this case, column "X")
CombinedDetectionData <- CombinedDetectionData[, !colnames(CombinedDetectionData) %in% 'X']

# Convert the "Date" column to Date format for proper handling
CombinedDetectionData$Date <- as.Date(CombinedDetectionData$Date, format = "%Y-%m-%d")

# Visualize the hourly distribution of detections
ggpubr::gghistogram(data = CombinedDetectionData, x = 'Hour', stat = "count") +
  ylab('True positive detections')  # Create a histogram of detections by hour

ggpubr::gghistogram(data = CombinedDetectionData, x = 'Date', stat = "count") +
  ylab('True positive detections')  # Create a histogram of detections by date

# Process detection files that don't contain calls
JahooFilesBase <- list.files("data/spatial-and-temporal/JahooBirdNETupdatedmodel", pattern = '.txt', recursive = TRUE, full.names = FALSE)  # List all .txt detection files
length(JahooFilesBase)  # Display the number of files found

# Extract base file names (without file extension)
JahooFilesBase <- str_split_fixed(basename(JahooFilesBase), pattern = '.BirdNET', n = 2)[, 1]
JahooFilesBaseShort <- substr(JahooFilesBase, 1, 17)  # Create a short version of the file names for comparison

# Match detection dates and times by combining recorder name, date, and hour
DateMatch <- str_remove_all(DetectionsDF$Date, '-')  # Remove dashes from the date for easier matching
HourPad <- str_pad(DetectionsDF$Hour, 2, pad = "0")  # Ensure hours are padded to 2 digits
TruePositiveDetectionsName <- paste(DetectionsDF$Recorder, DateMatch, HourPad, sep = '_')  # Create unique names for each detection

# Identify files that don't contain calls (i.e., not in the TruePositiveDetectionsName list)
JahooFilesBaseNocall <- JahooFilesBase[-which(JahooFilesBaseShort %in% TruePositiveDetectionsName)]  # Exclude files with true detections
JahooFilesBaseNocallSplit <- str_split_fixed(JahooFilesBaseNocall, pattern = '_', n = 4)  # Split file names into components

# Create a data frame for files that don't contain calls
Recorder <- JahooFilesBaseNocallSplit[, 1]
Date <- as.Date(JahooFilesBaseNocallSplit[, 2], format = "%Y%m%d")  # Convert date format
Month <- as.numeric(substr(Date, 6, 7))  # Extract the month from the date
Hour <- substr(JahooFilesBaseNocallSplit[, 3], 1, 2)  # Extract the hour from the file name
JahooNoCallDF <- data.frame(Recorder, Date, Month, Hour)  # Create a data frame for the no-call detections
head(JahooNoCallDF)  # Display the first few rows

# Analyze the distribution of non-call detections by hour
table(JahooNoCallDF$Hour)  # Create a table showing the distribution of detections by hour

# Merge the no-call detection data with the GPS data
JahooNoCallDF <- merge(JahooNoCallDF, GPSPoints, by.x = "Recorder", by.y = "Recorder")  # Merge with GPS data based on the "Recorder" field
head(JahooNoCallDF)  # Display the first few rows of the merged no-call data

# Mark the detections (1 for true gibbon detection, 0 for no-call detection)
CombinedDetectionData$GibbonDetect <- '1'
JahooNoCallDF$GibbonDetect <- '0'

# Combine both datasets (gibbon detections and no-call detections)
CombinedCallNoCallDF <- rbind.data.frame(CombinedDetectionData, JahooNoCallDF)

# Convert the "GibbonDetect" column to numeric for further analysis
CombinedCallNoCallDF$GibbonDetect <- as.numeric(CombinedCallNoCallDF$GibbonDetect)

# Display the first few rows of the combined dataset
head(CombinedCallNoCallDF)

# Display the date range of the combined dataset
range(CombinedCallNoCallDF$Date)

# Save output
#write.csv(CombinedCallNoCallDF,'data/spatial-and-temporal/CombinedCallNoCallDF.csv',row.names = F)

# Figure 6. Temporal plot -----------------------------------------------------------
# Define the monsoon period (May to October) for the years in your dataset
monsoon_start_1 <- as.Date("2022-05-01")
monsoon_end_1 <- as.Date("2022-10-31")
monsoon_start_2 <- as.Date("2023-05-01")
monsoon_end_2 <- as.Date("2023-10-31")

# Step 1: Summarize data
summary_df <- CombinedCallNoCallDF %>%
  group_by(Date) %>%
  summarize(
    TotalRecorders = n(),
    Detections = sum(GibbonDetect),
    ProportionDetected = Detections / TotalRecorders
  ) %>%
  filter(Date > as.Date("2022-02-28"))  # Exclude data before March 2022


# Step 2: Plot the proportion over time
DetectionsByDatePlot <- ggplot(summary_df, aes(x = as.Date(Date), y = ProportionDetected)) +
  geom_line(color = "grey", size = 1) +
  #geom_point(color = "red", size = 2) +
  labs(
    #title = "Proportion of Recorders with Detections Over Time",
    x = "Date",
    y = "Proportion of Recorders with Detections"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )+
  geom_smooth(method = "gam", color = "darkgreen", fill = "lightgreen", size = 1.2, alpha = 0.3) +  # Smoothed trend line
  # Add shaded area for the monsoon period across multiple years
  geom_rect(aes(xmin = monsoon_start_1, xmax = monsoon_end_1, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.01) +  # Light blue shading for 2022
  geom_rect(aes(xmin = monsoon_start_2, xmax = monsoon_end_2, ymin = -Inf, ymax = Inf),
            fill = "lightblue", alpha = 0.01)    # Light blue shading for 2023


# Create a table for total number of hours sampled (denominator)
total_hours <- table(JahooNoCallDF$Hour)

# Create a table for true positives (numerator)
true_positives <- table(CombinedDetectionData$Hour)

# Align the data by hour
hours <- as.numeric(names(total_hours))  # Extract hour labels
true_positives <- true_positives[as.character(hours)]  # Match hours in the two tables
true_positives[is.na(true_positives)] <- 0  # Handle hours with no detections

# Calculate standardized rate (True Positives per Total Hours)
standardized_rate <- true_positives / total_hours

# Create a data frame for visualization
rate_df <- data.frame(
  Hour = as.numeric(names(total_hours)),
  TruePositives = as.numeric(true_positives),
  TotalHours = as.numeric(total_hours),
  StandardizedRate = as.numeric(standardized_rate)
)

rate_df$Time <- format(strptime(rate_df$Hour, format = "%H"), format = "%H:%M")
rate_df$Time <- factor(rate_df$Time, levels = sort(unique(rate_df$Time)))

# Filter the data frame for hours between 04:00 and 18:00
rate_df <- rate_df[rate_df$Hour >= 4 & rate_df$Hour <= 18, ]

# Create the bar plot
StandarizedBarplot <- ggbarplot(data = rate_df, x = 'Time', y = 'StandardizedRate') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Gibbon detections \n per hour') +
  xlab('Local time')

# Display the plot
print(StandarizedBarplot)

StandarizedBarplot <- ggbarplot(data=rate_df,x='Time',y='StandardizedRate')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +ylab('Gibbon detections \n per hour')+xlab('Local time')

cowplot::plot_grid(StandarizedBarplot,DetectionsByDatePlot,
                   labels = c('A','B'), label_x = 0.9)


# Figure 7. Interpolate call density based on GPS data-------------------------------------------------------------------------

recorder.index <- unique(CombinedCallNoCallDF$Recorder)  # Get unique recorders

interpolation.df <- data.frame()                         # Initialize data frame for interpolation

for (x in 1:length(recorder.index)) {
  index.sub <- recorder.index[x]                                     # Subset recorder
  gps.sub <- subset(CombinedCallNoCallDF, Recorder == as.character(index.sub))  # Subset GPS data
  new.df <- cbind.data.frame(gps.sub$Recorder[1], gps.sub$Latitude[1],
                             gps.sub$Longitude[1], sum(gps.sub$GibbonDetect))  # Create a new row
  colnames(new.df) <- c("name", "y", "x", "n.detect")               # Rename columns

  TempRecRow <- subset(JahooNoCallDF, Recorder == index.sub)        # Subset non-call data
  Nuniquerecordings <- nrow(gps.sub)

  new.df$n.detect <- new.df$n.detect / Nuniquerecordings  # Normalize detections

  interpolation.df <- rbind.data.frame(interpolation.df, new.df)   # Append to interpolation data frame
}

# Prepare data for interpolation and plotting
interpolation.df.for.pts <- interpolation.df
x.range <- as.numeric(c(min(interpolation.df$x), max(interpolation.df$x)))  # Set longitude range
y.range <- as.numeric(c(min(interpolation.df$y), max(interpolation.df$y)))  # Set latitude range
sp::coordinates(interpolation.df) <- ~x + y                                # Set spatial coordinates
grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.00001),
                   y = seq(from = y.range[1], to = y.range[2], by = 0.00001))  # Create grid
sp::coordinates(grd) <- ~x + y                                              # Set grid coordinates
sp::gridded(grd) <- TRUE                                                    # Make grid spatial

# Perform interpolation using inverse distance weighting (IDW)
idw <- gstat::idw(formula = n.detect ~ 1, locations = interpolation.df, newdata = grd)  # Perform IDW interpolation
idw.output <- as.data.frame(idw)                                           # Convert to data frame
names(idw.output)[1:3] <- c("long", "lat", "var1.pred")                    # Rename columns

# Generate color palette
vid.cols <- viridis::viridis (10)

# Plot call density
Jahoo.call.density.plot <- ggplot2::ggplot() +
  geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) +  # Add interpolated data
  geom_label(data = interpolation.df.for.pts, mapping = aes(x = x, y = y, label = name), size = 4) +  # Add labels
  scale_fill_gradientn(colors = vid.cols) +                                # Set color gradient
  xlab("Longitude") + ylab("Latitude") +                                   # Set axis labels
  theme_bw() +                                                             # Set theme
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid lines
  guides(fill = guide_legend(title = "Jahoo detections \n per hour")) +    # Add legend
  theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))  # Style legend

Jahoo.call.density.plot  # Display plot


