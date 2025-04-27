# Required packages
library(ggpubr)      # For creating plots and visualizations
library(caret)       # For confusion matrix and model evaluation
library(ROCR)        # For calculating AUC (Area Under Curve)
library(stringr)     # For string manipulation (e.g., splitting and padding strings)
library(gpx)         # For reading GPX files (GPS data)
library(sp)          # For spatial data manipulation (coordinates and gridding)
library(gstat)       # For spatial interpolation (IDW)
library(viridis)     # For color scales in plots
library(magick)      # For GIF creation and manipulation

# Add monthly -------------------------------------------------------------
CombinedCallNoCallDF <- read.csv('data/spatial-and-temporal/CombinedCallNoCallDF.csv')

# Interpolate call density based on GPS data
recorder.index <- unique(CombinedCallNoCallDF$Recorder)  # Get unique recorders

CombinedCallNoCallDF$Month <- str_pad(CombinedCallNoCallDF$Month, width=2, side="left", pad="0")
CombinedCallNoCallDF$YearMonth <-  paste(substr(CombinedCallNoCallDF$Date,1,4),CombinedCallNoCallDF$Month,sep='')

month.index <- sort(unique(CombinedCallNoCallDF$YearMonth))

# Create a gif ------------------------------------------------------------

# Create a folder to save the images
dir.create("data/spatial-and-temporal/MonthlyGIFFrames", showWarnings = FALSE)

for(w in c(3:27)){
  CombinedCallNoCallDFSub <- subset(CombinedCallNoCallDF, YearMonth == month.index[w])
  interpolation.df <- data.frame()

  for (x in 1:length(recorder.index)) {
    index.sub <- recorder.index[x]
    gps.sub <- subset(CombinedCallNoCallDFSub, Recorder == as.character(index.sub))
    if (nrow(gps.sub) == 0) next

    new.df <- cbind.data.frame(gps.sub$Recorder[1], gps.sub$Latitude[1],
                               gps.sub$Longitude[1], sum(gps.sub$GibbonDetect))
    colnames(new.df) <- c("name", "y", "x", "n.detect")
    Nuniquerecordings <- nrow(gps.sub)
    new.df$n.detect <- new.df$n.detect / Nuniquerecordings
    interpolation.df <- rbind.data.frame(interpolation.df, new.df)
  }

  if (nrow(interpolation.df) == 0) next

  interpolation.df <- na.omit(interpolation.df)
  interpolation.df.for.pts <- interpolation.df

  x.range <- range(interpolation.df$x)
  y.range <- range(interpolation.df$y)
  sp::coordinates(interpolation.df) <- ~x + y
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.0001),
                     y = seq(from = y.range[1], to = y.range[2], by = 0.0001))
  sp::coordinates(grd) <- ~x + y
  sp::gridded(grd) <- TRUE

  idw <- gstat::idw(formula = n.detect ~ 1, locations = interpolation.df, newdata = grd)
  idw.output <- as.data.frame(idw)
  names(idw.output)[1:3] <- c("long", "lat", "var1.pred")

  breaks <- seq(0, 0.4, length.out = 6)
  colors <- rev(viridis::viridis(4))
  color_scale <- scale_fill_gradientn(
    colors = colors,
    values = scales::rescale(breaks),
    limits = c(0.0, 0.4),
    breaks = breaks
  )

  monthval <- as.numeric(substr(month.index[w],5,6))

  season <- ifelse(monthval >= 5 & monthval <= 11, 'wet', 'dry')


  p <- ggplot() +
    geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) +
    geom_point(data = interpolation.df.for.pts, aes(x = x, y = y), color = "black", size = 1) +
    color_scale +
    labs(x = "Longitude", y = "Latitude", fill = "Hourly\ndetections", title = paste(month.index[w],season) ) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.text = element_text(size = 12), legend.title = element_text(size = 12))

  ggsave(
    filename = sprintf("data/spatial-and-temporal/MonthlyGIFFrames/frame_%02d_%s.png", w, month.index[w]),
    plot = p,
    width = 8, height = 6, dpi = 150
  )
}


library(magick)

# List and read all frame images
frames <- list.files("data/spatial-and-temporal/MonthlyGIFFrames", pattern = "\\.png$", full.names = TRUE)
frames <- image_read(frames)

# Animate and save as GIF
animated <- image_animate(image_join(frames), fps = 2)
image_write(animated, "/Users/denaclink/Desktop/RStudioProjects/benchmark-gibbon-calls/MonthlyGIFFramesCallDensityOverTime.gif")
