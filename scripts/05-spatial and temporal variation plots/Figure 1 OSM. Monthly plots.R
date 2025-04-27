# Add monthly -------------------------------------------------------------
CombinedCallNoCallDF <- read.csv('data/spatial-and-temporal/CombinedCallNoCallDF.csv')

# Interpolate call density based on GPS data
recorder.index <- unique(CombinedCallNoCallDF$Recorder)  # Get unique recorders

CombinedCallNoCallDF$Month <- str_pad(CombinedCallNoCallDF$Month, width=2, side="left", pad="0")
CombinedCallNoCallDF$YearMonth <-  paste(substr(CombinedCallNoCallDF$Date,1,4),CombinedCallNoCallDF$Month,sep='')

month.index <- sort(unique(CombinedCallNoCallDF$YearMonth))

# Open PDF device to combine all plots in a single file
output_pdf <- paste("Combined_Plots_multipanel_wide.tiff")
tiff(output_pdf, height = 6000, width = 10000, res = 300)  # Set height and width to fit two plots per row

# List to store ggplot objects
plot_list <- list()


for(w in c(3:27)){
  CombinedCallNoCallDFSub <- subset(CombinedCallNoCallDF, YearMonth == month.index[w])

  interpolation.df <- data.frame()  # Initialize data frame for interpolation

  for (x in 1:length(recorder.index)) {
    index.sub <- recorder.index[x]  # Subset recorder
    gps.sub <- subset(CombinedCallNoCallDFSub, Recorder == as.character(index.sub))  # Subset GPS data
    new.df <- cbind.data.frame(gps.sub$Recorder[1], gps.sub$Latitude[1],
                               gps.sub$Longitude[1], sum(gps.sub$GibbonDetect))  # Create a new row
    colnames(new.df) <- c("name", "y", "x", "n.detect")  # Rename columns

    TempRecRow <- subset(JahooNoCallDF, Recorder == index.sub)  # Subset non-call data
    Nuniquerecordings <- nrow(gps.sub)

    new.df$n.detect <- new.df$n.detect / Nuniquerecordings  # Normalize detections

    interpolation.df <- rbind.data.frame(interpolation.df, new.df)  # Append to interpolation data frame
  }

  interpolation.df <- na.omit(interpolation.df)

  #if (nrow(interpolation.df) == 10) {
  # Prepare data for interpolation and plotting
  interpolation.df.for.pts <- interpolation.df
  x.range <- as.numeric(c(min(interpolation.df$x), max(interpolation.df$x)))  # Set longitude range
  y.range <- as.numeric(c(min(interpolation.df$y), max(interpolation.df$y)))  # Set latitude range
  sp::coordinates(interpolation.df) <- ~x + y  # Set spatial coordinates
  grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.0001),
                     y = seq(from = y.range[1], to = y.range[2], by = 0.0001))  # Create grid
  sp::coordinates(grd) <- ~x + y  # Set grid coordinates
  sp::gridded(grd) <- TRUE  # Make grid spatial

  # Perform interpolation using inverse distance weighting (IDW)
  idw <- gstat::idw(formula = n.detect ~ 1, locations = interpolation.df, newdata = grd)  # Perform IDW interpolation
  idw.output <- as.data.frame(idw)  # Convert to data frame
  names(idw.output)[1:3] <- c("long", "lat", "var1.pred")  # Rename columns

  # Define value ranges and corresponding colors
  breaks <- seq(0, 0.4, length.out = 6)  # Adjust the number of breaks if needed
  # Example breaks within your range
  colors <-  rev(viridis::viridis(4)) # Corresponding colors for the ranges

  # Set the same color scale across all plots
  color_scale <- scale_fill_gradientn(
    colors = colors,
    values = scales::rescale(breaks),
    limits = c(0.0, 0.4),  # Set the limits to the full range of values
    breaks = breaks  # Define the breaks for the color scale
  )

  # Create ggplot object for each month and store it in the list
  p <- ggplot2::ggplot() +
    geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) +  # Add interpolated data
    geom_point(data = interpolation.df.for.pts, mapping = aes(x = x, y = y), color = "black", size = 1) +  # Add points instead of labels
    color_scale +  # Apply the fixed color scale
    xlab("Longitude") + ylab("Latitude") +  # Set axis labels
    theme_bw() +  # Set theme
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid lines
    guides(fill = guide_legend(title = "Hourly gibbon \n detections")) +  # Add legend
    ggtitle(month.index[w]) +  # Add title with the month name
    theme(legend.text = element_text(size = 12), legend.title = element_text(size = 12))  # Style legend

  plot_list[[w]] <- p  # Add the plot to the list
}
#}

#plot_list <- plot_list[!sapply(plot_list, is.null)]

# Combine all plots into a 2xN grid
grid.arrange(grobs = plot_list, ncol = 6)

# Close the PDF device after all plots are added
dev.off()



# Open PDF device to combine all plots in a single file
output_pdf <- paste("Combined_Plots_multipanel_subset.pdf")
pdf(output_pdf, height = 18, width = 32)  # Set height and width to fit two plots per row

# Subset the list using single brackets [] to extract multiple elements
#selected_plots <- plot_list[c(3, 4, 8, 9, 15, 16, 20, 21, 27, 28)]
#selected_plots <- plot_list[c(3, 6, 9, 12, 15, 18, 21, 24, 27)]

# Arrange the selected plots in a grid
grid.arrange(grobs = plot_list, ncol = 4)

# Close the PDF device after all plots are added
dev.off()


# Create a gif ------------------------------------------------------------

# Create a folder to save the images
dir.create("MonthlyGIFFrames", showWarnings = FALSE)

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

  p <- ggplot() +
    geom_tile(data = idw.output, aes(x = long, y = lat, fill = var1.pred)) +
    geom_point(data = interpolation.df.for.pts, aes(x = x, y = y), color = "black", size = 1) +
    color_scale +
    labs(x = "Longitude", y = "Latitude", fill = "Hourly\ndetections", title = month.index[w]) +
    theme_bw() +
    theme(panel.grid = element_blank(), legend.text = element_text(size = 12), legend.title = element_text(size = 12))

  ggsave(
    filename = sprintf("MonthlyGIFFrames/frame_%02d_%s.png", w, month.index[w]),
    plot = p,
    width = 8, height = 6, dpi = 150
  )
}


library(magick)

# List and read all frame images
frames <- list.files("MonthlyGIFFrames", pattern = "\\.png$", full.names = TRUE)
frames <- image_read(frames)

# Animate and save as GIF
animated <- image_animate(image_join(frames), fps = 2)
image_write(animated, "CallDensityOverTime.gif")
