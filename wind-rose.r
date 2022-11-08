#!/usr/bin/env Rscript

library(ggplot2)
library(CircStats)

set.seed(1)
data <- data.frame(direction = deg(rwrpnorm(100000, rad(225), rho=0.8, sd=1)))

# Take a quick look...
hist(data$direction, main = "Histogram of hypothetical direction frequencies.", xlab = "Direction", ylab = "Frequency")
# choose bin size (degrees/bin)
deg <- 30
# define the range of each bin
dir.breaks <- seq(0-(deg/2), 360+(deg/2), deg)

# assign each direction to a bin range
dir.binned <- cut(data$direction,
                       breaks = dir.breaks,
                       ordered_result = TRUE)

# generate pretty lables
dir.labels <- as.character(c(seq(0, 360-deg, by = deg), 0))
# replace ranges with pretty bin lables
levels(dir.binned) <- dir.labels
# Assign bin names to the original data set
data$dir.binned <- dir.binned

thm <- theme_bw() +
    theme(axis.text.x = element_text(size=8, face = "plain"),
          axis.text.y = element_text(size=8, face = "plain"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size=8, face = "plain", hjust = 0.9, vjust = 1.3),
          panel.border = element_blank(),
          panel.grid  = element_blank())

# initialise the plot
plt.dirrose <- ggplot() +
    # since the plot background is blank we'll add a series of horizontal lines, at 5000 count intervals, up to 25000.
    geom_hline(yintercept = seq(0, 25000, by = 5000), colour = "grey60", size = 0.3) +
    # Now we add a darker horizontal line as the top border at 30000.
    geom_hline(yintercept = 30000, colour = "black", size = 0.3) +
    # We want 12 vertical lines representing the centers of the 30° ranges.
    geom_vline(xintercept = c(seq(1,12,1)), colour = "grey60", size = 0.3) +
    # On top of everything we place the histogram bars.
    geom_bar(data = data, aes(x = dir.binned), width = 1, colour="black", size = 0.1, alpha=0.5) +
    # Add the x-axis labels
    scale_x_discrete( drop = FALSE, labels = c(0, "", "", 90, "", "", 180, "", "", 270, "", "")) +
    # Add the y-axis labels
    scale_y_continuous(limits = c(0, 30000), expand = c(0, 0),
                       breaks = c(0, 5000, 10000, 15000, 20000, 25000, 30000),
                       labels = c(0, 5, 10, 15, 20, 25, 30)) +
    # Add the axis titles
    labs(x = 'Outward step bearing (°)', y = 'Count of outward steps (x10³)') +
    # If you only use the plot code up till here you will get a histogram.
    # the next line wraps the histogram into a windrose
    coord_polar(start = -(deg/2)*(pi/180)) +
    # apply theme
    thm

plt.dirrose

ggsave(filename = 'plt.dirrose.png', plot = plt.dirrose, width = 84, height = 84, units="mm", dpi = 300, type="cairo-png")
