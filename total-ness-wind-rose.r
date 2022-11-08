#!/usr/bin/env Rscript

library(ggplot2)
nessData <- read.csv(file="wind_analysis.csv", header=TRUE, sep=",")
data <- data.frame(nessData)
data

deg <- 22.5

thm <- theme_minimal() +
    theme(axis.text.x = element_text(size=6, face = "plain"),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid  = element_blank(),
          plot.title = element_text(size=10, face = "bold", hjust = .5 )
        )


plt.dirrose <- ggplot() +

  geom_hline(yintercept = seq(0, 5200, by = 1300), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 5200, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$total[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=total), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("Total")+
  thm


ggsave(filename = 'total-dirrose.png', plot = plt.dirrose, width = 84, height = 84, units="mm", dpi = 300, type="cairo-png")
