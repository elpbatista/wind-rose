#!/usr/bin/env Rscript

library(ggplot2)
nessData <- read.csv(file="wind_analysis.csv", header=TRUE, sep=",")
data <- data.frame(nessData)
data

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

deg <- 22.5

thm0 <- theme_minimal() +
    theme(axis.text.x = element_text(size=6, face = "plain"),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.border = element_blank(),
          panel.grid  = element_blank(),
          plot.title = element_text(size=10, face = "bold", hjust = .5 )
        )

thm <- theme_minimal() +
  theme(axis.text.x = element_text(size=4, face = "plain"),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid  = element_blank(),
        plot.title = element_text(size=6, face = "bold", hjust = .5 )
      )


plt.total <- ggplot() +

  geom_hline(yintercept = seq(0, 5200, by = 1300), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 5200, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$total[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=total), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("Total")+
  thm0

plt.jan <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$jan[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=jan), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("JAN")+
  thm

plt.feb <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$feb[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=feb), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("FEB")+
  thm

plt.mar <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$mar[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=mar), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("MAR")+
  thm

plt.apr <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$apr[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=apr), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("APR")+
  thm

plt.may <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$may[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=may), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("MAY")+
  thm

plt.jun <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$jun[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=jun), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("JUN")+
  thm

plt.jul <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$jul[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=jul), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("JUL")+
  thm

plt.aug <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$aug[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=aug), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("AUG")+
  thm

plt.sep <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$sep[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=sep), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("SEP")+
  thm

plt.oct <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$oct[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=oct), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("OCT")+
  thm

plt.nov <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$nov[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=nov), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("NOV")+
  thm

plt.dec <- ggplot() +

  geom_hline(yintercept = seq(0, 555, by = 111), colour = "grey80", size = 0.1) +
  geom_hline(yintercept = 555, colour = "grey60", size = 0.1) +
  geom_hline(yintercept = data$dec[17], colour = "orange", size = 0.2) + #BLANK
  geom_vline(xintercept = data$dir_deg, colour = "grey80", size = 0.1) +
  geom_bar(data=data, aes(x=dir_deg, y=dec), width = deg, size = 0.1, alpha=0.6, stat="identity") +
  scale_x_discrete(limits = data$dir_deg, labels = data$dir_label)+
  coord_polar(start = -(deg/2)*(pi/180))+
  ggtitle("DEC")+
  thm

#ggsave(filename = 'plt.dec.png', plot = plt.dec, width = 84, height = 84, units="mm", dpi = 300, type="cairo-png")

ggsave(filename = 'months.dirrose.png', plot = multiplot(plt.jan, plt.may, plt.sep, plt.feb, plt.jun, plt.oct, plt.mar, plt.jul, plt.nov, plt.apr, plt.aug, plt.dec, cols=4), width = 200, height = 150, units="mm", dpi = 150, type="cairo-png")
