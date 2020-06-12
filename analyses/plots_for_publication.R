
library(tidyverse)

setwd("~/git/generic-pattern-among-irap-effects/analysis")

plot_1 <- read_rds("models/plot_1.rds")
plot_2 <- read_rds("models/plot_2.rds")


pdf(NULL)
dev.control(displaylist = "enable")
plot_1
p1 <- recordPlot()
invisible(dev.off())
pdf("plot_1.pdf",
    width = 6, 
    height = 6)
p1
dev.off()

pdf(NULL)
dev.control(displaylist = "enable")
plot_2
p2 <- recordPlot()
invisible(dev.off())
pdf("plot_2.pdf",
    width = 6, 
    height = 4)
p2
dev.off()
