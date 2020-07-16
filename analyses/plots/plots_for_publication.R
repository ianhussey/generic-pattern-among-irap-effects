
library(tidyverse)
library(patchwork)

setwd("~/git/generic-pattern-among-irap-effects/analyses")
dir.create("plots")

plot_domains <- read_rds("models/plot_domains.rds")
plot_stimuli <- read_rds("models/plot_stimuli.rds")
plot_recommendations <- read_rds("models/plot_recommendations.rds")
plot_effect_sizes <- read_rds("models/plot_effect_sizes.rds")
plot_generic_pattern <- read_rds("models/plot_generic_pattern.rds")



pdf(NULL)
dev.control(displaylist = "enable")
plot_domains + plot_stimuli + plot_layout(ncol = 1)
p1 <- recordPlot()
invisible(dev.off())
pdf("plots/plot_generic_pattern_1.pdf",
    width = 6, 
    height = 7)
p1
dev.off()

pdf(NULL)
dev.control(displaylist = "enable")
plot_recommendations + plot_layout(ncol = 1)
p1 <- recordPlot()
invisible(dev.off())
pdf("plots/plot_recommendations.pdf",
    width = 6, 
    height = 3.5)
p1
dev.off()


pdf(NULL)
dev.control(displaylist = "enable")
plot_effect_sizes
p2 <- recordPlot()
invisible(dev.off())
pdf("plots/plot_effect_sizes.pdf",
    width = 4, 
    height = 4.5)
p2
dev.off()


pdf(NULL)
dev.control(displaylist = "enable")
plot_generic_pattern
p3 <- recordPlot()
invisible(dev.off())
pdf("plots/plot_generic_pattern_2.pdf",
    width = 5, 
    height = 3.5)
p3
dev.off()


