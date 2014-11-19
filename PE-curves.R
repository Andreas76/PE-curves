# Load libraries ----------------------------------------------------------
library(data.table)
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)

WT_1 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/WT_4_1.csv", header=TRUE)
WT_2 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/WT_4_2.csv", header=TRUE)
WT_3 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/WT_4_3.csv", header=TRUE)
L1_1 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/L1_4_1.csv", header=TRUE)
L1_2 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/L1_4_2.csv", header=TRUE)
L1_3 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/L1_4_3.csv", header=TRUE)
L2_1 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/L2_4_1.csv", header=TRUE)
L2_2 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/L2_4_2.csv", header=TRUE)
L2_3 <- read.csv("/media/Science/Organized/Thesis/Chapter 3/FRRF PE-curves/Data/L2_4_3.csv", header=TRUE)

data <- list(WT_1, WT_2, WT_3, L1_1, L1_2, L1_3, L2_1, L2_2, L2_3)
data <- merge_all(data)

data <- group_by(data, PAR, Strain, Replicate)

Data_var <- "Sigma"

Sum_replicates <- summarise(data, count = n(), Rep_Mean = mean(Data_var))
Stats <- summarise(Sum_replicates, Mean = mean(Rep_Mean), Stdev = sd(Rep_Mean))

# Plot the figure ---------------------------------------------------------
setwd("./Plots")
png(filename = "PE-curves.png", width = 600, height = 800, units = "px")
plot1 <- ggplot(data=Stats, aes(PAR, Mean, group=Strain, color=Strain))

plot(plot1 + 
         #   scale_color_manual(values=c("#636363", "#FD8D3C", "#41B6C4")) +
         geom_line() + 
         geom_point() + 
#          facet_wrap(EXP~Light) + 
         geom_errorbar(aes(ymin = Mean - Stdev, ymax = Mean + Stdev), width=0.2) + 
#          facet_grid(EXP~Light, scales = "free") +
         theme(plot.title = element_text(lineheight=.8, face="bold")) +
         ggtitle("P vs. E curve - Sigma PSII") +
         ylab("Sigma PSII") +
         xlab("PAR") + 
         theme(legend.justification=c(0.01,1), legend.position=c(0.9,1)) +
         theme(panel.background = element_rect(fill="gray98")) +
         #   theme(strip.background = element_rect(fill="gray72")) +
         theme(axis.text.x  = element_text(color="gray16")) +
         theme(axis.text.y  = element_text(color="gray16")))
dev.off()
setwd("../")
