# Load libraries ----------------------------------------------------------
library(dplyr)
library(reshape)
library(reshape2)
library(ggplot2)

# Read data ---------------------------------------------------------------
setwd("./Data")
WT_1 <- read.csv("WT_4_1.csv", header=TRUE)
WT_2 <- read.csv("WT_4_2.csv", header=TRUE)
WT_3 <- read.csv("WT_4_3.csv", header=TRUE)
L1_1 <- read.csv("L1_4_1.csv", header=TRUE)
L1_2 <- read.csv("L1_4_2.csv", header=TRUE)
L1_3 <- read.csv("L1_4_3.csv", header=TRUE)
L2_1 <- read.csv("L2_4_1.csv", header=TRUE)
L2_2 <- read.csv("L2_4_2.csv", header=TRUE)
L2_3 <- read.csv("L2_4_3.csv", header=TRUE)
setwd("../")

# Combine into single dataset ---------------------------------------------
data <- list(WT_1, WT_2, WT_3, L1_1, L1_2, L1_3, L2_1, L2_2, L2_3)
data <- merge_all(data)

# Melt dataset ------------------------------------------------------------
data <- melt(data, id.vars = c("Strain", "Replicate", "Index", "Acq.type", "Seq.acq.1", "Date", "Time", "PMT.eht", "LED.set", "LED.flux", "dbar", "PAR"))

# Select variables for plotting -------------------------------------------
Select_variables <- c("Sigma", "Fv.Fm.or.Fq..Fm.", "Fo.or.F.", "Fm.or.Fm.")
data <- filter(data, variable == "Sigma" | variable == "Fv.Fm.or.Fq..Fm." | variable == "Fo.or.F." | variable == "Fm.or.Fm.")

# Group data --------------------------------------------------------------
data <- group_by(data,variable, PAR, Strain, Replicate)

# Calculate stats for plotting --------------------------------------------
Sum_replicates <- summarise(data, count = n(), Rep_Mean = mean(value))
Stats <- summarise(Sum_replicates, Mean = mean(Rep_Mean), Stdev = sd(Rep_Mean))

# Change variable names and order -----------------------------------------
Stats$variable <- factor(Stats$variable, levels = c("Fo.or.F.", "Fm.or.Fm.", "Fa.or.Fa.", "Fmr.or.Fmr.", "Fv.Fm.or.Fq..Fm.", "X.Chl.", "p", "RSigma", "Sigma", "Tau", "Ra", "PAR.x.Fq..Fm.", "C..1.qJ.", "C..1.qP.", "C..1.qL.", "JPSII.x.qJ", "JPSII.x.qP", "JPSII.x.qL", "SE"))
levels(Stats$variable) <- c("Fo", "Fm", "Fa.or.Fa.", "Fmr.or.Fmr.", "Fv/Fm", "X.Chl.", "p", "RSigma", "Sigma", "Tau", "Ra", "PAR.x.Fq..Fm.", "C..1.qJ.", "C..1.qP.", "C..1.qL.", "JPSII.x.qJ", "JPSII.x.qP", "JPSII.x.qL", "SE")
levels(Stats$Strain) <- c("WT", "lca1", "lca2")

# Prepare for plotting ----------------------------------------------------
plot1 <- ggplot(data=Stats, aes(PAR, Mean, color=Strain)) +
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(PAR, ymin = Mean - Stdev, ymax = Mean + Stdev, width=0.2)) + 
    facet_grid(variable~., scales = "free") +
    theme(plot.title = element_text(lineheight=.8, face="bold")) +
    ggtitle("Photosynthesis-irradiance response curves\n") +
    ylab("Parameter\n") +
    xlab("\nPAR")

Science_theme = theme(
    axis.line = element_line(size = 0.1, color = "black"),
    legend.justification=c(0.01,1), 
    legend.position=c(0,1),
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    strip.background = element_blank(),
    axis.text.x  = element_text(color="gray16"),
    axis.text.y  = element_text(color="gray16")
)

# Plot --------------------------------------------------------------------
setwd("./Plots")
png(filename = "PE-curves.png", width = 600, height = 800, units = "px")
print(plot1 + Science_theme)
dev.off()
setwd("../")
