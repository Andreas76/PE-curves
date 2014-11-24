# Load libraries ----------------------------------------------------------
library(plyr)
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

# Move Fo/Fm (first row) into new dataset ---------------------------------
Fo_Fm <- rbind(WT_1[1,], WT_2[1,], WT_3[1,], L1_1[1,], L1_2[1,], L1_3[1,], L2_1[1,], L2_2[1,], L2_3[1,])

# Remove Fo/Fm from main dataset ------------------------------------------
WT_1 <- WT_1[-1, ]
WT_2 <- WT_2[-1, ]
WT_3 <- WT_3[-1, ]
L1_1 <- L1_1[-1, ]
L1_2 <- L1_2[-1, ]
L1_3 <- L1_3[-1, ]
L2_1 <- L2_1[-1, ]
L2_2 <- L2_2[-1, ]
L2_3 <- L2_3[-1, ]

# Combine into single dataset ---------------------------------------------
data <- list(WT_1, WT_2, WT_3, L1_1, L1_2, L1_3, L2_1, L2_2, L2_3)
data <- merge_all(data)

# Melt dataset ------------------------------------------------------------
data <- melt(data, id.vars = c("Strain", "Replicate", "Index", "Acq.type", "Seq.acq.1", "Date", "Time", "PMT.eht", "LED.set", "LED.flux", "dbar", "PAR"))

# Select variables for plotting -------------------------------------------
data <- filter(data, variable == "Sigma" | variable == "Fv.Fm.or.Fq..Fm." | variable == "Fo.or.F." | variable == "Fm.or.Fm.")

# Group data --------------------------------------------------------------
data <- group_by(data,variable, PAR, Strain, Replicate)

# Select last 3 measurements for stats ------------------------------------
data_tail <- aggregate(data, by=data[c("Strain", "variable", "Replicate", "PAR")], FUN =  tail, 3)

# Rebuild the data frame --------------------------------------------------
names(data_tail)[2] <- "Experiment"
data_tail <- cbind(data_tail[,1:4], data_tail[,18])
data_tail <- melt(data_tail, id.vars = c("Strain", "Experiment", "Replicate", "PAR"))
names(data_tail)[5] <- "Measurement"

# Calculate and add F'o/Fo and F'm/Fm -------------------------------------



# data_tails <- group_by(data_tail, Strain, Experiment, Replicate, PAR, Measurement)

# Calculate stats for plotting --------------------------------------------
Stats <- ddply(data_tail, c("Strain", "Experiment", "PAR"), summarise,
               N    = length(value),
               Mean = mean(value),
               Stdev = sd(value),
               SE   = Stdev / sqrt(N))

# Sum_replicates <- summarise(data_tails, count = n(), Rep_Mean = mean(value))
# Stats <- summarise(Sum_replicates, Mean = mean(Rep_Mean), Stdev = sd(Rep_Mean))

# Change variable names and order -----------------------------------------
Stats$Experiment <- factor(Stats$Experiment, levels = c("Fo.or.F.", "Fm.or.Fm.", "Fa.or.Fa.", "Fmr.or.Fmr.", "Fv.Fm.or.Fq..Fm.", "X.Chl.", "p", "RSigma", "Sigma", "Tau", "Ra", "PAR.x.Fq..Fm.", "C..1.qJ.", "C..1.qP.", "C..1.qL.", "JPSII.x.qJ", "JPSII.x.qP", "JPSII.x.qL", "SE"))
levels(Stats$Experiment ) <- c("Fo", "Fm", "Fa.or.Fa.", "Fmr.or.Fmr.", "Fv/Fm", "X.Chl.", "p", "RSigma", "Sigma", "Tau", "Ra", "PAR.x.Fq..Fm.", "C..1.qJ.", "C..1.qP.", "C..1.qL.", "JPSII.x.qJ", "JPSII.x.qP", "JPSII.x.qL", "SE")
levels(Stats$Strain) <- c("WT", "lca1", "lca2")

# Prepare for plotting ----------------------------------------------------
plot1 <- ggplot(data=Stats, aes(PAR, Mean, color=Strain)) +
    geom_line() + 
    geom_point() + 
    geom_errorbar(aes(PAR, ymin = Mean - Stdev, ymax = Mean + Stdev, width=0.2)) + 
    facet_grid(Experiment~., scales = "free") +
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
png(filename = "PE-curves_tails.png", width = 600, height = 800, units = "px")
print(plot1 + Science_theme)
dev.off()
setwd("../")
