##### Create Plots of Detection Prob and Number of Surveys for Playback Surveys ####

library(tidyverse)
library(vioplot)
library(ggplot2)
library(ggdist)
library(corrgram)
library(reshape2)
source("./Function_Scripts/Create_HexCodes_CuckooColorBlind.R")


# Read in models and data
fit_pb <- readRDS("./Analysis/Analysis_Detection_Probability/Models_Ran_Outputs/JAGS_PBMod2.txt")
pb_dat <- read.csv("./Analysis/Analysis_Detection_Probability/Data/2023_PBData_FormatforOccModSCALED_8-6-24.csv")

fit_pam <- readRDS("./Analysis/Analysis_Detection_Probability/Models_Ran_Outputs/MetChap_ARUMod4.Rdata")
pam_dat <- read.csv("./Analysis/Analysis_Detection_Probability/Data/2023_PAMData_FormatforOccMod.Rdata")


# Get the value for the cumulative detection probability across the season for PAM

# Pull out detection probability for ARU
#p <- round(plogis(fit_pam$q50$b0),4)# add in the rest of the covariates and see what this does?
# Detection probability for a single aru survey:
p_aru_all <- plogis(fit_pam$q50$b0 + fit_pam$q50$b1*0 + fit_pam$q50$b1Q*0 + fit_pam$q50$b3*0 + fit_pam$q50$b4*0 + fit_pam$q50$b5*0 + fit_pam$q50$b6*0)
# Cumulative detection probability
n_aru <- 6 # Number of surveys 
# calculate cumulative detection probability
aru_p_star <- round(1 - (1 - p_aru_all)^n_aru, 4)



#### Figure 1: Cumulative Detection Probability for PB ####
# empty array for storing detection probability
#array: same number of rows as MCMC samples, same columns as number of surveys you want to investigate
nrow_pb <- fit_pb$mcmc.info$n.samples
max_surveys <- 10
pb_pstars <- array(NA, dim = c(nrow_pb, max_surveys))

# Set up x values with the same number of rows as your array
# x values will allow plotting of box values
x <-  cbind(rep(1, nrow_pb), rep(2, nrow_pb), rep(3, nrow_pb), 
            rep(4, nrow_pb), rep(5, nrow_pb), rep(6, nrow_pb), 
            rep(7, nrow_pb), rep(8, nrow_pb), rep(9, nrow_pb), 
            rep(10, nrow_pb))

#Set up a for-loop that will run the same amount of iterations as the samples produced in the MCMC
for (i in 1:nrow_pb) { 
  # fills in data for each row 
  for (j in 1:max_surveys){ 
    # Fills in data for each column (i.e. number of columns)
    pb_pstars[i,j] <- 1 - (1 - plogis(fit_pb$sims.list$b0[i] + fit_pb$sims.list$b1[i]*0 + fit_pb$sims.list$b1Q[i]*0)+ fit_pb$sims.list$b3[i]*0+ fit_pb$sims.list$b4[i]*0)^j #Calculate estimated maximum detection probability for each survey using mean probability calculated in the MCMC 
  } 
}

pb_pstars_long <- melt(pb_pstars)
colnames(pb_pstars_long) <- c("mcmc_samp", "num_surveys", "cumulative_p")
# Convert Surveys from numeric to factor for ggplot2
pb_pstars_long$num_surveys <- as.factor(pb_pstars_long$num_surveys)


# Create the violin plot using ggplot2
pb_pnumsurveys <- ggplot(data = pb_pstars_long, aes(x = num_surveys, y = cumulative_p)) + 
  geom_violin(fill = pb_palette[8]) +  # Violin plot with your custom color
  geom_hline(yintercept = aru_p_star, linetype = "dashed", linewidth = 1.5, color = d_palette[3]) +  # Dashed line for pstar
  labs(x = "Number of Playback Survey Rounds", y = "Cumulative Detection Probability") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) + # Adjust text angle and position
  stat_summary(fun.y="median", geom="point", color = pb_palette[1], size = 5) +
  annotate("label", x = 9.5, y = 0.1, label = "", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) 

