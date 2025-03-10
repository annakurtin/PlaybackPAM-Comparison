##### Create Visualizations for Methods Chapter Detection Model Results #####
library(ggplot2)
library(tidyverse)
library(patchwork)
library(ggdist)
library(jagsUI)
library(jagshelper)
source("./R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in models #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detection model - ARU
fit_aru <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Methods_Chapter/ARU_Model/Models_Ran_Outputs/MetChap_ARUMod4.Rdata")
# Detection model - PB
fit_pb <- readRDS("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/2_Analyses/Methods_Chapter/Playback_Model/Models_Ran_Outputs/JAGS_PBMod2.txt")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots - ARU Model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot posterior distribution
chains_m <- jags_df(fit_aru)
# Select the chains for our covariates of interest
chains_viol <- chains_m %>% select(b1,b1Q,b3,b4,b5,b6)
# select intercept estimate
posterior_b0 <- chains_m %>% select(b0)

# Create the slab interval plot
dense_int_aru <- ggplot(data = posterior_b0, aes(x = b0)) + 
  # Plot posterior distributions
  stat_slabinterval(fill = d_palette[1]) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0) +
  # Remove background color from plots
  theme_minimal() +
  # Turn off the legend
  guides(fill = "none") +
  scale_x_continuous(limits = c(-8,8)) +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  # Adjust axis titles
  labs(y = "Intercept", x = "Posterior Estimate") +
  annotate("label", x = -5, y = 1, label = "A", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) 

# Rename them to be more interpretable
colnames(chains_viol) <- c("Julian Date",
                           "Julian Date²",
                           "Background Noise",
                           "Survey Effort",
                           "Vegetation Density",
                           "ARU Type")
# Pivot this longer so that we can visualize it
chains_viol_long <- chains_viol %>% pivot_longer(cols = c("Julian Date",
                                                          "Julian Date²",
                                                          "Background Noise",
                                                          "Survey Effort",
                                                          "Vegetation Density",
                                                          "ARU Type"),names_to = "parameter", values_to = "values")
# Order values
chains_viol_long$parameter <- factor(chains_viol_long$parameter, 
                                     levels = c("Julian Date",
                                                "Julian Date²",
                                                "Background Noise",
                                                "Survey Effort",
                                                "Vegetation Density",
                                                "ARU Type"))
# Pull out f statistics 
f_stat <- data.frame(
  parameter = c("Julian Date",
                "Julian Date²",
                "Background Noise",
                "Survey Effort",
                "Vegetation Density",
                "ARU Type"),
  median_value = c(round(fit_aru$f$b1,2), 
                   round(fit_aru$f$b1Q,2), 
                   round(fit_aru$f$b3,2), 
                   round(fit_aru$f$b4,2),
                   round(fit_aru$f$b5,2),
                   round(fit_aru$f$b6,2))
)

# Create the slab interval plot 
dense_det_aru <- ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors
  scale_fill_manual(values = c("Vegetation Density"=d_palette[8], 
                               "Background Noise" = d_palette[4],
                               "Survey Effort"=d_palette[6], 
                               "Julian Date" = d_palette[1], 
                               "Julian Date²" = d_palette[3],
                               "ARU Type" = d_palette[10])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), hjust = 1,
        axis.title.x = element_text(size = 13)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 5, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 4, y = 7, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  annotate("label", x = -5, y = 7, label = "B", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create plots - PB Model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chains_2 <- jags_df(fit_pb)

chains_beta <- chains_2 %>% select(b1, b1Q, b3, b4)

# select intercept estimate
posterior_b0 <- chains_2 %>% select(b0)
# Create the slab interval plot
dense_int_pb <- ggplot(data = posterior_b0, aes(x = b0)) + 
  # Plot posterior distributions
  stat_slabinterval(fill = pb_palette[4]) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0) +
  # Remove background color from plots
  theme_minimal() +
  # Turn off the legend
  guides(fill = "none") +
  scale_x_continuous(limits = c(-8,8)) +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  annotate("label", x = -5, y = 1, label = "C", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Adjust axis titles
  labs(y = "Intercept", x = "Posterior Estimate") 

colnames(chains_beta) <- c("Julian Date", 
                           "Julian Date²",
                           "Wind Strength",
                           "Temperature")
chains_beta_l <- chains_beta %>% pivot_longer(cols = c("Julian Date", 
                                                       "Julian Date²",
                                                       "Wind Strength",
                                                       "Temperature"),names_to = "parameter", values_to = "values")
chains_beta_l$parameter <- factor(chains_beta_l$parameter,levels = c("Julian Date",
                                                                     "Julian Date²",
                                                                     "Wind Strength",
                                                                     "Temperature"))

f_stat <- data.frame(
  parameter = c("Julian Date", 
                "Julian Date²",
                "Wind Strength",
                "Temperature"),
  median_value = c(round(fit_pb$f$b1,2), 
                   round(fit_pb$f$b1Q,2), 
                   round(fit_pb$f$b3,2), 
                   round(fit_pb$f$b4,2))
)

# Create the slab interval plot 
dense_det_pb <- ggplot(data = chains_beta_l, aes(x = values, y = parameter, fill = parameter)) + 
  # Plot posterior distributions
  stat_slabinterval(height = 1.5) +
  # Establish colors - originally 3,5,7,9
  scale_fill_manual(values = c("Julian Date" = pb_palette[4], 
                               "Julian Date²" = pb_palette[6],
                               "Wind Strength" = pb_palette[7],
                               "Temperature" = pb_palette[8])) +
  # Remove background color from plots
  theme_minimal() +
  # Adjust axis titles
  labs(y = NULL, x = "Posterior Estimate") +
  # Adjust axis labels
  theme(axis.text.x = element_text(size = 13), 
        axis.text.y = element_text(size = 13), hjust = 1,
        axis.title.x = element_text(size = 13)) +
  # Adjust x axis
  scale_x_continuous(limits = c(-5,5), breaks = seq(-5,5, by = 1)) +
  # Add a line for 0 to show overlap of posterior
  geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
  # Add median values as text labels
  geom_label(data = f_stat, 
             aes(x = 5, y = parameter, label = median_value),
             size = 6, color = "black", 
             hjust = .6, label.size = 0) +
  annotate("label", x = 4, y = 5, label = "F Statistic:", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  annotate("label", x = -5, y = 5, label = "D", 
           size = 6, color = "black", fill = "white", 
           label.size = 0) +
  # Turn off the legend
  guides(fill = FALSE)
















#### Code Graveyard #####
# Old label
# # Adjust x axis
# scale_x_continuous(limits = c(-3.5,3), breaks = seq(-3,2, by = 1)) +
#   # Add a line for 0 to show overlap of posterior
#   geom_vline(xintercept = 0, color = "gray12", linetype = "dashed") +
#   # Add median values as text labels
#   geom_label(data = f_stat, 
#              aes(x = 2.75, y = parameter, label = median_value),
#              size = 6, color = "black", 
#              hjust = .6, label.size = 0) +
#   annotate("label", x = 2.65, y = 7, label = "F Statistic:", 
#            size = 6, color = "black", fill = "white", 
#            label.size = 0) +
#   # Turn off the legend
#   guides(fill = FALSE)


# Create the density plots

# # Create the slab interval plot
# ggplot(data = chains_viol_long, aes(x = values, y = parameter, fill = parameter)) + 
#   # Plot posterior distributions
#   stat_slabinterval() +
#   # Establish colors
#   scale_fill_manual(values = c("veg_density"=palette_5[1], 
#                                "background_noise" = palette_5[2],
#                                "effort"=palette_5[3], 
#                                "date" = palette_5[4], 
#                                "quadratic date" = palette_5[5],
#                                "SMM present" = palette_8[1])) +
#   # Add a line for 0 to show overlap of posterior
#   geom_vline(xintercept = 0) +
#   # Remove background color from plots
#   theme_minimal() +
#   # Turn off the legend
#   guides(fill = "none")+
#   # Add median values as text labels
#   geom_text(data = f_stat, aes(x = 3.5, y = parameter, label = median_value), color = "black", hjust = 2) 