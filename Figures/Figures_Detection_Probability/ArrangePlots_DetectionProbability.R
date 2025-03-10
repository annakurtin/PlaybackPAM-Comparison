##### Arrange Methods Chapter Plots ####
library(ggplot2)
library(patchwork)
source("./Figures/Figures_Detection_Probability/CreatePlots_PosteriorDist_DetectionMods_Manuscript.R")
source("./Figures/Figures_Detection_Probability/CreatePlots_PBDetProb_NumSurveys_Manuscript.R")

# Figure 2: Posterior distributions
## Trying out a different format
(dense_int_aru | dense_det_aru ) / (dense_int_pb | dense_det_pb )
#ggsave("./Figures/Figures_Detection_Probability/PAM_PB_Posteriors_lighterPBPalette.jpg", width=12, height=8)

# Figure 5: Playbacks for cumulative p PAM
pb_pnumsurveys
#ggsave("./Figures/Figures_Detection_Probability/PB_NumSurveys.jpg", width=12, height=6)
