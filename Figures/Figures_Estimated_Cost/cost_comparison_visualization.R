#### Create Visualizations for Cost Analysis #####

library(tidyverse)
library(patchwork)
source("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/R_Scripts/5_Visualization/Create_HexCodes_CuckooColorBlind.R")

# read in data
cost_dat <- read.csv("./Data/Cost_Analysis/Cost_Summary_Totals_v4.csv")


# Visualization 1: bar graph with year on x axis, cost on y, bar by organization for each method ####
v1_pam <- cost_dat %>% filter(method == "pam") %>% filter(! item == "hours") %>% group_by(org, year) %>% summarize(total = sum(value))

v1_pb <- cost_dat %>% filter(method == "pb") %>% filter(! item == "hours")%>% group_by(org, year) %>% summarize(total = sum(value))
plot1_pam <- ggplot(data = v1_pam) +
  geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("agency"=d_palette[5],
                               "univ_lab" = d_palette[8]),
                    labels = c("agency" = "Wildlife Agency",
                               "univ_lab" = "University Lab"),
                    name = "Organization") +
  theme(axis.text.y = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 13), 
        axis.title.x = element_text(size = 13),
        text = element_text(size = 13),
        legend.position = "bottom")+
  scale_y_continuous(limits = c(0,20000), breaks = c(0,5000,10000,15000,20000))+
  labs(title = "PAM",y = "Total (USD $)", x = "Project Year")

plot1_pb <- ggplot(data = v1_pb) +
  geom_bar(aes(x = year, y = total, fill = org), position = "dodge", stat= "identity") +
  theme_minimal() +
  scale_fill_manual(values = c("agency"=pb_palette[5],
                               "univ_lab" = pb_palette[8]),
                    labels = c("agency" = "Wildlife Agency",
                               "univ_lab" = "University Lab"),
                    name = "Organization") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 13), 
        axis.title.x = element_text(size = 13),
        text = element_text(size = 13),
        legend.position = "bottom")+
  scale_y_continuous(limits = c(0,20000), breaks = c(0,5000,10000,15000,20000))+
  labs(title = "Playback", x = "Project Year")


# Visualization 2: each method across years regardless of organization ####
v2 <- cost_dat %>% group_by(method, year) %>% summarize(total = sum(value))
# Try this with facet_wrap
costperyear_fortalk <- ggplot(data = v2) +
  geom_bar(aes(x = year, y = total,fill = method), stat= "identity") +
  facet_wrap(~ method, ncol = 2, labeller = as_labeller(c("pb" = "Playback", "pam" = "PAM"))) +
  scale_fill_manual(values = c("pb"=pb_palette[4],
                               "pam" = d_palette[4]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM"),
                    guide = "none") +
   theme_minimal() +
   labs(y = "Total (USD)", x = "Year") +
  theme(text = element_text(size = 20))

v_pam <- v2 %>% filter(method == "pam")
costperyear_PAM <- ggplot(data = v_pam) +
  geom_bar(aes(x = factor(year), y = total), fill = d_palette[4], stat= "identity") +
  theme_minimal() +  
  scale_y_continuous(limits = c(0,35000))+
  scale_x_discrete(labels = c("1","2","3"))+
  labs(y = "Total (USD $)", x = "Year",title = "PAM") +
  theme(text = element_text(size = 25))
jpeg("./Deliverables/MetChap_CostVisualizations/CostByYear_PAM_1-28.jpeg", width=400, height=500)
costperyear_PAM
dev.off()

v_pb <- v2 %>% filter(method == "pb")
costperyear_PB <- ggplot(data = v_pb) +
  geom_bar(aes(x = factor(year), y = total), fill = pb_palette[4], stat= "identity") +
  theme_minimal() +
  scale_y_continuous(limits = c(0,35000))+
  scale_x_discrete(labels = c("1","2","3"))+
  labs(y = "Total (USD $)", x = "Year", title = "Playback") +
  theme(text = element_text(size = 25))
jpeg("./Deliverables/MetChap_CostVisualizations/CostByYear_PB_1-28.jpeg", width=400, height=500)
costperyear_PB
dev.off()


# Visualization 3: components of each method ####
v3 <- cost_dat %>% filter(!item == "hours") %>% group_by(method, item) %>% summarize(total = sum(value))%>%
  mutate(item = tools::toTitleCase(item))

sites_total <- 40
years <- 3
combined_sites <- sites_total * years
by_site <- v3 %>% group_by(method) %>% summarize(cost_persite = sum(total)/combined_sites)

# without facet wrapping
plot_items <- ggplot(data = v3) +
  geom_bar(aes(x = item, y = total, fill = method), position = "dodge", stat = "identity") + 
  scale_fill_manual(values = c("pb"=pb_palette[4],
                               "pam" = d_palette[3]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM"),
                    name = "Method",) +
  theme_minimal()+
  labs(y = "Total (USD $)", x = "Cost Type") +
  theme(text = element_text(size = 13),
        legend.position = "bottom",
        legend.justification = "right")

# Create a version of this for the talk
plot_items_fortalk <- ggplot(data = v3) +
  geom_bar(aes(x = method, y = total, fill = item), position = "stack", stat = "identity") + 
  # use palette_8 3,5 ,6 
  scale_fill_manual(values = c("Salary"=palette_8[3],
                               "Supplies" = palette_8[5],
                               "Transportation" = palette_8[6])) +
  scale_x_discrete(labels = c("pam" = "PAM", "pb" = "Playback"))+
  theme_minimal()+
  labs(y = "Total (USD $)", x = NULL, fill = "Cost Type") +
  theme(text = element_text(size = 20))


# Visualization 4: hours by method and year ####
v4 <- cost_dat %>% filter(item == "hours") %>% group_by(method, year) %>% summarize(total = sum(value))
plot_hours <- ggplot(data = v4) +
  geom_bar(aes(x = year, y = total, fill = method), position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("pb"=pb_palette[4],
                               "pam" = d_palette[3]),
                    labels = c("pb" = "Playback",
                               "pam" = "PAM"),
                    name = "Method", guide = "none") +
  theme_minimal() +
  labs(y = "Personnel Hours", x = "Project Year")  +
  theme(text = element_text(size = 13),
        legend.position = "bottom",
        legend.justification = "left")






# TO look at palettes
# "#332288" "#117733" "#44AA99" "#88CCEE" "#DDCC77" "#CC6677" "#AA4499" "#882255"

# Why are these not high quality?????
#### Export graphics you want #####
final1 <- plot1_pam | plot1_pb
jpeg("./Deliverables/MetChap_CostVisualizations/Cost_byOrgMethod_v4_2-27.jpeg", width=800, height=400)
final1
dev.off()
#ggsave("./Deliverables/MetChap_CostVisualizations/Cost_byOrgMethod_v4_2-27.jpeg", width=800, height=400)

# Combine hours and split apart costs into one
final2 <- plot_items | plot_hours
jpeg("./Deliverables/MetChap_CostVisualizations/HrsYear_CostSupplies_byMethod_v4_2-27.jpeg", width=800, height=400)
final2 
dev.off()


