#### Create Visualizations for Cost Analysis #####

library(tidyverse)
library(patchwork)
source("./Function_Scripts/Create_HexCodes_CuckooColorBlind.R")

# read in data
cost_dat <- read.csv("./Analysis/Analysis_Estimated_Cost/Cost_Summary_Totals_v4.csv")


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



#### Export graphics you want #####
final1 <- plot1_pam | plot1_pb
jpeg("./Analysis/Analysis_Estimated_Cost/Cost_byOrgMethod_v4_2-27.jpeg", width=800, height=400)
final1
dev.off()

# Combine hours and split apart costs into one
final2 <- plot_items | plot_hours
jpeg("./Analysis/Analysis_Estimated_Cost/HrsYear_CostSupplies_byMethod_v4_2-27.jpeg", width=800, height=400)
final2 
dev.off()


