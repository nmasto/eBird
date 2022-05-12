###############################################
#
# Plotting for eBird Manuscript
# by Nick Masto
# last updated: 4/9/2022 ( for manuscript )
#
# Creating figures of posterior distributions
# for supplementary material. Creating/combining
# figures of estimated marginal medians CIs for
# figure 2 and 3
#
###############################################

# Set it up------------------------------------
library(tidybayes)
library(tidyverse)
library(emmeans)
library(rstan)
library(rstanarm)
library(bayesplot)
library(patchwork)

# Source modeling script-----------------------

p1 <- emmeans::emmip(m3, ~year_bin, CIs = TRUE, type = "response", dodge = 0.4,
                     linearg = list(linetype = NA), 
                     dotarg = list(size = 4),
                     CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Extreme Climatic Event", y = "North-South Distribution Shift (km)") +
  scale_x_discrete(labels = c("non-ECE Years","February 2021 ECE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_viridis("ECE", discrete = TRUE, option = "D") +
  scale_y_continuous(breaks = seq(-200, 300, 100),
                     limits = c(-200, 300)) +
  ggtitle("Atlantic") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))

p2 <- emmeans::emmip(m3, ~year_bin ~Foraging, CIs = TRUE, type = "response", dodge = 0.4,
                     linearg = list(linetype = NA), 
                     dotarg = list(size = 4),
                     CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Foraging Strategy", y = "North-South Distribution Shift (km)") +
  scale_x_discrete(labels = c("Generalists","Grubbing", "Wetland Obligate")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_manual("Extreme Climatic Event", values = c("darkred", "skyblue2"),
                     labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(-400, 400, 100),
                     limits = c(-400, 400)) +
  ggtitle("Atlantic") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))

p3 <- emmeans::emmip(m4, ~year_bin , CIs = TRUE, type = "response", dodge = 0.4,
               linearg = list(linetype = NA), 
               dotarg = list(size = 4),
               CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Extreme Climatic Event", y = "North-South Distribution Shift (km)") +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-200, 300, 100),
                     limits = c(-200, 300)) +
  scale_x_discrete(labels = c("non-ECE Years","February 2021 ECE")) +
  scale_color_viridis("ECE", discrete = TRUE, option = "D") +
  theme_classic(base_size = 15) +
  ggtitle("Mississippi") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))



p4 <- emmeans::emmip(m4, ~year_bin ~ Foraging, CIs = TRUE, type = "response", dodge = 0.4,
                     linearg = list(linetype = NA), 
                     dotarg = list(size = 4),
                     CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Foraging Strategy", y = "") +
  scale_x_discrete(labels = c("Generalists","Grubbing", "Wetland Obligate")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_manual("Extreme Climatic Event", values = c("darkred", "skyblue2"),
                     labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(-400, 400, 100),
                     limits = c(-400, 400)) +
  ggtitle("Mississippi") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))

p5 <- emmeans::emmip(m5, ~year_bin , CIs = TRUE, type = "response", dodge = 0.4,
                     linearg = list(linetype = NA), 
                     dotarg = list(size = 4),
                     CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Extreme Climatic Event", y = "North-South Distribution Shift (km)") +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-200, 300, 100),
                     limits = c(-200, 300)) +
  scale_x_discrete(labels = c("non-ECE Years","February 2021 ECE")) +
  scale_color_viridis("ECE", discrete = TRUE, option = "D") +
  theme_classic(base_size = 15) +
  ggtitle("Central") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))


p6 <-  emmeans::emmip(m5, ~year_bin ~ Foraging, CIs = TRUE, type = "response", dodge = 0.4,
                      linearg = list(linetype = NA), 
                      dotarg = list(size = 4),
                      CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Foraging Strategy", y = "") +
  scale_x_discrete(labels = c("Generalists","Grubbing", "Wetland Obligate")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_manual("Extreme Climatic Event", values = c("darkred", "skyblue2"),
                     labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(-400, 400, 100),
                     limits = c(-400, 420)) +
  ggtitle("Central") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))


p7 <- emmeans::emmip(m6, ~year_bin , CIs = TRUE, type = "response", dodge = 0.4,
                     linearg = list(linetype = NA), 
                     dotarg = list(size = 4),
                     CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Extreme Climatic Event", y = "North-South Distribution Shift (km)") +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_y_continuous(breaks = seq(-200, 300, 100),
                     limits = c(-200, 300)) +
  scale_x_discrete(labels = c("non-ECE Years","February 2021 ECE")) +
  scale_color_viridis("ECE", discrete = TRUE, option = "D") +
  theme_classic(base_size = 15) +
  ggtitle("Pacific") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))



p8 <-  emmeans::emmip(m6, ~year_bin ~ Foraging, CIs = TRUE, type = "response", dodge = 0.4,
                      linearg = list(linetype = NA), 
                      dotarg = list(size = 4),
                      CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Foraging Strategy", y = "") +
  scale_x_discrete(labels = c("Generalists","Grubbing", "Wetland Obligate")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_manual("Extreme Climatic Event", values = c("darkred", "skyblue2"),
                     labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(-400, 400, 100),
                     limits = c(-400, 420)) +
  ggtitle("Pacific") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 16))

ggpubr::ggarrange(p1, p2,
                  p3, p4,
                  p5, p6,
                  p7, p8,
                  ncol = 2,
                  nrow = 4,
                  align = "hv",
                  legend = "none"
                  #common.legend = TRUE,
                  #labels = c("Atlantic","Atlantic",
                  #           "Mississippi", "Mississippi",
                  #           "Central", "Central",
                  #           "Pacific", "Pacific"
                  #),
                  #label.x = c(.4, .4, 0.31, 0.31, 0.4, .4, 0.4, 0.4)
)

ggsave("plots/figureS4_manuscript.jpeg", width = 12, height = 15, units = "in", dpi = 300)

# Plotting eemeans across continents-----------------------------------------

p9 <- emmeans::emmip(m1, ~year_bin, CIs = TRUE, type = "response", dodge = 0.4,
               linearg = list(linetype = NA), 
               dotarg = list(size = 4),
               CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Extreme Climatic Event", y = "North-South Distribution Shift (km)") +
  scale_x_discrete(labels = c("non-ECE Years","February 2021 ECE")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_viridis("ECE", discrete = TRUE, option = "D") +
  scale_y_continuous(breaks = seq(-100, 200, 100),
                     limits = c(-100, 200)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")
#axis.title.y = element_text(angle = 0, vjust = 0.5))

p10 <-  emmeans::emmip(m1, ~year_bin ~ Foraging, CIs = TRUE, type = "response", dodge = 0.4,
                      linearg = list(linetype = NA), 
                      dotarg = list(size = 4),
                      CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Foraging Strategy", y = "") +
  scale_x_discrete(labels = c("Generalists","Grubbing", "Wetland Obligate")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_manual("Extreme Climatic Event", values = c("darkred", "skyblue2"),
                     labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(-200, 300, 100),
                     limits = c(-200, 300)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")

p11 <- emmeans::emmip(m2, ~year_bin, CIs = TRUE, type = "response", dodge = 0.4,
                     linearg = list(linetype = NA), 
                     dotarg = list(size = 4),
                     CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Severe February Temperature", y = "North-South Distribution Shift (km)") +
  scale_x_discrete(labels = c("non-ECE Years","Severe Temperature")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_viridis("ECE", discrete = TRUE, option = "D") +
  scale_y_continuous(breaks = seq(-100, 200, 100),
                     limits = c(-100, 200)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")
#axis.title.y = element_text(angle = 0, vjust = 0.5))

p12 <-  emmeans::emmip(m2, ~year_bin ~ Foraging, CIs = TRUE, type = "response", dodge = 0.4,
                       linearg = list(linetype = NA), 
                       dotarg = list(size = 4),
                       CIarg = list(lwd = 1.2, alpha = 1)) +
  labs(x = "Foraging Strategy", y = "") +
  scale_x_discrete(labels = c("Generalists","Grubbing", "Wetland Obligate")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme_classic(base_size = 15) +
  geom_hline(yintercept = 0, color = "gray", linetype = "dashed") +
  scale_color_manual("Severe February Temperature", values = c("darkred", "skyblue2"),
                     labels = c("No", "Yes")) +
  scale_y_continuous(breaks = seq(-200, 300, 100),
                     limits = c(-200, 300)) +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(color = "black", size = 18),
        axis.title = element_text(color = "black", size = 20),
        legend.position = "none")

combined_continental <- p9 + p10 + p11 + p12
combined_continental <- combined_continental + plot_annotation(tag_levels = "a")

ggsave("plots/figure3_manuscript.jpeg", width = 13, height = 12, units = "in", dpi = 500)
# Plotting the posterior distributions for all flyways

patch <- atlpost + mspost + cenpost + pacpost
patch[[2]] <- patch[[2]] + theme(axis.text.y = element_blank())
patch[[4]] <- patch[[4]] + theme(axis.text.y = element_blank())
patch
ggsave("plots/figureS3_manuscript.jpeg", width = 12, height = 10, units = "in", dpi = 500)


# Plotting the posterior distributions for continent

patch2 <- contECE + contseverefebs 
patch2[[2]] <- patch2[[2]] + theme(axis.text.y = element_blank())
patch2

ggsave("plots/figureS2_manuscript.jpeg", width = 12, height = 7, units = "in", dpi = 500)

