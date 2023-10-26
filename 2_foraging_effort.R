########################
# Code associated with the paper:
# Diving efficiency at depth and pre-breeding foraging effort increase with haemoglobin levels in gentoo penguins
# Authors: Sarah P. McComb-Turbitt, Glenn T. Crossin, Megan Tierney, Paul Brickle, Philip Trathan, Tony D. Williams, Marie Auger-Méthé
# doi: 10.3354/meps14441
# Part 2: Foraging effort analyses
#
########################

library(tidyr)
library(dplyr)
library(ggplot2)
library(MuMIn)
library(ggpubr)

############ Loading and prepping data #########

df_full <- read.csv("2_dive_summaries.csv", stringsAsFactors = TRUE)

# Make breeding status and penguin ID factors
df_full$breed <-as.factor(df_full$breed)
df_full$peng <-as.factor(df_full$peng)
head(df_full)

# peng: individual penguin ID (was integer, now factor)
# sex: sex (including NA for individuals with no sex data)
# col: colony 
# breed: reproductive status, 0 = Non-breeding, 1 = Breeding, 2 = Early-laying
# lay: lay date
# hct: haematocrit (%)
# hb: haemoglobin (g/dL)
# mass: weight of penguin in kg at recapture
# sumvkm: sum of the vertical distance for the time at sea
# tripdur: mean trip duration
# sea: total time at sea
# tripn: number of trips
# meand: mean of max depth of dives
# maxd: maximum depth reached during the 40 days
# mass0: weight of penguin in kg at capture
# massGain: difference in weight between capture and recapture


# We exclude all of the penguins with sex info missing
# and we focus on Pebble Island since the penguins of Race Point were not breeding yet
df_PI_noNA <- df_full %>% filter(col == "PI" & !is.na(sex))


########################## Looking at relationships between covariates ########################

# Set up for plt
bp.col <- grey(0.6)
bp.text.size <- 10
bp.exp <- c(0,0.5)
bp.exp.hct <- c(0,2.8) 
inc.brack <- 0.2

# What to compare in plots
comp.sex <- list(c("F", "M"))
comp.b <- list(c("0", "1"), c("1", "2"), c("0", "2"))

set.seed(12) # for jitter

# Hb across sex
sex_hb_plot <- ggplot(df_PI_noNA, aes(x=sex, y=hb)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  geom_jitter(width=0.06, height = 0, fill="lightgrey", shape=21) +
  labs(y = "Hb (g/dL)", x= "") +
  scale_x_discrete(labels=c("F" = "Female", "M" = "Male")) + 
  theme(axis.text.x = element_text(size = bp.text.size), 
      axis.text.y = element_text(size = bp.text.size),
      axis.title = element_text(size = bp.text.size+2)) +
  stat_compare_means(comparisons = comp.sex) +
  scale_y_continuous(expand = bp.exp.hct) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("A")

# Hb across breeding status
b_hb_plot <- ggplot(df_PI_noNA, aes(x=breed, y=hb)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  geom_jitter(width=0.06, height = 0, fill="lightgrey", shape=21) +
  labs(y = "Hb (g/dL)", x= "") +
  scale_x_discrete(labels=c("0" = "Non-breeding", "1" = "Breeding", "2" = "Early laying")) +
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) +
  stat_compare_means(comparisons = comp.b, p.adjust.method = "bonferroni", step.increase = inc.brack) +
  scale_y_continuous(expand = bp.exp.hct) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("B")

# Hct across sex
sex_hct_plot <- ggplot(df_PI_noNA, aes(x=sex, y=hct)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  geom_jitter(width=0.06, height = 0, fill="lightgrey", shape=21) +
  labs(y = "Hct (%)", x= "") +
  scale_x_discrete(labels=c("F" = "Female", "M" = "Male")) + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) +
  stat_compare_means(comparisons = comp.sex) +
  scale_y_continuous(expand = bp.exp.hct) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("C")


# Hct across breeding status
b_hct_plot <- ggplot(df_PI_noNA, aes(x=breed, y=hct)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  geom_jitter(width=0.06, height = 0, fill="lightgrey", shape=21) +
  labs(y = "Hct (%)", x= "") +
  scale_x_discrete(labels=c("0" = "Non-breeding", "1" = "Breeding", "2" = "Early laying")) +
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) + 
  stat_compare_means(comparisons = comp.b, p.adjust.method = "bonferroni", step.increase = inc.brack) +
  scale_y_continuous(expand = bp.exp.hct) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("D")

# Mass across sex
sex_mass_plot <- ggplot(df_PI_noNA, aes(x=sex, y=mass)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  geom_jitter(width=0.06, height = 0, fill="lightgrey", shape=21) +
  labs(y = "Mass (kg)", x= "") +
  scale_x_discrete(labels=c("F" = "Female", "M" = "Male")) + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) +
  stat_compare_means(comparisons = comp.sex) +
  scale_y_continuous(expand = bp.exp) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("E")

# Mass across breeding status
b_mass_plot <- ggplot(df_PI_noNA, aes(x=breed, y=mass)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  geom_jitter(width=0.06, height = 0, fill="lightgrey", shape=21) +
  labs(y = "Mass (kg)", x= "") +
  scale_x_discrete(labels=c("0" = "Non-breeding", "1" = "Breeding", "2" = "Early laying")) +
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) + 
  stat_compare_means(comparisons = comp.b, p.adjust.method = "bonferroni", step.increase = inc.brack) +
  scale_y_continuous(expand = bp.exp) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("F")

ggarrange(sex_hb_plot, b_hb_plot,
          sex_hct_plot, b_hct_plot,
          sex_mass_plot, b_mass_plot, ncol=2, nrow =3)
# The warnings indicate that the normal approximation was used because of the ties in hct and mass.

ggsave("Fig4_relationships_between_vars.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")


############### Foraging effort analyses ############

# Plot set up
lm.col <- grey(0.3)
exp_x <- exp_y <- c(0.01,0.01)


####### Analyses for vertical distanced traveled
### First we do model selection, looking at all possible combinations of covariates (up to 3 cov per model)
sumvkm_dredge <- dredge(lm(sumvkm ~ breed + sex + mass + hb + hct, data = df_PI_noNA, na.action=na.fail), m.lim=c(0,3))

# Information for Table 4
sumvkm_dredge
# Best model is with only hb

### Look at best model
best_sumvkm <- lm(sumvkm ~ hb, data = df_PI_noNA)
# Information for Table S4
summary(best_sumvkm)

# Check assumptions
qqnorm(best_sumvkm$residuals)
qqline(best_sumvkm$residuals)
plot(best_sumvkm$residuals ~ best_sumvkm$fitted.values)

# Plot
sumvkm_best_plot <- ggplot(df_PI_noNA, aes(x = hb, y = sumvkm)) +
  stat_smooth(method = "lm", col=lm.col) +
  geom_point() +
  #scale_size_continuous("Hct (%)", range = c(1,2)) +
  scale_radius("Hct (%)", range = c(0.3,2)) +
  labs(y = "Vertical distance travelled (km)", x = "Hb (g/dL)") + theme_bw() +
  scale_x_continuous(expand = exp_x) +
  scale_y_continuous(expand = exp_y) +
  theme(axis.text.x = element_text(size = bp.text.size),
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.3, 'cm'), #change legend key height
        legend.key.width = unit(0.8, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10),
        plot.title = element_text(margin=margin(0,0,0,0))) +
  ggtitle("A")


####### Analyses for time at sea
### First we do model selection, looking at all possible combinations of covariates (up to 3 cov per model)
sea_dredge <- dredge(lm(sea ~ breed + sex + mass + hb + hct, data = df_PI_noNA, na.action=na.fail), m.lim=c(0,3))

# Information for Table 4
sea_dredge 
# Best  model is breeding status

# Look at best model
best_sea <- lm(sea ~ breed, data=df_PI_noNA)

# Information for Table S4
summary(best_sea)

# Check of assumptions
qqnorm(best_sea$residuals)
qqline(best_sea$residuals)
plot(best_sea$residuals ~ best_sea$fitted.values)

# For plot
sea_best_plot <- ggplot(df_PI_noNA, aes(x=breed, y=sea)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  geom_point() +
  scale_radius("Hct (%)",range = c(0.3,2)) +
  labs(y = "Time at sea (d)", x= "") +
  scale_x_discrete(labels=c("0" = "Non-breeding", "1" = "Breeding", "2" = "Early laying")) +
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(0.3, 'cm'), #change legend key height
        legend.key.width = unit(0.8, 'cm'), #change legend key width
        legend.title = element_text(size=12), #change legend title font size
        legend.text = element_text(size=10),
        plot.title = element_text(margin=margin(0,0,0,0))) +
  ggtitle("B")

####### Analyses for trip duration
### First we do model selection, looking at all possible combinations of covariates (up to 3 cov per model)
dur_dredge <- dredge(lm(tripdur ~ breed + sex + mass + hb + hct, data = df_PI_noNA, na.action=na.fail), m.lim=c(0,3))

# Information for Table 4
dur_dredge 
# The best model is the null model, we stop here

# Fig. 3
ggarrange(sumvkm_best_plot, sea_best_plot)

ggsave("Fig3_foraging_effort_best_model.jpg", dpi=500, width = 169, height = 80,  units = "mm", device = "jpg")
