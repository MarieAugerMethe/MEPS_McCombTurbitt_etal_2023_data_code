########################
# Code associated with the paper:
# Diving efficiency at depth and pre-breeding foraging effort increase with haemoglobin levels in gentoo penguins
# Authors: Sarah P. McComb-Turbitt, Glenn T. Crossin, Megan Tierney, Paul Brickle, Philip Trathan, Tony D. Williams, Marie Auger-Méthé
# doi: 10.3354/meps14441
# Part 1: Diving efficiency analyses
#
########################

library(tidyr)
library(ggplot2)
library(mgcv)
library(mgcViz)
#remotes::install_github("gavinsimpson/gratia")
library(gratia) # Need the Github version: https://github.com/gavinsimpson/gratia
library(dplyr)
library(ggnewscale) # For transparent layer in 3d plots
library(ggpubr)
library(MuMIn) # For AICc

####################################
# Setting plot parameters
##

# Text size
bp.text.size <- 8
# Share parameters
exp_x <- c(0.01, 0.01)
exp_y <- c(0.05, 0.05)

# For plotting se
se.breaks <- seq(0, 0.12, by=0.02)
my.cols.se <- rgb(0.9, 0.9, 0.9, seq(0, 1, length.out = length(se.breaks)))
col.se.line <- rgb(0.9, 0.9, 0.9 ,0.7)
size.se.line <- 0.3

# For slices
deep.threshold <- 140
shallow <- 65
deep.col <- "darkblue"
shallow.col <- "skyblue"
general.depth.col <- "blue3"
slice.size <- 0.9
dive.eff.breaks <- seq(0, 0.5, by=0.05)

######################## Plotting function #############
# Depth slice
mdepth_slice_plot <- function(model, dataset, ptitle = "A", type="response"){
  
  slice_uni <- data_slice(model, mdepth= evenly(mdepth, n=100))
  pred_uni <- predict(model, slice_uni, se.fit=TRUE, type=type)
  slice_pred <- cbind(slice_uni, est=pred_uni$fit, se=pred_uni$se.fit)
  
  # Plot
  depth_plot <- slice_pred %>%
    add_confint() %>%
    ggplot(aes(y = est, x = mdepth)) +
    geom_point(data=dataset, aes(x=mdepth, y=de)) +
    geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
                alpha = 0.2, fill = general.depth.col) +
    geom_line(colour = general.depth.col, linewidth = 1.5) +
    labs(y = "Diving efficiency", x = "Depth (m)") +
    theme_bw() +
    scale_x_continuous(expand = exp_x) +
    scale_y_continuous(expand = exp_x + 0.01) + 
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(0.3, 'cm'), #change legend key height
          legend.key.width = unit(1.1, 'cm'), #change legend key width
          legend.title = element_text(size=12), #change legend title font size
          legend.text = element_text(size=10),
          plot.title = element_text(margin=margin(0,0,0,0))) + 
    ggtitle(ptitle)
  
  return(depth_plot)
}

# Depth and Hb slice

depth_hb_slice_plot <- function(model, ptitle = "B", type="response"){
  
  slice_depth_hb <- data_slice(model, mdepth= evenly(mdepth, n=100), hb=evenly(hb, n=100))
  pred_depth_hb <- predict(model, slice_depth_hb, se.fit=TRUE, type=type)
  slice_depth_hb_pred <- cbind(slice_depth_hb, est=pred_depth_hb$fit, se=pred_depth_hb$se.fit)
  
  depth_hb_plot <- ggplot() +
    geom_contour_filled(data = slice_depth_hb_pred, aes(y = hb, x=mdepth, z=est), breaks=dive.eff.breaks) +
    guides(fill = guide_legend(title="Diving efficiency", order=1)) +
    labs(y = "Hb (g/dL)", x= "Depth (m)") +
    new_scale("fill") +
    geom_contour(data = slice_depth_hb_pred, aes(y = hb, x=mdepth, z=se), color=col.se.line, linewidth=size.se.line, breaks=se.breaks) +
    geom_contour_filled(data = slice_depth_hb_pred, aes(y = hb, x=mdepth, z=se), breaks=se.breaks) +
    scale_fill_manual("Standard error", values = my.cols.se) +
    theme_bw() +
    scale_x_continuous(expand = exp_x-0.005) +
    scale_y_continuous(expand = exp_x) +
    ggtitle(ptitle) + 
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(0.4, 'cm'), #change legend key height
          legend.key.width = unit(0.3, 'cm'), #change legend key width
          legend.title = element_text(size=9), #change legend title font size
          legend.text = element_text(size=9),
          plot.title = element_text(margin=margin(0,0,0,0))) +
    guides(fill = guide_legend(override.aes = list(color = col.se.line, lwd = size.se.line)))
  
  return(depth_hb_plot)
}

#### Depth and Hct slice
depth_hct_slice_plot <- function(model, ptitle = "C", type="response"){
  
  slice_depth_hct <- data_slice(model, mdepth= evenly(mdepth, n=100), hct=evenly(hct, n=100))
  pred_depth_hct <- predict(model, slice_depth_hct, se.fit=TRUE, type=type)
  slice_depth_hct_pred <- cbind(slice_depth_hct, est=pred_depth_hct$fit, se=pred_depth_hct$se.fit)
  
  depth_hct_plot <- ggplot() +
    geom_contour_filled(data = slice_depth_hct_pred, aes(y = hct, x=mdepth, z=est), breaks=dive.eff.breaks) + 
    guides(fill=guide_legend(title="Diving efficiency", order =1)) +
    labs(y = "Hct (%)", x= "Depth (m)") +
    new_scale("fill") + 
    geom_contour(data = slice_depth_hct_pred, aes(y = hct, x=mdepth, z=se), color =col.se.line, linewidth=size.se.line, breaks=se.breaks) +
    geom_contour_filled(data = slice_depth_hct_pred, aes(y = hct, x=mdepth, z=se), breaks=se.breaks) +
    scale_fill_manual("Standard error", values = my.cols.se) +
    theme_bw() +
    scale_x_continuous(expand = exp_x - 0.005) +
    scale_y_continuous(expand = exp_x) +
    ggtitle(ptitle) + 
    theme(legend.key.size = unit(1, 'cm'), #change legend key size
          legend.key.height = unit(0.4, 'cm'), #change legend key height
          legend.key.width = unit(0.3, 'cm'), #change legend key width
          legend.title = element_text(size=9), #change legend title font size
          legend.text = element_text(size=9),
          plot.title = element_text(margin=margin(0,0,0,0))) +
    guides(fill = guide_legend(override.aes = list(color = col.se.line, lwd = size.se.line)))
  
  return(depth_hct_plot)
}

#################################### Load and prep the data #####################
dfdives <- read.csv("1_individual_dives_40days.csv", stringsAsFactors = TRUE)

# Penguin ID should be a factor
dfdives$peng <- as.factor(dfdives$peng) # Make penguin ID a factor

######
# Description of variables
# peng: individual penguin ID (integer when loaded, changed as factor)
# time_of_dive: the time of the start of the dive
# trip_start: time of the start of the trip
# trip_end: time of the end of the trip
# sex: sex
# col: colony 
# hb: haemoglobin (g/dL)
# hct: haematocrit (%)
# mass: weight of penguin in kg
# diveT: duration of dive (sec)
# bottomT: time at the bottom (sec)
# psdi: post dive surface interval (sec)
# mdepth: max depth (m)
# de: diving efficiency (proportion of the time within a dive spent at the bottom)

#################################### Select subsetting #####################
# There is autocorrelation in the residuals of the full model.
# In this section, we subset the data to remove the autocorrelation.
# We choose the smallest subsetting value that remove the autocorrelation, which is one dive every 70
# We can look at different subsetting, by changing the value of dive_by_num

dive_by_num <- 70 # still some correlation according to acf at 50, 60 (+ convergence problem) 
subset_s <- dfdives[seq(1, nrow(dfdives), dive_by_num),]

full_select_ti_subset_s <- gamm(de ~ s(mdepth) + s(mass) + s(hct) + s(hb) + 
                                ti(mdepth, mass) +
                                ti(mdepth, hct) +
                                ti(mdepth, hb) +
                                sex + col,
                              data = subset_s, 
                              random = list(peng = ~ 1), select=TRUE, method="ML")

res_full_subset_s <- residuals(full_select_ti_subset_s$lme, type = "normalized")
acf(res_full_subset_s, main = "Auto-correlation plot for residuals",lag.max = 50)
pacf(res_full_subset_s, main = "Partial auto-correlation plot for residuals",lag.max = 50)
# Still autocorellation at 50
# Still autocorellation at 60 (and convergence issues)
# Good at 70

# Final subsetting
dive_by_num <- 70 
subset_70 <- dfdives[seq(1, nrow(dfdives), dive_by_num), ]

# Only a small portion of the dataset is kept:
nrow(subset_70)/nrow(dfdives %>% na.omit()) * 100
# But we have still > 35 dives per individuals, and average of 85 dives +- 20 per individuals
dives_per_ind <- table(subset_70$peng)
min(dives_per_ind)
mean(dives_per_ind)
sd(dives_per_ind)

############################ Assessing whether we need sex in the analysis #################
# There are 3 individuals for which we do not have sex data.
summarize(group_by(subset_70,peng), sex = unique(sex))
# Here are looking analyses where we remove these individuals, 
# and see whether sex is an important covariate.


# By removing NAs (which would be done automatically when using gamm), 
# we are dropping the three individuals for which we do not have sex.
subset_70 <- subset_70 %>% na.omit()
# Checking that the 3 individuals with no sex data are removed
summarize(group_by(subset_70,peng), sex = unique(sex))

full_select_ti_subset <- gamm(de ~ s(mdepth) + s(mass) + s(hct) + s(hb) + 
                                ti(mdepth, mass) +
                                ti(mdepth, hct) +
                                ti(mdepth, hb) +
                                sex + col,
                              data = subset_70, 
                              random = list(peng = ~ 1), select=TRUE, method="ML")

# Quick check of assumptions
acf(residuals(full_select_ti_subset$lme, type = "normalized"), main = "Auto-correlation plot for residuals",lag.max = 50)
# No autocorrelation
appraise(full_select_ti_subset$gam)
# Looks ok, though some heteroskedasticity, likely due to the fact that it's proportion data.
# We address this later.

# Table S1
summary(full_select_ti_subset$gam)
# According to the results, sex is not a important (p-value > 0.05)

# Plot results (Fig. S2)
depth_plot_sex <- mdepth_slice_plot(full_select_ti_subset$gam, subset_70)
depth_hb_plot_sex <- depth_hb_slice_plot(full_select_ti_subset$gam)
depth_hct_plot_sex <- depth_hct_slice_plot(full_select_ti_subset$gam)

ggarrange(depth_plot_sex, depth_hb_plot_sex, depth_hct_plot_sex, ncol = 1, common.legend = TRUE, legend = "right")

ggsave("FigS2_gamm_res_sex.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")

###########
# We now see, whether a model without sex as a covariate but with the same data (i.e., without penguins that are missing sex info)
# performs as well or better according to AICc.

full_select_ti_subset_removeSex <- gamm(de ~ s(mdepth) + s(mass) + s(hct) + s(hb) + 
                                ti(mdepth, mass) +
                                ti(mdepth, hct) +
                                ti(mdepth, hb) +
                                col,
                              data = subset_70, 
                              random = list(peng = ~ 1), select=TRUE, method="ML")
# With sex as covariate
AICc(full_select_ti_subset$lme)
# Without sex as covariate
AICc(full_select_ti_subset_removeSex$lme)
# The model without sex performs better (lower AICc), delta AICc:
diff(AICc(full_select_ti_subset$lme, full_select_ti_subset_removeSex$lme)[,2])

############## Final main GAMM (in main text), no sex as covariate and 70 subsetting #################
# Redoing subsetting from original data, 
# to retrieve the 3 individuals that were removed above (that had no sex data)
subset_70_nosex <- dfdives[seq(1, nrow(dfdives), dive_by_num), ]
# Checking that we have all individuals
summarize(group_by(subset_70_nosex,peng), sex = unique(sex))

full_select_ti_subset_noSex <- gamm(de ~ 
                                      s(mdepth) + s(mass) + s(hct) + s(hb) + 
                                      ti(mdepth, mass) +
                                      ti(mdepth, hct) +
                                      ti(mdepth, hb) +
                                      col,
                                    data = subset_70_nosex, 
                                    random = list(peng = ~ 1), select=TRUE, method="ML")
# Check of assumptions
# Is there autocorrelation when we include the 3 individuals?
res_full_subset_noSex <- residuals(full_select_ti_subset_noSex$lme, type = "normalized")
acf(res_full_subset_noSex, main = "Auto-correlation plot for residuals",lag.max = 50)
pacf(res_full_subset_noSex, main = "Partial auto-correlation plot for residuals",lag.max = 50)
# Maybe in this case some small lag 2 issue, but pretty minor.

# Create model object to simplify later calls
final_gam <- full_select_ti_subset_noSex$gam

appraise(final_gam)
# Generally good, though some heteroscedasticity, likely due to the fact that de is proportion data.
# We return to this below.

# Plot to show the heteroscedasticity
gamm_final_residuals <- as.data.frame(cbind(resid = final_gam$residuals, fitted = final_gam$fitted.values))
ggplot(gamm_final_residuals, aes(y=resid, x=fitted)) +
  geom_point() +
  labs(y = "Residuals", x = "Fitted values")
ggsave("FigS5_residuals_main_gamm.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")



######## Main results
# Table 2
summary(final_gam)

########
# Main plots for main text

# Depth
depth_plot <- mdepth_slice_plot(final_gam, subset_70_nosex)
# Depth & hb
depth_hb_plot <- depth_hb_slice_plot(final_gam)
# depth & hct
depth_hct_plot <- depth_hct_slice_plot(final_gam)

# Plot together
ggarrange(depth_plot, depth_hb_plot, depth_hct_plot, ncol = 1, common.legend = TRUE, legend = "right")

# Fig. 1
ggsave("Fig1_main_gamm_res.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")


############## Post-hoc analyses of deep dives #################

# Calculate the proportion of dives that are deep, 
# need to keep covariates that were selected as important in GAMMs above: hb, hct, col
subset_70_nosex <- subset_70_nosex %>% group_by(peng)
subset_70_nosex_sum <- summarise(subset_70_nosex, 
                         n_dives=n(), 
                         n_deep_dives=sum(mdepth >= deep.threshold), 
                         prop_deep_dives = n_deep_dives/n_dives,
                         hb = unique(hb),
                         hct = unique(hct),
                         col = unique(col))

# We cannot do both hb and hct as covariates in the models since very few data points
# and we are using a GAM (as opposed to a linear regression) because there are nonlinearity

#### GAM on proportion of deep dives with hb
deep_hb_gam <- gam(prop_deep_dives ~ 
                     s(hb) + col, 
                   data=subset_70_nosex_sum, select=TRUE, method="ML")
appraise(deep_hb_gam)
# Looks ok, one observation is much higher than others.
# We look into this in the bonus section below.

# Information for Table 3
summary(deep_hb_gam)

# For Fig. 2
slice_deep_prop_hb <- data_slice(deep_hb_gam, hb= evenly(hb, n=100))
pred_deep_prop_hb  <- predict(deep_hb_gam, slice_deep_prop_hb, se.fit=TRUE)
slice_deep_prop_hb_pred <- cbind(slice_deep_prop_hb, est=pred_deep_prop_hb$fit, se=pred_deep_prop_hb$se.fit)

deep_prop_plot <- slice_deep_prop_hb_pred %>%
  add_confint() %>%
  ggplot(aes(y = est, x = hb)) +
  geom_line(colour = deep.col, linewidth = 1.5) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.1, fill = deep.col) +
  geom_point(subset_70_nosex_sum, mapping = aes(x=hb, y=prop_deep_dives)) +
  labs(y = expression("Proportion of dives" >= "140 m deep"), x = "Hb (g/dL)") +
  theme_bw() +
  scale_x_continuous(expand = exp_x) +
  scale_y_continuous(expand = exp_y - 0.042) +
  ggtitle("A")  + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(margin=margin(0,0,0,0)))


#### GAM on proportion of deep dives with hct
deep_hct_gam <- gam(prop_deep_dives ~ 
                      s(hct) + col, 
                    data=subset_70_nosex_sum, select=TRUE, method="ML")
appraise(deep_hct_gam)
# There are some normality and heteroskedasticity issue,
# again likely in part becuase this is proportion data.
# Information for Table 3 
summary(deep_hct_gam)
# Relationship with Hct not significant,
# we do not explore this further.

#### GAMM on PDSI of deep dives with hb
pdsi_deep_hb_gam <- gamm(pdsi ~ 
                           s(hb) + col, 
                         data=subset(subset_70_nosex, mdepth >= deep.threshold), 
                         random = list(peng = ~ 1), select=TRUE, method="ML")
appraise(pdsi_deep_hb_gam$gam)
# Looks good overall, some minor deviations

# Information for Table 3
summary(pdsi_deep_hb_gam$gam)

# For Fig. 2
slice_deep_pdsi_hb <- data_slice(pdsi_deep_hb_gam$gam, hb= evenly(hb, n=100))
pred_deep_pdsi_hb  <- predict(pdsi_deep_hb_gam$gam, slice_deep_pdsi_hb, se.fit=TRUE)
slice_deep_pdsi_hb_pred <- cbind(slice_deep_pdsi_hb, est=pred_deep_pdsi_hb$fit, se=pred_deep_pdsi_hb$se.fit)

deep_pdsi_plot <- slice_deep_pdsi_hb_pred %>%
  add_confint() %>%
  ggplot(aes(y = est, x = hb)) +
  geom_line(colour = deep.col, size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.1, fill = deep.col) +
  geom_point(subset(subset_70_nosex, mdepth >= deep.threshold), mapping = aes(x=hb, y=pdsi)) +
  labs(y = "Post-dive surface interval (s)", x = "Hb (g/dL)") +
  theme_bw() +
  scale_x_continuous(expand = exp_x) +
  scale_y_continuous(expand = exp_y) +
  ggtitle("B")  + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(margin=margin(0,0,0,0)))

#### GAMM on PDSI of deep dives with hct
pdsi_deep_hct_gam <- gamm(pdsi ~ 
                            s(hct) + col, 
                          data=subset(subset_70_nosex, mdepth >= deep.threshold), 
                          random = list(peng = ~ 1), select=TRUE, method="ML")
appraise(pdsi_deep_hct_gam$gam)
# Looks good

# Information for Table 3
summary(pdsi_deep_hct_gam$gam)

#### GAMM on bottom time of deep dives with hb
bottomT_deep_hb_gam <- gamm(bottomT ~ 
                              s(hb)+ col, 
                            data=subset(subset_70_nosex, mdepth >= deep.threshold), 
                            random = list(peng = ~ 1), select=TRUE, method="ML")
appraise(bottomT_deep_hb_gam$gam)
# Looks good, potentially some minor deviations

# Information for Table 3
summary(bottomT_deep_hb_gam$gam)

# For Fig. 2
slice_deep_bottomT_hb <- data_slice(bottomT_deep_hb_gam$gam, hb= evenly(hb, n=100))
pred_deep_bottomT_hb  <- predict(bottomT_deep_hb_gam$gam, slice_deep_bottomT_hb, se.fit=TRUE)
slice_deep_bottomT_hb_pred <- cbind(slice_deep_bottomT_hb, est=pred_deep_bottomT_hb$fit, se=pred_deep_bottomT_hb$se.fit)

deep_bottomT_plot <- slice_deep_bottomT_hb_pred %>%
  add_confint() %>%
  ggplot(aes(y = est, x = hb)) +
  geom_line(colour = deep.col, size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.1, fill = deep.col) +
  geom_point(subset(subset_70_nosex, mdepth >= deep.threshold), mapping = aes(x=hb, y=bottomT)) +
  scale_radius("Hct (%)", range = c(0.3,2)) + 
  labs(y = "Bottom time (s)", x = "Hb (g/dL)") +
  theme_bw() +
  scale_x_continuous(expand = exp_x) +
  scale_y_continuous(expand = exp_y) +
  ggtitle("C")  + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(margin=margin(0,0,0,0)))

#### GAMM on bottom time of deep dives with hct
bottomT_deep_hct_gam <- gamm(bottomT ~ 
                               s(hct)+ col, 
                             data=subset(subset_70_nosex, mdepth >= deep.threshold), 
                             random = list(peng = ~ 1), select=TRUE, method="ML")
appraise(bottomT_deep_hct_gam$gam)
# Looks good

# Information for Table 3
summary(bottomT_deep_hct_gam$gam)
# Not significant, so not further explored


### Fig. 2
# Since only hb is important, plotting Hb for prop of dive, pdsi, and bottom time
# The Hct results are in Table 3
ggarrange(deep_prop_plot, 
          deep_pdsi_plot, 
          deep_bottomT_plot, ncol=1)

# Fig. 2
ggsave("Fig2_deepdives.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")


################# Supplementary analyses ##################
# Here are a set of supplementary analyses that verify that the results are robust
# to deviation from assumptions. 
# Most of these are included in the supplement, though we have added additional checks.


###### Colinearity and Concurvity
cor(subset_70_nosex$hb, subset_70_nosex$hct)
cor(subset_70_nosex$hct, subset_70_nosex$mass)
cor(subset_70_nosex$hb, subset_70_nosex$mass)

# Fig. S3
cor_hct_hb <- ggplot(subset_70_nosex, aes(y = hct, x = hb)) +
  geom_point() + 
  theme_bw() +  
  theme(legend.text = element_text(size=10)) +
  labs(y = "Hct (%)", x= "Hb (g/dL)") + ggtitle("A")

cor_hct_mass <- ggplot(subset_70_nosex, aes(y = hct, x = mass)) +
  geom_point() + 
  theme_bw() +  
  theme(legend.text = element_text(size=10)) +
  labs(y = "Hct (%)", x= "Mass (kg)") + ggtitle("B")

cor_hb_mass <- ggplot(subset_70_nosex, aes(y = hb, x = mass)) +
  geom_point() + 
  theme_bw() +  
  theme(legend.text = element_text(size=10)) +
  labs(y = "Hb (g/dL)", x= "Mass (kg)") + ggtitle("C")

ggarrange(cor_hct_hb, 
          cor_hct_mass, 
          cor_hb_mass, ncol=1)

ggsave("FigS3_cor.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")

# Table S2
concurvity(final_gam, full=TRUE)
# We definitely have concurvity problems

# We look at each important covariate separately, to see that the results are robust
# hb + depth
hb_depth_gamm <- gamm(de ~ 
                        s(mdepth) + s(hb) +
                        ti(mdepth, hb) +
                        col,
                      data = subset_70_nosex, 
                      random = list(peng = ~ 1))
summary(hb_depth_gamm$gam)
# Generally same results
concurvity(hb_depth_gamm$gam, full=TRUE) # Ok
# Much better

# For Fig. S4 - to show that the general results are affected by the concurvity
depth_depth_hb_only_plot <- mdepth_slice_plot(hb_depth_gamm$gam, subset_70_nosex) 
depth_hb_only_plot <- depth_hb_slice_plot(hb_depth_gamm$gam, ptitle="C")


# hct + depth
hct_depth_gamm <- gamm(de ~ 
                        s(mdepth) + s(hct) +
                        ti(mdepth, hct) +
                        col,
                      data = subset_70_nosex, 
                      random = list(peng = ~ 1))
summary(hct_depth_gamm$gam)
concurvity(hct_depth_gamm$gam, full=TRUE) 
# Much better

# For Fig. S4 - to show that the general results are affected by the concurvity
depth_depth_hct_only_plot <- mdepth_slice_plot(hct_depth_gamm$gam, subset_70_nosex, ptitle = "B") 
depth_hct_only_plot <- depth_hct_slice_plot(hct_depth_gamm$gam, ptitle = "D")


ggarrange(ggarrange(depth_depth_hb_only_plot, depth_depth_hct_only_plot,  
                    nrow=1, ncol=2), 
          depth_hb_only_plot, depth_hct_only_plot, nrow=3, ncol=1, common.legend = TRUE, legend = "right")

ggsave("FigS4_single_indices.jpg", dpi=500, width = 169, height = 190,  units = "mm", device = "jpg")


####### Beta regression ###
# While the diving efficiency is proportion data, 
# we use a GAMM with a normal distribution in most analysis,
# as there are no easy solution to fit a GAMM in R that is for proportion data 
# and allow for things like shrinkage.

# Here we explore a GAMM with a beta distribution.
# A beta regression is more appropriate for proportion data.
# However, there are a few issues with this analyses. 
# For example, we need to add an arbitrary value to 0s, 
# since the model does not allow for values of exactly 0.
subset_70_nosex_n0 <- subset_70_nosex
subset_70_nosex_n0$de[subset_70_nosex$de == 0] <- 1e-14

full_select_ti_subset_noSex_br <- gamm(de ~ 
                                         s(mdepth) + s(mass) + s(hct) + s(hb) + 
                                         ti(mdepth, mass) +
                                         ti(mdepth, hct) +
                                         ti(mdepth, hb) +
                                         col,
                                       data = subset_70_nosex_n0, family = betar(link="logit"), 
                                       random = list(peng = ~ 1))
# Note that, as the warning indicates, gamm is not designed for beta distribution,
# another reason, why we favour using the normal distribution in main text, despite it's issues.

# Table S3
summary(full_select_ti_subset_noSex_br$gam)


# Depth
depth_plot_beta <- mdepth_slice_plot(full_select_ti_subset_noSex_br$gam, subset_70_nosex_n0)
# Depth & hb
depth_hb_plot_beta <- depth_hb_slice_plot(full_select_ti_subset_noSex_br$gam)
# depth & hct
depth_hct_plot_beta <- depth_hct_slice_plot(full_select_ti_subset_noSex_br$gam)

# Plot together
ggarrange(depth_plot_beta, depth_hb_plot_beta, depth_hct_plot_beta, ncol = 1, common.legend = TRUE, legend = "right")

# Fig. S6
ggsave("FigS6_beta_gamm_results.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")



# We see however that using the beta distribution removes the heteroskedasticity
gamm_beta_residuals <- as.data.frame(cbind(resid = full_select_ti_subset_noSex_br$gam$residuals, fitted = full_select_ti_subset_noSex_br$gam$fitted.values))
ggplot(gamm_beta_residuals, aes(y=resid, x=fitted)) +
  geom_point() +
  labs(y = "Residuals", x = "Fitted values")
ggsave("FigS7_residuals_gamm_beta.jpg", dpi=500, width = 169, height = 180,  units = "mm", device = "jpg")



### Bonus: Quantile regression
# In the post-hoc analyses, 
# the gam with proportion of deep dive appears influenced by the highest value.
# Given, that we are interested the penguins' *capacity* to dive, 
# rather than their preference, it makes sense to look at the upper quantile.
# This allows us to see whether this potential influential point is more in line. 

library(quantreg)

deep_hb_qgam <- qgam(prop_deep_dives ~ s(hb) + col, data=subset_70_nosex_sum, qu=0.8)
summary(deep_hb_qgam)
# Hb is still significant


slice_deep_prop_hb_q <- data_slice(deep_hb_qgam, hb= evenly(hb, n=100))
pred_deep_prop_hb_q  <- predict(deep_hb_qgam, slice_deep_prop_hb_q, se.fit=TRUE)
slice_deep_prop_hb_pred_q <- cbind(slice_deep_prop_hb_q, est=pred_deep_prop_hb_q$fit, se=pred_deep_prop_hb_q$se.fit)

deep_prop_plot_q <- slice_deep_prop_hb_pred_q %>%
  add_confint() %>%
  ggplot(aes(y = est, x = hb)) +
  geom_line(colour = deep.col, size = 1.5) +
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              alpha = 0.1, fill = deep.col) +
  geom_point(subset_70_nosex_sum, mapping = aes(x=hb, y=prop_deep_dives)) +
  scale_radius("Hct (%)", range = c(0.3,2)) + 
  labs(y = expression("Proportion of dives" >= "140 m deep"), x = "Hb (g/dL)") +
  theme_bw() +
  scale_x_continuous(expand = exp_x) +
  scale_y_continuous(expand = exp_y - 0.042) +
  ggtitle("A")  + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(margin=margin(0,0,0,0)))
deep_prop_plot_q
