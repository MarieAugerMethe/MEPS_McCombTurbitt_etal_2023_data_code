########################
# Code associated with the paper:
# Diving efficiency at depth and pre-breeding foraging effort increase with haemoglobin levels in gentoo penguins
# Authors: Sarah P. McComb-Turbitt, Glenn T. Crossin, Megan Tierney, Paul Brickle, Philip Trathan, Tony D. Williams, Marie Auger-Méthé
# doi: 10.3354/meps14441
# Part 3: Effects of tags analyses
#
########################

library(tidyr)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(MuMIn)
library(nnet) # multinom

#### Load and prep data
tag.df <- read.csv("3_tagging_effect.csv", stringsAsFactors = TRUE)

head(tag.df)

# tdr: whether the tdr tag was still attached at recapture
# tag: whether the individual was tagged (n = means control penguin)
# penguin: individual ID
# sex: Female or male (based on DNA)
# col: colony, RP = Race Point, PB = Pebble Island
# b: reproductive status: NB = Non breeding, B = Breeding, L = Early laying
# hct: haematocrit (%)
# hb: haemoglobin (g/dL)
# mass1: weight of penguin in kg at capture (not the control penguin were not capture in April)
# m2: weight of penguin in kg at recapture 
# massGain: change in mass between capture and recapture
# Comments: comments based on field notes

#### Plot set up
bp.col <- grey(0.6)
bp.text.size <- 10
bp.exp <- c(0,0.4)
bp.exp.hct <- c(0,3) 

# For comparison in ggplot
comp.tag <- list(c("y", "n"))
comp.tdr <- list(c("y", "n"))


############## t-test for potential tagging effect on Hb

t.test(hb~tag, filter(tag.df, !is.na(hb)))

# Sample large enough to rely on CLT, but check that data is unimodal 
hist(filter(tag.df, !is.na(hb) & tag == "y")$hb, main="Hb tagged", xlab = "Hb")
hist(filter(tag.df, !is.na(hb) & tag == "n")$hb, main="Hb untagged", xlab = "Hb")

mean(filter(tag.df, !is.na(hb))$hb)
sd(filter(tag.df, !is.na(hb))$hb)

# For plot
tag_hb_plot <- ggplot(subset(tag.df, !is.na(hb)), aes(x=tag, y=hb)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  labs(y = "Hb (g/dL)", x= "Tagged") +
  scale_x_discrete(labels=c("y" = "Yes", "n" = "No")) + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) +
  stat_compare_means(comparisons = comp.tag, method= "t.test") +
  scale_y_continuous(expand = bp.exp.hct) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("A")


############## t-test for potential tagging effect on Hct

t.test(hct~tag, filter(tag.df, !is.na(hct)))

# Sample large enough to rely on CLT, but check that data is unimodal 
hist(filter(tag.df, !is.na(hct) & tag == "y")$hct, main="Hct tagged", xlab = "Hct")
hist(filter(tag.df, !is.na(hct) & tag == "n")$hct, main="Hct untagged", xlab = "Hct")

mean(filter(tag.df, !is.na(hct))$hct)
sd(filter(tag.df, !is.na(hct))$hct)

# For plot
tag_hct_plot <- ggplot(subset(tag.df, !is.na(hct)), aes(x=tag, y=hct)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  labs(y = "Hct (%)", x= "Tagged") +
  scale_x_discrete(labels=c("y" = "Yes", "n" = "No")) + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) +
  stat_compare_means(comparisons = comp.tag, method= "t.test") +
  scale_y_continuous(expand = bp.exp.hct) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("B")



############## t-test for potential tagging effect on Mass
t.test(m2~tag, filter(tag.df, !is.na(m2)))

# Sample large enough to rely on CLT, but check that data is unimodal 
hist(filter(tag.df, !is.na(m2) & tag == "y")$m2, main="Mass tagged", xlab = "Mass")
hist(filter(tag.df, !is.na(m2) & tag == "n")$m2, main="Mass untagged", xlab = "Mass")

# For plot
tag_mass_plot <- ggplot(subset(tag.df, !is.na(m2)), aes(x=tag, y=m2)) + 
  geom_boxplot(fill = bp.col) + theme_bw() +
  labs(y = "Mass (kg)", x= "Tagged") +
  scale_x_discrete(labels=c("y" = "Yes", "n" = "No")) + 
  theme(axis.text.x = element_text(size = bp.text.size), 
        axis.text.y = element_text(size = bp.text.size),
        axis.title = element_text(size = bp.text.size+2)) +
  stat_compare_means(comparisons = comp.tag, method= "t.test") +
  scale_y_continuous(expand = bp.exp) +
  theme(plot.title = element_text(margin=margin(0,0,0,0))) + 
  ggtitle("C")

mean(tag.df$m2[tag.df$tag == "y"])
sd(tag.df$m2[tag.df$tag == "y"])
range(tag.df$m2[tag.df$tag == "y"])

mean(tag.df$m2[tag.df$tag == "n"])
sd(tag.df$m2[tag.df$tag == "n"])
range(tag.df$m2[tag.df$tag == "n"])

mean(tag.df$m2[tag.df$tag == "y"])/mean(tag.df$m2[tag.df$tag == "n"])

ggarrange(tag_hb_plot, 
          tag_hct_plot,
          tag_mass_plot,nrow=3, ncol=1)

ggsave("FigS9_tag_effects.jpg", dpi=500, width = 84, height = 180,  units = "mm", device = "jpg")


######## Look at mass gains
# Look at mass gain for the tagged individuals
tag.df.tag.y <- filter(tag.df, tag == "y")
nrow(tag.df.tag.y) # Number of tagged penguins recaptured
nrow(filter(tag.df.tag.y, massGain > 0)) # Number of recaptured penguins that gained weight

mean(tag.df.tag.y$massGain)
sd(tag.df.tag.y$massGain)

######### Looking at whether tagging affects breeding status #####

####
# Focusing on PI, since RP didn't have breeding birds
set.seed(123)
tag.df.PI <- tag.df %>% filter(col == "PI")
# Given that it's a bunch of categorical variables, can use a goodness of fit test
tag.b.table.PI <- tag.df.PI %>% select("tag", "b") %>% table() %>% as.data.frame.matrix()
csq_P <- chisq.test(tag.b.table.PI, simulate.p.value = TRUE)
csq_P
csq_P$expected
sum(tag.b.table.PI) # Sample size

# Information for Table S5
tag.b.table.PI
tag.b.table.PI/rowSums(tag.b.table.PI)

#####
# Getting data from Race Point
tag.df.RP <- tag.df %>% filter(col == "RP")
tag.b.table.RP <- tag.df.RP %>% select("tag", "b") %>% table() %>% as.data.frame.matrix()

# Information for Table S5
tag.b.table.RP
tag.b.table.RP/rowSums(tag.b.table.RP)


# Total
set.seed(88776)
tag.b.table.T <- tag.df %>% select("tag", "b") %>% table() %>% as.data.frame.matrix()
csq_T <- chisq.test(tag.b.table.T, simulate.p.value = TRUE)
csq_T
csq_T$expected
sum(tag.b.table.T)

# Information for Table S5
tag.b.table.T
tag.b.table.T/rowSums(tag.b.table.T)
