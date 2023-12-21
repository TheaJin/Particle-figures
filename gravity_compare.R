#### compare 0g and 1g
library(ggpubr)
library(ggplot2)
library(ggh4x)
library('rlang')
library(readr)
#options(scipen = 999)
library(scales)
library(reshape)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

## colour packages
library(wesanderson)
library("ggsci")
library(RColorBrewer)
library(viridis)

##### ds vs. dp

### data step
sbj_0G <- read_excel("~/Desktop/compare1.xlsx", col_names = T, sheet = '0G')
sbj_1G <- read_excel("~/Desktop/compare1.xlsx", col_names = T, sheet = '1G')
sbj_all <- cbind(sbj_0G[, c(2:3)], 
                 (sbj_0G[, c(5:8)] - sbj_1G[, c(5:8)])/sbj_1G[, c(5:8)]*100)
colnames(sbj_all) <- c("Sex", "dp", "DE_ET", "TDF_lung", "BDF", "ADF")#, "ADE")
sbj_all <- as.data.frame(sbj_all) # fix match.name error
sbj_all_melt <- melt(sbj_all, id.vars = c('Sex', 'dp'))


##### ds vs. dp

### data step
sbj_0G <- read_excel("~/Desktop/compare1.xlsx", col_names = T, sheet = '0G')
sbj_1G <- read_excel("~/Desktop/compare1.xlsx", col_names = T, sheet = '1G')
sbj_all <- cbind(sbj_0G[, c(2:3)], 
                 (sbj_0G[, c(5:8)] - sbj_1G[, c(5:8)])/sbj_1G[, c(5:8)]*100)
colnames(sbj_all) <- c("Sex", "dp", "DE_ET", "TDF_lung", "BDF", "ADF")#, "ADE")
sbj_all <- as.data.frame(sbj_all) # fix match.name error
sbj_all_melt <- melt(sbj_all, id.vars = c('Sex', 'dp'))


levels(sbj_all_melt$variable)
#variable.labs <- levels(sbj_all_melt$variable)
levels(sbj_all_melt$variable) <- c("DE_ET", "Total Lung deposition fraction", 
                                   "Bronchial deposition fraction", "Alveolar deposition fraction")
names(variable.labs) <- c("DE_ET", "Total Lung deposition fraction", 
                          "Bronchial deposition fraction", "Alveolar deposition fraction")

pg <- ggplot(subset(sbj_all_melt, variable != "DE_ET"), aes(x = factor(dp), y = value)) + 
  geom_boxplot(aes(fill= Sex),  outlier.shape = 8, position = 'identity') + #, outlier.colour =
  facet_wrap( ~ variable, scale = "free", 
              labeller = labeller(variable = variable.labs)) + 
  stat_summary(fun = mean, geom = 'point', aes( group = Sex, shape = Sex),
               size = 2) + #, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = mean, geom = 'line', aes(group = Sex)) + 
  #  position = position_dodge(width = 0.7)) +
  #stat_compare_means(method = "t.test")+
  scale_fill_manual(values = c("white", "grey75"), labels = c("Female", "Male")) +
  theme_classic() + xlab("Particle size [micron]") + 
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 16)) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  scale_y_continuous(name ="% DF changes") +
  #scale_color_brewer(palette = 'Accent', labels = c("Female", "Male")) +
  scale_shape_discrete(labels = c("Female", "Male")) +
  ggtitle("Proportion changes of Deposition Fractions under 0G for different particle sizes, comparing with the baseline 1G") 

tag_facet(pg, hjust = -0.5, vjust = 3)pg <- ggplot(subset(sbj_all_melt, variable != "DE_ET"), aes(x = factor(dp), y = value)) + 
  geom_boxplot(aes(fill= Sex),  outlier.shape = 8, position = 'identity') + #, outlier.colour =
  facet_wrap( ~ variable, scale = "free", 
              labeller = labeller(variable = variable.labs)) + 
  stat_summary(fun = mean, geom = 'point', aes( group = Sex, shape = Sex),
               size = 2) + #, position = position_dodge(width = 0.7)) + 
  stat_summary(fun = mean, geom = 'line', aes(group = Sex)) + 
  #  position = position_dodge(width = 0.7)) +
  #stat_compare_means(method = "t.test")+
  scale_fill_manual(values = c("white", "grey75"), labels = c("Female", "Male")) +
  theme_classic() + xlab("Particle size [micron]") + 
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 16)) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  scale_y_continuous(name ="% DF changes") +
  #scale_color_brewer(palette = 'Accent', labels = c("Female", "Male")) +
  scale_shape_discrete(labels = c("Female", "Male")) +
  ggtitle("Proportion changes of Deposition Fractions under 0G for different particle sizes, comparing with the baseline 1G") 

tag_facet(pg, hjust = -0.5, vjust = 3)
