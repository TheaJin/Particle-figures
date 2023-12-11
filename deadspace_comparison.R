#### compare 0.9 and 1.1 ds
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



## function

datastep_ds <- function(sbj, sex)
  {
  address <- paste0("~/Desktop/paper 1/ds/", sbj, "_ds.xlsx")
  d09 <- read_excel(address, col_names = T, sheet = 'd09')
  d10 <- read_excel(address, col_names = T, sheet = 'd10')
  d11 <- read_excel(address, col_names = T, sheet = 'd11')
  
  d09$ds <- 0.9
  d11$ds <- 1.1
  d09 <- cbind(d09[, c(11,3)], 
                    (d09[, c(5,7:9)] - d10[, c(5,7:9)])/d10[, c(5,7:9)])
  d11 <- cbind(d11[, c(11,3)], 
                    (d11[, c(5,7:9)] - d10[, c(5,7:9)])/d10[, c(5,7:9)])
  
  ds_prop <- rbind(d09, d11) 
  colnames(ds_prop) <- c("ds", "dp", "TDE", "TDE_lung", "BDE", "ADE")
  ds_prop$Sex <- sex
  
  melt_ds <- melt(ds_prop, id = c('ds', 'dp', 'Sex'))
  melt_ds$value <- melt_ds$value*100
  
  melt_ds <- data.frame(melt_ds)
  
  #new_name <- paste0("melt_ds_", sbj)
  #return(assign(new_name, melt_ds))
  #new_name <<- melt_ds
  #melt_ds
} 

# Datastep
melt_ds_H653 <- datastep_ds('H653', 'female')
melt_ds_H673 <- datastep_ds('H673', 'female')
melt_ds_H682 <- datastep_ds('H682', 'female')
melt_ds_H684 <- datastep_ds('H684', 'female')
melt_ds_H1335 <- datastep_ds('H1335', 'female')
melt_ds_H6229 <- datastep_ds('H6229', 'female')
melt_ds_H6310 <- datastep_ds('H6310', 'female')

melt_ds_H5977 <- datastep_ds('H5977', 'male')
melt_ds_H11303 <- datastep_ds('H11303', 'male')
melt_ds_H11750 <- datastep_ds('H11750', 'male')
melt_ds_H11779 <- datastep_ds('H11779', 'male')
melt_ds_H12816 <- datastep_ds('H12816', 'male')

melt_all_ds <- rbind(melt_ds_H653, melt_ds_H673,
                     melt_ds_H682, melt_ds_H684,
                     melt_ds_H1335, melt_ds_H6229,
                     melt_ds_H6310, melt_ds_H5977,
                     melt_ds_H11303, melt_ds_H11750,
                     melt_ds_H11779, melt_ds_H12816)
unique(melt_all_ds$ds)

levels(melt_all_ds$variable) <- c("TDE", "Total Lung deposition fraction", 
                                   "Bronchial deposition fraction", "Alveolar deposition fraction")

### t-test
# ds = 0.9
lapply(split(subset(melt_all_ds, ds == 0.9 & variable %in% 'Total Lung deposition fraction'), 
             factor(melt_all_ds$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_ds, ds == 0.9 & variable %in% "Bronchial deposition fraction"), 
             factor(melt_all_ds$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_ds, ds == 0.9 & variable %in% "Alveolar deposition fraction"), 
             factor(melt_all_ds$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
# ds = 1.1
lapply(split(subset(melt_all_ds, ds == 1.1 & variable %in% 'Total Lung deposition fraction'), 
             factor(melt_all_ds$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_ds, ds == 1.1 & variable %in% "Bronchial deposition fraction"), 
             factor(melt_all_ds$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_ds, ds == 0.9 & variable %in% "Alveolar deposition fraction"), 
             factor(melt_all_ds$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)

# grey
pd <- ggplot(subset(melt_all_ds, variable != "TDE"), aes(x = factor(dp), y = value)) + 
  geom_boxplot(aes(fill = Sex), position = 'identity',  outlier.shape = 8) + #, outlier.colour =
  facet_nested_wrap( ~  ds + variable, scale = "free", ncol = 3) + 
  stat_summary(fun = mean, geom = 'point', aes(group = Sex)) + #,
  #position = position_dodge(width = 0.7)) +
  stat_summary(fun = mean, geom = 'line', aes(group = Sex)) +#,
  #position = position_dodge(width = 0.7)) +
  xlab("Particle size [micron]") + theme_classic() +
  scale_y_continuous(name ="% DF changes") +
  scale_fill_manual(values = c("white", "grey75"), labels = c("Female", "Male")) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size = 15)) + 
  theme(axis.text = element_text(size = 11)) +
  theme(axis.title = element_text(size = 16)) +
  theme(plot.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16)) +
  # scale_color_brewer(palette = 'Accent',
  #                    labels = c("Female", "Male")) +
  scale_shape_discrete(name = "Dead space (ml)", labels = c("0.9*baseline", "1.1*baseline")) +
  ggtitle("Proportion changes of Deposition Fractions under different dead space volume, comparing with the baseline") 

tag_facet(pd)

