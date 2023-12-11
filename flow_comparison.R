#### compare 100 and 500ml flow
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
library('patchwork')
#install.packages('egg', dependencies = TRUE)
library(egg)


## colour packages
library(wesanderson)
library("ggsci")
library(RColorBrewer)
library(viridis)






## function

datastep_flow <- function(sbj, sex)
{
  address <- paste0("~/Desktop/paper 1/flow/", sbj, "_flow.xlsx")
  f100 <- read_excel(address, col_names = T, sheet = 'f100')
  f250 <- read_excel(address, col_names = T, sheet = 'f250')
  f500 <- read_excel(address, col_names = T, sheet = 'f500')
  
  #d09$ds <- 0.9
  #d11$ds <- 1.1
  f100 <- cbind(f100[, c(1,2)], 
                       (f100[, c(4, 6:8)] - f250[, c(4, 6:8)])/f250[, c(4, 6:8)])
  f500 <- cbind(f500[, c(1,2)], 
                       (f500[, c(4, 6:8)] - f250[, c(4, 6:8)])/f250[, c(4, 6:8)])
  
  flow_prop <- rbind(f100, f500) 
  colnames(flow_prop) <- c("flow", "dp", "TDE", "TDE_lung", "BDE", "ADE")
  flow_prop$Sex <- sex
  
  melt_flow <- melt(flow_prop, id = c('flow', 'dp', 'Sex'))
  melt_flow$value <- melt_flow$value*100
  
  melt_flow <- data.frame(melt_flow)
  
  #new_name <- paste0("melt_flow_", sbj)
  #return(assign(new_name, melt_flow))
  #new_name <<- melt_flow
  #melt_flow
} 

# Datastep
melt_flow_H653 <- datastep_flow('H653', 'female')
melt_flow_H673 <- datastep_flow('H673', 'female')
melt_flow_H682 <- datastep_flow('H682', 'female')
melt_flow_H684 <- datastep_flow('H684', 'female')
melt_flow_H1335 <- datastep_flow('H1335', 'female')
melt_flow_H6229 <- datastep_flow('H6229', 'female')
melt_flow_H6310 <- datastep_flow('H6310', 'female')

melt_flow_H5977 <- datastep_flow('H5977', 'male')
melt_flow_H11303 <- datastep_flow('H11303', 'male')
melt_flow_H11750 <- datastep_flow('H11750', 'male')
melt_flow_H11779 <- datastep_flow('H11779', 'male')
melt_flow_H12816 <- datastep_flow('H12816', 'male')

melt_all_flow <- rbind(melt_flow_H653, melt_flow_H673,
                     melt_flow_H682, melt_flow_H684,
                     melt_flow_H1335, melt_flow_H6229,
                     melt_flow_H6310, melt_flow_H5977,
                     melt_flow_H11303, melt_flow_H11750,
                     melt_flow_H11779, melt_flow_H12816)
unique(melt_all_flow$flow)

levels(melt_all_flow$variable) <- c("TDE", "Total Lung deposition fraction", 
                                  "Bronchial deposition fraction", "Alveolar deposition fraction")


### t-test
# flow = 100
lapply(split(subset(melt_all_flow, flow == 100 & variable %in% 'Total Lung deposition fraction'), 
             factor(melt_all_flow$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_flow, flow == 100 & variable %in% "Bronchial deposition fraction"), 
             factor(melt_all_flow$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_flow, flow == 100 & variable %in% "Alveolar deposition fraction"), 
             factor(melt_all_flow$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
# flow = 500
lapply(split(subset(melt_all_flow, flow == 500 & variable %in% 'Total Lung deposition fraction'), 
             factor(melt_all_flow$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_flow, flow == 500 & variable %in% "Bronchial deposition fraction"), 
             factor(melt_all_flow$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)
lapply(split(subset(melt_all_flow, flow == 500 & variable %in% "Alveolar deposition fraction"), 
             factor(melt_all_flow$dp)), 
       function(x)t.test(data = x, value ~ Sex, paired = FALSE)$p.value)


### ggplot, boxplot grey!
pf <- ggplot(subset(melt_all_flow, variable != "TDE"), aes(x = factor(dp), y = value)) + 
  geom_boxplot(aes(fill = Sex),  position = 'identity', outlier.shape = 8) + #, outlier.colour =
  facet_nested_wrap( ~  flow + variable, scale = "free", nrow = 2) + 
  stat_summary(fun = mean, geom = 'point', aes(group = Sex)) + #,
  stat_summary(fun = mean, geom = 'line', aes(group = Sex)) + #,
  xlab("Particle size [micron]") + theme_classic() +
  scale_y_continuous(name ="% DF changes") + 
  scale_fill_manual(values = c("white", "grey75"), labels = c("Female", "Male")) +
  scale_shape_discrete(name = "Flow (ml)", labels = c("100 ml", "500 ml")) +
  theme(legend.position = "bottom") +
  theme(strip.text.x = element_text(size = 14))+ 
  theme(axis.text = element_text(size = 11))+
  theme(axis.title = element_text(size = 16))+
  theme(plot.title = element_text(size = 16)) +
  theme(legend.text = element_text(size = 14), legend.title = element_text(size = 16))+
  ggtitle("Proportion changes of Deposition Fractions under different inlet flow, comparing with the baseline") 


### keep the label!
tag_facet <- function(p, open = "", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                      hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE) 
}

tag_facet(pf)
