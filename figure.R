#library(ggpubr)
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
library(readxl)
library(ggstar)
library(patchwork)
library(gridExtra)
library(cowplot)
library(data.table)



### figure 2
### import weibel's model result
### weibel's model was comapring with vt = 1000, q = 500 ml, with vt = 500, q = 250 ml
weibel <- read_excel("/hpc/gjin250/Downloads/weibel.xlsx")
weibel_1000 <- read_excel("/hpc/gjin250/Downloads/weibel.xlsx", sheet = "Sheet2")


# calculate based on kim 2006
dp <- c(1,3,5)
vt <- 1000
q <- 500
omega <- dp^1.894*vt^1.498*q^-0.406
tdf <- 1-0.91/(1+7.908*10^-5*omega)
tdf
# heyder
Heyder <- read_excel("/hpc/gjin250/Downloads/Heyder.xlsx", sheet = "Sheet1")
View(Heyder)  
Heyder_1000 <- read_excel("/hpc/gjin250/Downloads/Heyder.xlsx", sheet = "Sheet2")
View(Heyder_1000)  

# kim
kim_unisex <- read_excel("/hpc/gjin250/Downloads/literature_result2.xlsx", sheet = "Sheet14")
View(kim_unisex)  
kim <- read_excel("/hpc/gjin250/Downloads/literature_result.xlsx", sheet = "Sheet7")
View(kim)  
kim_f <- read_excel("/hpc/gjin250/Downloads/literature_result.xlsx", sheet = "Sheet8")
View(kim_f)    


# tdf
tdf_plot <- ggplot(weibel, aes(x = dp, y = tdf)) +
  geom_star(size = 3, fill = 'gray') + geom_line() + 
  
  geom_star(data = weibel_1000, aes(x = dp, y = tdf), size = 3, starshape = 13, fill = 'gray')+
  geom_line(data = weibel_1000, aes(x = dp, y = tdf), linetype = 'dashed')+
        
  # Heyder
  geom_star(data = Heyder, aes(x = dp, y = DE, starshape = interaction(Q, Vt)), size = 3) +
  geom_star(data = Heyder_1000, aes(x = dp, y = DE, starshape = interaction(Q, Vt)), size = 3) +
  
  geom_star(data = kim_unisex, aes(x = dp, y = TDF, starshape = interaction(Q, Vt)), 
            fill = 'black',size = 3) +
  scale_starshape_manual(name = "Breathing Pattern", 
                       labels = c("Vt = 500, Q = 250 ml", "Vt = 1000, Q = 500 ml"),
                       values = c(1, 13)) +
  theme_classic() + scale_x_log10() + 
  xlab("Particle size [micron]") + ylab("Total Deposition Fraction")

# Assuming tdf_plot is your original plot and labelled_plot is the one with labels
legend_tdf <- get_legend(tdf_plot)
print(legend_tdf)
  

# restore tdf_plot without label
tdf_plot <- ggplot(weibel, aes(x = dp, y = tdf)) +
    geom_star(size = 3, fill = 'gray') + geom_line() + 
    
    geom_star(data = weibel_1000, aes(x = dp, y = tdf), size = 3, starshape = 13, fill = 'gray')+
    geom_line(data = weibel_1000, aes(x = dp, y = tdf), linetype = 'dashed')+
    
    # Heyder
    geom_star(data = Heyder, aes(x = dp, y = DE, starshape = interaction(Q, Vt)), size = 3) +
    geom_star(data = Heyder_1000, aes(x = dp, y = DE, starshape = interaction(Q, Vt)), size = 3) +
    
    geom_star(data = kim_unisex, aes(x = dp, y = TDF, starshape = interaction(Q, Vt)), 
              fill = 'black',size = 3) +
    
    theme_classic() + scale_x_log10() + xlab(NULL) +
    # xlab("Particle size [micron]") + 
  ylab("Total Deposition Fraction") +
    theme(legend.position = "none")
  
  
# bdf
bdf_plot <- ggplot(weibel, aes(x = dp, y = bdf)) +
  geom_star(size = 3, fill = 'gray') + geom_line() + 
  # weibel, vt =1000, q = 500
    geom_star(data = weibel_1000, aes(x = dp, y = bdf), size = 3, starshape = 13, fill = 'gray')+
    geom_line(data = weibel_1000, aes(x = dp, y = bdf), linetype = 'dashed')+
    # Heyder
    geom_star(data = Heyder, aes(x = dp, y = DEB, starshape = interaction(Q, Vt)), size = 3) +
    # kim
    geom_star(data = kim, aes(x = dp, y = TB_M), starshape = 1, fill = 'black', size = 3) +
    geom_star(data = kim_f, aes(x = dp, y = TB_F), starshape = 1, fill = 'black', size = 3) +
    theme_classic() + scale_x_log10() + 
    #xlab("Particle size [micron]") + 
  xlab(NULL) +
  ylab("Bronchial Deposition Fraction") +
  theme(legend.position = "none")
  
# adf  
adf_plot <- ggplot(weibel, aes(x = dp, y = adf)) +
  geom_star(size = 3, fill = 'gray') + geom_line() + 
  # weibel, vt =1000, q = 500
  geom_star(data = weibel_1000, aes(x = dp, y = adf), size = 3, starshape = 13, fill = 'gray')+
  geom_line(data = weibel_1000, aes(x = dp, y = adf), linetype = 'dashed')+
  # Heyder
  geom_star(data = Heyder, aes(x = dp, y = DEA, starshape = interaction(Q, Vt)), size = 3) +
  # kim
  geom_star(data = kim, aes(x = dp, y = AL_M), starshape = 1, fill = 'black',size = 3) +
  geom_star(data = kim_f, aes(x = dp, y = AL_F), starshape = 1, fill = 'black',size = 3) +
  theme_classic() + scale_x_log10() + 
  xlab("Particle size [micron]") + ylab("Alveolar Deposition Fraction") +
  theme(legend.position = "none")
  

# Assuming your plots are stored in plot1, plot2, and plot3 variables
labeled_plots <- grid.arrange(tdf_plot + labs(title = "a)"), 
                              bdf_plot + labs(title = "b)"), 
                              adf_plot + labs(title = "c)"), ncol = 1)
# Print the combined plots
labeled_plots

# Combine the labelled plot and the legend
combined_plot <- plot_grid(labeled_plots, legend_tdf, ncol = 2, align = "v")

# Print the combined plot
print(combined_plot)


### figure 3
### import subject specific's model result
### Thea: find the simulation data file
literature_result <- read_excel("/hpc/gjin250/Downloads/literature_result.xlsx")
cheng1996 <- read_excel("/hpc/gjin250/Downloads/literature_result.xlsx", sheet = "Sheet11")

result_new <- read_excel("/hpc/gjin250/Downloads/result_new.xlsx", sheet = "TDF_Lung")
View(result_new)


# Convert data frame to data.table
setDT(result_new)

# Melt the data table using data.table::melt()
sbj_tdf_melt <- melt(result_new, id.vars = c('Sbj', 'Sex'), variable.name = 'dp')
sbj_tdf_melt$Study = 'Simulation'
sbj_tdf_melt$Study = as.character(sbj_tdf_melt$Study)

# Check the structure of sbj_tdf_melt to see if the 'variable' column contains any data
str(sbj_tdf_melt)


plot_tdf <- ggplot(sbj_tdf_melt, aes(x = as.numeric(as.character(dp)), y = value, 
                         starshape = Study, fill = Sex, group = Sex)) + 
  # simulation
  stat_summary(fun = mean, geom = "star", size = 4, show.legend = T,
               aes(starshape = Study)) +
  stat_summary(fun = mean, geom = "line",  aes(linetype = Sex), 
               colour = 'black', show.legend = F) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               fun.args = list(mult = 2), show.legend = F) + 
  # Schiller
  geom_star(data = literature_result, show.legend = F, 
            aes(x = dp, y = TDF_O, fill = Sex_2, starshape = Study), size = 3) +
  # Cheng
  geom_star(data = cheng1996, show.legend = F, 
            aes(x = dp, y = IDF_O, fill = Sex_2, starshape = Study), size = 3) +
  # kim
  geom_star(data = kim, aes(x = dp, y = INTRA_M, starshape = Study),  show.legend = F, size = 3) +
  geom_star(data = kim, aes(x = dp, y = TDF_M, starshape = Study_2), show.legend = F,size = 3) +
  
  geom_star(data = kim_f, aes(x = dp, y = INTRA_F, starshape = Study), show.legend = F,size = 3) +
  geom_star(data = kim_f, aes(x = dp, y = TDF_F, starshape = Study_2), show.legend = F,size = 3) +
  
  scale_fill_manual(name = "Sex", values = c("white", 'black', "grey75"), guide = "none") +
  scale_starshape_manual(name = "Study",
                     values = c(1, 13, 15, 11, 12, 14, 28)) +
  theme_classic() + xlab(NULL) +
  # xlab("Particle size [micron]") + 
  ylab("Total Deposition Fraction") + theme(legend.position = "none") +
  scale_x_log10() + guides(shape = guide_legend(title = "Sex", override.aes = list(shape = NA)))


# get legend
legend_tdf <- get_legend(plot_tdf)
print(legend_tdf)


### bdf
BDF <- read_excel("/hpc/gjin250/Downloads/result_new.xlsx", sheet = "BDF")
# Convert data frame to data.table
setDT(BDF)
# Melt the data table using data.table::melt()
sbj_bdf_melt <- melt(BDF, id.vars = c('Sbj', 'Sex'), variable.name = 'dp')
sbj_bdf_melt$Study = 'Simulation'


plot_bdf <- ggplot(sbj_bdf_melt, aes(x = as.numeric(as.character(dp)), y = value, 
                                     starshape = Study, fill = Sex, group = Sex)) + 
  # simulation
  stat_summary(fun = mean, geom = "star", size = 4, show.legend = T,
               aes(starshape = Study)) +
  stat_summary(fun = mean, geom = "line",  aes(linetype = Sex), 
               colour = 'black', show.legend = F) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               fun.args = list(mult = 2), show.legend = F) + 
  # kim
  geom_star(data = kim, aes(x = dp, y = TB_M, starshape = Study),  show.legend = F, size = 3) +
  geom_star(data = kim_f, aes(x = dp, y = TB_F, starshape = Study), show.legend = F,size = 3) +
  
  scale_fill_manual(name = "Sex", values = c("white", 'black'), guide = "none") +
  scale_starshape_manual(name = "Study", values = c(13, 15, 28)) +
  theme_classic() + xlab(NULL) +
  # xlab("Particle size [micron]") + 
  ylab("Bronchial Deposition Fraction")+ 
  guides(shape = guide_legend(title = "Sex", override.aes = list(shape = NA))) + 
  theme(legend.position = "none") +
  scale_x_log10() 

#### adf

ADF <- read_excel("/hpc/gjin250/Downloads/result_new.xlsx", sheet = "ADF")
View(ADF)
# Convert data frame to data.table
setDT(ADF)

# Melt the data table using data.table::melt()
sbj_adf_melt <- melt(ADF, id.vars = c('Sbj', 'Sex'), variable.name = 'dp')
sbj_adf_melt$Study = 'Simulation'


plot_adf <- ggplot(sbj_adf_melt, aes(x = as.numeric(as.character(dp)), y = value, 
                         starshape = Study, group = Sex, fill = Sex)) + 
  # simulation
  stat_summary(fun = mean, geom = "star", size = 4, show.legend = T,
               aes(starshape = Study)) +
  stat_summary(fun = mean, geom = "line", aes(linetype = Sex), 
               colour = 'black', show.legend = FALSE) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               fun.args = list(mult = 2), show.legend = FALSE) + 
  # kim
  geom_star(data = kim, aes(x = dp, y = AL_M, starshape = Study), size = 3, show.legend = FALSE) +
  geom_star(data = kim_f, aes(x = dp, y = AL_F, starshape = Study), size = 3, show.legend = FALSE) +
  
  scale_fill_manual(name = "Sex", values = c("white", 'black'), guide = "none") +
  scale_starshape_manual(name = "Study", values = c(13, 15, 28)) +
  
  xlab("Particle size [micron]") + 
  ylab("Alveolar Deposition Fraction") + 
  theme_classic() + 
  scale_x_log10() +
  theme(legend.position = "none")

# Assuming your plots are stored in plot1, plot2, and plot3 variables
labeled_plot <- grid.arrange(plot_tdf + labs(title = "a)"), 
                             plot_bdf + labs(title = "b)"), 
                             plot_adf + labs(title = "c)"), ncol = 1)
# Print the combined plots
#labeled_plot

# Combine the labelled plot and the legend
combined_plot <- plot_grid(labeled_plot, legend_tdf, ncol = 2, align = "v")

# Print the combined plot
print(combined_plot)


