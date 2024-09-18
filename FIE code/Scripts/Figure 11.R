# Moksness, Ine Elise 
# August 2023
# Figure 11

#############################################################
# LC (leading cohort oocyte diameter) vs. age/length
#############################################################

#Install and load packages
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(egg)
library(dplyr)
library(readxl)
#library(package)
theme_set(theme_pubr())

#Import and prepare data

#Survey data
#setwd
NEA <- read_excel("Data/LC_diameter_NEAC_autumn_2017.xlsx", sheet="Data")
names(NEA)
summary(NEA)
str(NEA)

#######################################################################

### Prepare for March-April 2018
#Subset for Spawning survey
Spa <- subset(NEA, subset = Survey == "Spawning" )
#Remove spesific rows
Spa2<-Spa[!(Spa$ID==106 ),]
Spa2
# Extract spesific rows
Spa1<- subset(Spa,  ID == 106)

#######################################################################

### Prepare for August-September 2017
#Subset survey
Eco <- subset(NEA,  Survey == 'Ecosystem')
str(Eco)

#Histology data from August-September 2017
#setwd 
Histology_es17 <- read_excel("Data/Histology_es17.xlsx",sheet = "Data")
str(Histology_es17)

#Merge histology data with survey data
Histology.ecosystem <- merge(Eco, Histology_es17)
str(Histology.ecosystem)

# Subset for samples that are PVO4c, CAO og EVO only

#Remove spesific rows
hist1<-Histology.ecosystem[!(Histology.ecosystem$o.stage.cat=="s4A" ),]
hist2<-hist1[!(hist1$o.stage.cat=="s4B" ),]
hist3<-hist2[!(hist2$o.stage.cat=="Stage 3" ),]
str(hist3)



#Moksness_Figure10A.pdf

Eco_Age <- ggplot(hist3, aes(x=Age, y=LC.W )) +
  geom_point(aes(color=o.stage.cat, 
                 shape=o.stage.cat, fill=o.stage.cat), stroke = 3, size = 5, alpha = 0.5, show.legend = FALSE) +
  theme_article() +
  # facet_grid(cols = vars(Treatment)) +
  geom_smooth(method = "lm", se = TRUE, col = "black", size = 1.5) +
  scale_colour_manual(values=c( "turquoise3", "tomato2", "yellow3"),
                      name="") +
  scale_x_continuous(limits = c(5, 15), breaks = seq(5,15,1)) +
  scale_y_continuous(limits = c(150, 410), breaks = seq(150,410,50))+
  scale_fill_manual(values=c(  "turquoise3", "tomato2", "yellow3"),
                    name="")+
  scale_shape_manual(name = "", values = c(3,4,8)) +
  labs(x = "", y = "")#+

Eco_Age

#Regression equation:
# model <- lm(LC.W ~ Age, data = hist3)
# model
# 
# model <- lm(Dia.max ~ Age, data = hist3)
# model
# summary(model)

#setwd
ggsave(Eco_Age, file = "Moksness_Figure10A.pdf", dpi = "retina", width = 180
       , height = 180, units = "mm", bg = "white")

############################################################
############################################################

#Moksness_Figure10B.pdf

spa_Age <- ggplot(Spa2, aes(x=Age, y=Dia.max, color=Whole.mount, shape=Whole.mount, fill=Whole.mount)) +
  geom_point(size = 5, stroke =1, alpha = 0.5, show.legend = FALSE)+
  scale_shape_manual(values=c(19, 15, 23), name="")+ 
  scale_color_manual(values=c( "#E7B800", "forestgreen", "violetred"),
                     name="")+
  scale_fill_manual(values=c("#E7B800", "forestgreen", "violetred"),
                    name="")+
  theme_article() +  scale_y_continuous(breaks=seq(100,1500,100), limits = c(100,1500))+
  labs(x = "", y = "")+
  scale_x_continuous(breaks = seq(from = 5, to = 17, by = 1), limits = c(5,16))+
  geom_point(data = Spa1, 
             shape = 15, 
             alpha = 0.5, 
             size = 5, 
             color = 'forestgreen',
             stroke = 1, 
             mapping = aes(x=Age, y= LC.W), 
             show.legend = FALSE)
spa_Age



#correlation coefficient
# cor(Spa2$Age, Spa2$LC.W)
#0.1401879

#setwd
ggsave(spa_Age, file = "Moksness_Figure10B.pdf", dpi = "retina", width = 180
       , height = 180, units = "mm", bg = "white")

#############################################################
#############################################################

#Moksness_Figure10C

Eco_Length <- ggplot(hist3, aes(x=Length, y=LC.W )) +
  geom_point(aes(color=o.stage.cat, 
                 shape=o.stage.cat, fill=o.stage.cat), stroke = 3, size = 5, alpha = 0.5, show.legend = FALSE) +
  theme_article() +
  # facet_grid(cols = vars(Treatment)) +
  geom_smooth(method = "lm", se = TRUE, col = "black", size = 1.5) +
  scale_colour_manual(values=c("turquoise3", "tomato2", "yellow3"),
                      name="") +
  scale_x_continuous(limits = c(50, 125), breaks = seq(50,125,10)) +
  scale_y_continuous(limits = c(150, 410), breaks = seq(150,410,50))+
  scale_fill_manual(values=c("turquoise3", "tomato2", "yellow3"),
                    name="")+
  scale_shape_manual(name = "", values = c(3,4,8)) +
  labs(x = "", y = "")

Eco_Length

# model <- lm(LC.W ~ Length, data = hist3)
# model
# model <- lm(Dia.max ~ Length, data = hist3)
# model
# summary(model)

#setwd
ggsave(Eco_Length, file = "Moksness_Figure10C.pdf", dpi = "retina", width = 180
       , height = 180, units = "mm", bg = "white")


#############################################################
#############################################################

#Moksness_Figure10D.jpeg

spa_Length <- ggplot(Spa2, aes(x=Length, y=Dia.max, color=Whole.mount, shape=Whole.mount, fill=Whole.mount)) +
  geom_point(size = 5, stroke =1, alpha = 0.5, show.legend = FALSE)+
  scale_shape_manual(values=c(19, 15, 23), name="")+ 
  scale_color_manual(values=c( "#E7B800", "forestgreen", "violetred"),
                     name="")+
  scale_fill_manual(values=c("#E7B800", "forestgreen", "violetred"),
                    name="")+
  theme_article() +  scale_y_continuous(breaks=seq(100,1500,100), limits = c(100,1500))+
  labs(x = "", y = "")+
  scale_x_continuous(breaks = seq(from = 60, to = 135, by = 10), limits = c(60,135))+
  geom_point(data = Spa1, 
             shape = 15, 
             alpha = 0.5, 
             size = 6, 
             color = 'forestgreen',
             stroke = 1.5, 
             mapping = aes(x=Length, y= LC.W), 
             show.legend = FALSE)
spa_Length

#correlation coefficient
# cor(Spa2$Length, Spa2$LC.W)
#0.1083864

setwd("~/2021/Maturity staging - NEAC - publication/Utkast/1. Figures/Figures")
ggsave(spa_Length, file = "Moksness_Figure10D.pdf", dpi = "retina", width = 180
       , height = 180, units = "mm", bg = "white")


############################################################
############################################################




