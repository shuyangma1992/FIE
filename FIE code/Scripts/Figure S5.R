# Ma, Shuyang 
# September 2024
# Figure S5

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(ggh4x)
library(ggsci)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_classic())

#read length data
length_data <- read_excel("Data/TL_at_age_NEAC.xlsx",sheet = "Data") %>% 
  select(Year,`3`,`4`,`5`,`6`,`7`)

#figure data
figdata <- length_data %>% 
  pivot_longer(-Year,names_to = "age class",values_to = "length")

fs5 <- ggplot(figdata)+
  geom_line(aes(x=Year,y=length,color=`age class`),show.legend = T)+
  geom_point(aes(x=1994,y=44.8),size=3)+
  geom_errorbar(aes(x=1994,ymin=44.8-1.96*6.2,ymax=44.8+1.96*6.2))+
  labs(title="Mean total length at age class: NEAC (1985-2022)")+
  scale_x_continuous("Year",limits = c(1980,2022),expand = c(0,0),breaks = seq(1980,2020,10),
                     minor_breaks = seq(1980,2020,5),guide = "axis_minor")+
  scale_y_continuous("Both-sexes mean total length (cm)",limits = c(20,81),expand = c(0,0),breaks = seq(20,80,10),
                     minor_breaks = seq(20,80,5),guide = "axis_minor")+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        axis.ticks = element_line(linewidth = 0.1),
        plot.title = element_text(face="bold",family="Calibri"),
        legend.title = element_text(face="bold",family = "Calibri"),
        legend.text = element_text(face="bold",family = "Calibri"),
        legend.position = c(0.1,0.7),
        legend.background = element_blank(),
        ggh4x.axis.ticks.length.minor = rel(0.5))

ggsave("Figures/Figure S5.pdf",device = cairo_pdf,width = 6,height = 4)










