# Ma, Shuyang 
# March 2024
# Figure 11

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(strucchange)
library(splines)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_classic())

#read data
data <- read_excel("Data/Length_of_spawning-survey_NEAC_A50.xlsx",sheet = "Data") %>% 
  select(1,2,5,6,7)

#linear regression information
mod <- lm(`A50 (year)`~`Survey_length_days<20_days`,data=data)
summary(mod)

f11 <- ggplot(data)+
  geom_smooth(aes(x=`Survey_length_days<20_days`,y=`A50 (year)`),color="blue",fill="blue",alpha=0.2,method = "lm")+
  geom_text(aes(x=`Survey_length_days<20_days`,y=`A50 (year)`,label=str_sub(Year,3,4)),color="blue",family="Calibri",check_overlap = F)+
  geom_text(aes(x=`Survey_length_days>20_days`,y=`A50 (year)`,label=str_sub(Year,3,4)),color="blue",family="Calibri")+
  labs(title=" A50 vs. spawning survey duration NEAC (1992-2021)")+
  scale_x_continuous("Survey duration (days)",limits = c(8,30),expand=c(0,0),breaks=seq(10,30,5),minor_breaks = NULL)+
  scale_y_continuous("Combined-sexes A50 (year)",limits = c(6,8),expand=c(0,0),breaks=seq(6.0,8.0,0.2),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Calibri"),
        axis.title = element_text(family = "Calibri"),
        panel.grid = element_blank())

ggsave("Figures/Figure 11.pdf",device = cairo_pdf,width = 6,height = 4)



