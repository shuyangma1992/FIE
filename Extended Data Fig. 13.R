# Ma, Shuyang 
# September 2023
# Extended Data Fig. 13

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(psych)
library(ggh4x)
library(ggsci)
library(lsmeans)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_classic())

#age older than 5
study_length <- read_excel("Data/TL_W_age_NEAC_surveys.xlsx",sheet = "Data") %>% 
  mutate(Season=str_replace_all(Survey,c("Ecosystem"="Autumn","Winter"="Winter","Spawning"="Spring"))) %>% 
  mutate(Group=ifelse(Season=="Autumn","Group1","Group2")) %>% 
  mutate(Season=factor(Season,levels = c("Autumn","Winter","Spring"))) %>% 
  filter(age>=5) %>% 
  mutate(age=ifelse(age>=10,"10+",age)) %>% 
  mutate(age=factor(age,levels = c("5","6","7","8","9","10+")))

population_length <- read_excel("Data/Population_TL_at_age_NEAC.xlsx",sheet = "Data") %>% 
  filter(Year==2018) %>% 
  pivot_longer(-Year,names_to = "age",values_to = "length")

population_length  %>% 
  filter(age %in% c(10:14)) %>% 
  summarise(mean=mean(length)) #98.5

population_length <- population_length %>% 
  add_row(Year=2018,age="10+",length=98.5) %>% 
  filter(age %in% c("5","6","7","8","9","10+"))

#t test
t.test(data=filter(study_length,age=="5"),length~Group)
t.test(data=filter(study_length,age=="6"),length~Group)
t.test(data=filter(study_length,age=="7"),length~Group)
t.test(data=filter(study_length,age=="8"),length~Group)
t.test(data=filter(study_length,age=="9"),length~Group)
t.test(data=filter(study_length,age=="10+"),length~Group)

data_t_test <- data.frame(age=c("5","6","7","8","9","10+"),
                          p=c("p=0.002","p=0.960","p=0.001","p=0.533","p=0.815","p=0.528"))

fs13 <- ggplot(study_length)+
  geom_boxplot(aes(x=age,y=length,color=Season),show.legend = T,size=0.375)+
  stat_summary(aes(x=age,y=length,color=Season),position=position_dodge(width = 0.75),
               fun.y=mean, geom="point", shape=4, size=3,show.legend = F)+
  scale_color_aaas()+
  geom_point(data=population_length,aes(x=age,y=length),shape=0,size=2)+
  # geom_text(data=data_label,aes(x=age,y=130,label=paste0(round(delta,2),"%")),family="Calibri")+
  geom_text(data=data_t_test,aes(x=age,y=45,label=p),family="Calibri",size=3)+
  labs(title="Total length at age — autumn 2017 and winter-spring 2018 survey")+
  scale_x_discrete("Age (year)")+
  scale_y_continuous("Total length (cm)",limits = c(40,140),expand = c(0,0),breaks = seq(40,140,10),
                     minor_breaks = seq(40,150,5),guide = "axis_minor")+
  theme(axis.text = element_text(family = "Calibri"),
        axis.title = element_text(family = "Calibri"),
        # axis.ticks = element_line(linewidth = 0.1),
        plot.title = element_text(family="Calibri",face="bold"),
        legend.title = element_text(family = "Calibri"),
        legend.text = element_text(family = "Calibri"),
        legend.position = c(0.2,0.8),
        legend.background = element_blank(),
        legend.box = "horizontal",
        ggh4x.axis.ticks.length.minor = rel(0.625))
  
ggsave("Figures/Extended Data Fig. 13.pdf",device = cairo_pdf,width = 6,height = 4)



