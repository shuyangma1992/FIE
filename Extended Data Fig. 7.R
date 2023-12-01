# Ma, Shuyang 
# September 2023
# Extended Data Fig. 7

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(psych)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"),C=windowsFont("Arial"))
#Theme
theme_set(theme_classic())


# 1 Data ------------------------------------------------------------------
#read data
#abundance
abundance <- read_xlsx("Data/Catch_structure_NEAC.xlsx",sheet="Number-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021) 

abundance_10plus <- abundance %>% 
  mutate(tenplus=`10`+`11`+`12`+`13`+`14`+gp,
         total=`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`+`13`+`14`+gp) %>% 
  mutate(tenplus_proportion_abundance=tenplus/total) %>% 
  select(Year,tenplus_proportion_abundance)

#biomass
biomass <- read_xlsx("Data/Catch_structure_NEAC.xlsx",sheet="Biomass-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021) 

biomass_10plus <- biomass %>% 
  mutate(tenplus=`10`+`11`+`12`+`13`+`14`+gp,
         total=`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`+`13`+`14`+gp) %>% 
  mutate(tenplus_proportion_biomass=tenplus/total) %>% 
  select(Year,tenplus_proportion_biomass)

#A50
A50 <- read_excel("Data/A50_NEAC_NSC_NC.xlsx",sheet = "Data") %>% 
  select(1,2) %>% 
  drop_na() %>% 
  rename(A50=2) %>% 
  mutate(A50=as.numeric(A50))

#combine data
data <- left_join(A50,abundance_10plus) %>% 
  left_join(biomass_10plus) %>% 
  drop_na()

data_1946_1981 <- data %>% 
  filter(Year<=1981)
data_1982_2020 <- data %>% 
  filter(Year>1981)


# 2 Extended Data Fig. 7a -------------------------------------------------------------
mod1 <- lm(A50~tenplus_proportion_abundance,data=data_1946_1981)
summary(mod1)
mod2 <- lm(A50~tenplus_proportion_abundance,data=data_1982_2020)
summary(mod2)

cor.test(data_1946_1981$A50,data_1946_1981$tenplus_proportion_abundance)
cor.test(data_1982_2020$A50,data_1982_2020$tenplus_proportion_abundance)

fs7a <- ggplot()+
  # geom_point(aes(x=tenplus_proportion_abundance,y=A50))+
  geom_smooth(data=data_1946_1981,aes(x=tenplus_proportion_abundance,y=A50),method = "lm",color="red")+
  geom_smooth(data=data_1982_2020,aes(x=tenplus_proportion_abundance,y=A50),method = "lm",color="blue")+
  geom_text(data=data_1946_1981,aes(x=tenplus_proportion_abundance,y=A50,label=str_sub(Year,3,4)),family="Arial",check_overlap = T,size=2,color="red")+
  geom_text(data=data_1982_2020,aes(x=tenplus_proportion_abundance,y=A50,label=str_sub(Year,3,4)),family="Arial",check_overlap = T,size=2,color="blue")+
  scale_x_continuous("Abundance proportion in catch (10 year+)",minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Arial"),
        axis.title = element_text(face="bold",family = "Arial"),
        plot.title = element_text(face="bold",family="Arial"),
        plot.subtitle = element_text(face=4,family="Arial"),
        legend.title = element_text(face="bold",family = "Arial"),
        legend.text = element_text(face="bold",family = "Arial")) 


# 3 Extended Data Fig. 7b ------------------------------------------------------------
mod3 <- lm(A50~tenplus_proportion_biomass,data=data_1946_1981)
summary(mod3)
mod4 <- lm(A50~tenplus_proportion_biomass,data=data_1982_2020)
summary(mod4)
cor.test(data_1946_1981$A50,data_1946_1981$tenplus_proportion_biomass)
cor.test(data_1982_2020$A50,data_1982_2020$tenplus_proportion_biomass)

fs7b <- ggplot(data)+
  # geom_point(aes(x=tenplus_proportion_biomass,y=A50))+
  geom_smooth(data=data_1946_1981,aes(x=tenplus_proportion_biomass,y=A50),method = "lm",color="red")+
  geom_smooth(data=data_1982_2020,aes(x=tenplus_proportion_biomass,y=A50),method = "lm",color="blue")+
  geom_text(data=data_1946_1981,aes(x=tenplus_proportion_biomass,y=A50,label=str_sub(Year,3,4)),family="Arial",check_overlap = T,size=2,color="red")+
  geom_text(data=data_1982_2020,aes(x=tenplus_proportion_biomass,y=A50,label=str_sub(Year,3,4)),family="Arial",check_overlap = T,size=2,color="blue")+
  scale_x_continuous("Biomass proportion in catch (10 year+)",minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Arial"),
        axis.title = element_text(face="bold",family = "Arial"),
        plot.title = element_text(face="bold",family="Arial"),
        plot.subtitle = element_text(face=4,family="Arial"),
        legend.title = element_text(face="bold",family = "Arial"),
        legend.text = element_text(face="bold",family = "Arial")) 


# 4 combine figures ---------------------------------------------------------------
figure <- ggarrange(fs7a,fs7b,nrow=1,common.legend = T,legend = "right")
annotate_figure(figure, top=text_grob("Commercial catch proportion of 10 years+ vs. A50 — NEAC (1946-2020)", family="Arial",face = "bold"))

ggsave("Figures/Extended Data Fig. 7.pdf",device = cairo_pdf,width = 6,height = 4)




