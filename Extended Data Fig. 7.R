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
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
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

# 2 Extended Data Fig. 7a -------------------------------------------------------------
cor.test(data$A50,data$tenplus_proportion_abundance)

fs7a <- ggplot(data)+
  geom_point(aes(x=tenplus_proportion_abundance,y=A50))+
  geom_smooth(aes(x=tenplus_proportion_abundance,y=A50),method = "lm")+
  geom_text(data=filter(data,Year %in% c(1946:1950,2015:2020)),aes(x=tenplus_proportion_abundance,y=A50,label=Year),family="Calibri")+
  geom_text(aes(x=Inf,y=-Inf,label="r = 0.326, p < 0.01"),vjust=-1,hjust=1,family="Calibri")+
  scale_x_continuous("Abundance proportion (10 year+)",minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri"),
        plot.subtitle = element_text(face=4,family="Calibri"),
        legend.title = element_text(face="bold",family = "Calibri"),
        legend.text = element_text(face="bold",family = "Calibri")) 


# 3 Extended Data Fig. 7b ------------------------------------------------------------
cor.test(data$A50,data$tenplus_proportion_biomass)

fs7b <- ggplot(data)+
  geom_point(aes(x=tenplus_proportion_biomass,y=A50))+
  geom_smooth(aes(x=tenplus_proportion_biomass,y=A50),method = "lm")+
  geom_text(data=filter(data,Year %in% c(1946:1950,2015:2020)),aes(x=tenplus_proportion_biomass,y=A50,label=Year),family="Calibri")+
  geom_text(aes(x=Inf,y=-Inf,label="r = 0.426, p < 0.001"),vjust=-1,hjust=1,family="Calibri")+
  scale_x_continuous("Biomass proportion (10 year+)",minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri"),
        plot.subtitle = element_text(face=4,family="Calibri"),
        legend.title = element_text(face="bold",family = "Calibri"),
        legend.text = element_text(face="bold",family = "Calibri")) 


# 4 combine figures ---------------------------------------------------------------
figure <- ggarrange(fs7a,fs7b,nrow=1,common.legend = T,legend = "right")
annotate_figure(figure, top=text_grob("Commercial catch proportion of 10 year+ vs. A50 — NEAC (1946-2020)", family="Calibri",face = "bold"))

ggsave("Figures/Extended Data Fig. 7.pdf",device = cairo_pdf,width = 6,height = 4)




