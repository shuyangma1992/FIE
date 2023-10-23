# Ma, Shuyang 
# September 2023
# Extended Data Fig. 9

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(icesSAG)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_classic())

# 1 Fishing mortality ----------------------------------------------------------
meta_2021 <- getListStocks(2021)

#check all stocks
cod_stocks <- meta_2021 %>% 
  filter(SpeciesName == "Gadus morhua") %>% 
  filter(Purpose == "Advice") %>% #only use advice data
  filter(StockKeyLabel == "cod.27.1-2") #use 2021 data

#cod.27.1-2
cod.27.1_2 <- getSAG("cod.27.1-2",2021,data = "source") %>% 
  filter(StockKeyLabel == "cod.27.1-2") %>% #sometimes get several stocks start with the same name
  select(Year,FishingPressure) 

# 2 H of age structure ----------------------------------------------------
#read data
abundance <- read_xlsx("Data/Age_structure_NEAC.xlsx",sheet="Number-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021)

biomass <- read_xlsx("Data/Age_structure_NEAC.xlsx",sheet="Biomass-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021)

#Shannon diversity index function
Shannon <- function(x) {
  
  x <- x[!x==0]
  total <- sum(x)
  s <- NULL
  for (i in 1:length(x)) {
    
    # i=1
    s_cycle <- -x[i]/total*log(x[i]/total)
    s <- c(s,s_cycle)
    
    }
  return(sum(s))
  
}

#calculate H
#abundance
H_abundance_data <- abundance %>% 
  select(-c(Year,`3`,`4`)) 

H_abundance <- data.frame(Year=abundance$Year,
                          H_abu=apply(H_abundance_data,1,Shannon))

#biomass
H_biomass_data <- biomass %>% 
  select(-c(Year,`3`,`4`)) 

H_biomass <- data.frame(Year=biomass$Year,
                          H_bio=apply(H_biomass_data,1,Shannon))

#combine data
H_age <- left_join(H_abundance,H_biomass) %>% 
  rename(H_age_abundance=2,H_age_biomass=3)

# 3 Extended Data Fig. 9a---------------------------------------------------------------
data <- left_join(H_age,cod.27.1_2)

#Linear regression
mod <- lm(data=data,H_age_abundance~FishingPressure)
summary(mod)

#figure
fs9a <- ggplot(data)+
  geom_text(aes(x=FishingPressure,y=H_age_abundance,label=str_sub(Year,3,4)),family="Calibri",show.legend = F)+
  geom_smooth(aes(x=FishingPressure,y=H_age_abundance),method = "lm",color="blue",fill="blue",alpha=0.1)+
  labs(title="H of spawning stock vs. Fishing mortality\nNEAC (1946-2020)")+
  scale_x_continuous("Fishing mortality",minor_breaks = NULL)+
  scale_y_continuous("H of spawning stock (5 year+)",minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri"),
        plot.subtitle = element_text(face=4,family="Calibri")) 

# 4 Extended Data Fig. 9b -------------------------------------------------------------
SSB <- biomass %>% 
  mutate(SSB=`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`+`13`+`14`+`gp`) %>% 
  select(Year,SSB) %>% 
  mutate(SSB=SSB/1000000)

data <- left_join(H_age,SSB) %>% 
  mutate(period=ifelse(Year<=1981,"Before 1981","After 1981"))

mod <- lm(data=data,H_age_abundance~SSB)
summary(mod)

fs9b <- ggplot(data)+
  geom_text(aes(x=SSB,y=H_age_abundance,label=str_sub(Year,3,4),color=period),family="Calibri",show.legend = F)+
  scale_color_manual(values = c("Before 1981"="red","After 1981"="blue"))+
  geom_smooth(aes(x=SSB,y=H_age_abundance),method = "lm",color="black",fill="black",alpha=0.1)+
  labs(title="H of spawning stock vs. SSB\nNEAC (1946-2020)")+
  scale_x_continuous("SSB (5 year+, million tons)",minor_breaks = NULL)+
  scale_y_continuous("H of spawning stock (5 year+)",minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri"),
        plot.subtitle = element_text(face=4,family="Calibri"),
        legend.title = element_text(face="bold",family = "Calibri"),
        legend.text = element_text(face="bold",family = "Calibri")) 


# 5 combine figures -------------------------------------------------------
figure <- ggarrange(fs9a,fs9b,nrow=2)
ggsave("Figures/Extended Data Fig. 9.pdf",device = cairo_pdf,width = 6,height = 8)









