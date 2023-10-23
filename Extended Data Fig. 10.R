# Ma, Shuyang 
# September 2023
# Extended Data Fig. 10

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

# 1 Original SSB ----------------------------------------------------------
meta_2021 <- getListStocks(2021)

#check all stocks
cod_stocks <- meta_2021 %>% 
  filter(SpeciesName == "Gadus morhua") %>% 
  filter(Purpose == "Advice") %>% #only use advice data
  filter(StockKeyLabel == "cod.27.1-2") #use 2021 data

#cod.27.1-2
cod.27.1_2 <- getSAG("cod.27.1-2",2021,data = "source") %>% 
  filter(StockKeyLabel == "cod.27.1-2") %>% #sometimes get several stocks start with the same name
  select(Year,StockSize) %>% 
  rename(SSB_original=2)

# 2 Reconstructed SSB -----------------------------------------------------
#calculate 
biomass <- read_xlsx("Data/Age_structure_NEAC.xlsx",sheet="Biomass-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021) %>% 
  mutate(SSB_5plus=`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`+`13`+`14`+`gp`,
         SSB_6plus=`6`+`7`+`8`+`9`+`10`+`11`+`12`+`13`+`14`+`gp`,
         SSB_7plus=`7`+`8`+`9`+`10`+`11`+`12`+`13`+`14`+`gp`) %>% 
  select(Year,SSB_5plus,SSB_6plus,SSB_7plus)

# 3 Extended Data Fig. 10 -------------------------------------------------------------------
SSB <- left_join(cod.27.1_2,biomass) %>% 
  mutate(across(SSB_original:SSB_7plus,~.x/1000000)) %>% 
  pivot_longer(-Year,names_to = "SSB name",values_to = "biomass") %>% 
  mutate(type=rep(c("Original","Reconstructed","Reconstructed","Reconstructed"),times=length(1946:2021)))

fs10 <- ggplot(SSB)+
  geom_line(aes(x=Year,y=biomass,color=`SSB name`,linetype=type))+
  labs(title="Reconstructed spawning stock biomass — NEAC (1946-2021)")+
  scale_color_brewer(palette = "Dark2")+
  scale_linetype_manual(values = c("Original"="dashed","Reconstructed"="solid"))+
  scale_x_continuous("Year",breaks = seq(1950,2020,10),minor_breaks = NULL)+
  scale_y_continuous("SSB (million tons)",minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri",size=11),
        plot.subtitle = element_text(face=4,family="Calibri"),
        legend.position=c(0.5,0.8),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(face="bold",family = "Calibri")) 

ggsave("Figures/Extended Data Fig. 10.pdf",device = cairo_pdf,width = 6,height = 4)
