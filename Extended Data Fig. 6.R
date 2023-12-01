# Ma, Shuyang 
# September 2023
# Extended Data Fig. 6

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(ggridges)
library(viridis)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_classic())

# 1 Data -------------------------------------------------------------
#abundance
abundance <- read_xlsx("Data/Age_structure_NEAC.xlsx",sheet="Number-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021) %>% 
  select(-c(`3`,`4`)) %>% 
  rename("15"=12)

#catch
catch <- read_xlsx("Data/Catch_structure_NEAC.xlsx",sheet="Number-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021) %>% 
  select(-c(`3`,`4`)) %>% 
  rename("15"=12)

# 2 Extended Data Fig. 6a --------------------------------------------------------------
fig_abundance <- abundance %>% 
  pivot_longer(-Year,names_to = "age",values_to = "abundance") %>% 
  mutate(abundance=abundance/100) %>% 
  group_by(Year) %>% 
  summarise(freq=rep(age,abundance)) %>% 
  ungroup() %>% 
  mutate(Year=factor(Year,levels = unique(Year)),
         freq=as.numeric(freq))

fs6a <- ggplot(fig_abundance,aes(x=freq,y=Year))+
  geom_density_ridges(stat="binline",binwidth=1,size=0.25,scale=2,fill="red",alpha=0.5)+
  labs(title="a) Age structure — NEAC")+
  scale_fill_viridis_c(direction = -1) +
  scale_x_continuous("Age (year)",limits = c(4,16),expand = c(0,0),breaks = c(5:15),minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
      axis.title = element_text(face="bold",family = "Calibri"),
      plot.title = element_text(face="bold",family="Calibri"),
      plot.subtitle = element_text(face=4,family="Calibri"),
      legend.title = element_text(face="bold",family = "Calibri"),
      legend.text = element_text(face="bold",family = "Calibri")) 


# 3 Extended Data Fig. 6b --------------------------------------------------------------
fig_catch <- catch %>% 
  pivot_longer(-Year,names_to = "age",values_to = "abundance") %>% 
  mutate(abundance=abundance/100) %>% 
  group_by(Year) %>% 
  summarise(freq=rep(age,abundance)) %>% 
  ungroup() %>% 
  mutate(Year=factor(Year,levels = unique(Year)),
         freq=as.numeric(freq))

fs6b <- ggplot(fig_catch,aes(x=freq,y=Year))+
  # geom_density_ridges_gradient(scale = 2, rel_min_height = 0)+  
  geom_density_ridges(stat="binline",binwidth=1,size=0.25,scale=2,fill="blue",alpha=0.5)+
  labs(title="b) Commerical catch structure — NEAC")+
  scale_fill_viridis_c(direction = -1) +
  scale_x_continuous("Age (year)",limits = c(4,16),expand = c(0,0),breaks = c(5:15),minor_breaks = NULL)+
  # scale_fill_brewer(palette = "PuBu")+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri"),
        plot.subtitle = element_text(face=4,family="Calibri"),
        legend.title = element_text(face="bold",family = "Calibri"),
        legend.text = element_text(face="bold",family = "Calibri")) 


# 4 Combine figures -------------------------------------------------------
figure <- ggarrange(fs6a,fs6b,ncol=2)
ggsave("Figures/Extended Data Fig. 6.pdf",device = cairo_pdf,width = 8,height = 10)

