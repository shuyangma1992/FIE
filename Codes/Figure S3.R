# Ma, Shuyang 
# March 2023
# Figure S3

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(ggh4x)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_bw())

# 1 Data ------------------------------------------------------------------
abundance <- read_xlsx("Data/Age_structure_NEAC.xlsx",sheet="Number-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021) %>% 
  select(-c(`3`,`4`))

biomass <- read_xlsx("Data/Age_structure_NEAC.xlsx",sheet="Biomass-at-age") %>% 
  rename(Year=1) %>% 
  filter(!Year==2021) %>% 
  select(-c(`3`,`4`))

# 2 Figure S3a ------------------------------------------------------------
abundance_figure <- abundance %>% 
  pivot_longer(-Year,names_to = "age",values_to = "abundance") %>% 
  mutate(age=factor(age,levels=rev(unique(age))))

fs3a <- ggplot(abundance_figure)+
  geom_bar(aes(x=Year,y=abundance/1000000,fill=age),stat = "identity",position = "fill",color="black",linewidth=0.01,width=1)+
  scale_fill_manual(values = rev(c(brewer.pal(3,"Reds"),brewer.pal(3,"Greens"),brewer.pal(3,"Blues"),brewer.pal(3,"Purples")[c(1,3)])))+
  labs(title="(a) Age structure: abundance",fill="Age\nclass")+
  scale_x_continuous("Year",expand = c(0,0),breaks = seq(1950,2020,10),minor_breaks = seq(1946,2020,1),guide = "axis_minor")+
  scale_y_continuous("Abundance (proportion)",expand = c(0,0))+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        axis.ticks = element_line(linewidth = 0.1),
        plot.title = element_text(face="bold",family="Calibri",size=11),
        plot.subtitle = element_text(face=4,family="Calibri"),
        legend.title = element_text(face="bold",family = "Calibri"),
        legend.text = element_text(face="bold",family = "Calibri"),
        ggh4x.axis.ticks.length.minor = rel(0.5)) 


# 3 Figure S3b ------------------------------------------------------------
biomass_figure <- biomass %>% 
  pivot_longer(-Year,names_to = "age",values_to = "biomass") %>% 
  mutate(age=factor(age,levels=rev(unique(age))))

fs3b <- ggplot(biomass_figure)+
  geom_bar(aes(x=Year,y=biomass/1000000,fill=age),stat = "identity",position = "fill",color="black",linewidth=0.01,width=1)+
  scale_fill_manual(values = rev(c(brewer.pal(3,"Reds"),brewer.pal(3,"Greens"),brewer.pal(3,"Blues"),brewer.pal(3,"Purples")[c(1,3)])))+
  labs(title="(b) Age structure: biomass",fill="Age\nclass")+
  scale_x_continuous("Year",expand = c(0,0),breaks = seq(1950,2020,10),minor_breaks = seq(1946,2020,1),guide = "axis_minor")+
  scale_y_continuous("Biomass (proportion)",expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        axis.ticks = element_line(linewidth = 0.1),
        plot.title = element_text(face="bold",family="Calibri",size=11),
        plot.subtitle = element_text(face=4,family="Calibri"),
        legend.title = element_text(face="bold",family = "Calibri"),
        legend.text = element_text(face="bold",family = "Calibri"),
        ggh4x.axis.ticks.length.minor = rel(0.5))  


# 4 Combine figures -------------------------------------------------------
figure <- ggarrange(fs3a,fs3b,nrow=2,common.legend = T,legend = "right")
annotate_figure(figure, top=text_grob("Spawning stock age structure — NEAC (1946-2020)", family="Calibri",face = "bold"))
ggsave("Figures/Figure S3.pdf",device = cairo_pdf,width = 6,height = 6)






