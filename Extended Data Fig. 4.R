# Ma, Shuyang 
# September 2023
# Extended Data Fig. 4

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(mgcv)
library(strucchange)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_classic())

# 1 Regime shift detection ------------------------------------------------
#read data
A50 <- read_excel("Data/A50_NEAC_NSC_NC.xlsx",sheet = "Data") %>% 
  select(1,4) %>% 
  drop_na() %>% 
  rename(A50=2) %>% 
  mutate(A50=as.numeric(A50)) 

#breakpoints
abre<-breakpoints(ts(A50$A50,start = A50$Year[1])~1,h=0.25) #breakpoints analysis
summary(abre) #breakpoints analysis results
b <- breakpoints(abre) #breakpoints
ci.b <- confint(abre,breaks = length(b$breakpoints))

fm <- lm(ts(A50$A50,start = A50$Year[1]) ~ breakfactor(abre, breaks = length(abre$breakpoints))) #fit the model
A50$regime.mean <- fitted(fm) #get regime means

confidence.interval<- as.data.frame(ci.b$confint +ci.b$datatsp[1]-1) #get 95% confidence intervals of breakpoints
colnames(confidence.interval) <- c("low","mean","up") #rename them


# 2 Extended Data Fig. 4 -------------------------------------------------------------
fs4 <- ggplot(A50)+
  geom_rect(data=confidence.interval,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  geom_line(aes(x=Year,y=A50),color="darkcyan",linetype="solid")+
  geom_line(aes(x=Year,y=regime.mean),color="black",linetype="dashed")+
  geom_text(data=confidence.interval,aes(x=mean,y=-Inf,label=mean),vjust=-1,family="Calibri",fontface=1)+
  labs(title="Single-year analysis A50 — NSC (1978-2021)")+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1975,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",expand = c(0,0),limits = c(2,5),minor_breaks = NULL)+
  theme(axis.text.x = element_text(face="bold",family = "Calibri"),
        axis.text.y = element_text(face="bold",family = "Calibri"),
        axis.text.y.right = element_text(face="bold",family = "Calibri",color="red"),
        axis.title.x = element_text(face="bold",family = "Calibri"),
        axis.title.y = element_text(face="bold",family = "Calibri"),
        axis.title.y.right = element_text(face="bold",family = "Calibri",color="red",angle=90),
        axis.line.y.right = element_line(color="red"),
        plot.title = element_text(face="bold",family="Calibri",size=11),
        plot.subtitle = element_text(face=4,family="Calibri",size=9),
        legend.position= c(0.6,0.8),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(face="bold",family = "Calibri"))

ggsave("Figures/Extended Data Fig. 4.pdf",device = cairo_pdf,width = 6,height = 4)
