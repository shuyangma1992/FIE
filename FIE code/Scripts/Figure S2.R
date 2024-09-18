# Ma, Shuyang 
# September 2024
# Figure S2

library(tidyverse)
library(strucchange)
library(ggsci)
library(ggpubr)
library(scales)
library(readxl)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"))
#Theme
theme_set(theme_classic())
#Color
#ggsci package 
show_col(pal_aaas()(10))
mypal <- pal_aaas()(10)


# 1 Regime shift detection --------------------------------------------------
#read data
data <- read_excel("Data/A50_L50_incl_cohort_NEAC.xlsx",sheet = "Data") %>% 
  select(1,4,5) %>% 
  filter(Year %in% c(1937:2022))
data <- as.data.frame(apply(data,2,as.numeric))

#regime shift detection
data_regime <- NULL
regime_shift_year <- NULL
for (i in 1:2) {
  
  data_cycle <- data %>% 
    select(Year,i+1) %>% 
    drop_na() %>% 
    rename(variable=2)
  
  #breakpoints analysis
  abre<-breakpoints(ts(data_cycle$variable,start = data_cycle$Year[1])~1,h=0.15) #breakpoints analysis
  summary(abre) #breakpoints analysis results
  b <- breakpoints(abre) #breakpoints
  ci_b <- confint(abre,breaks = length(b$breakpoints))
  
  #figure data lines
  fm <- lm(ts(data_cycle$variable,start = data_cycle$Year[1]) ~ breakfactor(abre, breaks = length(abre$breakpoints))) #fit the model
  data_cycle$regime_mean <- fitted(fm) #get regime means
  data_cycle$name <- colnames(data)[i+1]
  data_regime <- bind_rows(data_regime,data_cycle)
  
  #figure data regime shift years
  confidence_interval<- as.data.frame(ci_b$confint +ci_b$datatsp[1]-1) #get 95% confidence intervals of breakpoints
  colnames(confidence_interval) <- c("low","mean","up") #rename them
  confidence_interval$name <- colnames(data)[i+1]
  regime_shift_year <- bind_rows(regime_shift_year,confidence_interval)
  
}

# 2 Figure S2a ---------------------------------------------------------------
#year data
data_regime_A50 <- filter(data_regime,name %in% colnames(data)[2]) 
regime_shift_year_A50 <- filter(regime_shift_year,name %in% colnames(data)[2]) 

fs2a <- ggplot(data_regime_A50)+
  geom_rect(data=regime_shift_year_A50,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  geom_line(aes(x=Year,y=variable),color="darkcyan",linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_A50,aes(x=mean,y=-Inf,label=mean),vjust=-1,family="Calibri",fontface=1)+
  labs(title="a) Combined-sexes A50")+
  scale_x_continuous("Cohort birth year",expand = c(0,0),limits = c(1920,2025),breaks = seq(1920,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Combined-sexes A50 (year)",expand = c(0,0),limits = c(6,12),minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri",size=11),
        panel.grid = element_blank())

# 3 Figure S2b ---------------------------------------------------------------
data_regime_L50 <- filter(data_regime,name %in% colnames(data)[3]) 
regime_shift_year_L50 <- filter(regime_shift_year,name %in% colnames(data)[3]) 

fs2b <- ggplot(data_regime_L50)+
  geom_rect(data=regime_shift_year_L50,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  geom_line(aes(x=Year,y=variable),color="grey",linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_L50,aes(x=mean,y=-Inf,label=mean),vjust=-1,family="Calibri",fontface=1)+
  labs(title="b) Combined-sexes L50")+
  scale_x_continuous("Cohort birth year",expand = c(0,0),limits = c(1920,2025),breaks = seq(1920,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Combined-sexes L50 (cm)",expand = c(0,0),limits = c(60,100),breaks = seq(60,100,5),minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri",size=11),
        panel.grid = element_blank())


# 4 Combine figures ---------------------------------------------------------
figure <- ggarrange(fs2a,fs2b,nrow=2)
annotate_figure(figure, top=text_grob("Cohort analysis — NEAC (1936-2016)", family="Calibri",face = "bold"))
ggsave("Figures/Figure S2.pdf",device = cairo_pdf,width = 6,height = 6)

