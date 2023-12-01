# Ma, Shuyang 
# September 2023
# Fig. 2

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

# 1 Regime shift detection --------------------------------------------------
#read data
data <- read_excel("Data/A50_L50_incl_cohort_NEAC.xlsx",sheet="Data") %>% 
  select(1,2,3)
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

# 2 Fig. 2a ---------------------------------------------------------------
#year data
data_regime_A50 <- filter(data_regime,name %in% colnames(data)[2]) 
regime_shift_year_A50 <- filter(regime_shift_year,name %in% colnames(data)[2]) 

f2a <- ggplot(data_regime_A50)+
  geom_rect(data=regime_shift_year_A50,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  geom_line(aes(x=Year,y=variable),color="darkcyan",linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_A50,aes(x=mean,y=-Inf,label=mean),vjust=-1,family="Calibri",fontface=1)+
  labs(title="a) Both-sexes A50")+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",expand = c(0,0),limits = c(6,12),minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri",size=11))

# 3 Fig. 2b ---------------------------------------------------------------
data_regime_L50 <- filter(data_regime,name %in% colnames(data)[3]) 
regime_shift_year_L50 <- filter(regime_shift_year,name %in% colnames(data)[3]) 

f2b <- ggplot(data_regime_L50)+
  geom_rect(data=regime_shift_year_L50,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  geom_line(aes(x=Year,y=variable),color="grey",linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_L50,aes(x=mean,y=-Inf,label=mean),vjust=-1,family="Calibri",fontface=1)+
  labs(title="b) Both-sexes L50")+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Both-sexes L50 (cm)",expand = c(0,0),limits = c(60,100),breaks = seq(60,100,5),minor_breaks = NULL)+
  theme(axis.text = element_text(face="bold",family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri",size=11))


# 4 Combine figures ---------------------------------------------------------
figure <- ggarrange(f2a,f2b,nrow=2,align = "v")
annotate_figure(figure, top=text_grob("Single-year analysis — NEAC (1946-2022)", family="Calibri",face = "bold"))
ggsave("Figures/Fig. 2.pdf",device = cairo_pdf,width = 6,height = 6)

