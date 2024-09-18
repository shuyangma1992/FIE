# Ma, Shuyang 
# September 2024
# Figure 2

library(tidyverse)
library(strucchange)
library(ggsci)
library(ggpubr)
library(scales)
library(readxl)

#Something about figures
#Font
windowsFonts(A=windowsFont("Times New Roman"),B=windowsFont("Calibri"),C=windowsFont("Arial"))
#Theme
theme_set(theme_classic())
#Color
mypal <- pal_npg()(9)
show_col(mypal)

# 1 Regime shift detection --------------------------------------------------
#read data
#A50 and L50
data <- read_excel("Data/A50_L50_incl_cohort_NEAC.xlsx",sheet="Data") %>% 
  select(1,2,3)
data <- as.data.frame(apply(data,2,as.numeric))

#SSB and F
SSB_F <- read_excel("Data/SSB_F_NEAC.xlsx") %>% 
  select(1,5,13)

#Kola temperature
Kola_T <- read_excel("Data/Kola section temperature.xlsx") 

#weight at age
weight_at_age <- read_excel("Data/Weight_at_age_NEAC.xlsx") %>% 
  rename(Year=1) %>% 
  select(1,4,5,6)

#TSB
TSB <- read_excel("Data/SSB_F_NEAC.xlsx") %>% 
  select(1,12)

#combine data
data <- left_join(data,SSB_F) %>% 
  left_join(Kola_T) %>% 
  left_join(weight_at_age) %>%
  left_join(TSB) %>% 
  filter(Year>1945)

#regime shift detection
data_regime <- NULL
regime_shift_year <- NULL
for (i in 1:9) {
  
  data_cycle <- data %>% 
    select(Year,i+1) %>% 
    drop_na() %>% 
    rename(variable=2)
  
  #breakpoints analysis
  abre<-breakpoints(ts(data_cycle$variable,start = data_cycle$Year[1])~1,h=0.20) #breakpoints analysis
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

# 2 Figure 2a ---------------------------------------------------------------
#year data
data_regime_A50 <- filter(data_regime,name %in% colnames(data)[2]) 
regime_shift_year_A50 <- filter(regime_shift_year,name %in% colnames(data)[2]) 

f2a <- ggplot(data_regime_A50)+
  # geom_rect(data=regime_shift_year_A50,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect",xmin=-Inf,xmax=1981.5,ymin=-Inf,ymax=Inf,fill=alpha(mypal[6],0.1))+
  annotate("rect",xmin=1981.5,xmax=Inf,ymin=-Inf,ymax=Inf,fill=alpha(mypal[8],0.1))+
  geom_vline(data=regime_shift_year_A50,aes(xintercept = low),linetype="dotted")+
  geom_vline(data=regime_shift_year_A50,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x=Year,y=variable),color=mypal[1],linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_A50,aes(x=mean,y=-Inf,label=mean),vjust=-0.5,family="Arial",fontface=1)+
  # labs(title="a) Both-sexes A50")+
  annotate("text",x=1942,y=Inf,hjust=0,vjust=1.5,label="Combined-sexes A50",family="Arial",fontface="bold",size=4)+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("A50 (year)",expand = c(0,0),limits = c(6,12),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        plot.title = element_text(family = "Arial"))

# 3 Figure 2b ---------------------------------------------------------------
data_regime_L50 <- filter(data_regime,name %in% colnames(data)[3]) 
regime_shift_year_L50 <- filter(regime_shift_year,name %in% colnames(data)[3]) 

f2b <- ggplot(data_regime_L50)+
  # geom_rect(data=regime_shift_year_L50,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect",xmin=-Inf,xmax=1981.5,ymin=-Inf,ymax=Inf,fill=alpha(mypal[6],0.1))+
  annotate("rect",xmin=1981.5,xmax=Inf,ymin=-Inf,ymax=Inf,fill=alpha(mypal[8],0.1))+
  geom_vline(data=regime_shift_year_L50,aes(xintercept = low),linetype="dotted")+
  geom_vline(data=regime_shift_year_L50,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x=Year,y=variable),color=mypal[2],linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_L50,aes(x=mean,y=-Inf,label=mean),vjust=-0.5,family="Arial",fontface=1)+
  # labs(title="b) Both-sexes L50")+
  annotate("text",x=1942,y=Inf,hjust=0,vjust=1.5,label="Combined-sexes L50",family="Arial",fontface="bold",size=4)+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("L50 (cm)",expand = c(0,0),limits = c(60,100),breaks = seq(60,100,5),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        plot.title = element_text(family="Arial"))

# 4 Figure 2c ---------------------------------------------------------------
data_regime_SSB <- filter(data_regime,name %in% colnames(data)[4]) 
regime_shift_year_SSB <- filter(regime_shift_year,name %in% colnames(data)[4]) 

f2c <- ggplot(data_regime_SSB)+
  # geom_rect(data=regime_shift_year_SSB,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect",xmin=-Inf,xmax=1981.5,ymin=-Inf,ymax=Inf,fill=alpha(mypal[6],0.1))+
  annotate("rect",xmin=1981.5,xmax=Inf,ymin=-Inf,ymax=Inf,fill=alpha(mypal[8],0.1))+
  geom_vline(data=regime_shift_year_SSB,aes(xintercept = low),linetype="dotted")+
  geom_vline(data=regime_shift_year_SSB,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x=Year,y=variable/1000000),color=mypal[3],linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean/1000000),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_SSB,aes(x=mean,y=-Inf,label=mean),vjust=-0.5,family="Arial",fontface=1)+
  # labs(title="c) SSB")+
  annotate("text",x=1942,y=Inf,hjust=0,vjust=1.5,label="SSB",family="Arial",fontface="bold",size=4)+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("SSB (million ton)",expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        plot.title = element_text(family="Arial"))

# 5 Figure 2d ---------------------------------------------------------------
data_regime_F <- filter(data_regime,name %in% colnames(data)[5]) 
regime_shift_year_F <- filter(regime_shift_year,name %in% colnames(data)[5]) 

f2d <- ggplot(data_regime_F)+
  # geom_rect(data=regime_shift_year_F,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect",xmin=-Inf,xmax=1981.5,ymin=-Inf,ymax=Inf,fill=alpha(mypal[6],0.1))+
  annotate("rect",xmin=1981.5,xmax=Inf,ymin=-Inf,ymax=Inf,fill=alpha(mypal[8],0.1))+
  geom_vline(data=regime_shift_year_F,aes(xintercept = low),linetype="dotted")+
  geom_vline(data=regime_shift_year_F,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x=Year,y=variable),color=mypal[4],linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_F,aes(x=mean,y=-Inf,label=mean),vjust=-0.5,family="Arial",fontface=1)+
  annotate("text",x=1942,y=Inf,hjust=0,vjust=1.5,label="Fishing mortality, ages 5-7 years",family="Arial",fontface="bold",size=4)+
  # labs(title="d) F")+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Mortality",expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        plot.title = element_text(family="Arial"))

# 5 Figure 2e ---------------------------------------------------------------
data_regime_T <- filter(data_regime,name %in% colnames(data)[6]) 
regime_shift_year_T <- filter(regime_shift_year,name %in% colnames(data)[6]) 

f2e <- ggplot(data_regime_T)+
  # geom_rect(data=regime_shift_year_T,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect",xmin=-Inf,xmax=1981.5,ymin=-Inf,ymax=Inf,fill=alpha(mypal[6],0.1))+
  annotate("rect",xmin=1981.5,xmax=Inf,ymin=-Inf,ymax=Inf,fill=alpha(mypal[8],0.1))+
  geom_vline(data=regime_shift_year_T,aes(xintercept = low),linetype="dotted")+
  geom_vline(data=regime_shift_year_T,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x=Year,y=variable),color=mypal[5],linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_T,aes(x=mean,y=-Inf,label=mean),vjust=-0.5,family="Arial",fontface=1)+
  annotate("text",x=1942,y=Inf,hjust=0,vjust=1.5,label="Kola temperature",family="Arial",fontface="bold",size=4)+
  # labs(title="e) T")+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Temperature (\u00B0C)",expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        plot.title = element_text(family="Arial"))


# 6 Figure 2f ----------------------------------------------------------------
data_regime_weight_at_age <- filter(data_regime,name %in% colnames(data)[7:9]) 
regime_shift_year_weight_at_age <- filter(regime_shift_year,name %in% colnames(data)[7:9]) 

f2f <- ggplot(data_regime_weight_at_age)+
  # geom_rect(data=regime_shift_year_T,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect",xmin=-Inf,xmax=1981.5,ymin=-Inf,ymax=Inf,fill=alpha(mypal[6],0.1))+
  annotate("rect",xmin=1981.5,xmax=Inf,ymin=-Inf,ymax=Inf,fill=alpha(mypal[8],0.1))+
  # geom_vline(data=regime_shift_year_weight_at_age,aes(xintercept = low),linetype="dotted")+
  # geom_vline(data=regime_shift_year_weight_at_age,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x=Year,y=variable,group=name),color=mypal[9],linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean,group=name),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_weight_at_age,aes(x=mean,y=-Inf,label=mean),vjust=-0.5,family="Arial",fontface=1)+
  annotate("text",x=1942,y=Inf,hjust=0,vjust=1.5,label="Weight-at-age",family="Arial",fontface="bold",size=4)+
  # labs(title="e) T")+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("Weight (kg)",expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        plot.title = element_text(family="Arial"))



# 7 Figure 2g ---------------------------------------------------------------
data_regime_TSB <- filter(data_regime,name %in% colnames(data)[10]) 
regime_shift_year_TSB <- filter(regime_shift_year,name %in% colnames(data)[10]) 

f2g <- ggplot(data_regime_TSB)+
  # geom_rect(data=regime_shift_year_T,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect",xmin=-Inf,xmax=1981.5,ymin=-Inf,ymax=Inf,fill=alpha(mypal[6],0.1))+
  annotate("rect",xmin=1981.5,xmax=Inf,ymin=-Inf,ymax=Inf,fill=alpha(mypal[8],0.1))+
  geom_vline(data=regime_shift_year_TSB,aes(xintercept = low),linetype="dotted")+
  geom_vline(data=regime_shift_year_TSB,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x=Year,y=variable/1000000,group=name),color=mypal[7],linetype="solid")+
  geom_line(aes(x=Year,y=regime_mean/1000000,group=name),color="black",linetype="dashed")+
  geom_text(data=regime_shift_year_TSB,aes(x=mean,y=-Inf,label=mean),vjust=-0.5,family="Arial",fontface=1)+
  annotate("text",x=1942,y=Inf,hjust=0,vjust=1.5,label="TSB, 3 years+",family="Arial",fontface="bold",size=4)+
  # labs(title="e) T")+
  scale_x_continuous("Year",expand = c(0,0),limits = c(1940,2025),breaks = seq(1930,2020,10),minor_breaks = NULL)+
  scale_y_continuous("TSB (million tons)",expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"),
        plot.title = element_text(family="Arial"))


# 8 Combine figures ---------------------------------------------------------
figure <- ggarrange(f2a,f2b,f2c,f2g,f2d,f2e,f2f,nrow=7,align = "v",
                    labels = c("(a)","(b)","(c)","(d)","(e)","(f)","(g)"),font.label = list(family="Arial"))
annotate_figure(figure, top=text_grob("Single-year analysis — NEAC (1946-2022)", family="Arial",face = "bold"))
ggsave("Figures/Figure 2.pdf",device = cairo_pdf,width = 4,height = 14)

