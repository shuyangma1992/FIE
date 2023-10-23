# Ma, Shuyang 
# September 2023
# Fig. 3

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

# 1 H calculation NEAC ----------------------------------------------------
#read data
abundance <- read_xlsx("Data/Age_structure_NEAC.xlsx",sheet="Number-at-age") %>% 
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
H_data <- abundance %>% 
  select(-c(Year,`3`,`4`)) #spawning stock age 5+

H <- data.frame(Year=abundance$Year,
                H_abu=apply(H_data,1,Shannon))


# 2 Fig. 3a   -----------------------------------------------
#read data
A50 <- read_excel("Data/A50_L50_incl_cohort_NEAC.xlsx",sheet="Data") %>% 
  select(1,2) %>% 
  drop_na() %>% 
  rename(A50=2) %>% 
  mutate(A50=as.numeric(A50)) %>% 
  mutate(period=ifelse(Year<=1981,"Before 1981","After 1981"))

#combine H and A50
data <- left_join(H,A50)

#linear regression information
mod1 <- lm(data=filter(data,Year<=1981),A50~H_abu)
summary(mod1)
mod2 <- lm(data=filter(data,Year>1981),A50~H_abu)
summary(mod2)

f3a <- ggplot(data)+
  geom_text(aes(x=H_abu,y=A50,label=str_sub(Year,3,4),color=period),family="Calibri",show.legend = F)+
  scale_color_manual(values = c("Before 1981"="red","After 1981"="blue"))+
  geom_smooth(data=filter(data,Year<=1981),aes(x=H_abu,y=A50),method = "lm",color="red",fill="red",alpha=0.1)+
  geom_smooth(data=filter(data,Year>1981),aes(x=H_abu,y=A50),method = "lm",color="blue",fill="blue",alpha=0.1)+
  labs(title="a) H of spawning stock (5 year+) vs. A50 — NEAC (1946-2020)")+
  scale_x_continuous("H of spawning stock (5 year+)",limits = c(0.5,2.0),expand = c(0,0),minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",limits = c(6,11),expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Calibri"),
        axis.title = element_text(family = "Calibri"),
        plot.title = element_text(face="bold",family="Calibri"),
        panel.grid = element_blank()) 



# 3 H calculation NSC -----------------------------------------------------
#read data
abundance <- read_xlsx("Data/Age_structure_NSC.xlsx",sheet="Number-at-age") %>% 
  rename(Year=1) 

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
H_data <- abundance %>% 
  select(-c(Year,`1`)) 

H <- data.frame(Year=abundance$Year,
                H_abu=apply(H_data,1,Shannon))


# 4 Fig. 3b -----------------------------------------------
#read data
A50 <- read_excel("Data/A50_NEAC_NSC_NC.xlsx",sheet = "Data") %>% 
  select(1,4) %>% 
  drop_na() %>% 
  rename(A50=2) %>% 
  mutate(A50=as.numeric(A50)) %>% 
  mutate(period=ifelse(Year<=1991,"Before 1991","After 1991"))

#combine data
data <- left_join(H,A50) %>% 
  drop_na()

#linear regression information
mod1 <- lm(data=filter(data,Year<=1991),A50~H_abu)
summary(mod1)
mod2 <- lm(data=filter(data,Year>1991),A50~H_abu)
summary(mod2)

f3b <- ggplot(data)+
  geom_text(aes(x=H_abu,y=A50,label=str_sub(Year,3,4),color=period),family="Calibri",show.legend = F)+
  scale_color_manual(values = c("Before 1991"="red","After 1991"="blue"))+
  geom_smooth(data=filter(data,Year<=1991),aes(x=H_abu,y=A50),method = "lm",color="red",fill="red",alpha=0.1)+
  geom_smooth(data=filter(data,Year>1991),aes(x=H_abu,y=A50),method = "lm",color="blue",fill="blue",alpha=0.1)+
  labs(title="b) H of spawning stock (2 year+) vs. A50 —NSC (1978-2021)")+
  scale_x_continuous("H of spawning stock (2 year+)",limits = c(0.3,1.4),expand = c(0,0),minor_breaks = NULL)+
  scale_y_continuous("Both-sexes A50 (year)",limits = c(2,5),expand = c(0,0),minor_breaks = NULL)+
  theme(axis.text = element_text(family = "Calibri"),
        axis.title = element_text(face="bold",family = "Calibri"),
        panel.grid = element_blank())
 

# 4 Combine figures -------------------------------------------------------
figure <- ggarrange(f3a,f3b,nrow=2)
ggsave("Figures/Fig. 3.pdf",device = cairo_pdf,width = 5,height = 10)




