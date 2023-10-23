# dos Santos Schmidt, Thassya Christina
# September 2023
# Figure 4

##########################################

## Load packages
library(ggplot2)
library(gridExtra)
library(egg)
library(patchwork)
library(tidyr)
library(dplyr)
library(tidyverse)
library(readr) 
library(tibble)
library(readxl)

##########################################
## Data
data <- read_excel("Data/Markers_maturity_ogive_NEAC_autumn_2017.xlsx",sheet = "Data")
names(data)
summary(data)
str(data)

ices2017 <- read_excel("Data/Maturity_ogive_NEAC_2017_ICES.xlsx",sheet = "Data") %>% 
  mutate(fit_link=NA,se_link=NA,fit_rep=NA,right_upr=NA,right_lwr=NA)
names(ices2017)
ices2018 <- read_excel("Data/Maturity_ogive_NEAC_2018_ICES.xlsx",sheet = "Data") %>% 
  mutate(fit_link=NA,se_link=NA,fit_rep=NA,right_upr=NA,right_lwr=NA)
names(ices2018)
F2017 <- read_excel("Data/Maturity_ogive_NEAC_2017.xlsx",sheet = "Data") %>% 
  mutate(fit_link=NA,se_link=NA,fit_rep=NA,right_upr=NA,right_lwr=NA)
names(F2017)
F2018 <- read_excel("Data/Maturity_ogive_NEAC_2018.xlsx",sheet = "Data") %>% 
  mutate(fit_link=NA,se_link=NA,fit_rep=NA,right_upr=NA,right_lwr=NA)
names(F2018)

################################################

## Figure 

#################################################

################################################

## Age-at-maturity

#################################################
## PVO4c 
M4 <- glm(PVO4c ~ (age)^2, 
          data=data, na.action="na.exclude",  family="binomial") 

summary(M4)
M4$coef


## CAO
M5 <-  glm(CAO ~ (age)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M5)
M5$coef

## EVO
M6 <-  glm(EVO ~ (age)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M6)
M6$coef

##### estimate A50
range(data$age)
age_range <- seq(from=min(data$age), to=max(data$age), by=1)

## PVO4c
M4.b0 <- M4$coef[1] ## intercept
M4.age <- M4$coef[2]

a_logits <- M4.b0 + 
  M4.age*age_range

a_probs <- exp(a_logits)/(1 + exp(a_logits))

A50_PVO4c <- -(M4.b0)/M4.age
A50_PVO4c

## CAO
M5.b0 <- M5$coef[1] ## intercept
M5.age <- M5$coef[2]

b_logits <- M5.b0 + 
  M5.age*age_range

b_probs <- exp(b_logits)/(1 + exp(b_logits))

A50_CAO <- -(M5.b0)/M5.age
A50_CAO
 
### EVO
M6.b0 <- M6$coef[1] ## intercept
M6.age <- M6$coef[2]

c_logits <- M6.b0 + 
  M6.age*age_range

c_probs <- exp(c_logits)/(1 + exp(c_logits))

A50_EVO<- -(M6.b0)/M6.age
A50_EVO 

### GGplot 
# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data2 <- data.frame(PVO4c = a_probs, CAO =b_probs, EVO = c_probs, age=age_range)
plot.data2 <- gather(plot.data2, key=group, value=prob, PVO4c:EVO)
head(plot.data2)

#label1<-"L[50]"
label2 <- "A[50]"

neworder1 <- c("PVO4c", "CAO", "EVO")
plot.data2 <- arrange(transform(plot.data2, Group1=factor(group,levels=neworder1)),group)

### FIT value is also now taking from the model
###CI - POF
ndata4 <- with(data, data_frame(age = seq(min(age), max(age),
                                          length = 100)))
## add the fitted values by predicting from the model for the new data
ndata4 <- add_column(ndata4, fit = predict(M4, newdata = ndata4, type = 'response'))

fam_PVO4c <- family(M4)
str(fam_PVO4c)

ilink_PVO4c <- fam_PVO4c$linkinv
ilink_PVO4c

ilink_PVO4c <- family(M4)$linkinv
## add fit and se.fit on the **link** scale
ndata4 <- bind_cols(ndata4, setNames(as_tibble(predict(M4, ndata4, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata4 <- mutate(ndata4,
                 fit_resp  = ilink_PVO4c(fit_link),
                 right_upr = ilink_PVO4c(fit_link + (2 * se_link)),
                 right_lwr = ilink_PVO4c(fit_link - (2 * se_link)))
## show
ndata4

ndata4$group <- "PVO4c" 

### CI - CAO
ndata5 <- with(data, data_frame(age = seq(min(age), max(age),
                                          length = 100)))
## add the fitted values by predicting from the model for the new data
ndata5 <- add_column(ndata5, fit = predict(M5, newdata = ndata5, type = 'response'))

fam_CAO <- family(M5)
str(fam_CAO)

ilink_CAO <- fam_CAO$linkinv
ilink_CAO

ilink_CAO <- family(M5)$linkinv
## add fit and se.fit on the **link** scale
ndata5 <- bind_cols(ndata5, setNames(as_tibble(predict(M5, ndata5, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata5 <- mutate(ndata5,
                 fit_resp  = ilink_CAO(fit_link),
                 right_upr = ilink_CAO(fit_link + (2 * se_link)),
                 right_lwr = ilink_CAO(fit_link - (2 * se_link)))
## show
ndata5

ndata5$group <- "CAO" 

### CI - EVO
ndata6 <- with(data, data_frame(age = seq(min(age), max(age),
                                          length = 100)))
## add the fitted values by predicting from the model for the new data
ndata6 <- add_column(ndata6, fit = predict(M6, newdata = ndata6, type = 'response'))

fam_EVO <- family(M6)
str(fam_EVO)

ilink_EVO <- fam_EVO$linkinv
ilink_EVO

ilink_EVO <- family(M6)$linkinv
## add fit and se.fit on the **link** scale
ndata6 <- bind_cols(ndata6, setNames(as_tibble(predict(M6, ndata6, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata6 <- mutate(ndata6,
                 fit_resp  = ilink_EVO(fit_link),
                 right_upr = ilink_EVO(fit_link + (2 * se_link)),
                 right_lwr = ilink_EVO(fit_link - (2 * se_link)))
## show
ndata6

ndata6$group <- "EVO" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata4), colnames(ndata5))
new.dt4 <- rbind(ndata4[,cols], ndata5[,cols])
cols <- intersect(colnames(new.dt4), colnames(ndata6))
new.dt5 <- rbind(new.dt4[,cols], ndata6[,cols])
cols <- intersect(colnames(new.dt5), colnames(ices2018))
new.dt6 <- rbind(new.dt5[,cols], ices2018[,cols])
cols <- intersect(colnames(new.dt6), colnames(F2017))
new.dt7 <- rbind(new.dt6[,cols], F2017[,cols])
cols <- intersect(colnames(new.dt7), colnames(F2018))
new.dt8 <- rbind(new.dt7[,cols], F2018[,cols])

label2 <- "A[50]"

neworder1 <- c("PVO4c", "CAO", "EVO", "ICES2018", "F2017", "F2018")
new.dt9 <- arrange(transform(new.dt8, Group1=factor(group,levels=neworder1)),group)

age1a <- subset(new.dt9, Group1 == "ICES2018") 
age1c <- subset(new.dt9, Group1 == "F2018") 
age1b <- subset(plot.data2) 

p3 <- new.dt9 %>%
  filter (Group1 != "F2017") %>%
  ggplot() + 
  geom_line(aes(x=age, y=fit, color=Group1), lwd = 1.5) +
  geom_point(data = age1b, aes(age, prob, colour = Group1), size = 4) +  ## predict points
  geom_ribbon(aes(x=age, y=fit_link, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +#c("dashed", "dotdash", "longdash", "twodash")) + 
  scale_color_manual(values = c( "#00AFBB", "#E7B800", "brown", "black", "darkgrey"), 
                     labels = c("PVO4c", "CAO", "EVO", "ICES 2018", "Females 2018")) +
  scale_fill_manual(values = c ( "#00AFBB", "#E7B800", "brown", "black", "darkgrey"), 
                    labels = c("PVO4c", "CAO", "EVO", "ICES 2018", "Females 2018")) +
  scale_linetype(labels =  c("PVO4c", "CAO", "EVO", "ICES 2018", "Females 2018")) +
  labs(x="Age (years)", y="Proportion", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_line(data = age1a, aes(age, fit_link), linewidth = 2) +
  geom_point(data = age1a, aes(age, fit_link), size = 4)+
  geom_line(data = age1c, aes(age, fit_link), linewidth = 2, col = "darkgrey") +
  geom_point(data = age1c, aes(age, fit_link), size = 4, col = "darkgrey")+
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous( limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous( limits = c(1,14), breaks = seq(1, 14, 1)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), ## top, right, bottom and left
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5, margin = margin(r = 15)),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = c(0.20, 0.925),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid")) +
  guides(fill = guide_legend(byrow = TRUE))
p3

################################################

## Length-at-maturity

#################################################
## Getting the value for a and B
## PVO4c 
M <- glm(PVO4c ~ (length)^2, 
         data=data, na.action="na.exclude",  family="binomial") 

summary(M)
M$coef

## CAO
M1 <-  glm(CAO ~ (length)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M1)
M1$coef

## EVO
M2 <-  glm(EVO ~ (length)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M2)
M2$coef

### L50 for each oocyte stage - calculated value
range(data$length)
length_range <- seq(from=min(20), to=max(125), by=10)

## PVO4c
M.b0 <- M$coef[1] ## intercept
M.length <- M$coef[2]

a_logits <- M.b0 + 
  M.length*length_range

a_probs <- exp(a_logits)/(1 + exp(a_logits))

L50_PVO4c <- -(M.b0)/M.length
L50_PVO4c

## CAO
M1.b0 <- M1$coef[1] ## intercept
M1.length <- M1$coef[2]

b_logits <- M1.b0 + 
  M1.length*length_range

b_probs <- exp(b_logits)/(1 + exp(b_logits))

L50_CAO <- -(M1.b0)/M1.length
L50_CAO

### EVO
M2.b0 <- M2$coef[1] ## intercept
M2.length <- M2$coef[2]

c_logits <- M2.b0 + 
  M2.length*length_range

c_probs <- exp(c_logits)/(1 + exp(c_logits))

L50_EVO <- -(M2.b0)/M2.length
L50_EVO

##################################
# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data <- data.frame(PVO4c=a_probs, CAO=b_probs, EVO = c_probs, length=length_range)
plot.data <- gather(plot.data, key=group, value=prob, PVO4c:EVO)
head(plot.data)

label1<-"L[50]"

neworder1 <- c("PVO4c", "CAO", "EVO")
plot.data <- arrange(transform(plot.data, Group1=factor(group,levels=neworder1)),group)


#### FIT value is also now taking from the model
###CI - PVO4c
ndata <- with(data, data_frame(length = seq(min(20), max(length),
                                            length = 100)))


## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(M, newdata = ndata, type = 'response'))

fam_PVO4c <- family(M)
str(fam_PVO4c)

ilink_PVO4c <- fam_PVO4c$linkinv
ilink_PVO4c

ilink_PVO4c <- family(M)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(M, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_PVO4c(fit_link),
                right_upr = ilink_PVO4c(fit_link + (2 * se_link)),
                right_lwr = ilink_PVO4c(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "PVO4c" 


### CI - CAO
ndata1 <- with(data, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1 <- add_column(ndata1, fit = predict(M1, newdata = ndata1, type = 'response'))

fam_CAO <- family(M1)
str(fam_CAO)

ilink_CAO <- fam_CAO$linkinv
ilink_CAO

ilink_CAO <- family(M1)$linkinv
## add fit and se.fit on the **link** scale
ndata1 <- bind_cols(ndata1, setNames(as_tibble(predict(M1, ndata1, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata1 <- mutate(ndata1,
                 fit_resp  = ilink_CAO(fit_link),
                 right_upr = ilink_CAO(fit_link + (2 * se_link)),
                 right_lwr = ilink_CAO(fit_link - (2 * se_link)))
## show
ndata1

ndata1$group <- "CAO" 

### CI - Visual
ndata2 <- with(data, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata2 <- add_column(ndata2, fit = predict(M2, newdata = ndata2, type = 'response'))

fam_EVO <- family(M2)
str(fam_EVO)

ilink_EVO <- fam_EVO$linkinv
ilink_EVO

ilink_EVO <- family(M2)$linkinv
## add fit and se.fit on the **link** scale
ndata2 <- bind_cols(ndata2, setNames(as_tibble(predict(M2, ndata2, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata2 <- mutate(ndata2,
                 fit_resp  = ilink_EVO(fit_link),
                 right_upr = ilink_EVO(fit_link + (2 * se_link)),
                 right_lwr = ilink_EVO(fit_link - (2 * se_link)))
## show
ndata2

ndata2$group <- "EVO" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1))
new.dt <- rbind(ndata[,cols], ndata1[,cols])
cols <- intersect(colnames(new.dt), colnames(ndata2))
new.dt1 <- rbind(new.dt[,cols], ndata2[,cols])

label1<-"L[50]"

neworder <- c("PVO4c", "CAO", "EVO")
new.dt1 <- arrange(transform(new.dt1, Group1=factor(group,levels=neworder)),group)

neworder <- c("PVO4c", "CAO", "EVO")
plot.data <- arrange(transform(plot.data, Group1=factor(group,levels=neworder)),group)

#write.table(new.dt2, "Length-at-maturity_prediction.txt")

p2 <- new.dt1 %>%
  ggplot() + 
  geom_line(aes(x=length, y=fit, color=Group1),lwd = 1.5) +
  geom_ribbon(aes(x = length, y = fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +
  scale_color_manual(values = c( "#00AFBB", "#E7B800", "brown"), 
                     labels = c("PVO4c (histology)", "CAO (histology)", "EVO (histology)")) +
  scale_fill_manual(values = c ( "#00AFBB", "#E7B800", "brown"), 
                    labels = c("PVO4c (histology)", "CAO (histology)", "EVO (histology)")) +
  scale_linetype(labels =  c("PVO4c", "CAO", "EVO"))+
  labs(x="Total length (cm)", y="", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous(limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous(limits = c(20,125), breaks = seq(20, 125, 10)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), ## top, right, bottom and left
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = "none",
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))

p2  

TLa <- subset(plot.data, Group1 != "POF" & Group1 != "Otolith") 
p2a <- p2 + geom_point(data = TLa, aes(length, prob, colour = Group1), size = 4)
p2a

forecast.p <- (p3 + p2a) + plot_annotation(title = "Forecast technique") & theme(plot.title = element_text(size = 20, hjust = 0.5))

forecast.p

###########################################################
### Hindcast technique

################################################

## Age-at-maturity

################################################
## POF 
M4 <- glm(pof.bi ~ (age)^2, 
          data=data, na.action="na.exclude",  family="binomial") 

summary(M4)
M4$coef

## otolith
M5 <-  glm(otolith.spawn.marker ~ (age)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M5)
M5$coef

## visual
M6 <-  glm(visual.no ~ (age)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M6)

## histology
M7 <-  glm(hist.bi ~ (age)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M7)

##### estimate A50
range(data$age)
age_range <- seq(from=min(data$age), to=max(data$age), by=1)

## POF
M4.b0 <- M4$coef[1] ## intercept
M4.age <- M4$coef[2]

a_logits <- M4.b0 + 
  M4.age*age_range

a_probs <- exp(a_logits)/(1 + exp(a_logits))

A50_POF <- -(M4.b0)/M4.age
A50_POF

## Otolith
M5.b0 <- M5$coef[1] ## intercept
M5.age <- M5$coef[2]

b_logits <- M5.b0 + 
  M5.age*age_range

b_probs <- exp(b_logits)/(1 + exp(b_logits))

A50_Otolith <- -(M5.b0)/M5.age
A50_Otolith

### Visual
M6.b0 <- M6$coef[1] ## intercept
M6.age <- M6$coef[2]

c_logits <- M6.b0 + 
  M6.age*age_range

c_probs <- exp(c_logits)/(1 + exp(c_logits))

A50_Visual <- -(M6.b0)/M6.age
A50_Visual

## Histology
M7.b0 <- M7$coef[1] ## intercept
M7.age <- M7$coef[2]

d_logits <- M7.b0 + 
  M7.age*age_range

d_probs <- exp(d_logits)/(1 + exp(d_logits))
A50_Hist <- -(M7.b0)/M7.age
A50_Hist

# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data2 <- data.frame(POF=a_probs, Otolith=b_probs, Visual = c_probs, Histology = d_probs, age=age_range)
plot.data2 <- gather(plot.data2, key=group, value=prob, POF:Histology)
head(plot.data2)

label2 <- "A[50]"

neworder1 <- c("Visual", "Otolith", "Histology", "POF")
plot.data2 <- arrange(transform(plot.data2, Group1=factor(group,levels=neworder1)),group)


### FIT value is also now taking from the model
###CI - POF
ndata4 <- with(data, data_frame(age = seq(min(age), max(age),
                                          length = 100)))
## add the fitted values by predicting from the model for the new data
ndata4 <- add_column(ndata4, fit = predict(M4, newdata = ndata4, type = 'response'))

fam_POFa <- family(M4)
str(fam_POFa)

ilink_POFa <- fam_POFa$linkinv
ilink_POFa

ilink_POFa <- family(M4)$linkinv
## add fit and se.fit on the **link** scale
ndata4 <- bind_cols(ndata4, setNames(as_tibble(predict(M4, ndata4, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata4 <- mutate(ndata4,
                 fit_resp  = ilink_POFa(fit_link),
                 right_upr = ilink_POFa(fit_link + (2 * se_link)),
                 right_lwr = ilink_POFa(fit_link - (2 * se_link)))
## show
ndata4

ndata4$group <- "POF" 

### CI - Otolith
ndata5 <- with(data, data_frame(age = seq(min(age), max(age),
                                          length = 100)))
## add the fitted values by predicting from the model for the new data
ndata5 <- add_column(ndata5, fit = predict(M5, newdata = ndata5, type = 'response'))

fam_OTOa <- family(M5)
str(fam_OTOa)

ilink_OTOa <- fam_OTOa$linkinv
ilink_OTOa

ilink_OTOa <- family(M5)$linkinv
## add fit and se.fit on the **link** scale
ndata5 <- bind_cols(ndata5, setNames(as_tibble(predict(M5, ndata5, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata5 <- mutate(ndata5,
                 fit_resp  = ilink_OTOa(fit_link),
                 right_upr = ilink_OTOa(fit_link + (2 * se_link)),
                 right_lwr = ilink_OTOa(fit_link - (2 * se_link)))
## show
ndata5

ndata5$group <- "Otolith" 

### CI - Visual
ndata6 <- with(data, data_frame(age = seq(min(age), max(age),
                                          length = 100)))
## add the fitted values by predicting from the model for the new data
ndata6 <- add_column(ndata6, fit = predict(M6, newdata = ndata6, type = 'response'))

fam_Visuala <- family(M6)
str(fam_Visuala)

ilink_Visuala <- fam_Visuala$linkinv
ilink_Visuala

ilink_Visuala <- family(M6)$linkinv
## add fit and se.fit on the **link** scale
ndata6 <- bind_cols(ndata6, setNames(as_tibble(predict(M6, ndata6, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata6 <- mutate(ndata6,
                 fit_resp  = ilink_Visuala(fit_link),
                 right_upr = ilink_Visuala(fit_link + (2 * se_link)),
                 right_lwr = ilink_Visuala(fit_link - (2 * se_link)))
## show
ndata6

ndata6$group <- "Visual" 

### CI - Histology
ndata7 <- with(data, data_frame(age = seq(min(age), max(age),
                                          length = 100)))
## add the fitted values by predicting from the model for the new data
ndata7 <- add_column(ndata7, fit = predict(M7, newdata = ndata7, type = 'response'))

fam_Hista <- family(M7)
str(fam_Hista)

ilink_Hista <- fam_Hista$linkinv
ilink_Hista

ilink_Hista <- family(M7)$linkinv
## add fit and se.fit on the **link** scale
ndata7 <- bind_cols(ndata7, setNames(as_tibble(predict(M7, ndata7, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata7 <- mutate(ndata7,
                 fit_resp  = ilink_Hista(fit_link),
                 right_upr = ilink_Hista(fit_link + (2 * se_link)),
                 right_lwr = ilink_Hista(fit_link - (2 * se_link)))
## show
ndata7

ndata7$group <- "Histology" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata4), colnames(ndata5))
new.dt4 <- rbind(ndata4[,cols], ndata5[,cols])
cols <- intersect(colnames(new.dt4), colnames(ndata6))
new.dt5 <- rbind(new.dt4[,cols], ndata6[,cols])
cols <- intersect(colnames(new.dt5), colnames(ndata7))
new.dt6 <- rbind(new.dt5[,cols], ndata7[,cols])
cols <- intersect(colnames(new.dt6), colnames(ices2017))
new.dt7 <- rbind(new.dt6[,cols], ices2017[,cols])
cols <- intersect(colnames(new.dt7), colnames(F2017))
new.dt8 <- rbind(new.dt7[,cols], F2017[,cols])

label2 <- "A[50]"

neworder1 <- c("Visual", "Otolith", "Histology", "POF", "ICES2017", "F2017")
new.dt8 <- arrange(transform(new.dt8, Group1=factor(group,levels=neworder1)),group)

age1d <-subset(plot.data2, Group1 != "Histology" & Group1 != "Visual") 
age1e <- subset(new.dt8, Group1 == "ICES2017") 
age1f <- subset(new.dt8, Group1 == "F2017") 

p5 <- new.dt8 %>%
  filter (Group1 != "Histology" & Group1 != "Visual") %>%
  ggplot() +
  geom_line(aes(x=age, y=fit, color=Group1), lwd = 1.5) + 
  geom_ribbon(aes(x=age, y=fit,ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +
  geom_point(data = age1d, aes(age, prob, colour = Group1), size = 4) +
  scale_color_manual(values = c("tan3","darkorchid3", "black", "darkgray"), 
                     labels = c("Spawning zone (otolith)", "POF (histology)", "ICES 2017 (visual)", "Females 2107")) +
  scale_fill_manual(values = c ("tan3", "darkorchid3", "black", "darkgrey"), 
                    labels = c("Spawning zone (otolith)", "POF (histology)", "ICES 2017 (visual)", "Females 2107")) +
  scale_linetype(labels = c("Spawning zone (otolith)", "POF (histology)", "ICES 2017 (visual)", "Females 2107"))+
  labs(x="Age (years)", y="Proportion", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_point(data = age1e, aes(age, fit), size = 4, colour = "black")+
  geom_point(data = age1f, aes(age, fit), size = 4, colour = "darkgray")+
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous(limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous( limits = c(1,14), breaks = seq(1, 14, 1)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), ## top, right, bottom and left
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5, margin = margin(r = 15)),
        strip.text.x = element_text(size=15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        #legend.position = "none",
        legend.position = c(0.25, 0.925),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))
p5

#hindcast.p <- (p5 + p4a) + plot_annotation(title = "Hindcast technique") & theme(plot.title = element_text(size = 20, hjust = 0.5))

################################################

## Length-at-maturity

#################################################

## POF 
M <- glm(pof.bi ~ (length)^2, 
         data=data, na.action="na.exclude",  family="binomial") 

summary(M)
M$coef

## otolith
M1 <-  glm(otolith.spawn.marker ~ (length)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M1)
M1$coef

## visual
M2 <-  glm(visual.no ~ (length)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M2)

## histology
M3 <-  glm(hist.bi ~ (length)^2, 
           data=data, na.action="na.exclude",  family="binomial") 

summary(M3)

###############################
## use in all plots
range(data$length)
length_range <- seq(from=min(20), to=max(120), by=10)

## POF
M.b0 <- M$coef[1] ## intercept
M.length <- M$coef[2]

a_logits <- M.b0 + 
  M.length*length_range

a_probs <- exp(a_logits)/(1 + exp(a_logits))

L50_POF <- -(M.b0)/M.length
L50_POF

## Otolith
M1.b0 <- M1$coef[1] ## intercept
M1.length <- M1$coef[2]

b_logits <- M1.b0 + 
  M1.length*length_range

b_probs <- exp(b_logits)/(1 + exp(b_logits))

L50_Otolith <- -(M1.b0)/M1.length
L50_Otolith

### Visual
M2.b0 <- M2$coef[1] ## intercept
M2.length <- M2$coef[2]

c_logits <- M2.b0 + 
  M2.length*length_range

c_probs <- exp(c_logits)/(1 + exp(c_logits))

L50_Visual <- -(M2.b0)/M2.length
L50_Visual

## Histology
M3.b0 <- M3$coef[1] ## intercept
M3.length <- M3$coef[2]
d_logits <- M3.b0 + 
  M3.length*length_range

d_probs <- exp(d_logits)/(1 + exp(d_logits))
L50_Hist <- -(M3.b0)/M3.length
L50_Hist

########################################################
# first you have to get the information into a long dataframe, which is what ggplot likes 
plot.data <- data.frame(POF=a_probs, Otolith=b_probs, Visual = c_probs, Histology = d_probs, length=length_range)
plot.data <- gather(plot.data, key=group, value=prob, POF:Histology)
head(plot.data)

label1<-"L[50]"

neworder1 <- c("Visual", "Otolith", "Histology", "POF")
plot.data <- arrange(transform(plot.data, Group1=factor(group,levels=neworder1)),group)

#### FIT value is also now taking from the model
###CI - POF
ndata <- with(data, data_frame(length = seq(min(20), max(length),
                                            length = 100)))


## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(M, newdata = ndata, type = 'response'))

fam_POF <- family(M)
str(fam_POF)

ilink_POF <- fam_POF$linkinv
ilink_POF

ilink_POF <- family(M)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(M, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_POF(fit_link),
                right_upr = ilink_POF(fit_link + (2 * se_link)),
                right_lwr = ilink_POF(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "POF" 


### CI - Otolith
ndata1 <- with(data, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1 <- add_column(ndata1, fit = predict(M1, newdata = ndata1, type = 'response'))

fam_OTO <- family(M1)
str(fam_OTO)

ilink_OTO <- fam_OTO$linkinv
ilink_OTO

ilink_OTO <- family(M1)$linkinv
## add fit and se.fit on the **link** scale
ndata1 <- bind_cols(ndata1, setNames(as_tibble(predict(M1, ndata1, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata1 <- mutate(ndata1,
                 fit_resp  = ilink_OTO(fit_link),
                 right_upr = ilink_OTO(fit_link + (2 * se_link)),
                 right_lwr = ilink_OTO(fit_link - (2 * se_link)))
## show
ndata1

ndata1$group <- "Otolith" 


### CI - Visual
ndata2 <- with(data, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata2 <- add_column(ndata2, fit = predict(M2, newdata = ndata2, type = 'response'))

fam_Visual <- family(M2)
str(fam_Visual)

ilink_Visual <- fam_Visual$linkinv
ilink_Visual

ilink_Visual <- family(M2)$linkinv
## add fit and se.fit on the **link** scale
ndata2 <- bind_cols(ndata2, setNames(as_tibble(predict(M2, ndata2, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata2 <- mutate(ndata2,
                 fit_resp  = ilink_Visual(fit_link),
                 right_upr = ilink_Visual(fit_link + (2 * se_link)),
                 right_lwr = ilink_Visual(fit_link - (2 * se_link)))
## show
ndata2

ndata2$group <- "Visual" 

### CI - Histology
ndata3 <- with(data, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata3 <- add_column(ndata3, fit = predict(M3, newdata = ndata3, type = 'response'))

fam_Hist <- family(M3)
str(fam_Hist)

ilink_Hist <- fam_Hist$linkinv
ilink_Hist

ilink_Hist <- family(M3)$linkinv
## add fit and se.fit on the **link** scale
ndata3 <- bind_cols(ndata3, setNames(as_tibble(predict(M3, ndata3, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata3 <- mutate(ndata3,
                 fit_resp  = ilink_Hist(fit_link),
                 right_upr = ilink_Hist(fit_link + (2 * se_link)),
                 right_lwr = ilink_Hist(fit_link - (2 * se_link)))
## show
ndata3

ndata3$group <- "Histology" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1))
new.dt <- rbind(ndata[,cols], ndata1[,cols])
cols <- intersect(colnames(new.dt), colnames(ndata2))
new.dt1 <- rbind(new.dt[,cols], ndata2[,cols])
cols <- intersect(colnames(new.dt1), colnames(ndata3))
new.dt2 <- rbind(new.dt1[,cols], ndata3[,cols])

label1<-"L[50]"

neworder <- c("Visual", "Otolith", "Histology", "POF")
new.dt2 <- arrange(transform(new.dt2, Group1=factor(group,levels=neworder)),group)

neworder <- c("Visual", "Otolith", "Histology", "POF")
plot.data <- arrange(transform(plot.data, Group1=factor(group,levels=neworder)),group)

p4 <- new.dt2 %>%
  filter (Group1 != "Histology" & Group1 != "Visual") %>%
  ggplot()  + 
  geom_line(aes(x=length, y=fit, color=Group1), lwd = 1.5) +
  geom_ribbon(aes(x = length, y = fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +
    scale_color_manual(values = c("tan3","darkorchid3"), 
                     labels = c("Spawning zone (otolith)", "POF (histology)")) +
  scale_fill_manual(values = c ("tan3", "darkorchid3"), 
                    labels = c("Spawning zone (otolith)", "POF (histology)")) +
  scale_linetype(labels = c("Spawning zone (otolith)", "POF (histology)"))+
  labs(x="Total length (cm)", y="", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous(limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous(limits = c(20,122.5), breaks = seq(20, 122.5, 10)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), ## top, right, bottom and left
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size=15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = "none",
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))

p4 
TL2a <- subset(plot.data, Group1 != "Histology" & Group1 != "Visual") 
p4a <- p4 + geom_point(data = TL2a, aes(length, prob, colour = Group1), size = 4)
p4a

final1 <- (p3 + p2a)/ (p5 + p4a) + plot_layout(heights = unit(c(3,3), c("null", "null")))
final1

ggsave(final1, file = "Kjesbu et al._Figure 4.pdf", dpi = "retina", width = 14, height = 16)


