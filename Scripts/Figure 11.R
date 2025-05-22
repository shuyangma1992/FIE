# dos Santos Schmidt, Thassya Christina
# September 2023
# Figure 11

#################################
# Maturity stages
# Winter file 
# Visual
# Op1. 0 immature + 1 maturing and spawning  == macro.stage_assessment1 (Visual)
# Op2. 0 immature + 1 maturing, spawning and spent  == macro.stage_assessment2(Visual 1 to 4)
# Op3. 0 immature + 1 maturing, spawning, spent, and uncertain == macro.stage_assessment3 (Visual 1 to 5)
# 
# Histology
# Op1. 0 immature + 1 maturing == micro.stage2_assessment1
# Op2. 0 immature + 1 maturing, skip == micro.stage2_assessment2
# Op3. 0 immature + 1 maturing, skip, uncertain == micro.stage2_assessment3
# 
# Spawning survey
# Visual
# Op1. 0 immature + 1 maturing, spawning and spent == macro.stage_assessment1 (Visual)
# Op2. 0 immature + 1 maturing, spawning and spent == macro.stage_assessment2 (Visual 1 to 4)
# Op3. 0 immature + 1 maturing, spawning, spent, and uncertain == macro.stage_assessment3 (Visual 1 to 5)
# 
# WM.micro (Histology)
# Op1. 0 non-developed + 1 maturing and running == micro.stage2_assessment1
# Op2. 0 non-developed + 1 maturing and running == micro.stage2_assessment2
# Op3. 0 non-developed + 1 maturing and running == micro.stage2_assessment3
# 
#################################

# Load packages
library(ggplot2)
library(gridExtra)
library(egg)
library(patchwork)
library(dplyr)
library(tidyr)
library('readr')
library('tibble')
library(readxl)

#################################
cod.winter <- read_excel("Data/Winter.xlsx",sheet = "Data")
names(cod.winter)

cod.spawning <- read_excel("Data/Spawning.xlsx",sheet = "Data")
names(cod.spawning)

ices2018 <- read_excel("Data/Maturity_ogive_NEAC_2018_ICES.xlsx",sheet = "Data") %>% 
  mutate(fit_link=NA,se_link=NA,fit_rep=NA,right_upr=NA,right_lwr=NA)
names(ices2018)

F2018 <- read_excel("Data/Maturity_ogive_NEAC_2018.xlsx",sheet = "Data") %>% 
  mutate(fit_link=NA,se_link=NA,fit_rep=NA,right_upr=NA,right_lwr=NA)
names(F2018)

cod.spawning1 <- cod.spawning %>% 
  filter(Survey == "Spawning")

table(cod.spawning1$micro.stage2_assessment1)
table(cod.spawning1$macro.stage, cod.spawning1$macro.stage_assessment1)
table(cod.winter$macro.stage, cod.winter$macro.stage_assessment1)
table(cod.winter$macro.stage, cod.winter$micro.stage2_assessment1)

### combine dataset
cols <- intersect(colnames(cod.winter), colnames(cod.spawning1))
cod <- rbind(cod.winter[,cols], cod.spawning1[,cols])

names(cod)
str(cod)

cod$macro.stage_assessment1 <- as.numeric(cod$macro.stage_assessment1)
cod$micro.stage2_assessment1 <- as.numeric(cod$micro.stage2_assessment1)

################################################

## Age-at-maturity

#################################################
## Getting the value for a and B
## Visual (macro.stage_assessment)
M <- glm(macro.stage_assessment1 ~ (age)^2, 
         data=cod, na.action="na.exclude",  family="binomial") 

summary(M)
M$coef

## Histology (micro.stage2_assessment))
M1 <-  glm(micro.stage2_assessment1 ~ (age)^2, 
           data=cod, na.action="na.exclude",  family="binomial") 

summary(M1)
M1$coef

### L50 for each oocyte stage - calculated value
range(cod$age)
age_range <- seq(from=min(1), to=max(16), by=1)

## Visual
M.b0 <- M$coef[1] ## intercept
M.age <- M$coef[2]

a_logits <- M.b0 + 
  M.age*age_range

a_probs <- exp(a_logits)/(1 + exp(a_logits))

A50_macro <- -(M.b0)/M.age
A50_macro

## Micro
M1.b0 <- M1$coef[1] ## intercept
M1.age <- M1$coef[2]

b_logits <- M1.b0 + 
  M1.age*age_range

b_probs <- exp(b_logits)/(1 + exp(b_logits))

A50_micro <- -(M1.b0)/M1.age
A50_micro

##################################
### Plot 
# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data2 <- data.frame(Visual = a_probs, Microscopic =b_probs, age=age_range)
plot.data2 <- gather(plot.data2, key=group, value=prob, Visual:Microscopic)
head(plot.data2)

#label1<-"L[50]"
label2 <- "A[50]"

neworder1 <- c("Microscopic", "Visual")
plot.data2 <- arrange(transform(plot.data2, Group1=factor(group,levels=neworder1)),group)

#### FIT value is also now taking from the model
###CI - Visual
ndata <- with(cod, data_frame(age = seq(min(1), max(age),
                                        length = 100)))

## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(M, newdata = ndata, type = 'response'))

fam_Visual <- family(M)
str(fam_Visual)

ilink_Visual <- fam_Visual$linkinv
ilink_Visual

ilink_Visual <- family(M)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(M, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_Visual(fit_link),
                right_upr = ilink_Visual(fit_link + (2 * se_link)),
                right_lwr = ilink_Visual(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "Visual" 

### CI - Microscopic
ndata1 <- with(cod, data_frame(age = seq(min(1), max(age),
                                         length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1 <- add_column(ndata1, fit = predict(M1, newdata = ndata1, type = 'response'))

fam_Micro <- family(M1)
str(fam_Micro)

ilink_Micro <- fam_Micro$linkinv
ilink_Micro

ilink_Micro <- family(M1)$linkinv
## add fit and se.fit on the **link** scale
ndata1 <- bind_cols(ndata1, setNames(as_tibble(predict(M1, ndata1, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata1 <- mutate(ndata1,
                 fit_resp  = ilink_Micro(fit_link),
                 right_upr = ilink_Micro(fit_link + (2 * se_link)),
                 right_lwr = ilink_Micro(fit_link - (2 * se_link)))
## show
ndata1

ndata1$group <- "Microscopic" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1))
new.dt <- rbind(ndata[,cols], ndata1[,cols])
cols1 <- intersect(colnames(new.dt), colnames(ices2018))
new.dt1 <- rbind(new.dt[,cols1], ices2018[,cols1])
cols2 <- intersect(colnames(new.dt1), colnames(F2018))
new.dt2 <- rbind(new.dt1[,cols2], F2018[,cols2])

label1<-"A[50]"
#label2 <- "A[50]"

neworder1 <- c("Microscopic", "Visual", "ICES2018", "F2018")
new.dt2 <- arrange(transform(new.dt2, Group1=factor(group,levels=neworder1)),group)

age1a <- subset(new.dt2, Group1 == "ICES2018") 
age1b <- subset(plot.data2)
age1c <- subset(new.dt2, Group1 == "F2018") 

p3 <- new.dt2 %>%
  ggplot() + 
  geom_line(aes(x=age, y=fit, color=Group1), lwd = 1.5) +
  geom_point(data = age1b, aes(age, prob, colour = Group1), size = 4) +  ## predict points
  geom_ribbon(aes(x=age, y=fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +#c("dashed", "dotdash", "longdash", "twodash")) + 
  scale_color_manual(values = c( "seagreen3", "darkorange3", "black", "darkgrey"), 
                     labels = c("Microscopic", "Visual", "ICES 2018 (visual)", "Females 2018")) +
  scale_fill_manual(values = c ( "seagreen3", "darkorange3", "black", "darkgrey"), 
                    labels = c("Microscopic", "Visual", "ICES 2018 (visual)", "Females 2018")) +
  scale_linetype(labels =  c("Microscopic", "Visual", "ICES 2018 (visual)", "Females 2018")) +
  labs(x="Age (years)", y="Proportion", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_line(data = age1a, aes(age, fit), linewidth = 2) +
  geom_point(data = age1a, aes(age, fit), size = 4)+
  geom_line(data = age1c, aes(age, fit), linewidth = 2, col = "darkgrey") +
  geom_point(data = age1c, aes(age, fit), size = 4, col = "darkgrey")+
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous( limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous( limits = c(1,14), breaks = seq(1, 14, 1)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), 
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5, margin = margin(r = 15)),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = c(0.25, 0.95),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))
p3

################################################

## Length-at-maturity

#################################################
## Getting the value for a and B
## Visual (macro.stage_assessment)
M2 <- glm(macro.stage_assessment1 ~ (length)^2, 
          data=cod, na.action="na.exclude",  family="binomial") 

summary(M2)
M2$coef

## Histology (micro.stage2_assessment))
M3 <-  glm(micro.stage2_assessment1 ~ (length)^2, 
           data=cod, na.action="na.exclude",  family="binomial") 

summary(M3)
M3$coef

### L50 for each oocyte stage - calculated value
range(cod$length)
length_range <- seq(from=min(20), to=max(135), by=10)

## Visual
M2.b0 <- M2$coef[1] ## intercept
M2.length <- M2$coef[2]

a2_logits <- M2.b0 + 
  M2.length*length_range

a2_probs <- exp(a2_logits)/(1 + exp(a2_logits))

L50_macro <- -(M2.b0)/M2.length
L50_macro


## Micro
M3.b0 <- M3$coef[1] ## intercept
M3.length <- M3$coef[2]

b3_logits <- M3.b0 + 
  M3.length*length_range

b3_probs <- exp(b3_logits)/(1 + exp(b3_logits))

L50_micro <- -(M3.b0)/M3.length
L50_micro

##################################
### Plot 
# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data <- data.frame(Visual=a2_probs, Microscopic=b3_probs, length=length_range)
plot.data <- gather(plot.data, key=group, value=prob, Visual:Microscopic)
head(plot.data)

neworder1 <- c("Microscopic", "Visual")
plot.data <- arrange(transform(plot.data, Group1=factor(group,levels=neworder1)),group)


#### FIT value is also now taking from the model
###CI - Visual
ndata <- with(cod, data_frame(length = seq(min(20), max(length),
                                           length = 100)))


## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(M2, newdata = ndata, type = 'response'))

fam2_Visual <- family(M2)
str(fam2_Visual)

ilink_Visual2 <- fam2_Visual$linkinv
ilink_Visual2

ilink_Visual2 <- family(M2)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(M2, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_Visual2(fit_link),
                right_upr = ilink_Visual2(fit_link + (2 * se_link)),
                right_lwr = ilink_Visual2(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "Visual" 


### CI - Microscopic
ndata1a <- with(cod, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1a <- add_column(ndata1a, fit = predict(M3, newdata = ndata1a, type = 'response'))

fam3_Micro <- family(M3)
str(fam3_Micro)

ilink3_Micro <- fam3_Micro$linkinv
ilink3_Micro

ilink3_Micro <- family(M3)$linkinv
## add fit and se.fit on the **link** scale
ndata1a <- bind_cols(ndata1a, setNames(as_tibble(predict(M3, ndata1a, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))
## create the interval and backtransform
ndata1a <- mutate(ndata1a,
                  fit_resp  = ilink3_Micro(fit_link),
                  right_upr = ilink3_Micro(fit_link + (2 * se_link)),
                  right_lwr = ilink3_Micro(fit_link - (2 * se_link)))
## show
ndata1a

ndata1a$group <- "Microscopic" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1a))
new.dt <- rbind(ndata[,cols], ndata1a[,cols])

label1<-"L[50]"
#label2 <- "A[50]"

neworder <- c("Microscopic", "Visual")
new.dt <- arrange(transform(new.dt, Group1=factor(group,levels=neworder)),group)

p2 <- new.dt %>%
  ggplot() + 
  geom_line(aes(x=length, y=fit, color=Group1),lwd = 1.5) +
  geom_ribbon(aes(x = length, y = fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +
  scale_color_manual(values = c( "seagreen3", "darkorange3"), 
                     labels = c("Microscopic", "Visual")) +
  scale_fill_manual(values = c ( "seagreen3", "darkorange3"), 
                    labels = c("Microscopic", "Visual")) +
  scale_linetype(labels =  c("Microscopic", "Visual"))+
  labs(x="Total length (cm)", y="", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous(limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous(limits = c(20,125), breaks = seq(20, 125, 10)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), 
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = c(0.25, 0.95),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))

p2  

TLa <- subset(plot.data, Group1 != "Visual" & Group1 != "Visual") 
p2a <- p2 + geom_point(data = plot.data, aes(length, prob, colour = Group1), size = 4)
p2a

p4 <- p3 + p2a

##################
## Maturity ogive when group stages from 1 to 4 (immature to spent)

table(cod.spawning1$micro.stage2_assessment2)
table(cod.spawning1$macro.stage_assessment2)
table(cod.winter$macro.stage_assessment2)
table(cod.winter$micro.stage2_assessment2)

### combine dataset
cols <- intersect(colnames(cod.winter), colnames(cod.spawning1))
cod <- rbind(cod.winter[,cols], cod.spawning1[,cols])

names(cod)
str(cod)

cod$macro.stage_assessment2 <- as.numeric(cod$macro.stage_assessment2)
cod$micro.stage2_assessment2 <- as.numeric(cod$micro.stage2_assessment2)

################################################

## Age-at-maturity

#################################################
## Getting the value for a and B
## Visual (macro.stage_assessment)
Md <- glm(macro.stage_assessment2 ~ (age)^2, 
          data=cod, na.action="na.exclude",  family="binomial") 

summary(Md)
Md$coef

## Histology (micro.stage2_assessment))
Md1 <-  glm(micro.stage2_assessment2 ~ (age)^2, 
            data=cod, na.action="na.exclude",  family="binomial") 

summary(Md1)
Md1$coef

### L50 for each oocyte stage - calculated value
range(cod$age)
age_range <- seq(from=min(1), to=max(16), by=1)

## Visual
M.b0 <- Md$coef[1] ## intercept
M.age <- Md$coef[2]

a_logits <- M.b0 + 
  M.age*age_range

a_probs <- exp(a_logits)/(1 + exp(a_logits))

A50_macro <- -(M.b0)/M.age
A50_macro

## Micro
M1.b0 <- Md1$coef[1] ## intercept
M1.age <- Md1$coef[2]

b_logits <- M1.b0 + 
  M1.age*age_range

b_probs <- exp(b_logits)/(1 + exp(b_logits))

A50_micro <- -(M1.b0)/M1.age
A50_micro

##################################
### Plot 
# first you have to get the information into a long dataframe, which is what ggplot likes 
plot.data2 <- data.frame(Visual = a_probs, Microscopic =b_probs, age=age_range)
plot.data2 <- gather(plot.data2, key=group, value=prob, Visual:Microscopic)
head(plot.data2)

label2 <- "A[50]"

neworder1 <- c("Microscopic", "Visual")
plot.data2 <- arrange(transform(plot.data2, Group1=factor(group,levels=neworder1)),group)

#### FIT value is also now taking from the model
###CI - Visual
ndata <- with(cod, data_frame(age = seq(min(1), max(age),
                                        length = 100)))

## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(Md, newdata = ndata, type = 'response'))

fam_Visual <- family(Md)
str(fam_Visual)

ilink_Visual <- fam_Visual$linkinv
ilink_Visual

ilink_Visual <- family(Md)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(Md, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_Visual(fit_link),
                right_upr = ilink_Visual(fit_link + (2 * se_link)),
                right_lwr = ilink_Visual(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "Visual" 

### CI - Microscopic
ndata1 <- with(cod, data_frame(age = seq(min(1), max(age),
                                         length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1 <- add_column(ndata1, fit = predict(Md1, newdata = ndata1, type = 'response'))

fam_Micro <- family(Md1)
str(fam_Micro)

ilink_Micro <- fam_Micro$linkinv
ilink_Micro

ilink_Micro <- family(Md1)$linkinv
## add fit and se.fit on the **link** scale
ndata1 <- bind_cols(ndata1, setNames(as_tibble(predict(Md1, ndata1, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata1 <- mutate(ndata1,
                 fit_resp  = ilink_Micro(fit_link),
                 right_upr = ilink_Micro(fit_link + (2 * se_link)),
                 right_lwr = ilink_Micro(fit_link - (2 * se_link)))
## show
ndata1

ndata1$group <- "Microscopic" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1))
new.dt <- rbind(ndata[,cols], ndata1[,cols])
cols1 <- intersect(colnames(new.dt), colnames(ices2018))
new.dt1 <- rbind(new.dt[,cols1], ices2018[,cols1])
cols2 <- intersect(colnames(new.dt1), colnames(F2018))
new.dt2 <- rbind(new.dt1[,cols2], F2018[,cols2])

neworder1 <- c("Microscopic", "Visual", "ICES2018", "F2018")
new.dt2 <- arrange(transform(new.dt2, Group1=factor(group,levels=neworder1)),group)

age1a <- subset(new.dt2, Group1 == "ICES2018") 
age1b <- subset(plot.data2) 
age1c <- subset(new.dt2, Group1 == "F2018") 

p5 <- new.dt2 %>%
  ggplot() + 
  geom_line(aes(x=age, y=fit, color=Group1), lwd = 1.5) +
  geom_point(data = age1b, aes(age, prob, colour = Group1), size = 4) +  ## predict points
  geom_ribbon(aes(x=age, y=fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +
  scale_color_manual(values = c( "seagreen3", "darkorange3", "black", "darkgrey"), 
                     labels = c("Microscopic", "Visual (1 to 4)", "ICES 2018 (visual)", "Females 2018")) +
  scale_fill_manual(values = c ("seagreen3", "darkorange3", "black", "darkgrey"), 
                    labels = c("Microscopic",  "Visual (1 to 4)", "ICES 2018 (visual)", "Females 2018")) +
  scale_linetype(labels =  c("Microscopic", "Visual (1 to 4)", "ICES 2018 (visual)", "Females 2018")) +
  labs(x="Age (years)", y="Proportion", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_line(data = age1a, aes(age, fit), linewidth = 2) +
  geom_point(data = age1a, aes(age, fit), size = 4)+
  geom_line(data = age1c, aes(age, fit), linewidth = 2, col = "darkgrey") +
  geom_point(data = age1c, aes(age, fit), size = 4, col = "darkgrey")+
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous( limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous( limits = c(1,14), breaks = seq(1, 14, 1)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), 
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5, margin = margin(r = 15)),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = c(0.25, 0.95),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))
p5

################################################

## Length-at-maturity

#################################################
## Getting the value for a and B
## Visual (macro.stage_assessment)
Md2 <- glm(macro.stage_assessment2 ~ (length)^2, 
           data=cod, na.action="na.exclude",  family="binomial") 

summary(Md2)
Md2$coef

## Histology (micro.stage2_assessment))
Md3 <-  glm(micro.stage2_assessment2 ~ (length)^2, 
            data=cod, na.action="na.exclude",  family="binomial") 

summary(Md3)
Md3$coef

### L50 for each oocyte stage - calculated value
range(cod$length)
length_range <- seq(from=min(20), to=max(135), by=10)

## Visual
M2.b0 <- Md2$coef[1] ## intercept
M2.length <- Md2$coef[2]

a2_logits <- M2.b0 + 
  M2.length*length_range

a2_probs <- exp(a2_logits)/(1 + exp(a2_logits))

L50_macro <- -(M2.b0)/M2.length
L50_macro

## Micro
M3.b0 <- Md3$coef[1] ## intercept
M3.length <- Md3$coef[2]

b3_logits <- M3.b0 + 
  M3.length*length_range

b3_probs <- exp(b3_logits)/(1 + exp(b3_logits))

L50_micro <- -(M3.b0)/M3.length
L50_micro

##################################
### Plot 
# first you have to get the information into a long dataframe
plot.data <- data.frame(Visual=a2_probs, Microscopic=b3_probs, length=length_range)
plot.data <- gather(plot.data, key=group, value=prob, Visual:Microscopic)
head(plot.data)

neworder1 <- c("Microscopic", "Visual")
plot.data <- arrange(transform(plot.data, Group1=factor(group,levels=neworder1)),group)

#### FIT value is also now taking from the model
###CI - Visual
ndata <- with(cod, data_frame(length = seq(min(20), max(length),
                                           length = 100)))

## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(Md2, newdata = ndata, type = 'response'))

fam2_Visual <- family(Md2)
str(fam2_Visual)

ilink_Visual2 <- fam2_Visual$linkinv
ilink_Visual2

ilink_Visual2 <- family(Md2)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(Md2, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_Visual2(fit_link),
                right_upr = ilink_Visual2(fit_link + (2 * se_link)),
                right_lwr = ilink_Visual2(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "Visual" 


### CI - Microscopic
ndata1a <- with(cod, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1a <- add_column(ndata1a, fit = predict(Md3, newdata = ndata1a, type = 'response'))

fam3_Micro <- family(Md3)
str(fam3_Micro)

ilink3_Micro <- fam3_Micro$linkinv
ilink3_Micro

ilink3_Micro <- family(Md3)$linkinv
## add fit and se.fit on the **link** scale
ndata1a <- bind_cols(ndata1a, setNames(as_tibble(predict(Md3, ndata1a, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))
## create the interval and backtransform
ndata1a <- mutate(ndata1a,
                  fit_resp  = ilink3_Micro(fit_link),
                  right_upr = ilink3_Micro(fit_link + (2 * se_link)),
                  right_lwr = ilink3_Micro(fit_link - (2 * se_link)))
## show
ndata1a

ndata1a$group <- "Microscopic" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1a))
new.dt <- rbind(ndata[,cols], ndata1a[,cols])

label1<-"L[50]"

neworder <- c("Microscopic", "Visual")
new.dt <- arrange(transform(new.dt, Group1=factor(group,levels=neworder)),group)

p6 <- new.dt %>%
  ggplot() +
  geom_line(aes(x=length, y=fit, color=Group1),lwd = 1.5) +
  geom_ribbon(aes(x = length, y = fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +
  scale_color_manual(values = c( "seagreen3", "darkorange3"), 
                     labels = c("Microscopic", "Visual (1 to 4)")) +
  scale_fill_manual(values = c ( "seagreen3", "darkorange3"), 
                    labels = c("Microscopic", "Visual (1 to 4)")) +
  scale_linetype(labels =  c("Microscopic", "Visual (1 to 4)"))+
  labs(x="Total length (cm)", y="", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous(limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous(limits = c(20,125), breaks = seq(20, 125, 10)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), 
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = c(0.25, 0.95),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))

p6  

TLa <- subset(plot.data, Group1 != "Visual" & Group1 != "Visual") 
p6a <- p6 + geom_point(data = plot.data, aes(length, prob, colour = Group1), size = 4)
p6a

p7 <- p5 + p6a

###########
## Maturity ogive combined all stages (1 to 5)

table(cod.spawning1$micro.stage2_assessment3)
table(cod.spawning1$macro.stage_assessment3)
table(cod.winter$macro.stage_assessment3)
table(cod.winter$micro.stage2_assessment3)

### combine dataset
cols <- intersect(colnames(cod.winter), colnames(cod.spawning1))
cod <- rbind(cod.winter[,cols], cod.spawning1[,cols])

names(cod)
str(cod)

cod$macro.stage_assessment3 <- as.numeric(cod$macro.stage_assessment3)
cod$micro.stage2_assessment3 <- as.numeric(cod$micro.stage2_assessment3)

################################################

## Age-at-maturity

#################################################
## Getting the value for a and B
## Visual (macro.stage_assessment)
Mdl <- glm(macro.stage_assessment3 ~ (age)^2, 
           data=cod, na.action="na.exclude",  family="binomial") 

summary(Mdl)
Mdl$coef

## Histology (micro.stage2_assessment))
Mdl1 <-  glm(micro.stage2_assessment3 ~ (age)^2, 
             data=cod, na.action="na.exclude",  family="binomial") 

summary(Mdl1)
Mdl1$coef

### L50 for each oocyte stage - calculated value
range(cod$age)
age_range <- seq(from=min(1), to=max(16), by=1)

## Visual
M.b0 <- Mdl$coef[1] ## intercept
M.age <- Mdl$coef[2]

a_logits <- M.b0 + 
  M.age*age_range

a_probs <- exp(a_logits)/(1 + exp(a_logits))

A50_macro <- -(M.b0)/M.age
A50_macro

## Micro
M1c.b0 <- Mdl1$coef[1] ## intercept
M1c.age <- Mdl1$coef[2]

b_logits <- M1c.b0 + 
  M1c.age*age_range

b_probs <- exp(b_logits)/(1 + exp(b_logits))

A50_micro <- -(M1c.b0)/M1c.age
A50_micro

##################################
### Plot 
# first you have to get the information into a long dataframe
plot.data2 <- data.frame(Visual = a_probs, Microscopic =b_probs, age=age_range)
plot.data2 <- gather(plot.data2, key=group, value=prob, Visual:Microscopic)
head(plot.data2)

label2 <- "A[50]"

neworder1 <- c("Visual", "Microscopic")
plot.data2 <- arrange(transform(plot.data2, Group1=factor(group,levels=neworder1)),group)

#### FIT value is also now taking from the model
###CI - Visual
ndata <- with(cod, data_frame(age = seq(min(1), max(age),
                                        length = 100)))

## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(Mdl, newdata = ndata, type = 'response'))

fam_Visual <- family(Mdl)
str(fam_Visual)

ilink_Visual <- fam_Visual$linkinv
ilink_Visual

ilink_Visual <- family(Mdl)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(Mdl, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_Visual(fit_link),
                right_upr = ilink_Visual(fit_link + (2 * se_link)),
                right_lwr = ilink_Visual(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "Visual" 

### CI - Microscopic
ndata1 <- with(cod, data_frame(age = seq(min(1), max(age),
                                         length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1 <- add_column(ndata1, fit = predict(Mdl1, newdata = ndata1, type = 'response'))

fam_Micro <- family(Mdl1)
str(fam_Micro)

ilink_Micro <- fam_Micro$linkinv
ilink_Micro

ilink_Micro <- family(Mdl1)$linkinv
## add fit and se.fit on the **link** scale
ndata1 <- bind_cols(ndata1, setNames(as_tibble(predict(Mdl1, ndata1, se.fit = TRUE)[1:2]),
                                     c('fit_link','se_link')))
## create the interval and backtransform
ndata1 <- mutate(ndata1,
                 fit_resp  = ilink_Micro(fit_link),
                 right_upr = ilink_Micro(fit_link + (2 * se_link)),
                 right_lwr = ilink_Micro(fit_link - (2 * se_link)))
## show
ndata1

ndata1$group <- "Microscopic" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1))
new.dt <- rbind(ndata[,cols], ndata1[,cols])
cols1 <- intersect(colnames(new.dt), colnames(ices2018))
new.dt1 <- rbind(new.dt[,cols1], ices2018[,cols1])
cols2 <- intersect(colnames(new.dt1), colnames(F2018))
new.dt2 <- rbind(new.dt1[,cols2], F2018[,cols2])

label1<-"A[50]"

neworder1 <- c("Microscopic", "Visual", "ICES2018", "F2018")
new.dt2 <- arrange(transform(new.dt2, Group1=factor(group,levels=neworder1)),group)

age1a <- subset(new.dt2, Group1 == "ICES2018") 
age1b <- subset(plot.data2) 
age1c <- subset(new.dt2, Group1 == "F2018") 

p8 <- new.dt2 %>%
  ggplot() + 
  geom_line(aes(x=age, y=fit, color=Group1), lwd = 1.5) +
  geom_point(data = age1b, aes(age, prob, colour = Group1), size = 4) +  ## predict points
  geom_ribbon(aes(x=age, y=fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +#c("dashed", "dotdash", "longdash", "twodash")) + 
  scale_color_manual(values = c( "seagreen3", "darkorange3", "black", "darkgrey"), 
                     labels = c("Microscopic", "Visual (1 to 5)", "ICES 2018 (visual)", "Females 2018")) +
  scale_fill_manual(values = c ("seagreen3", "darkorange3", "black", "darkgrey"), 
                    labels = c("Microscopic",  "Visual (1 to 5)", "ICES 2018 (visual)", "Females 2018")) +
  scale_linetype(labels =  c("Microscopic", "Visual (1 to 5)", "ICES 2018 (visual)", "Females 2018")) +
  labs(x="Age (years)", y="Proportion", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_line(data = age1a, aes(age, fit), linewidth = 2) +
  geom_point(data = age1a, aes(age, fit), size = 4)+
  geom_line(data = age1c, aes(age, fit), linewidth = 2, col = "darkgrey") +
  geom_point(data = age1c, aes(age, fit), size = 4, col = "darkgrey")+
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous( limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous( limits = c(1,14), breaks = seq(1, 14, 1)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), 
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5, margin = margin(r = 15)),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = c(0.25, 0.95),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))
p8


################################################

## Length-at-maturity

#################################################
## Getting the value for a and B
## Visual (macro.stage_assessment)
Mdl2 <- glm(macro.stage_assessment3 ~ (length)^2, 
            data=cod, na.action="na.exclude",  family="binomial") 

summary(Mdl2)
Mdl2$coef

## Histology (micro.stage2_assessment))
Mdl3 <-  glm(micro.stage2_assessment3 ~ (length)^2, 
             data=cod, na.action="na.exclude",  family="binomial") 

summary(Mdl3)
Mdl3$coef

### L50 for each oocyte stage - calculated value
range(cod$length)
length_range <- seq(from=min(20), to=max(135), by=10)

## Visual
M2.b0 <- Mdl2$coef[1] ## intercept
M2.length <- Mdl2$coef[2]

a2_logits <- M2.b0 + 
  M2.length*length_range

a2_probs <- exp(a2_logits)/(1 + exp(a2_logits))

L50_macro <- -(M2.b0)/M2.length
L50_macro

## Micro
M3.b0 <- Mdl3$coef[1] ## intercept
M3.length <- Mdl3$coef[2]

b3_logits <- M3.b0 + 
  M3.length*length_range

b3_probs <- exp(b3_logits)/(1 + exp(b3_logits))

L50_micro <- -(M3.b0)/M3.length
L50_micro

##################################
### Plot 
# first you have to get the information into a long dataframe, which is what ggplot likes :)
plot.data <- data.frame(Visual=a2_probs, Microscopic=b3_probs, length=length_range)
plot.data <- gather(plot.data, key=group, value=prob, Visual:Microscopic)
head(plot.data)

neworder1 <- c("Microscopic", "Visual")
plot.data <- arrange(transform(plot.data, Group1=factor(group,levels=neworder1)),group)

#### FIT value is also now taking from the model
###CI - Visual
ndata <- with(cod, data_frame(length = seq(min(20), max(length),
                                           length = 100)))


## add the fitted values by predicting from the model for the new data
ndata <- add_column(ndata, fit = predict(Mdl2, newdata = ndata, type = 'response'))

fam2_Visual <- family(Mdl2)
str(fam2_Visual)

ilink_Visual2 <- fam2_Visual$linkinv
ilink_Visual2

ilink_Visual2 <- family(Mdl2)$linkinv
## add fit and se.fit on the **link** scale
ndata <- bind_cols(ndata, setNames(as_tibble(predict(Md2, ndata, se.fit = TRUE)[1:2]),
                                   c('fit_link','se_link')))
## create the interval and backtransform
ndata <- mutate(ndata,
                fit_resp  = ilink_Visual2(fit_link),
                right_upr = ilink_Visual2(fit_link + (2 * se_link)),
                right_lwr = ilink_Visual2(fit_link - (2 * se_link)))
## show
ndata

ndata$group <- "Visual" 


### CI - Microscopic
ndata1a <- with(cod, data_frame(length = seq(min(20), max(length),
                                             length = 100)))
## add the fitted values by predicting from the model for the new data
ndata1a <- add_column(ndata1a, fit = predict(Mdl3, newdata = ndata1a, type = 'response'))

fam3_Micro <- family(Mdl3)
str(fam3_Micro)

ilink3_Micro <- fam3_Micro$linkinv
ilink3_Micro

ilink3_Micro <- family(Mdl3)$linkinv
## add fit and se.fit on the **link** scale
ndata1a <- bind_cols(ndata1a, setNames(as_tibble(predict(Md3, ndata1a, se.fit = TRUE)[1:2]),
                                       c('fit_link','se_link')))
## create the interval and backtransform
ndata1a <- mutate(ndata1a,
                  fit_resp  = ilink3_Micro(fit_link),
                  right_upr = ilink3_Micro(fit_link + (2 * se_link)),
                  right_lwr = ilink3_Micro(fit_link - (2 * se_link)))
## show
ndata1a

ndata1a$group <- "Microscopic" 

####################
## COMBINE ALL DATA SET
cols <- intersect(colnames(ndata), colnames(ndata1a))
new.dt <- rbind(ndata[,cols], ndata1a[,cols])

label1<-"L[50]"

neworder <- c("Microscopic", "Visual")
new.dt <- arrange(transform(new.dt, Group1=factor(group,levels=neworder)),group)

p9 <- new.dt %>%
  ggplot() + 
  geom_line(aes(x=length, y=fit, color=Group1),lwd = 1.5) +
  geom_ribbon(aes(x = length, y = fit, ymin = right_lwr, ymax = right_upr, fill = Group1), alpha = 0.175, colour = NA) +
  scale_color_manual(values = c( "seagreen3", "darkorange3"), 
                     labels = c("Microscopic", "Visual (1 to 5)")) +
  scale_fill_manual(values = c ( "seagreen3", "darkorange3"), 
                    labels = c("Microscopic", "Visual (1 to 5)")) +
  scale_linetype(labels =  c("Microscopic", "Visual (1 to 5)"))+
  labs(x="Total length (cm)", y="", title="", color = "", fill = "", linetype = "") +
  theme_article() +
  geom_hline(yintercept = 0.5, lty = 2, color = "black") +
  scale_y_continuous(limits = c(0,1.0), breaks = seq(0, 1.0, 0.1)) +
  scale_x_continuous(limits = c(20,125), breaks = seq(20, 125, 10)) +
  theme(plot.margin = margin(0.10, 0.25, 0.25, 0.25, "cm"), 
        axis.text.x = element_text(size=20, colour = "black"),
        axis.text.y = element_text(size=20, colour = "black"), 
        axis.title.x = element_text(size=22.5, margin = margin(t = 15)), 
        axis.title.y = element_text(size=22.5),
        strip.text.x = element_text(size=17.5),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15),
        legend.position = c(0.25, 0.95),
        legend.key.width = unit(2, "line"),
        panel.spacing = unit(1.5, "lines"),
        plot.title = element_text(size = 20),
        axis.line = element_line(colour = "black", linetype = "solid"))

p9  

TLa <- subset(plot.data, Group1 != "Visual" & Group1 != "Visual") 
p9a <- p9 + geom_point(data = plot.data, aes(length, prob, colour = Group1), size = 4)
p9a

p10 <- p8 + p9a

final <- p4/p7/p10

ggsave(final, file = "Kjesbu et al._Extended figure.pdf", dpi = "retina", width = 14, height = 16)
 

