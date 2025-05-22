# Ma, Shuyang
# May 2025
# Figure 13

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(icesSAG)
library(FSA)
library(modelr)

# Something about figures
# Font
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Calibri"))
# Theme
theme_set(theme_classic())

# 1 Official SSB ----------------------------------------------------------
meta_2021 <- getListStocks(2021)

# check all stocks
cod_stocks <- meta_2021 %>%
  filter(SpeciesName == "Gadus morhua") %>%
  filter(Purpose == "Advice") %>% # only use advice data
  filter(StockKeyLabel == "cod.27.1-2") # use 2021 data

# cod.27.1-2
cod.27.1_2 <- getSAG("cod.27.1-2", 2021, data = "source") %>%
  filter(StockKeyLabel == "cod.27.1-2")
cod.27.1_2 <- cod.27.1_2 %>% # sometimes get several stocks start with the same name
  select(Year, StockSize) %>%
  rename(SSB_official = 2)

# 2 Reconstructed SSB -----------------------------------------------------
# calculate
biomass <- read_xlsx("Data/Age_structure_NEAC.xlsx", sheet = "Biomass-at-age") %>%
  rename(Year = 1) %>%
  filter(!Year == 2021) %>%
  mutate(
    SSB_5plus = `5` + `6` + `7` + `8` + `9` + `10` + `11` + `12` + `13` + `14` + `gp`,
    SSB_6plus = `6` + `7` + `8` + `9` + `10` + `11` + `12` + `13` + `14` + `gp`,
    SSB_7plus = `7` + `8` + `9` + `10` + `11` + `12` + `13` + `14` + `gp`
  ) %>%
  select(Year, SSB_5plus, SSB_6plus, SSB_7plus)

# 3 Figure 13a -------------------------------------------------------------------
SSB <- left_join(cod.27.1_2, biomass) %>%
  mutate(across(SSB_official:SSB_7plus, ~ .x / 1000000)) %>%
  pivot_longer(-Year, names_to = "SSB name", values_to = "biomass") %>%
  mutate(type = rep(c("Official", "Reconstructed", "Reconstructed", "Reconstructed"), times = length(1946:2021)))

f13a <- ggplot(SSB) +
  geom_line(aes(x = Year, y = biomass, color = `SSB name`, linetype = type)) +
  labs(title = "Reconstructed SSB — NEAC (1946-2021)") +
  scale_color_brewer(palette = "Dark2") +
  scale_linetype_manual(values = c("Official" = "solid", "Reconstructed" = "dashed")) +
  scale_x_continuous("Year", breaks = seq(1950, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("SSB (million ton)", minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Calibri"),
    axis.title = element_text(family = "Calibri"),
    plot.title = element_text(face = "bold", family = "Calibri", size = 11),
    plot.subtitle = element_text(face = 4, family = "Calibri"),
    legend.position = c(0.5, 0.8),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", family = "Calibri")
  )


# 4 Figure 13b -------------------------------------------------------------
# cod.27.1-2
recruitment <- getSAG("cod.27.1-2", 2021, data = "source") %>%
  filter(StockKeyLabel == "cod.27.1-2") %>% # sometimes get several stocks start with the same name
  select(Year, Recruitment) %>%
  arrange(Year)

# S-R relationship using SSB official and reconstructed SSB 7+
S_R_data <- left_join(cod.27.1_2, biomass) %>%
  left_join(recruitment) %>%
  select(Year, SSB_official, SSB_7plus, Recruitment) %>%
  arrange(Year)

# recruitment (3 year old) lead 3
S_R_data <- S_R_data %>%
  mutate(Recruitment = lead(Recruitment, 3)) %>%
  na.omit()

# fit Ricker and Beverton-Holt models

# SSB official-Recruitment
# Ricker
start.Ricker_SSB_official <- srStarts(Recruitment ~ SSB_official, data = S_R_data, type = "Ricker", param = 1)
formula.Ricker_SSB_official <- log(Recruitment)~log(a * SSB_official * exp(-b * SSB_official))
model.Ricker_SSB_official <- nls(formula.Ricker_SSB_official, data = S_R_data, start = start.Ricker_SSB_official)
summary(model.Ricker_SSB_official)
rsquare(model.Ricker_SSB_official, S_R_data)

# Beverton-Holt model
start.BH_SSB_official <- srStarts(Recruitment ~ SSB_official, data = S_R_data, type = "BevertonHolt", param = 1)
formula.BH_SSB_official <- log(Recruitment)~log((a * SSB_official) / (1 + b * SSB_official))
model.BH_SSB_official <- nls(formula.BH_SSB_official, data = S_R_data, start = start.BH_SSB_official)
summary(model.BH_SSB_official)
rsquare(model.BH_SSB_official, S_R_data)

# fitted results
S_R_data <- S_R_data %>%
  mutate(Ricker.fitted_SSB_official = coef(model.Ricker_SSB_official)[1] * SSB_official * exp(-coef(model.Ricker_SSB_official)[2] * SSB_official)) %>%
  mutate(BH.fitted_SSB_official = (coef(model.BH_SSB_official)[1] * SSB_official) / (1 + coef(model.BH_SSB_official)[2] * SSB_official))

# SSB_7plus-Recruitment
# Ricker
start.Ricker_SSB_7plus <- srStarts(Recruitment ~ SSB_7plus, data = S_R_data, type = "Ricker", param = 1)
formula.Ricker_SSB_7plus <- log(Recruitment)~log(a * SSB_7plus * exp(-b * SSB_7plus))
model.Ricker_SSB_7plus <- nls(formula.Ricker_SSB_7plus, data = S_R_data, start = start.Ricker_SSB_7plus)
summary(model.Ricker_SSB_7plus)
rsquare(model.Ricker_SSB_7plus, S_R_data)

# Beverton-Holt model
start.BH_SSB_7plus <- srStarts(Recruitment ~ SSB_7plus, data = S_R_data, type = "BevertonHolt", param = 1)
formula.BH_SSB_7plus <- log(Recruitment)~log((a * SSB_7plus) / (1 + b * SSB_7plus))
model.BH_SSB_7plus <- nls(formula.BH_SSB_7plus, data = S_R_data, start = start.BH_SSB_7plus)
summary(model.BH_SSB_7plus)
rsquare(model.BH_SSB_7plus, S_R_data)

# fitted results
S_R_data <- S_R_data %>%
  mutate(Ricker.fitted_SSB_7plus = coef(model.Ricker_SSB_7plus)[1] * SSB_7plus * exp(-coef(model.Ricker_SSB_7plus)[2] * SSB_7plus)) %>%
  mutate(BH.fitted_SSB_7plus = (coef(model.BH_SSB_7plus)[1] * SSB_7plus) / (1 + coef(model.BH_SSB_7plus)[2] * SSB_7plus))

# segmented regression
library(segmented)
# official SSB
s_r_seg_official <- segreg(Recruitment ~ seg(SSB_official, npsi = 1, est = c(1, 0)) + 0, data = S_R_data)
s_r_seg_official$psi
fitted_results_official <- data.frame(SSB_official = S_R_data$SSB_official, Recruitment = fitted(s_r_seg_official))
fitted_results_official <- fitted_results_official %>%
  add_row(SSB_official = 0, Recruitment = 0)

# reconstructed SSB
s_r_seg_reconstructed <- segreg(Recruitment ~ seg(SSB_7plus, npsi = 1, est = c(1, 0)) + 0, data = S_R_data)
s_r_seg_reconstructed$psi
fitted_results_reconstructed <- data.frame(SSB_7plus = S_R_data$SSB_7plus, Recruitment = fitted(s_r_seg_reconstructed))
fitted_results_reconstructed <- fitted_results_reconstructed %>%
  add_row(SSB_7plus = 0, Recruitment = 0)

# # test, only use data after 2000
# # official
# s_r_seg_official <- segreg(Recruitment ~ seg(SSB_official, npsi=1, est=c(1,0))+0,
#                            data = filter(S_R_data, Year >= 2000))
# s_r_seg_official$psi
# fitted_results_official <- data.frame(SSB_official = S_R_data$SSB_official, Recruitment = fitted(s_r_seg_official))
# fitted_results_official <- fitted_results_official %>%
#   add_row(SSB_official = 0, Recruitment = 0)
# # reconstructed
# s_r_seg_reconstructed <- segreg(Recruitment ~ seg(SSB_7plus, npsi=1, est=c(1,0))+0,
#                                 data = filter(S_R_data, Year >= 2000))
# s_r_seg_reconstructed$psi
# fitted_results_reconstructed <- data.frame(SSB_7plus = S_R_data$SSB_7plus, Recruitment = fitted(s_r_seg_reconstructed))
# fitted_results_reconstructed <- fitted_results_reconstructed %>%
#   add_row(SSB_7plus = 0, Recruitment = 0)

# test, only use data after 1981
# # official
# s_r_seg_official <- segreg(Recruitment ~ seg(SSB_official, npsi=1, est=c(1,0))+0,
#                            data = filter(S_R_data, Year >= 1981))
# s_r_seg_official$psi
# fitted_results_official <- data.frame(SSB_official = S_R_data$SSB_official, Recruitment = fitted(s_r_seg_official))
# fitted_results_official <- fitted_results_official %>%
#   add_row(SSB_official = 0, Recruitment = 0)
# # reconstructed
# s_r_seg_reconstructed <- segreg(Recruitment ~ seg(SSB_7plus, npsi=1, est=c(1,0))+0,
#                                 data = filter(S_R_data, Year >= 1981))
# s_r_seg_reconstructed$psi
# fitted_results_reconstructed <- data.frame(SSB_7plus = S_R_data$SSB_7plus, Recruitment = fitted(s_r_seg_reconstructed))
# fitted_results_reconstructed <- fitted_results_reconstructed %>%
#   add_row(SSB_7plus = 0, Recruitment = 0)

# plot
f13b <- ggplot(S_R_data) +
  labs(title = "Stock-recruitment relationships") +
  geom_point(aes(x = SSB_official / 1000000, y = Recruitment / 1000000), colour = brewer.pal(8, "Dark2")[4]) +
  geom_line(aes(x = SSB_official / 1000000, y = Ricker.fitted_SSB_official / 1000000), colour = brewer.pal(8, "Dark2")[4]) +
  geom_point(aes(x = SSB_7plus / 1000000, y = Recruitment / 1000000), colour = brewer.pal(8, "Dark2")[3]) +
  geom_line(aes(x = SSB_7plus / 1000000, y = Ricker.fitted_SSB_7plus / 1000000), colour = brewer.pal(8, "Dark2")[3]) +
  geom_text(aes(x = SSB_7plus / 1000000, y = Recruitment / 1000000, label = str_sub(Year, 3, 4)),
    colour = brewer.pal(8, "Dark2")[3], check_overlap = T, size = 3, hjust = -0.25
  ) +
  geom_line(
    data = fitted_results_official,
    aes(x = SSB_official / 1000000, y = Recruitment / 1000000), colour = brewer.pal(8, "Dark2")[4], linetype = "dashed"
  ) +
  geom_line(
    data = fitted_results_reconstructed,
    aes(x = SSB_7plus / 1000000, y = Recruitment / 1000000), colour = brewer.pal(8, "Dark2")[3], linetype = "dashed"
  ) +
  scale_x_continuous("SSB (million tons)", minor_breaks = NULL) +
  scale_y_continuous("Recruitment (billions)", minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Calibri"),
    axis.title = element_text(, family = "Calibri"),
    plot.title = element_text(face = "bold", family = "Calibri", size = 11),
    plot.subtitle = element_text(face = 4, family = "Calibri"),
    legend.position = c(0.5, 0.8),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", family = "Calibri")
  )

# 5 Figure 13c -------------------------------------------------------------

f13c <- ggplot(S_R_data) +
  labs(title = "Recruitment success") +
  geom_line(aes(x = Year, y = Recruitment / SSB_official), colour = brewer.pal(8, "Dark2")[4]) +
  geom_line(aes(x = Year, y = Recruitment / SSB_7plus), colour = brewer.pal(8, "Dark2")[3]) +
  scale_x_continuous("Year", breaks = seq(1950, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("Recruitment success (billions/million ton)", minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Calibri"),
    axis.title = element_text(family = "Calibri"),
    plot.title = element_text(face = "bold", family = "Calibri", size = 11),
    plot.subtitle = element_text(face = 4, family = "Calibri"),
    legend.position = c(0.5, 0.8),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", family = "Calibri")
  )

# 5 Combine figures ---------------------------------------------------------
figure <- ggarrange(f13a, f13b, f13c,
  nrow = 3, align = "v",
  labels = c("(a)", "(b)", "(c)"), font.label = list(family = "Calibri")
)

ggsave("Figure 13.pdf", device = cairo_pdf, width = 4, height = 8)
