# Ma, Shuyang
# May 2025
# Figure 9

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(strucchange)
library(splines)
library(patchwork)
library(mgcv)

# Something about figures
# Font
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Calibri"))
# Theme
theme_set(theme_classic())

# read data
data <- read_excel("Data/Length_of_spawning-survey_NEAC_A50.xlsx", sheet = "Data") %>%
  select(1, 2, 3, 5, 6, 7) %>%
  rename(A50 = `A50 (year)`)

# linear regression information
mod <- lm(A50 ~ `Survey_length_days<20_days`, data = data)
summary(mod)

# gam
mod_gam <- gam(A50 ~ s(Survey_length_days), data = data)
summary(mod_gam)
plot(mod_gam, all.terms = T)

# #test
# mod_gam <- gam(A50 ~ s(Calendar_day_start) + s(Survey_length_days), data=data)
# summary(mod_gam)
# plot(mod_gam, all.terms = T)

# corrected A50
data <- data %>%
  mutate(
    effects_survey_duration = predict.gam(mod_gam, data = data, se.fit = T)$fit - 6.81133, # effects of survey duration
    effects_survey_duration_se = predict.gam(mod_gam, data = data, se.fit = T)$se.fit
  )

data <- data %>%
  mutate(effects_survey_duration = effects_survey_duration - min(effects_survey_duration)) # the longer the survey, the more accurate the data

mean(filter(data, Year >= 2007)$effects_survey_duration)


data <- data %>%
  mutate(
    corrected_A50_mean = A50 - effects_survey_duration,
    corrected_A50_low = A50 - effects_survey_duration - 2 * effects_survey_duration_se,
    corrected_A50_up = A50 - effects_survey_duration + 2 * effects_survey_duration_se
  )

# plot
f9a <- ggplot(data) +
  geom_smooth(aes(x = Survey_length_days, y = A50), color = "blue", fill = "blue", alpha = 0.2, method = "gam") +
  geom_text(aes(x = Survey_length_days, y = A50, label = str_sub(Year, 3, 4)), color = "blue", family = "Calibri", check_overlap = F) +
  geom_text(aes(x = Survey_length_days, y = A50, label = str_sub(Year, 3, 4)), color = "blue", family = "Calibri") +
  labs(title = "A50 vs. spawning survey duration NEAC (1992-2021)") +
  scale_x_continuous("Survey duration (days)", limits = c(8, 30), expand = c(0, 0), breaks = seq(10, 30, 5), minor_breaks = NULL) +
  scale_y_continuous("Combined-sexes A50 (year)", limits = c(6, 8), expand = c(0, 0), breaks = seq(6.0, 8.0, 0.2), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Calibri"),
    axis.title = element_text(family = "Calibri"),
    panel.grid = element_blank()
  )

f9b <- ggplot(data) +
  geom_line(aes(x = Year, y = A50)) +
  geom_ribbon(aes(x = Year, ymin = corrected_A50_low, ymax = corrected_A50_up), fill = "red", alpha = 0.25) +
  geom_line(aes(x = Year, y = corrected_A50_mean), color = "red") +
  geom_vline(xintercept = 2007, linetype = "dashed") +
  geom_smooth(
    data = filter(data, Year >= 2007), aes(x = Year, y = A50),
    method = "lm", color = "black", se = F
  ) +
  geom_smooth(
    data = filter(data, Year >= 2007), aes(x = Year, y = corrected_A50_mean),
    method = "lm", color = "red", se = F
  ) +
  labs(title = "Raw and corrected A50") +
  scale_x_continuous("Year", expand = c(0, 0), breaks = seq(1990, 2020, 5), minor_breaks = NULL) +
  scale_y_continuous("Combined-sexes A50 (year)", expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Calibri"),
    axis.title = element_text(family = "Calibri"),
    panel.grid = element_blank()
  )


f9a + annotation_custom(
  grob = ggplotGrob(f9b),
  ymin = 6.8, ymax = 8.0, xmin = 17, xmax = 30
)

ggsave("Figures/Figure 9.pdf", device = cairo_pdf, width = 6, height = 4)


# trend after 2007
summary(lm(A50 ~ Year, data = filter(data, Year >= 2007)))
summary(lm(corrected_A50_mean ~ Year, data = filter(data, Year >= 2007)))

# data <- data %>%
#   mutate(diff = A50-corrected_A50_mean)
#
# data <- data %>%
#   filter(Year >= 2007)
#
# mean(data$diff)
