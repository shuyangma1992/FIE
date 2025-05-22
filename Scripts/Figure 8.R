# Ma, Shuyang
# May 2025
# Figure 8

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(mgcv)
library(ggridges)
library(viridis)

# Something about figures
# Font
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Calibri"))
# Theme
theme_set(theme_classic())

# 1 H of age structure ----------------------------------------------------
# read data
abundance <- read_xlsx("Data/Age_structure_NEAC.xlsx", sheet = "Number-at-age") %>%
  rename(Year = 1) %>%
  filter(!Year == 2021)

biomass <- read_xlsx("Data/Age_structure_NEAC.xlsx", sheet = "Biomass-at-age") %>%
  rename(Year = 1) %>%
  filter(!Year == 2021)

# Shannon diversity index function
Shannon <- function(x) {
  x <- x[!x == 0]
  total <- sum(x)
  s <- NULL
  for (i in 1:length(x)) {
    # i=1
    s_cycle <- -x[i] / total * log(x[i] / total)
    s <- c(s, s_cycle)
  }
  return(sum(s))
}

# calculate H
# abundance
H_abundance_data <- abundance %>%
  select(-c(Year, `3`, `4`))

H_abundance <- data.frame(
  Year = abundance$Year,
  H_abu = apply(H_abundance_data, 1, Shannon)
)

# biomass
H_biomass_data <- biomass %>%
  select(-c(Year, `3`, `4`))

H_biomass <- data.frame(
  Year = biomass$Year,
  H_bio = apply(H_biomass_data, 1, Shannon)
)

# combine data
H_age <- left_join(H_abundance, H_biomass) %>%
  rename(H_age_abundance = 2, H_age_biomass = 3)

# 2 H of catch structure ----------------------------------------------------
# read data
abundance <- read_xlsx("Data/Catch_structure_NEAC.xlsx", sheet = "Number-at-age") %>%
  rename(Year = 1) %>%
  filter(!Year == 2021)

biomass <- read_xlsx("Data/Catch_structure_NEAC.xlsx", sheet = "Biomass-at-age") %>%
  rename(Year = 1) %>%
  filter(!Year == 2021)

# Shannon diversity index function
Shannon <- function(x) {
  x <- x[!x == 0]
  total <- sum(x)
  s <- NULL
  for (i in 1:length(x)) {
    # i=1
    s_cycle <- -x[i] / total * log(x[i] / total)
    s <- c(s, s_cycle)
  }
  return(sum(s))
}

# calculate H
# abundance
H_abundance_data <- abundance %>%
  select(-c(Year, `3`, `4`))

H_abundance <- data.frame(
  Year = abundance$Year,
  H_abu = apply(H_abundance_data, 1, Shannon)
)

# biomass
H_biomass_data <- biomass %>%
  select(-c(Year, `3`, `4`))

H_biomass <- data.frame(
  Year = biomass$Year,
  H_bio = apply(H_biomass_data, 1, Shannon)
)

# combine data
H_catch <- left_join(H_abundance, H_biomass) %>%
  rename(H_catch_abundance = 2, H_catch_biomass = 3)

# 3 Figure 8 ----------------------------------------------------------------
H <- left_join(H_age, H_catch)

f8 <- ggplot(H) +
  geom_line(aes(x = Year, y = H_age_abundance, color = "Spawning stock", linetype = "Abundance")) +
  geom_line(aes(x = Year, y = H_age_biomass, color = "Spawning stock", linetype = "Biomass")) +
  geom_line(aes(x = Year, y = H_catch_abundance, color = "Catch", linetype = "Abundance")) +
  geom_line(aes(x = Year, y = H_catch_biomass, color = "Catch", linetype = "Biomass")) +
  labs(title = "H of spawning stock and commercial catch — NEAC (1946-2020)") +
  scale_color_manual(values = c("Spawning stock" = "red", "Catch" = "blue")) +
  scale_linetype_manual(values = c("Abundance" = "solid", "Biomass" = "dashed")) +
  scale_x_continuous("Year", expand = c(0, 1), breaks = seq(1950, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("H (5 years+)", expand = c(0, 0), breaks = seq(0.5, 2, 0.5), minor_breaks = NULL) +
  theme(
    axis.text = element_text(face = "bold", family = "Calibri"),
    axis.title = element_text(face = "bold", family = "Calibri"),
    plot.title = element_text(face = "bold", family = "Calibri", size = 11),
    plot.subtitle = element_text(face = 4, family = "Calibri"),
    legend.position = c(0.7, 0.9),
    legend.direction = "vertical",
    legend.box = "horizontal",
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", family = "Calibri")
  )

ggsave("Figures/Figure 8.pdf", device = cairo_pdf, width = 6, height = 4)


# 4 Difference trend ------------------------------------------------------
H_diff <- H %>%
  mutate(
    H_abundance_diff = H_catch_abundance - H_age_abundance,
    H_biomass_diff = H_catch_biomass - H_age_biomass
  )

summary(lm(H_abundance_diff ~ Year, data = H_diff))
summary(lm(H_biomass_diff ~ Year, data = H_diff))
