# Ma, Shuyang
# May 2025
# Figure 6

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(ggridges)
library(viridis)
library(mgcv)

# Something about figures
# Font
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Calibri"))
# Theme
theme_set(theme_classic())

# 1 H calculation NEAC ----------------------------------------------------
# read data
abundance <- read_xlsx("Data/Age_structure_NEAC.xlsx", sheet = "Number-at-age") %>%
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
H_data <- abundance %>%
  select(-c(Year, `3`, `4`)) # spawning stock age 5+

H <- data.frame(
  Year = abundance$Year,
  H_abu = apply(H_data, 1, Shannon)
)


# 2 Figure 6a  -----------------------------------------------
# read data
A50 <- read_excel("Data/A50_L50_incl_cohort_NEAC.xlsx", sheet = "Data") %>%
  select(1, 2) %>%
  drop_na() %>%
  rename(A50 = 2) %>%
  mutate(A50 = as.numeric(A50)) %>%
  mutate(period = ifelse(Year <= 1981, "Before 1981", "After 1981"))

# combine H and A50
data <- left_join(H, A50)

# linear regression information
mod1 <- lm(data = filter(data, Year <= 1981), A50 ~ H_abu)
summary(mod1)
# AIC(mod1)
# mod1_nl <- gam(data=filter(data,Year<=1981),A50~s(H_abu, k = 4))
# summary(mod1_nl)
# AIC(mod1_nl)
# plot(mod1_nl)


mod2 <- lm(data = filter(data, Year > 1981), A50 ~ H_abu)
summary(mod2)
# AIC(mod2)
# mod2_nl <- gam(data=filter(data,Year>1981),A50~s(H_abu, k = 4))
# summary(mod2_nl)
# AIC(mod2_nl)
# plot(mod2_nl)

#
# #test
# A50_correction <- A50 %>%
#   rowwise() %>%
#   mutate(A50 = ifelse(Year <= 1958, A50-1, A50))
# data <- left_join(H,A50_correction)
#
# mod3 <- lm(data=filter(data,Year<=1981),A50~H_abu)
# summary(mod3)

f6a <- ggplot(data) +
  geom_text(aes(x = H_abu, y = A50, label = str_sub(Year, 3, 4), color = period), family = "Calibri", show.legend = F) +
  scale_color_manual(values = c("Before 1981" = "red", "After 1981" = "blue")) +
  geom_smooth(data = filter(data, Year <= 1981), aes(x = H_abu, y = A50), method = "lm", color = "red", fill = "red", alpha = 0.1) +
  geom_smooth(data = filter(data, Year > 1981), aes(x = H_abu, y = A50), method = "lm", color = "blue", fill = "blue", alpha = 0.1) +
  labs(title = "a) H of spawning stock (5 year+) vs. A50 — NEAC") +
  scale_x_continuous("H of spawning stock (5 year+)", limits = c(0.5, 2.0), expand = c(0, 0), minor_breaks = NULL) +
  scale_y_continuous("Combined-sexes A50 (year)", limits = c(6, 11), expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Calibri"),
    axis.title = element_text(family = "Calibri"),
    plot.title = element_text(face = "bold", family = "Calibri"),
    panel.grid = element_blank()
  )

# 3 H calculation NSC -----------------------------------------------------
# read data
abundance <- read_xlsx("Data/Age_structure_NSC.xlsx", sheet = "Number-at-age") %>%
  rename(Year = 1)

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
H_data <- abundance %>%
  select(-c(Year, `1`))

H <- data.frame(
  Year = abundance$Year,
  H_abu = apply(H_data, 1, Shannon)
)


# 4 Figure 6b -----------------------------------------------
# read data
A50 <- read_excel("Data/A50_NEAC_NSC_NC.xlsx", sheet = "Data") %>%
  select(1, 4) %>%
  drop_na() %>%
  rename(A50 = 2) %>%
  mutate(A50 = as.numeric(A50)) %>%
  mutate(period = ifelse(Year <= 1991, "Before 1991", "After 1991"))

# combine data
data <- left_join(H, A50) %>%
  drop_na()

# linear regression information
mod1 <- lm(data = filter(data, Year <= 1991), A50 ~ H_abu)
summary(mod1)
# AIC(mod1)
# mod1_nl <- gam(data=filter(data,Year<=1991),A50~s(H_abu, k = 4))
# summary(mod1_nl)
# AIC(mod1_nl)

mod2 <- lm(data = filter(data, Year > 1991), A50 ~ H_abu)
summary(mod2)
# AIC(mod2)
# mod2_nl <- gam(data=filter(data,Year>1991),A50~s(H_abu, k = 4))
# summary(mod2_nl)
# AIC(mod2_nl)
# plot(mod2_nl)

f6b <- ggplot(data) +
  geom_text(aes(x = H_abu, y = A50, label = str_sub(Year, 3, 4), color = period), family = "Calibri", show.legend = F) +
  scale_color_manual(values = c("Before 1991" = "red", "After 1991" = "blue")) +
  geom_smooth(data = filter(data, Year <= 1991), aes(x = H_abu, y = A50), method = "lm", color = "red", fill = "red", alpha = 0.1) +
  geom_smooth(data = filter(data, Year > 1991), aes(x = H_abu, y = A50), method = "lm", color = "blue", fill = "blue", alpha = 0.1) +
  labs(title = "b) H of spawning stock (2 year+) vs. A50 — NSC") +
  scale_x_continuous("H of spawning stock (2 year+)", limits = c(0.3, 1.4), expand = c(0, 0), minor_breaks = NULL) +
  scale_y_continuous("Combined-sexes A50 (year)", limits = c(2, 5), expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Calibri"),
    axis.title = element_text(family = "Calibri"),
    plot.title = element_text(face = "bold", family = "Calibri"),
    panel.grid = element_blank()
  )


# 5 Combine figures -------------------------------------------------------
figure <- ggarrange(f6a, f6b, nrow = 2)
ggsave("Figures/Figure 6.pdf", device = cairo_pdf, width = 5, height = 10)
