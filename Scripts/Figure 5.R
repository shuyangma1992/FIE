# Ma, Shuyang
# December 2024
# Figure 5

library(tidyverse)
library(strucchange)
library(ggsci)
library(ggpubr)
library(scales)
library(readxl)

# Something about figures
# Font
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Calibri"), C = windowsFont("Arial"))
# Theme
theme_set(theme_classic())
# Color
mypal <- pal_npg()(9)
show_col(mypal)

# 1 Regime shift detection --------------------------------------------------
# read data
# A50
A50 <- read_excel("Data/A50_NEAC_NSC_NC.xlsx", sheet = "Data") %>%
  select(1, 4) %>%
  drop_na() %>%
  rename(A50 = 2) %>%
  mutate(A50 = as.numeric(A50))

# SSB and F
SSB_F <- read_excel("Data/SSB_F_NSC.xlsx") %>%
  select(1, 5, 10)

# NAO
NAO <- read_excel("Data/NAO_winter_12_3.xlsx")

# weight at age
weight_at_age <- read_excel("Data/Weight_at_age_NSC.xlsx") %>%
  rename(Year = 1) %>%
  select(1, 3, 4, 5)

# TSB
TSB <- read_excel("Data/SSB_F_NSC.xlsx") %>%
  select(1, 13)

# combine data
data <- left_join(A50, SSB_F) %>%
  left_join(NAO) %>%
  left_join(weight_at_age) %>%
  left_join(TSB)

# regime shift detection
data_regime <- NULL
regime_shift_year <- NULL
for (i in 1:8) {
  data_cycle <- data %>%
    select(Year, i + 1) %>%
    drop_na() %>%
    rename(variable = 2)

  # breakpoints analysis
  abre <- breakpoints(ts(data_cycle$variable, start = data_cycle$Year[1]) ~ 1, h = 0.20) # breakpoints analysis
  summary(abre) # breakpoints analysis results
  b <- breakpoints(abre) # breakpoints
  ci_b <- confint(abre, breaks = length(b$breakpoints))

  # figure data lines
  fm <- lm(ts(data_cycle$variable, start = data_cycle$Year[1]) ~ breakfactor(abre, breaks = length(abre$breakpoints))) # fit the model
  data_cycle$regime_mean <- fitted(fm) # get regime means
  data_cycle$name <- colnames(data)[i + 1]
  data_regime <- bind_rows(data_regime, data_cycle)

  # figure data regime shift years
  confidence_interval <- as.data.frame(ci_b$confint + ci_b$datatsp[1] - 1) # get 95% confidence intervals of breakpoints
  colnames(confidence_interval) <- c("low", "mean", "up") # rename them
  confidence_interval$name <- colnames(data)[i + 1]
  regime_shift_year <- bind_rows(regime_shift_year, confidence_interval)
}

# 2 Figure 5a ---------------------------------------------------------------
# year data
data_regime_A50 <- filter(data_regime, name %in% colnames(data)[2])
regime_shift_year_A50 <- filter(regime_shift_year, name %in% colnames(data)[2])

f5a <- ggplot(data_regime_A50) +
  # geom_rect(data=regime_shift_year_A50,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect", xmin = -Inf, xmax = 1991.5, ymin = -Inf, ymax = Inf, fill = alpha(mypal[6], 0.1)) +
  annotate("rect", xmin = 1991.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = alpha(mypal[8], 0.1)) +
  geom_vline(data = regime_shift_year_A50, aes(xintercept = low), linetype = "dotted") +
  geom_vline(data = regime_shift_year_A50, aes(xintercept = up), linetype = "dotted") +
  geom_line(aes(x = Year, y = variable), color = mypal[1], linetype = "solid") +
  geom_line(aes(x = Year, y = regime_mean), color = "black", linetype = "dashed") +
  geom_text(data = regime_shift_year_A50, aes(x = mean, y = -Inf, label = mean), vjust = -0.5, family = "Arial", fontface = 1) +
  # labs(title="a) Both-sexes A50")+
  annotate("text", x = 1977, y = Inf, hjust = 0, vjust = 1.5, label = "Combined-sexes A50", family = "Arial", fontface = "bold", size = 4) +
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1975, 2025), breaks = seq(1930, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("A50 (year)", expand = c(0, 0), limits = c(2, 5), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial")
  )


# 4 Figure 5b ---------------------------------------------------------------
data_regime_SSB <- filter(data_regime, name %in% colnames(data)[3])
regime_shift_year_SSB <- filter(regime_shift_year, name %in% colnames(data)[3])

f5b <- ggplot(data_regime_SSB) +
  # geom_rect(data=regime_shift_year_SSB,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect", xmin = -Inf, xmax = 1991.5, ymin = -Inf, ymax = Inf, fill = alpha(mypal[6], 0.1)) +
  annotate("rect", xmin = 1991.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = alpha(mypal[8], 0.1)) +
  geom_vline(data = regime_shift_year_SSB, aes(xintercept = low), linetype = "dotted") +
  geom_vline(data = regime_shift_year_SSB, aes(xintercept = up), linetype = "dotted") +
  geom_line(aes(x = Year, y = variable / 1000000), color = mypal[3], linetype = "solid") +
  geom_line(aes(x = Year, y = regime_mean / 1000000), color = "black", linetype = "dashed") +
  geom_text(data = regime_shift_year_SSB, aes(x = mean, y = -Inf, label = mean), vjust = -0.5, family = "Arial", fontface = 1) +
  # labs(title="c) SSB")+
  annotate("text", x = 1977, y = Inf, hjust = 0, vjust = 1.5, label = "SSB", family = "Arial", fontface = "bold", size = 4) +
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1975, 2025), breaks = seq(1930, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("SSB (million ton)", expand = c(0, 0), breaks = seq(0.04, 0.12, 0.02), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial")
  )

# 5 Figure 5c ---------------------------------------------------------------
data_regime_F <- filter(data_regime, name %in% colnames(data)[4])
regime_shift_year_F <- filter(regime_shift_year, name %in% colnames(data)[4])

f5c <- ggplot(data_regime_F) +
  # geom_rect(data=regime_shift_year_F,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect", xmin = -Inf, xmax = 1991.5, ymin = -Inf, ymax = Inf, fill = alpha(mypal[6], 0.1)) +
  annotate("rect", xmin = 1991.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = alpha(mypal[8], 0.1)) +
  geom_vline(data = regime_shift_year_F, aes(xintercept = low), linetype = "dotted") +
  geom_vline(data = regime_shift_year_F, aes(xintercept = up), linetype = "dotted") +
  geom_line(aes(x = Year, y = variable), color = mypal[4], linetype = "solid") +
  geom_line(aes(x = Year, y = regime_mean), color = "black", linetype = "dashed") +
  geom_text(data = regime_shift_year_F, aes(x = mean, y = -Inf, label = mean), vjust = -0.5, family = "Arial", fontface = 1) +
  annotate("text", x = 1977, y = Inf, hjust = 0, vjust = 1.5, label = "Fishing mortality, ages 2-4 years", family = "Arial", fontface = "bold", size = 4) +
  # labs(title="d) F")+
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1975, 2025), breaks = seq(1930, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("Mortality", expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial")
  )

# 5 Figure 5d ---------------------------------------------------------------
data_regime_NAO <- filter(data_regime, name %in% colnames(data)[5])
regime_shift_year_NAO <- filter(regime_shift_year, name %in% colnames(data)[5])

f5d <- ggplot(data_regime_NAO) +
  # geom_rect(data=regime_shift_year_T,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect", xmin = -Inf, xmax = 1991.5, ymin = -Inf, ymax = Inf, fill = alpha(mypal[6], 0.1)) +
  annotate("rect", xmin = 1991.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = alpha(mypal[8], 0.1)) +
  geom_vline(data = regime_shift_year_NAO, aes(xintercept = low), linetype = "dotted") +
  geom_vline(data = regime_shift_year_NAO, aes(xintercept = up), linetype = "dotted") +
  geom_line(aes(x = Year, y = variable), color = mypal[5], linetype = "solid") +
  geom_line(aes(x = Year, y = regime_mean), color = "black", linetype = "dashed") +
  geom_text(data = regime_shift_year_NAO, aes(x = mean, y = -Inf, label = mean), vjust = -0.5, family = "Arial", fontface = 1) +
  annotate("text", x = 1977, y = Inf, hjust = 0, vjust = 1.5, label = "North Atlantic Osicllation", family = "Arial", fontface = "bold", size = 4) +
  # labs(title="e) T")+
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1975, 2025), breaks = seq(1930, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("NAO", expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial")
  )


# 6 Figure 5e ----------------------------------------------------------------
data_regime_weight_at_age <- filter(data_regime, name %in% colnames(data)[6:8])
regime_shift_year_weight_at_age <- filter(regime_shift_year, name %in% colnames(data)[6:8])

f5e <- ggplot(data_regime_weight_at_age) +
  # geom_rect(data=regime_shift_year_T,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect", xmin = -Inf, xmax = 1991.5, ymin = -Inf, ymax = Inf, fill = alpha(mypal[6], 0.1)) +
  annotate("rect", xmin = 1991.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = alpha(mypal[8], 0.1)) +
  # geom_vline(data=regime_shift_year_weight_at_age,aes(xintercept = low),linetype="dotted")+
  # geom_vline(data=regime_shift_year_weight_at_age,aes(xintercept = up),linetype="dotted")+
  geom_line(aes(x = Year, y = variable, group = name), color = mypal[9], linetype = "solid") +
  geom_line(aes(x = Year, y = regime_mean, group = name), color = "black", linetype = "dashed") +
  geom_text(data = regime_shift_year_weight_at_age, aes(x = mean, y = -Inf, label = mean), vjust = -0.5, family = "Arial", fontface = 1) +
  annotate("text", x = 1977, y = Inf, hjust = 0, vjust = 1.5, label = "Weight-at-age", family = "Arial", fontface = "bold", size = 4) +
  # labs(title="e) T")+
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1975, 2025), breaks = seq(1930, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("Weight (kg)", expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial")
  )

# 7 Figure 5f---------------------------------------------------------------
data_regime_TSB <- filter(data_regime, name %in% colnames(data)[9])
regime_shift_year_TSB <- filter(regime_shift_year, name %in% colnames(data)[9])

f5f <- ggplot(data_regime_TSB) +
  # geom_rect(data=regime_shift_year_T,aes(xmin=low,xmax=up),ymin=-Inf,ymax=Inf,fill=alpha("black",0.1))+
  annotate("rect", xmin = -Inf, xmax = 1991.5, ymin = -Inf, ymax = Inf, fill = alpha(mypal[6], 0.1)) +
  annotate("rect", xmin = 1991.5, xmax = Inf, ymin = -Inf, ymax = Inf, fill = alpha(mypal[8], 0.1)) +
  geom_vline(data = regime_shift_year_TSB, aes(xintercept = low), linetype = "dotted") +
  geom_vline(data = regime_shift_year_TSB, aes(xintercept = up), linetype = "dotted") +
  geom_line(aes(x = Year, y = variable / 1000000, group = name), color = mypal[7], linetype = "solid") +
  geom_line(aes(x = Year, y = regime_mean / 1000000, group = name), color = "black", linetype = "dashed") +
  geom_text(data = regime_shift_year_TSB, aes(x = mean, y = -Inf, label = mean), vjust = -0.5, family = "Arial", fontface = 1) +
  annotate("text", x = 1977, y = Inf, hjust = 0, vjust = 1.5, label = "TSB, 1 years+", family = "Arial", fontface = "bold", size = 4) +
  # labs(title="e) T")+
  scale_x_continuous("Year", expand = c(0, 0), limits = c(1975, 2025), breaks = seq(1930, 2020, 10), minor_breaks = NULL) +
  scale_y_continuous("TSB (million tons)", expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(family = "Arial"),
    axis.title = element_text(family = "Arial"),
    plot.title = element_text(family = "Arial")
  )

# 8 Combine figures ---------------------------------------------------------
figure <- ggarrange(f5a, f5b, f5f, f5c, f5d, f5e,
  nrow = 6, align = "v",
  labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), font.label = list(family = "Arial")
)
annotate_figure(figure, top = text_grob("Single-year analysis — NSC (1978-2021)", family = "Arial", face = "bold"))
ggsave("Figures/Figure 5.pdf", device = cairo_pdf, width = 4, height = 12)
