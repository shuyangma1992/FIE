# Ma, Shuyang
# May 2025
# Figure S2

library(tidyverse)
library(readxl)
library(RColorBrewer)
library(ggpubr)
library(ggh4x)
library(psych)

# Something about figures
# Font
windowsFonts(A = windowsFont("Times New Roman"), B = windowsFont("Calibri"))
# Theme
theme_set(theme_bw())

# 1 Data ------------------------------------------------------------------
abundance <- read_xlsx("Data/Age_structure_NEAC.xlsx", sheet = "Number-at-age") %>%
  rename(Year = 1) %>%
  filter(!Year == 2021) %>%
  select(-c(`3`, `4`))

biomass <- read_xlsx("Data/Age_structure_NEAC.xlsx", sheet = "Biomass-at-age") %>%
  rename(Year = 1) %>%
  filter(!Year == 2021) %>%
  select(-c(`3`, `4`))

# 2 Figure s2a ------------------------------------------------------------
abundance_figure <- abundance %>%
  pivot_longer(-Year, names_to = "age", values_to = "abundance") %>%
  mutate(age = factor(age, levels = rev(unique(age))))

fs2a <- ggplot(abundance_figure) +
  geom_bar(aes(x = Year, y = abundance / 1000000, fill = age), stat = "identity", position = "fill", color = "black", linewidth = 0.01, width = 1) +
  scale_fill_manual(values = rev(c(brewer.pal(3, "Reds"), brewer.pal(3, "Greens"), brewer.pal(3, "Blues"), brewer.pal(3, "Purples")[c(1, 3)]))) +
  labs(title = "(a) Age structure: abundance", fill = "Age\nclass") +
  scale_x_continuous("Year", expand = c(0, 0), breaks = seq(1950, 2020, 10), minor_breaks = seq(1946, 2020, 1), guide = "axis_minor") +
  scale_y_continuous("Abundance (proportion)", expand = c(0, 0)) +
  theme(
    axis.text = element_text(face = "bold", family = "Calibri"),
    axis.title = element_text(face = "bold", family = "Calibri"),
    axis.ticks = element_line(linewidth = 0.1),
    plot.title = element_text(face = "bold", family = "Calibri", size = 11),
    plot.subtitle = element_text(face = 4, family = "Calibri"),
    legend.title = element_text(face = "bold", family = "Calibri"),
    legend.text = element_text(face = "bold", family = "Calibri"),
    ggh4x.axis.ticks.length.minor = rel(0.5)
  )


# 3 Figure s2b ------------------------------------------------------------
biomass_figure <- biomass %>%
  pivot_longer(-Year, names_to = "age", values_to = "biomass") %>%
  mutate(age = factor(age, levels = rev(unique(age))))

fs2b <- ggplot(biomass_figure) +
  geom_bar(aes(x = Year, y = biomass / 1000000, fill = age), stat = "identity", position = "fill", color = "black", linewidth = 0.01, width = 1) +
  scale_fill_manual(values = rev(c(brewer.pal(3, "Reds"), brewer.pal(3, "Greens"), brewer.pal(3, "Blues"), brewer.pal(3, "Purples")[c(1, 3)]))) +
  labs(title = "(b) Age structure: biomass", fill = "Age\nclass") +
  scale_x_continuous("Year", expand = c(0, 0), breaks = seq(1950, 2020, 10), minor_breaks = seq(1946, 2020, 1), guide = "axis_minor") +
  scale_y_continuous("Biomass (proportion)", expand = c(0, 0), minor_breaks = NULL) +
  theme(
    axis.text = element_text(face = "bold", family = "Calibri"),
    axis.title = element_text(face = "bold", family = "Calibri"),
    axis.ticks = element_line(linewidth = 0.1),
    plot.title = element_text(face = "bold", family = "Calibri", size = 11),
    plot.subtitle = element_text(face = 4, family = "Calibri"),
    legend.title = element_text(face = "bold", family = "Calibri"),
    legend.text = element_text(face = "bold", family = "Calibri"),
    ggh4x.axis.ticks.length.minor = rel(0.5)
  )


# 4 Combine figures -------------------------------------------------------
figure <- ggarrange(fs2a, fs2b, nrow = 2, common.legend = T, legend = "right")
annotate_figure(figure, top = text_grob("Spawning stock age structure — NEAC (1946-2020)", family = "Calibri", face = "bold"))
ggsave("Figures/Figure S2.pdf", device = cairo_pdf, width = 6, height = 6)


# test proportion 5 6 7 and A50
abundance <- abundance %>%
  rowwise() %>%
  mutate(sum = sum(`5`, `6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, gp))

abundance <- abundance %>%
  mutate(
    proportion_5 = `5` / sum,
    proportion_6 = `6` / sum,
    proportion_7 = `7` / sum
  )

abundance <- abundance %>%
  select(Year, proportion_5, proportion_6, proportion_7)

A50 <- A50 %>%
  left_join(abundance)

A50_1946_1981 <- A50 %>%
  filter(Year %in% c(1946:1981)) %>%
  select(-period)

corr.test(A50_1946_1981)

A50_1982_2020 <- A50 %>%
  filter(Year %in% c(1985:2020)) %>%
  select(-period)

corr.test(A50_1982_2020)
