library(tidyverse)
library(dplyr)
source("C:/Users/Caritas_Data_Cleaning_2010_to_2021.R")

# Recoding

problems2_2010 <- problems2_2010 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2011 <- problems2_2011 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2012 <- problems2_2012 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2013 <- problems2_2013 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2014 <- problems2_2014 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2015 <- problems2_2015 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2016 <- problems2_2016 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2017 <- problems2_2017 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2018 <- problems2_2018 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    TRUE ~ Problemlage),
  ) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2019 <- problems2_2019 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2020 <- problems2_2020 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    Problemlage %in% "Krankheit" ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems2_2021 <- problems2_2021 %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    Problemlage %in% "Krankheit" ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

# Zeitreihe

ts_problems <- list(problems2_2010, problems2_2011, problems2_2012, problems2_2013,
                    problems2_2014, problems2_2015, problems2_2016, problems2_2017,
                    problems2_2018, problems2_2019, problems2_2020, problems2_2021) %>%
  reduce(left_join, by = "Problemlage")

ts_problems <- ts_problems %>% select(Problemlage, contains("Prozent"))
colnames(ts_problems) <- c("Problemlage", "2010", "2011", "2012", "2013", "2014", "2015", "2016",
                           "2017", "2018", "2019", "2020", "2021")

ts_problems2 <- ts_problems %>% pivot_longer(!Problemlage,
                                     names_to = "Jahr", 
                                     values_to = "Prozent") %>%
  mutate(Problemlage = factor(Problemlage,
                              levels = c("Alter/Pflegebedürftigkeit", "Behinderung", 
                                         "Krankheit (auch psychische Probleme)", "Schwangerschaft", 
                                         "Arbeitslosigkeit", "Sanktionen nach SGB II", "sozialrechtliche Probleme", 
                                         "Umgang mit Behörde", "Schulden", "Energie- oder Mietschulden", 
                                         "Sonstige finanzielle Schwierigkeiten", "Probleme im Bereich Wohnen", 
                                         "Partner/Erziehung/familiäre Probleme", "Sonstiges")))

prob_selection <- ts_problems2 %>%
  group_by(Problemlage) %>%
  summarize(max_p = max(Prozent, na.rm = TRUE)) %>%
  filter(max_p >= 10)

ts_problems3 <- ts_problems2 %>%
  semi_join(prob_selection, by="Problemlage")


plot_problems <- ts_problems3 %>%
  filter(Problemlage != "Sonstiges") %>%
  ggplot(aes(x = Jahr, y = Prozent, group = Problemlage, color = Problemlage)) +
  geom_line(size = 1.1) + 
  theme_linedraw() +
  labs(
    title = "Problemlagen",
    subtitle = "2010-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )
#ggsave(plot_problems, file = "plot_problems.jpeg", unit = "cm", height = 12, width = 21, dpi = 500)
