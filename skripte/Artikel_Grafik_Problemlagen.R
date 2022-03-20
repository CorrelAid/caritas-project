### Grafik 2: Problemlagen ###
### Artikel für "Neue Caritas" ###

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

ts_problems_fin <- ts_problems2 %>%
  filter(Problemlage %in% c("sozialrechtliche Probleme", 
                            "Umgang mit Behörde", 
                            "Schulden",
                            "Energie- oder Mietschulden",
                            "Sonstige finanzielle Schwierigkeiten",
                            "Probleme im Bereich Wohnen"))

# Colors of colorblind-friendly RColorBrewer palette "Dark2"
cbbPalette <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#E6AB02", "#66A61E")

plot_problems <- 
ts_problems_fin %>%
  ggplot(aes(x = Jahr, y = Prozent, group = Problemlage, color = Problemlage)) +
  geom_line(size = 1.1) + 
  scale_x_discrete(breaks = seq(2010, 2021, 2), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,55), breaks = seq(0, 55, 10)) +
  labs(title = "Entwicklung der Problemlagen",
       subtitle = "von Ratsuchenden der ASB, 2010\u20132021",
       color = "",
       x = "",
       y = "Prozent") +
  theme_minimal() +
  scale_colour_manual(values=cbbPalette) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = rel(0.8)),
        plot.title = element_text(color = "black", face = "bold"),
        plot.subtitle = element_text(size = rel(0.85)),
        axis.text = element_text(color = "black"),
        axis.title.y = element_text(size = rel(0.9), face = "bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

plot_problems