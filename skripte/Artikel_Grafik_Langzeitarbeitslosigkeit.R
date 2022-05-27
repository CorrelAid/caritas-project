### Grafik 1: Langzeitarbeitslosigkeit ###
### Artikel für "Neue Caritas" ###

if(!require(gridExtra)){
  install.packages("gridExtra")}
library(gridExtra)
library(tidyverse)
library(ggrepel)

raw_2021 <- read.csv2("A:/Rohdaten/Rohdaten_2021.csv", encoding = "UTF-8")


# Daten
algII <- raw_2021 %>% 
  select(Geschlecht, Familiensituation, Bildungsabschluss, ALGII = Länge.Bezug.von.ALGII..Kinderz..) %>%
  filter (ALGII != '', Familiensituation != '', Bildungsabschluss != '',  Geschlecht != '', Geschlecht != "Divers") %>%
  mutate(Langzeitarbeitslos = case_when(ALGII %in% c("12-24 Monate", "25-36 Monate", "mehr als 36 Monate") ~ "mehr als 12 Monate",
                                        TRUE ~ ALGII),
         Abschluss = case_when(Bildungsabschluss %in% c("abgeschlossene  Berufsausbildung", "Schulabschluss", "Studienabschluss") ~ "mit Abschluss",
                               TRUE ~ Bildungsabschluss),
         Alleinerziehend = case_when(Familiensituation %in% c("Ehe/Partnerschaft/Familie", "Alleinstehend (Einpersonenhaushalt)") ~ "Alleinstehend oder in Partnerschaft",
                                     Familiensituation == "Alleinerziehend (Ein-Eltern-Familie)" ~ "Alleinerziehend",
                                     TRUE ~ NA_character_),
         Geschlecht = case_when(Geschlecht == "Weiblich" ~ "Frauen",
                                Geschlecht == "Männlich" ~ "Männer",
                                TRUE ~ NA_character_))


# Alleinerziehend
alleinerziehend <- ggplot(algII, aes(x = Langzeitarbeitslos, fill = Alleinerziehend)) + 
  geom_bar(position = "fill", width = 0.7) +
  labs(title = "",
       fill = "") +
  scale_y_continuous(breaks = seq(0,1,0.25), labels = scales::percent) +
  scale_fill_manual(values = c("#E6AB02", "#66A61E")) +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ Geschlecht, 
             labeller = label_wrap_gen(width = 20)) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = rel(0.8), color = "black"),
        axis.text.y = element_text(size = rel(0.8), color = "black"),
        strip.text.x = element_text(size = rel(1), face = "bold"),
        panel.spacing.x=unit(1.2, "lines"),
        panel.spacing.y=unit(0.5, "lines")) 


# Abschluss
bildung <- ggplot(algII, aes(x = Langzeitarbeitslos, fill = Abschluss)) + 
  geom_bar(position = "fill", width = 0.7) +
  labs(title = "",
       fill = "") +
  scale_y_continuous(breaks = seq(0,1,0.25), labels = scales::percent) +
  scale_fill_manual(values = c("#40B0A6", "#E1BE6A")) +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ Geschlecht, 
             labeller = label_wrap_gen(width = 20)) +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(size = rel(0.8), color = "black"),
        axis.text.y = element_text(size = rel(0.8), color = "black"),
        strip.text.x = element_text(size = rel(1), face = "bold"),
        panel.spacing.x=unit(1.2, "lines"),
        panel.spacing.y=unit(0.5, "lines")) 


# Kombinierte Grafik
plot_langzeitarbeitslos <- grid.arrange(alleinerziehend, bildung, nrow = 2)
plot_langzeitarbeitslos


