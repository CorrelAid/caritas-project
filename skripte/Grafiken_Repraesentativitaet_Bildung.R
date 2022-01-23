### Vergleichsdaten zur Repräsentativität ###

##### Bildung #####
# Eurostat data zu Bildungsabschlüssen in Deutschland
# mehrere breaks, 2020 provisional 

# Rename & Verkleinern

data_haushalt2 <- read.csv2("data/raw/edat_lfse_03_1_Data.csv",na="NA", sep=",", dec=",")

df = subset(data_haushalt2, select= -c(GEO, SEX, UNIT, Flag.and.Footnotes))

# ISCED11 anpassen
df1 <- df %>% mutate(Bildungsabschluss = fct_recode(ISCED11, 
                                                    "Bis Realschulabschluss (ISCED 0 - 2)" = "Less than primary, primary and lower secondary education (levels 0-2)",
                                                    "Abitur, Ausbildung, Berufsschule (ISCED 3 - 4)" = 	
                                                      "Upper secondary and post-secondary non-tertiary education (levels 3 and 4)", 
                                                    "Meisterausbildung, Hochschulabschluss (ISCED 5 - 8)" = 
                                                      "Tertiary education (levels 5-8)"))


# Graphen erstellen
ggplot(df1, aes(x = TIME, y = Value, group = Bildungsabschluss, color = Bildungsabschluss)) +
  geom_line(size = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  scale_color_discrete(limits= c("bis Realschulabschluss (ISCED 0 - 2)", "Abitur, Ausbildung, Berufsschule (ISCED 3 - 4)", 
                                 "Meisterausbildung, Hochschulabschluss (ISCED 5 - 8)")) +
  geom_point(color = "black", size = 1.5) +
  labs(
    title = "Bildungsabschlüsse in Deutschland",
    subtitle = "2010-2020",
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.direction = "vertical"
  )




