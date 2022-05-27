### Vergleichsdaten zur Repräsentativität ###

##### Familienstand #####
# Eurostat data - HH-Type over years
# Selected HH-types in Germany, 2020 is break in series

data_haushalt2 <- read.csv2("data/raw/ilc_lvps02_1_Data.csv",na="NA", sep=",", dec=",")



# Rename & Verkleinern

df = subset(data_haushalt2, select= -c(GEO, Flag.and.Footnotes))

df1 <- df %>% mutate(INCGRP = case_when(INCGRP == "Below 60% of median equivalised income" ~ 1,
                                        INCGRP == "Total" ~ 0))
# Familienstand anpassen
df2 <- tidyr::pivot_wider(df1, names_from = HHTYP, values_from = Value)

df3 <- rename(df2,single_with_children ="Single person with dependent children", 
              single_f = "Single female", single_m = "Single male",
              two_adults = "Two adults", sum3 = "Two adults with one dependent child", 
              sum1 = "Two adults with two dependent children", 
              sum2 = "Two adults with three or more dependent children", 
              three_or_more_adults = "Three or more adults", 
              three_or_more_adults_with_children = "Three or more adults with dependent children" 
)

df4 <- mutate(df3, 
              two_adults_with_children = sum1 + sum2 + sum3)

df4$sum1 <- NULL
df4$sum2 <- NULL
df4$sum3 <- NULL

df5 <- df4 %>% pivot_longer(cols = single_with_children:two_adults_with_children,
                            names_to = "Familienstand",
                            values_to = "Prozent")

df6 <- df5 %>% mutate(Familienstand = fct_recode(Familienstand,
                                                 "Alleinstehende Männer" = "single_m",
                                                 "Alleinstehende Frauen" = "single_f",
                                                 "Alleinerziehend" = "single_with_children", 
                                                 "Zwei Erwachsene" = "two_adults", 
                                                 "Zwei Erwachsene mit Kind(ern)" = "two_adults_with_children", 
                                                 "Drei oder mehr Erwachsene" = "three_or_more_adults", 
                                                 "Drei oder mehr Erwachsene mit Kind(ern)" = "three_or_more_adults_with_children"))


# Split 

Familienstand_Armutsgrenze <- subset(df6, INCGRP==1)
Familienstand_Alle <-subset(df6, INCGRP ==0)

# Plot
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(Familienstand_Armutsgrenze, aes(x = TIME, y = Prozent, group = Familienstand, color = Familienstand)) +
  geom_line(size = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  labs(
    title = "Arme/armutsgefährdete Haushalte",
    subtitle = "2010-2020"
  ) +
  scale_colour_manual(values=cbbPalette) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


ggplot(Familienstand_Armutsgrenze, aes(x = TIME, y = Prozent, group = Familienstand, color = Familienstand)) +
  geom_line(size = 0.9) +
  geom_point(color = "black", size = 1.5) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  scale_color_discrete(limits= c("Alleinerziehend", "Zwei Erwachsene mit Kind(ern)", "Drei oder mehr Erwachsene mit Kind(ern)",
                                 "Alleinstehende Frauen", "Alleinstehende Männer", "Zwei Erwachsene", "Drei oder mehr Erwachsene")) +
  labs(
    title = "Familienstand für arme und armutsgefährdete Haushalte",
    subtitle = "2010-2020", 
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


ggplot(Familienstand_Alle, aes(x = TIME, y = Prozent, group = Familienstand, color = Familienstand)) +
  geom_line(size = 0.9) +
  geom_point(color = "black", size = 1.5) +
  scale_color_discrete(limits= c("Alleinerziehend", "Zwei Erwachsene mit Kind(ern)", "Drei oder mehr Erwachsene mit Kind(ern)",
                                 "Alleinstehende Frauen", "Alleinstehende Männer", "Zwei Erwachsene", "Drei oder mehr Erwachsene")) +
  labs(
    title = "Familienstand für alle Haushalte",
    subtitle = "2010-2020",
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )



