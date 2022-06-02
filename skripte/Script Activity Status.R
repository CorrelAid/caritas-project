# Eurostat Data: Activity status
# Population 18 years and over, 2020 is break in series (to be intepreted with caution)
# Persons outside the labour force are not included 

library(tidyverse)


setwd("~/CorrelAid/Referenzwerte_SILC/Activity status/ilc_lvhl02")


# Rename & Verkleinern

data_haushalt2 <- read.csv2("ilc_lvhl02_1_Data.csv",na="NA", sep=",", dec=",")

df = subset(data_haushalt2, select= -c(GEO, Flag.and.Footnotes))

df1 <- df %>% mutate(INCGRP = case_when(INCGRP == "Below 60% of median equivalised income" ~ 1,
                                               INCGRP == "Total" ~ 0))

# Aktivity level anpassen

df2 <- tidyr::pivot_wider(df1, names_from = WSTATUS, values_from = Value)

df4 <- rename(df2, Berufstätig = "Employed persons", 
              Arbeitslos = "Unemployed persons", 
              Verrentet = "Retired persons", 
              drop2 = "Employed persons except employees",
              drop3 = "Not employed persons", 
              drop4 = "Other persons outside the labour force (former name: inactive persons)"
           )

df4$Employees <- NULL
df4$drop2 <- NULL
df4$drop3 <- NULL
df4$drop4 <- NULL
df4$Population <- NULL

df4$SEX[df4$SEX == "Females"] <- NA
df4$SEX[df4$SEX == "Males"] <- NA

df5 <- na.omit(df4)

df_Aktivität <- df5 %>% pivot_longer(cols = Berufstätig:Verrentet,
                                         names_to = "Aktivität",
                                         values_to = "Prozent")

# Subsetten und Graphen erstellen

Aktivität_Armutsgrenze <- subset(df_Aktivität, INCGRP==1)
Aktivität_Alle <-subset(df_Aktivität, INCGRP ==0)

?ggplot

ggplot(Aktivität_Armutsgrenze, aes(x = TIME, y = Prozent, group = Aktivität, color = Aktivität)) +
  geom_line(size = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  geom_point(color = "black", size = 1.5) +
  labs(
    title = "Arbeitsmarktbeteiligung, arme und armutsbedrohte Menschen",
    subtitle = "2010-2020",
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


ggplot(Aktivität_Alle, aes(x = TIME, y = Prozent, group = Aktivität, color = Aktivität)) +
  geom_line(size = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  geom_point(color = "black", size = 1.5) +
  labs(
    title = "Arbeitsmarktbeteiligung, alle Menschen",
    subtitle = "2010-2020", 
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )





