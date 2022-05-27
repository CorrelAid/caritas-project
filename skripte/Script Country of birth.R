library(tidyverse)

# Country of birth by broad groups, 15 years of age and over
# EU = EU as of 2020, without UK
# Data for 2020 is provisional, breaks in series at different points in time (as always)

setwd("~/CorrelAid/Referenzwerte_SILC/Country of Birth/lfsa_pgacws")


# Rename & Verkleinern

data_haushalt2 <- read.csv2("lfsa_pgacws_1_Data.csv",na="NA", sep=",", dec=".")

df = subset(data_haushalt2, select= -c(GEO, AGE, WSTATUS, SEX, Flag.and.Footnotes))



# C_Birth anpassen

class(data_haushalt2$Value)
df$Value_num <- as.numeric(gsub(" " ,  "",df$Value))

df$Value <- NULL


df1 <- tidyr::pivot_wider(df, names_from = C_BIRTH, values_from = Value_num)



df2 <- rename(df1, 
              EU = "EU27 countries (from 2020) except reporting country", 
              Drittstaaten = "Non-EU27 countries (from 2020) nor reporting country", 
              Ausland = "Foreign country", 
              Deutschland = "Reporting country")


#### In Prozentsätze umrechnen


df2$EU <- (df2$EU / df2$Total) * 100
df2$Drittstaaten <- (df2$Drittstaaten / df2$Total) * 100
df2$Ausland <- (df2$Ausland / df2$Total) * 100
df2$Deutschland <- (df2$Deutschland / df2$Total) * 100


Geburtsland <- df2 %>% pivot_longer(cols = EU:Deutschland,
                                         names_to = "Geburtsland",
                                         values_to = "Prozent")


# Graphen erstellen


ggplot(Geburtsland, aes(x = TIME, y = Prozent, group = Geburtsland, color = Geburtsland)) +
  geom_line(size = 0.9) +
  geom_point(color = "black", size = 1.5) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  scale_color_discrete(limits= c("Deutschland", "Ausland", 
                                "EU", "Drittstaaten")) +
  labs(
    title = "Geburtsland, 2010 - 2020",
    subtitle = "EU und Drittstaaten 2017 - 2020", 
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


