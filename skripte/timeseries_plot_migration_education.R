library(tidyverse)


#Raw Data #####
raw_2013 <- read.csv2("A:/Rohdaten/Rohdaten_2013.csv", encoding = "UTF-8")
raw_2014 <- read.csv2("A:/Rohdaten/Rohdaten_2014.csv", encoding = "UTF-8")
raw_2014 <- raw_2014[-1,]
raw_2014 <- raw_2014[c(1:3307),]
raw_2015 <- read.csv2("A:/Rohdaten/Rohdaten_2015.csv", encoding = "UTF-8")
raw_2015 <- raw_2015[-1,]
raw_2015 <- raw_2015[c(1:3142),]
raw_2016 <- read.csv2("A:/Rohdaten/Rohdaten_2016.csv", encoding = "UTF-8")
raw_2016 <- raw_2016[-1,]
raw_2016 <- raw_2016[c(1:3482),]
raw_2017 <- read.csv2("A:/Rohdaten/Rohdaten_2017.csv", encoding = "UTF-8")
raw_2017 <- raw_2017[-1,]
raw_2017 <- raw_2017[c(1:3033),]
raw_2018 <- read.csv2("A:/Rohdaten/Rohdaten_2018.csv", encoding = "UTF-8")
raw_2018 <- raw_2018[c(2:2741),]
raw_2019 <- read.csv2("A:/Rohdaten/Rohdaten_2019.csv", encoding = "UTF-8")
raw_2019 <- raw_2019[c(2:2987),]
raw_2020 <- read.csv2("A:/Rohdaten/Rohdaten_2020.csv", encoding = "UTF-8")
raw_2021 <- read.csv2("A:/Rohdaten/Rohdaten_2021.csv", encoding = "UTF-8")

#Data Restructuring #####
#2013
total_migration_education_2013 <- raw_2013 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Migrationshintergrund != 4) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == "abgeschlossene  Berufsausbildung"), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ' '), "kein Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2013 <- total_migration_education_2013 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2013 <- total_migration_education_2013 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2014
total_migration_education_2014 <- raw_2014 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Migrationshintergrund != 2) %>%
  filter(Migrationshintergrund != 4) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2014 <- total_migration_education_2014 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2014 <- total_migration_education_2014 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2015
total_migration_education_2015 <- raw_2015 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Migrationshintergrund != 2) %>%
  filter(Migrationshintergrund != 4) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2015 <- total_migration_education_2015 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2015 <- total_migration_education_2015 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2016
total_migration_education_2016 <- raw_2016 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Migrationshintergrund != 2) %>%
  filter(Migrationshintergrund != 4) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2016 <- total_migration_education_2016 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2016 <- total_migration_education_2016 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2017
total_migration_education_2017 <- raw_2017 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Migrationshintergrund != 2) %>%
  filter(Migrationshintergrund != 4) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2017 <- total_migration_education_2017 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2017 <- total_migration_education_2017 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2018
total_migration_education_2018 <- raw_2018 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Bildungsabschluss != 0) %>%
  filter(Migrationshintergrund != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2018 <- total_migration_education_2018 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2018 <- total_migration_education_2018 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2019
total_migration_education_2019 <- raw_2019 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Bildungsabschluss != 0) %>%
  filter(Migrationshintergrund != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2019 <- total_migration_education_2019 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2019 <- total_migration_education_2019 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2020
total_migration_education_2020 <- raw_2020 %>% select(Migrationshintergrund = X6.1.Migrationshintergrund.des.Hilfes., Bildungsabschluss = X5..Bildungsabschluss) %>%
  filter(Bildungsabschluss != "Keine Angabe") %>%
  filter(Migrationshintergrund != "Keine Angabe") %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "Ja"), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "Nein"), "kein Migrationshintergrund")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == "abgeschlossene  Berufsausbildung"), "abgeschlossene Berufsausbildung")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2020 <- total_migration_education_2020 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2020 <- total_migration_education_2020 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#2021
total_migration_education_2021 <- raw_2021 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Migrationshintergrund != "Keine Angabe") %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == "abgeschlossene  Berufsausbildung"), "abgeschlossene Berufsausbildung")) %>%
  count(Migrationshintergrund, Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

yes_migration_education_2021 <- total_migration_education_2021 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

no_migration_education_2021 <- total_migration_education_2021 %>% select(Migrationshintergrund, Bildungsabschluss, n) %>%
  filter(Migrationshintergrund == "kein Migrationshintergrund") %>%
  mutate(Prozent = round((n/sum(n))*100,2))

#Time Series #####
ts_yes_migration_education <- data.frame(
  
  Jahr = as.character(c(2013,2014,2015,2016,2017,2018,2019,2020,2021)),
  
  abgeschlossene.Berufsausbildung = c(yes_migration_education_2013[1,4],yes_migration_education_2014[1,4],yes_migration_education_2015[1,4],
                                      yes_migration_education_2016[1,4],yes_migration_education_2017[1,4],yes_migration_education_2018[1,4],yes_migration_education_2019[1,4],yes_migration_education_2020[1,4],yes_migration_education_2021[1,4]),
  
  kein.Abschluss = c(yes_migration_education_2013[2,4],yes_migration_education_2014[2,4],yes_migration_education_2015[2,4],
                     yes_migration_education_2016[2,4],yes_migration_education_2017[2,4],yes_migration_education_2018[2,4],yes_migration_education_2019[2,4],yes_migration_education_2020[2,4],yes_migration_education_2021[2,4]),
  
  Schulabschluss = c(yes_migration_education_2013[3,4],yes_migration_education_2014[3,4],yes_migration_education_2015[3,4],
                     yes_migration_education_2016[3,4],yes_migration_education_2017[3,4],yes_migration_education_2018[3,4],yes_migration_education_2019[3,4],yes_migration_education_2020[3,4],yes_migration_education_2021[3,4]),
  
  Studienabschluss = c(yes_migration_education_2013[4,4],yes_migration_education_2014[4,4],yes_migration_education_2015[4,4],
                       yes_migration_education_2016[4,4],yes_migration_education_2017[4,4],yes_migration_education_2018[4,4],yes_migration_education_2019[4,4],yes_migration_education_2020[4,4],yes_migration_education_2021[4,4])
  
)


ts_no_migration_education <- data.frame(
  
  Jahr = as.character(c(2013,2014,2015,2016,2017,2018,2019,2020,2021)),
  
  abgeschlossene.Berufsausbildung = c(no_migration_education_2013[1,4],no_migration_education_2014[1,4],no_migration_education_2015[1,4],
                                      no_migration_education_2016[1,4],no_migration_education_2017[1,4],no_migration_education_2018[1,4],no_migration_education_2019[1,4],no_migration_education_2020[1,4],no_migration_education_2021[1,4]),
  
  kein.Abschluss = c(no_migration_education_2013[2,4],no_migration_education_2014[2,4],no_migration_education_2015[2,4],
                     no_migration_education_2016[2,4],no_migration_education_2017[2,4],no_migration_education_2018[2,4],no_migration_education_2019[2,4],no_migration_education_2020[2,4],no_migration_education_2021[2,4]),
  
  Schulabschluss = c(no_migration_education_2013[3,4],no_migration_education_2014[3,4],no_migration_education_2015[3,4],
                     no_migration_education_2016[3,4],no_migration_education_2017[3,4],no_migration_education_2018[3,4],no_migration_education_2019[3,4],no_migration_education_2020[3,4],no_migration_education_2021[3,4]),
  
  Studienabschluss = c(no_migration_education_2013[4,4],no_migration_education_2014[4,4],no_migration_education_2015[4,4],
                       no_migration_education_2016[4,4],no_migration_education_2017[4,4],no_migration_education_2018[4,4],no_migration_education_2019[4,4],no_migration_education_2020[4,4],no_migration_education_2021[4,4])
  
)

#Plots #####
ts_yes_migration_education2 <- ts_yes_migration_education %>% pivot_longer(cols = abgeschlossene.Berufsausbildung:Studienabschluss, 
                                                                           names_to = "Bildungsabschluss", 
                                                                           values_to = "Prozent")

COLORS <- c(abgeschlossene.Berufsausbildung = "grey", kein.Abschluss = "black", Schulabschluss = "blue",  
            Studienabschluss = "yellow")

ggplot(ts_yes_migration_education2, aes(x = Jahr, y = Prozent, group = Bildungsabschluss, color = Bildungsabschluss)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Bildungsabschluss mit Migrationshintergrund",
    subtitle = "2013-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


ts_no_migration_education2 <- ts_no_migration_education %>% pivot_longer(cols = abgeschlossene.Berufsausbildung:Studienabschluss, 
                                                                         names_to = "Bildungsabschluss", 
                                                                         values_to = "Prozent")

ggplot(ts_no_migration_education2, aes(x = Jahr, y = Prozent, group = Bildungsabschluss, color = Bildungsabschluss)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Bildungsabschluss ohne Migrationshintergrund",
    subtitle = "2013-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )

