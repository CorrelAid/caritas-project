library(tidyverse)

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

gender_family_2013 <- raw_2013 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != "Divers", Familiensituation != "") %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "Männlich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "weiblich"), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "männlich"), "Männlich")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "Alleinstehend (Einpersonenhaushalt)")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "Alleinerziehend (Ein-Eltern-Familie)")) %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2014 <- raw_2014 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != 0, Familiensituation != "", Familiensituation != 0) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "Männlich")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "Alleinstehend (Einpersonenhaushalt)")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "Alleinerziehend (Ein-Eltern-Familie)")) %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2015 <- raw_2015 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != 0, Familiensituation != "", Familiensituation != 0) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "Männlich")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "Alleinstehend (Einpersonenhaushalt)")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "Alleinerziehend (Ein-Eltern-Familie)")) %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2016 <- raw_2016 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != 0, Familiensituation != "", Familiensituation != 0) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "Männlich")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "Alleinstehend (Einpersonenhaushalt)")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "Alleinerziehend (Ein-Eltern-Familie)")) %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2017 <- raw_2017 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != 0, Familiensituation != "", Familiensituation != 0) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "Männlich")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "Alleinstehend (Einpersonenhaushalt)")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "Alleinerziehend (Ein-Eltern-Familie)")) %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2018 <- raw_2018 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != 0, Familiensituation != "", Familiensituation != 0) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "Männlich")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "Alleinstehend (Einpersonenhaushalt)")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "Alleinerziehend (Ein-Eltern-Familie)")) %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2019 <- raw_2019 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != 0, Geschlecht != 3, Familiensituation != "", Familiensituation != 0) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "Weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "Männlich")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "Alleinstehend (Einpersonenhaushalt)")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "Alleinerziehend (Ein-Eltern-Familie)")) %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2020 <- raw_2020 %>% select(Geschlecht = X1..Geschlecht, Familiensituation = X2..Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != "Keine Angabe", Geschlecht != "Divers", 
          Familiensituation != "", Familiensituation != "Keine Angabe") %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

gender_family_2021 <- raw_2021 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != "", Geschlecht != "Divers", Familiensituation != "") %>%
  count(Geschlecht, Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2))


ts_gender_family <- data.frame(
  
  Jahr = as.character(c(2013,2014,2015,2016,2017,2018,2019,2020,2021)),
  
  Männlich.Alleinerziehend = c(gender_family_2013[1,4],gender_family_2014[1,4],gender_family_2015[1,4],
                               gender_family_2016[1,4],gender_family_2017[1,4],gender_family_2018[1,4],gender_family_2019[1,4],gender_family_2020[1,4],gender_family_2021[1,4]),
  
  Männlich.Alleinstehend = c(gender_family_2013[2,4],gender_family_2014[2,4],gender_family_2015[2,4],
                             gender_family_2016[2,4],gender_family_2017[2,4],gender_family_2018[2,4],gender_family_2019[2,4],gender_family_2020[2,4],gender_family_2021[2,4]),
  
  Männlich.Ehe.Partnerschaft.Familie = c(gender_family_2013[3,4],gender_family_2014[3,4],gender_family_2015[3,4],
                                         gender_family_2016[3,4],gender_family_2017[3,4],gender_family_2018[3,4],gender_family_2019[3,4],gender_family_2020[3,4],gender_family_2021[3,4]),
  
  Weiblich.Alleinerziehend = c(gender_family_2013[4,4],gender_family_2014[4,4],gender_family_2015[4,4],
                               gender_family_2016[4,4],gender_family_2017[4,4],gender_family_2018[4,4],gender_family_2019[4,4],gender_family_2020[4,4],gender_family_2021[4,4]),
  
  Weiblich.Alleinstehend = c(gender_family_2013[5,4],gender_family_2014[5,4],gender_family_2015[5,4],
                             gender_family_2016[5,4],gender_family_2017[5,4],gender_family_2018[5,4],gender_family_2019[5,4],gender_family_2020[5,4],gender_family_2021[5,4]),
  
  Weiblich.Ehe.Partnerschaft.Familie = c(gender_family_2013[6,4],gender_family_2014[6,4],gender_family_2015[6,4],
                                         gender_family_2016[6,4],gender_family_2017[6,4],gender_family_2018[6,4],gender_family_2019[6,4],gender_family_2020[6,4],gender_family_2021[6,4])
)

ts_gender_family2 <- ts_gender_family %>% pivot_longer(cols = Männlich.Alleinerziehend:Weiblich.Ehe.Partnerschaft.Familie, 
                                                       names_to = "Familienstand", 
                                                       values_to = "Prozent")

COLORS <- c(Männlich.Alleinerziehend = "grey", Männlich.Alleinstehend = "black", Männlich.Ehe.Partnerschaft.Familie = "blue",  
            Weiblich.Alleinerziehend = "yellow", Weiblich.Alleinstehend = "orange", Weiblich.Ehe.Partnerschaft.Familie = "red")

ggplot(ts_gender_family2, aes(x = Jahr, y = Prozent, group = Familienstand, color = Familienstand)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Familienstand",
    subtitle = "2013-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )