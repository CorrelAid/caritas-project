library(tidyverse)
library(readxl)
library(dplyr)
library(randomForest)

### Daten einlesen und verarbeiten

raw_2021 <- read.csv2("Rohdaten_2021.csv", encoding = "UTF-8") 

#Beratungsgespr�ch 2021

talk_2021 <- raw_2021 %>% select(Beratungsgespr�ch = Beratungsgespr�ch.findet.statt.) %>%
  filter(Beratungsgespr�ch !='') %>%
  mutate(Beratungsgespr�ch = replace(Beratungsgespr�ch, which(Beratungsgespr�ch == "per Email/online"), "E-Mail")) %>%
  count(Beratungsgespr�ch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2021, aes(x = "", y = n, fill = Beratungsgespr�ch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespr�ch 2021"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Geschlecht 2021 (Der Einfachkeithalber die 3 'divers' F�lle nicht ber�cksichtigt)

gender_2021 <- raw_2021 %>% select(Geschlecht) %>%
  filter(Geschlecht != "Divers") %>%
  filter(Geschlecht != '') %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "Weiblich"), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "M�nnlich"), "m�nnlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2021, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2021"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2021

family_2021 <- raw_2021 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Alleinstehend (Einpersonenhaushalt)"), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Ehe/Partnerschaft/Familie"), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Alleinerziehend (Ein-Eltern-Familie)"), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2021, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2021"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2021

age_2021 <- raw_2021 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  mutate(Alter = replace(Alter, which(Alter == "bis 17"), "<17")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2021, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2021"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2021

children_2021 <- raw_2021 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == "Keine"), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == "Mehr als 2 Kinder"), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2021, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2021"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2021

education_2021 <- raw_2021 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2021, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2021"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2021

migration_2021 <- rbind(
  
  raw_2021 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    filter(Migrationshintergrund != "Keine Angabe") %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2021 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    filter(Migrationshintergrund != "Keine Angabe") %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2021

income1_2021 <- raw_2021 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
                                    Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Einkommensart..Kindergeld, Einkommensart...Unterhalt..vorschuss, Einkommensart..Krankengeld, 
                                    Einkommensart..Asylbewerb.leist.gesetz, Einkommensart..Kurzarbeitergeld) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
        Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Einkommensart..Kindergeld, Einkommensart...Unterhalt..vorschuss, Einkommensart..Krankengeld, 
        Einkommensart..Asylbewerb.leist.gesetz, Einkommensart..Kurzarbeitergeld)


missing <- as.numeric(income1_2021 %>% summarise(missing = first(n)))

income2_2021 <- rbind(
  
  raw_2021 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Aufstocker) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Aufstocker")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Elterngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Elterngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Kindergeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Kindergeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart...Unterhalt..vorschuss) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Unterhalt/-vorschuss")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Krankengeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Krankengeld")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Asylbewerb.leist.gesetz) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Asylbewerb.leist.gesetz")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Einkommen = Einkommensart..Kurzarbeitergeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "x"), "Kurzarbeitergeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
  
)

ggplot(data = income2_2021, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2021"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2021

problems1_2021 <- raw_2021 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Beh�rden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                      Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Krankheit, Problemlage..Alter.Pflegebed�rftigkeit,
                                      Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Beh�rden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit, Problemlage..Alter.Pflegebed�rftigkeit,
        Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme)


missing <- as.numeric(problems1_2021 %>% summarise(missing = first(n)))

problems2_2021 <- rbind(
  
  raw_2021 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Partner/Erziehung/famili�re Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Umgang.mit.Beh�rden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Umgang mit Beh�rde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Alter.Pflegebed�rftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Alter/Pflegebed�rftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..sonstige.finanzielle.Prob.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Sprachprobleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Sprachprobleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
  
)

ggplot(data = problems2_2021, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2021"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Ma�nahmen 2021

measures1_2021 <- raw_2021 %>% select(Ma�nahmen..Information...Beratung, Ma�nahmen..Weitervermittlung,
                                      Ma�nahmen..Hilfe.bei.Antr�gen.Korres..., Ma�nahmen..Hilfe.bei.Korrespondenz, Ma�nahmen..Finanzhilfe.Sachleistung) %>%
  count(Ma�nahmen..Information...Beratung, Ma�nahmen..Weitervermittlung,
        Ma�nahmen..Hilfe.bei.Antr�gen.Korres..., Ma�nahmen..Hilfe.bei.Korrespondenz, Ma�nahmen..Finanzhilfe.Sachleistung)

missing <- as.numeric(measures1_2021 %>% summarise(missing = first(n)))

measures2_2021 <- rbind (
  
  raw_2021 %>% select(Ma�nahme = Ma�nahmen..Information...Beratung) %>%
    mutate(Ma�nahme = replace(Ma�nahme, which(Ma�nahme == "x"), "Information/Beratung")) %>%
    count(Ma�nahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2021 %>% select(Ma�nahme = Ma�nahmen..Weitervermittlung) %>%
    mutate(Ma�nahme = replace(Ma�nahme, which(Ma�nahme == "x"), "Weitervermittlung")) %>%
    count(Ma�nahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Ma�nahme = Ma�nahmen..Hilfe.bei.Antr�gen.Korres...) %>%
    mutate(Ma�nahme = replace(Ma�nahme, which(Ma�nahme == "x"), "Hilfe bei Antr�gen")) %>%
    count(Ma�nahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Ma�nahme = Ma�nahmen..Hilfe.bei.Korrespondenz) %>%
    mutate(Ma�nahme = replace(Ma�nahme, which(Ma�nahme == "x"), "Hilfe bei Korrespondenz")) %>%
    count(Ma�nahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Ma�nahme = Ma�nahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Ma�nahme = replace(Ma�nahme, which(Ma�nahme == "x"), "Finanzhilfe/Sachleistung")) %>%
    count(Ma�nahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

### Daten vorbereiten f�r statistische Analysen
df <- bind_rows(raw_2021)
index <- c("x", "")  
values <- c(1,0)
mfh <- values[match(df$Ma�nahmen..Finanzhilfe.Sachleistung, index)] 
df$finanzhilfe <- factor(mfh, order = TRUE)

mfh <- values[match(df$Ma�nahmen..Finanzhilfe.Sachleistung, index)] 
df$finanzhilfe <- factor(mfh, order = TRUE)

mag <- values[match(df$Ma�nahmen..Hilfe.bei.Antr�gen.Korres..., index)] 
df$antraghilfe <- factor(mag, order = TRUE)

mkh <- values[match(df$Ma�nahmen..Hilfe.bei.Korrespondenz, index)] 
df$korrespondenzhilfe <- factor(mkh, order = TRUE)

mwv <- values[match(df$Ma�nahmen..Weitervermittlung, index)] 
df$weitervermittlung <- factor(mwv, order = TRUE)

mib <- values[match(df$Ma�nahmen..Information...Beratung, index)] 
df$beratung <- factor(mib, order = TRUE)

index <- c("telefonisch", "per Email/online", "pers�nlich")  
values <- c(1,1,0)
ob <- values[match(df$Beratungsgespr�ch.findet.statt., index)] 
df$online_beratung <- factor(ob, order = TRUE)

index <- c("Keine", "1 Kind", "2 Kinder", "Mehr als 2 Kinder")  
values <- c(0,1,2,3)
df$kinder <- values[match(df$Anzahl.der.Kinder.im.Haushalt, index)] 

index <- c("Weiblich", "M�nnlich", "Divers")  
values <- c(1,0,0)
w <- values[match(df$Geschlecht, index)] 
df$weiblich <- factor(w, order = TRUE)

index <- c("Alleinerziehend (Ein-Eltern-Familie)", "Alleinstehend (Einpersonenhaushalt)", "Ehe/Partnerschaft/Familie")  
values <- c(0,0,1)
fam <- values[match(df$Familiensituation, index)] 
df$ehe_familie <- factor(fam, order = TRUE)

index <- c("Alleinerziehend (Ein-Eltern-Familie)", "Alleinstehend (Einpersonenhaushalt)", "Ehe/Partnerschaft/Familie")  
values <- c(1,1,0)
fam <- values[match(df$Familiensituation, index)] 
df$allein <- factor(fam, order = TRUE)

index <- c("bis 17", "18-20", "21-24", "25-29", "30-39", "40-49", "50-59", "60-64", "ab 65", "unbekannt")  
values <- c(1,2,3,4,5,6,7,8,9, NA)
df$alter <- values[match(df$Alter.des.der.Klient.in, index)] 

index <- c("kein Abschluss", "Schulabschluss", "abgeschlossene  Berufsausbildung", "Studienabschluss")  
values <- c(0,1,1,1)
ba <- values[match(df$Bildungsabschluss, index)] 
df$schulabschluss <- factor(ba, order = TRUE)

index <- c("kein Abschluss", "Schulabschluss", "abgeschlossene  Berufsausbildung", "Studienabschluss")  
values <- c(0,0,1,1)
ba <- values[match(df$Bildungsabschluss, index)] 
df$ausbildung_oder_studium <- factor(ba, order = TRUE)

index <- c("x", "")  
values <- c(1,0)
alg <- values[match(df$Einkommensart..ALG.II, index)] 
df$alg2 <- factor(alg, order = TRUE)

rentner <- values[match(df$Einkommensart..Rente, index)] 
df$rente <- factor(rentner, order = TRUE)

kg <- values[match(df$Einkommensart..Rente, index)] 
df$kindergeld <- factor(kg, order = TRUE)

rentner <- values[match(df$Einkommensart..Rente, index)] 
df$rente <- factor(rentner, order = TRUE)

mig <- values[match(df$Migrationshintergrund.des.Hilfesuchenden, index)]
df$migrationshintergrund <- factor(mig, order = TRUE)

jc <- values[match(df$Schwierigkeiten.Erreichbarkeit, index)]
df$jobcenter_schwierig <- factor(jc, order = TRUE)

schlechtereink <- values[match(df$Einkommenssituation.verschlechtert, index)] 
df$fin_verschlechtert <- factor(schlechtereink, order = TRUE)

krank <- values[match(df$Problemlage..Krankheit, index)] 
df$krankheit <- factor(krank, order = TRUE)

beh <- values[match(df$Problemlage..Behinderung, index)] 
df$behinderung <- factor(beh, order = TRUE)

cfs <- values[match(df$Corona.Schulden, index)] 
df$corona_fin_schlechter <- factor(cfs, order = TRUE)

cws <- values[match(df$Corona.Wohnschulden, index)] 
df$corona_wohnschulden <- factor(cws, order = TRUE)

index <- c("Ja", "Nein")  
values <- c(1,0)
sp <- values[match(df$Sprachprobleme, index)] 
df$sprachprob <- factor(sp, order = TRUE)

bp <- values[match(df$Problemlage..Umgang.mit.Beh�rden, index)] 
df$behoerden_probleme <- factor(bp, order = TRUE)

# Beobachtungen mit fehlenden Daten entfernen:
df <- df[!is.na(df$weiblich),]
df <- df[!is.na(df$finanzhilfe),]
df <- df[!is.na(df$kinder),]
df <- df[!is.na(df$ehe_familie),]
df <- df[!is.na(df$alter),]
df <- df[!is.na(df$schulabschluss),]
df <- df[!is.na(df$ausbildung_oder_studium),]
df <- df[!is.na(df$alg2),]
df <- df[!is.na(df$kindergeld),]
df <- df[!is.na(df$jobcenter_schwierig),]
df <- df[!is.na(df$corona_fin_schlechter),]
df <- df[!is.na(df$corona_wohnschulden),]
df <- df[!is.na(df$krankheit),]
df <- df[!is.na(df$behinderung),]
df <- df[!is.na(df$online_beratung),]

# Analyse mit Random Forest: Wer bekommt eine Finanzhilfe?
library(randomForest)
fit_RF <- randomForest(as.factor(finanzhilfe) ~ kinder + allein + weiblich + alter + schulabschluss + ausbildung_oder_studium + alg2 + kindergeld + jobcenter_schwierig + krankheit + behinderung + corona_fin_schlechter + corona_wohnschulden + online_beratung, data=df,importance=TRUE,ntree=5000)
importance(fit_RF,type=1)
varImpPlot(fit_RF, main="Figure 1: Random Forest - Variable Importance Plot")

# Analyse mit logistischer Regression: Wer bekommt eine Finanzhilfe?
logit_finanzhilfe <- glm(as.factor(finanzhilfe) ~ kinder + allein + weiblich + alter + schulabschluss + ausbildung_oder_studium + alg2 + kindergeld + jobcenter_schwierig + krankheit + behinderung + corona_fin_schlechter + corona_wohnschulden + online_beratung, data = df, family = "binomial")
summary(logit_finanzhilfe)

### Antraghilfe
# A) Logistic regression
logit_antraghilfe <- glm(as.factor(antraghilfe) ~ kinder 
                         + allein + weiblich + alter 
                         + schulabschluss 
                         + ausbildung_oder_studium 
                         + alg2 + kindergeld 
                         + jobcenter_schwierig + krankheit
                         + behinderung 
                         + corona_fin_schlechter 
                         + corona_wohnschulden 
                         + online_beratung, data = df, 
                         family = "binomial")
summary(logit_antraghilfe)

# B) Run random forest algorithm
fit_RF_antrag <- randomForest(as.factor(antraghilfe) ~ kinder 
                              + ehe_familie + weiblich + alter 
                              + schulabschluss + 
                                ausbildung_oder_studium + alg2 
                              + kindergeld + jobcenter_schwierig 
                              + krankheit + behinderung 
                              + corona_fin_schlechter 
                              + corona_wohnschulden 
                              + online_beratung, data=df,
                              importance=TRUE,ntree=5000)
# Compute variable importance for each variable
importance(fit_RF_antrag,type=1)
# Produce variable importance plot
varImpPlot(fit_RF_antrag, main="Figure 2: Random Forest - Variable Importance Plot - Antraghilfe")

### Korrespondenzhilfe
# A) Logistic regression
logit_korrespondenzhilfe <- glm(as.factor(korrespondenzhilfe) ~ kinder 
                                + allein + weiblich + alter 
                                + schulabschluss 
                                + ausbildung_oder_studium 
                                + alg2 + kindergeld 
                                + jobcenter_schwierig + krankheit 
                                + behinderung 
                                + corona_fin_schlechter 
                                + corona_wohnschulden 
                                + online_beratung, data = df, 
                                family = "binomial")
summary(logit_korrespondenzhilfe)

# B) Run random forest algorithm
fit_RF_korr <- randomForest(as.factor(korrespondenzhilfe) ~ kinder 
                            + ehe_familie + weiblich + alter 
                            + schulabschluss + 
                              ausbildung_oder_studium + alg2 
                            + kindergeld + jobcenter_schwierig 
                            + krankheit + behinderung 
                            + corona_fin_schlechter 
                            + corona_wohnschulden 
                            + online_beratung, data=df,
                            importance=TRUE,ntree=5000)
# Compute variable importance for each variable
importance(fit_RF_korr,type=1)
# Produce variable importance plot
varImpPlot(fit_RF_korr, main="Figure 3: Random Forest - Variable Importance Plot - Korrespondenzhilfe")

### Weitervermittlung
# A) Logistic regression
logit_weitervermittlung <- glm(as.factor(weitervermittlung) ~ kinder 
                               + allein + weiblich + alter 
                               + schulabschluss 
                               + ausbildung_oder_studium 
                               + alg2 + kindergeld 
                               + jobcenter_schwierig + krankheit 
                               + behinderung 
                               + corona_fin_schlechter 
                               + corona_wohnschulden 
                               + online_beratung, data = df, 
                               family = "binomial")
summary(logit_weitervermittlung)

# B) Run random forest algorithm
fit_RF_wv <- randomForest(as.factor(weitervermittlung) ~ kinder 
                          + ehe_familie + weiblich + alter 
                          + schulabschluss + 
                            ausbildung_oder_studium + alg2 
                          + kindergeld + jobcenter_schwierig 
                          + krankheit + behinderung 
                          + corona_fin_schlechter 
                          + corona_wohnschulden 
                          + online_beratung, data=df,
                          importance=TRUE,ntree=5000)
# Compute variable importance for each variable
importance(fit_RF_wv,type=1)
# Produce variable importance plot
varImpPlot(fit_RF_wv, main="Figure 4: Random Forest - Variable Importance Plot - Weitervermittlung")

### Logistische Regression f�r Problemlagen
# Corona-Wohnschulden
log_mod <- glm(as.factor(corona_wohnschulden) ~ kinder + migrationshintergrund + weiblich + allein + alter + schulabschluss + ausbildung_oder_studium + online_beratung, data = df, family = binomial)
summary(log_mod)