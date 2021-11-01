library (tidyverse)


#Rohdaten 2010 leider nicht verfügbar - Ich nutze deshalb die Daten aus den zusammengefassten Tabellen im Excel Sheet

tables_2010 <- read.csv2("A:/Rohdaten/Tabellendaten_2010.csv", encoding = "UTF-8", check.names = FALSE)


#Beratungsgespräch 2010

talk_2010 <- tables_2010[c(1:3),c(1:3)]
colnames(talk_2010) <- c("Beratungsgespräch", "n", "Prozent")

ggplot(talk_2010, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2010"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Geschlecht 2010

gender_2010 <- tables_2010[c(1:2),c(4:6)]
colnames(gender_2010) <- c("Geschlecht", "n", "Prozent")

ggplot(gender_2010, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2010"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Familiensituation 2010

family_2010 <- tables_2010[c(1:3),c(7:9)]
colnames(family_2010) <- c("Familiensituation", "n", "Prozent")

ggplot(family_2010, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2010"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2010

age_2010 <- tables_2010[c(1:3),c(10:12)]
colnames(age_2010) <- c("Alter", "n", "Prozent")

ggplot(age_2010, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2010"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2010

children_2010 <- tables_2010[c(1:4),c(13:15)]
colnames(children_2010) <- c("Kinderanzahl", "n", "Prozent")

ggplot(children_2010, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2010"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2010

education_2010 <- tables_2010[c(1:4),c(16:18)]
colnames(education_2010) <- c("Bildungsabschluss", "n", "Prozent")

ggplot(education_2010, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2010"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migrationshintergrund 2010

migration_2010 <- tables_2010[c(1:2),c(19:21)]
colnames(migration_2010) <- c("Migrationshintergrund", "n", "Prozent")

ggplot(data = migration_2010, aes(x = Migrationshintergrund, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1.5, color="black", size=3.5) +
  labs(
    title = "Migrationshintergrund 2010"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Einkommen 2010

income2_2010 <- tables_2010[c(1:7),c(22:24)]
colnames(income2_2010) <- c("Einkommen", "n", "Prozent")

ggplot(data = income2_2010, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1.5, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2010"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2010

problems2_2010 <- tables_2010[c(1:15),c(25:27)]
colnames(problems2_2010) <- c("Problemlage", "n", "Prozent")

ggplot(data = problems2_2010, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2010"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2010

measures2_2010 <- tables_2010[c(1:8),c(28:30)]
colnames(measures2_2010) <- c("Maßnahmen", "n", "Prozent")

ggplot(data = measures2_2010, aes(x = Maßnahmen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2010"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Rohdaten 2011 einlesen (directory anpassen)

raw_2011 <- read.csv2("A:/Rohdaten/Rohdaten_2011.csv", encoding = "UTF-8")

#Beratungsgespräch 2011

talk_2011 <- raw_2011 %>% select(Beratungsgespräch = X0.Beratungsgespräch) %>%
  filter(Beratungsgespräch != 999) %>%
  filter(Beratungsgespräch != 99) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2011, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2011"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Geschlecht 2011

gender_2011 <- raw_2011 %>% select(Geschlecht = X1.Geschlecht) %>%
  filter(Geschlecht != 999) %>%
  filter(Geschlecht != 99) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2011, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2011"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")     

#Familiensituation 2011

family_2011 <- raw_2011 %>% select(Familiensituation = X2.Familiensituation) %>%
  filter(Familiensituation != 999) %>%
  filter(Familiensituation != 99) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2011, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2011"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Alter 2011

age_2011 <- raw_2011 %>% select(Alter = X3..Alter) %>%
  filter(Alter != 999) %>%
  filter(Alter != 99) %>%
  filter(Alter != 9) %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "ab 60")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2011, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2011"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Kinderanzahl 2011

children_2011 <- raw_2011 %>% select(Kinderanzahl = X4..Kinderzahl) %>%
  filter(Kinderanzahl != 999) %>%
  filter(Kinderanzahl != 99) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 0), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2011, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2011"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Bildungsabschluss
# Anmerkung: 2011 ist der einzige Datensatz, der nicht den höchsten Bildungsabschluss anzeigt, sondern alle Abschlüsse.
# Ich habe deshalb mit der Annahme 'kein Abschluss < Schulabschluss < Berufsausbildung < Studienabschluss' gearbeitet.

education2_2011 <- raw_2011 %>% select(Studienabschluss = X5_3Studienabschluss,
                                       Berufsausbildung = X5_2abgeschlossene.Berufsausbildung,
                                       Schulabschluss = X5_1Schulabschluss,
                                       kein_Abschluss = X5_4kein.Abschluss) %>%
  replace_na(list(Schulabschluss = 0)) %>%
  replace_na(list(Berufsausbildung = 0)) %>%
  replace_na(list(Studienabschluss = 0)) %>%
  replace_na(list(kein_Abschluss = 0)) %>%
  count(Studienabschluss,Berufsausbildung,Schulabschluss,kein_Abschluss)

missing <- as.numeric(education2_2011 %>% summarise(missing = first(n)))

education_2011 <- rbind(
  
  education2_2011 %>% select(numbers = n) %>%
    slice(5:8) %>%
    mutate(Bildungsabschluss = "abgeschlossene Berufsausbildung") %>%
    mutate(n = sum(numbers)) %>%
    select(Bildungsabschluss,n) %>%
    slice(1) %>%
    mutate(Prozent = round(n/(sum(education2_2011$n)-missing)*100,2)),
  
  education2_2011 %>% select(numbers = n) %>%
    slice(2) %>%
    mutate(Bildungsabschluss = "kein Abschluss") %>%
    mutate(n = sum(numbers)) %>%
    select(Bildungsabschluss,n) %>%
    slice(1) %>%
    mutate(Prozent = round(n/(sum(education2_2011$n)-missing)*100,2)),
  
  education2_2011 %>% select(numbers = n) %>%
    slice(3:4) %>%
    mutate(Bildungsabschluss = "Schulabschluss") %>%
    mutate(n = sum(numbers)) %>%
    select(Bildungsabschluss,n) %>%
    slice(1) %>%
    mutate(Prozent = round(n/(sum(education2_2011$n)-missing)*100,2)),
  
  education2_2011 %>% select(numbers = n) %>%
    slice(9:13) %>%
    mutate(Bildungsabschluss = "Studienabschluss") %>%
    mutate(n = sum(numbers)) %>%
    select(Bildungsabschluss,n) %>%
    slice(1) %>%
    mutate(Prozent = round(n/(sum(education2_2011$n)-missing)*100,2))
  
)


#Migrationshintergrund 2011

migration_2011 <- rbind(
  raw_2011 %>% select(Migrationshintergrund = X6_1.Migrationshintergrund.Hilfesuchender) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    replace_na(list(Migrationshintergrund = "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  raw_2011 %>% select(Migrationshintergrund = X6_2.Migrationshintergrund.Partner) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    replace_na(list(Migrationshintergrund = "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
)


#Einkommen 2011

income1_2011 <- raw_2011 %>% select(X7_1.Erwerbseinkommen, X7_2.ALG.I, X7_3.ALG.II, X7_4.SGB.XII, X7_5Rente, 
                                    X7_6Wohngeld, X7_7Kinder..Erziehungsgeld.Kinderzuschlag.Unterhalt, X7_8.Sonstiges) %>%
  mutate(X7_8.Sonstiges = replace(X7_8.Sonstiges, which(X7_8.Sonstiges != ''), 1)) %>%
  count(X7_1.Erwerbseinkommen, X7_2.ALG.I, X7_3.ALG.II, X7_4.SGB.XII, X7_5Rente, 
        X7_6Wohngeld, X7_7Kinder..Erziehungsgeld.Kinderzuschlag.Unterhalt, X7_8.Sonstiges) %>%
  arrange(desc(X7_8.Sonstiges))

missing <- as.numeric(income1_2011 %>% summarise(missing = last(n)))

income2_2011 <- rbind(
  
  raw_2011 %>% select(Einkommen = X7_1.Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Einkommen = X7_2.ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Einkommen = X7_3.ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Einkommen = X7_4.SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Einkommen = X7_5Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Einkommen = X7_6Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Einkommen = X7_7Kinder..Erziehungsgeld.Kinderzuschlag.Unterhalt) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinder-/Erziehungsgeld/Kinderzuschlag/Unterhalt")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Einkommen = X7_8.Sonstiges) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen != ''), 1)) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Sonstiges")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2011, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1.5, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2011"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )

#Problemlage 2011

problems1_2011 <- raw_2011 %>% select(X8_1.sozialrechtliche.Probleme, X8_2.Partner..Erziehung..familiäre.Probleme, X8_3.Umgang.mit.Behörde, X8_4.psychische.Probleme, X8_5.Arbeitslosigkeit, 
                                      X8_6.Krankheit, X8_7.Sanktionen.nach.SGB.II, X8_8.Alter.Pflegebedürftigkeit, X8_9.Schulden, X8_10.Schwangerschaft, X8_11.Energie..odder.Mietschulden,
                                      X8_12.Behinderung, X8_13.sonstige.finanzielle.Schwierigkeiten, X8_14.Probleme.im.Bereich.Wohnen, X8_15.Sonstiges) %>%
  mutate(X8_15.Sonstiges = replace(X8_15.Sonstiges, which(X8_15.Sonstiges != ''), 1)) %>%
  count(X8_1.sozialrechtliche.Probleme, X8_2.Partner..Erziehung..familiäre.Probleme, X8_3.Umgang.mit.Behörde, X8_4.psychische.Probleme, X8_5.Arbeitslosigkeit, 
        X8_6.Krankheit, X8_7.Sanktionen.nach.SGB.II, X8_8.Alter.Pflegebedürftigkeit, X8_9.Schulden, X8_10.Schwangerschaft, X8_11.Energie..odder.Mietschulden,
        X8_12.Behinderung, X8_13.sonstige.finanzielle.Schwierigkeiten, X8_14.Probleme.im.Bereich.Wohnen, X8_15.Sonstiges) %>%
  arrange(desc(X8_15.Sonstiges))

missing <- as.numeric(problems1_2011 %>% summarise(missing = last(n)))

problems2_2011 <- rbind(
  
  raw_2011 %>% select(Problemlage = X8_1.sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_2.Partner..Erziehung..familiäre.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_3.Umgang.mit.Behörde) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_4.psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_5.Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_6.Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_7.Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_8.Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_9.Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_10.Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_11.Energie..odder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_12.Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_13.sonstige.finanzielle.Schwierigkeiten) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_14.Probleme.im.Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Problemlage = X8_15.Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = problems2_2011, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2011"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )

#Maßnahmen 2011

measures1_2011 <- raw_2011 %>% select(X9_1.Information, X9_2.Klärung.der.Situation, X9_3.Beratung, X9_4.Weitervermittlung,
                                      X9_5.Hilfe..Anträgen.und.Korrespondenz, X9_6.Finanzhilfe.Sachleistung, X9_7.Sonstiges, X9_8.kein.Angebot.vorhanden) %>%
  count(X9_1.Information, X9_2.Klärung.der.Situation, X9_3.Beratung, X9_4.Weitervermittlung,
        X9_5.Hilfe..Anträgen.und.Korrespondenz, X9_6.Finanzhilfe.Sachleistung, X9_7.Sonstiges, X9_8.kein.Angebot.vorhanden) %>%
  arrange(desc(X9_8.kein.Angebot.vorhanden))

missing <- as.numeric(measures1_2011 %>% summarise(missing = last(n)))

measures2_2011 <- rbind(
  
  raw_2011 %>% select(Maßnahme = X9_1.Information) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Maßnahme = X9_2.Klärung.der.Situation) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Klärung der Situation")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Maßnahme = X9_3.Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Maßnahme = X9_4.Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Maßnahme = X9_5.Hilfe..Anträgen.und.Korrespondenz) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen und Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Maßnahme = X9_6.Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Maßnahme = X9_7.Sonstiges) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Sonstiges")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2011 %>% select(Maßnahme = X9_8.kein.Angebot.vorhanden) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "kein Angebot vorhanden")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = measures2_2011, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2011"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Rohdaten 2012 leider nicht verfügbar - Ich nutze deshalb die Daten aus den zusammengefassten Tabellen im Excel Sheet

tables_2012 <- read.csv2("A:/Rohdaten/Tabellendaten_2012.csv", encoding = "UTF-8", check.names = FALSE)

#Beratungsgespräch 2012

talk_2012 <- tables_2012[c(1:3),c(1:3)]
colnames(talk_2012) <- c("Beratungsgespräch", "n", "Prozent")

ggplot(talk_2012, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Geschlecht 2012

gender_2012 <- tables_2012[c(1:2),c(4:6)]
colnames(gender_2012) <- c("Geschlecht", "n", "Prozent")

ggplot(gender_2012, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Familiensituation 2012

family_2012 <- tables_2012[c(1:3),c(7:9)]
colnames(family_2012) <- c("Familiensituation", "n", "Prozent")

ggplot(family_2012, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2012

age_2012 <- tables_2012[c(1:10),c(10:12)]
colnames(age_2012) <- c("Alter", "n", "Prozent")

ggplot(age_2012, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2012

children_2012 <- tables_2012[c(1:4),c(13:15)]
colnames(children_2012) <- c("Kinderanzahl", "n", "Prozent")

ggplot(children_2012, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2012

education_2012 <- tables_2012[c(1:4),c(16:18)]
colnames(education_2012) <- c("Bildungsabschluss", "n", "Prozent")

ggplot(education_2012, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migrationshintergrund 2012

migration_2012 <- tables_2012[c(1:2),c(19:21)]
colnames(migration_2012) <- c("Migrationshintergrund", "n", "Prozent")

ggplot(data = migration_2012, aes(x = Migrationshintergrund, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1.5, color="black", size=3.5) +
  labs(
    title = "Migrationshintergrund 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Einkommen 2012

income2_2012 <- tables_2012[c(1:7),c(22:24)]
colnames(income2_2012) <- c("Einkommen", "n", "Prozent")

ggplot(data = income2_2012, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1.5, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2012

problems2_2012 <- tables_2012[c(1:14),c(25:27)]
colnames(problems2_2012) <- c("Problemlage", "n", "Prozent")

ggplot(data = problems2_2012, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2012

measures2_2012 <- tables_2012[c(1:8),c(28:30)]
colnames(measures2_2012) <- c("Maßnahmen", "n", "Prozent")

ggplot(data = measures2_2012, aes(x = Maßnahmen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )




#Rohdaten 2012 leider nicht verfügbar - Ich nutze deshalb die Daten aus den zusammengefassten Tabellen im Excel Sheet

#Beratungsgespräch 2012

talk_2012 <- data.frame(
  Beratungsgespräch = c("persönlich","telefonisch","E-Mail"),
  n = c(2207, 743, 52),
  Prozent = c(73.5, 24.8, 1.7)
)

ggplot(talk_2012, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Geschlecht 2012

gender_2012 <- data.frame(
  Geschlecht = c("weiblich","männlich"),
  n = c(1953, 1107),
  Prozent = c(63.82, 36.18)
)

ggplot(gender_2012, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Familiensituation 2012

family_2012 <- data.frame(
  Familiensituation = c("Ehe/Partnerschaft/Familie","alleinstehend","alleinerziehend"),
  n = c(1236, 1097, 616),
  Prozent = c(41.9, 37.2, 20.9)
)

ggplot(family_2012, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2012

age_2012 <- data.frame(
  Alter = c("<17", "18-20", "21-24", "25-29", "30-39", "40-49", "50-59", "60-64", "ab 65", "unbekannt"),
  n = c(49, 87, 207, 350, 748, 699, 488, 162, 270, 21),
  Prozent = c(1.59, 2.82, 6.71, 11.35, 24.25, 22.67, 15.82, 5.25, 8.75, 0.68)
)

ggplot(age_2012, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2012

children_2012 <- data.frame(
  Kinderanzahl = c("1 Kind", "2 Kinder", "keine", "mehr als 2 Kinder"),
  n = c(590, 533, 1437, 395),
  Prozent = c(19.97, 18.04, 48.63, 13.37)
)

ggplot(children_2012, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2012

education_2012 <- data.frame(
  Bildungsabschluss = c("abgeschlossene Berufsausbildung", "kein Abschluss", "Schulabschluss", "Studienabschluss"),
  n = c(1224, 778, 821, 142),
  Prozent = c(41.28, 26.24, 27.69, 4.79)
)

ggplot(education_2012, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2012"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migrationshintergrund 2012

migration_2012 <- data.frame(
  Migrationshintergrund = c("Migrationshintergrund des Hilfesuchenden", "Migrationshintergrund des Partners"),
  n = c(1134, 387),
  Prozent = c(36.3, 12.4)
)

ggplot(data = migration_2012, aes(x = Migrationshintergrund, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1.5, color="black", size=3.5) +
  labs(
    title = "Migrationshintergrund 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Einkommen 2012

income2_2012 <- data.frame(
  Einkommen = c("Erwerbseinkommen", "ALG I", "ALG II", "SGB XII", "Kinder/Erziehungsgeld/Kinderzuschlag/Unterhalt", "Rente", "Wohngeld"),
  n = c(827, 140, 1483, 222, 875, 525, 179),
  Prozent = c(28.6, 4.8, 51.3, 7.7, 30.2, 18.1, 6.2)
)

ggplot(data = income2_2012, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1.5, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2012

problems2_2012 <- data.frame(
  Einkommen = c("sozialrechtliche Probleme", "Partner/Erziehung/familiäre Probleme", "Umgang mit Behörde", "psychische Probleme", "Arbeitslosigkeit",
                "Krankheit", "Sanktionen nach SGB II", "Alter/Pflegebedürftigkeit", "Schulden", "Schwangerschaft", "Energie- oder Mietschulden",
                "Behinderung", "Sonstige finanzielle Schwierigkeiten", "Probleme im Bereich Wohnen"),
  n = c(782, 575, 969, 609, 422, 574, 106, 159, 768, 137, 312, 194, 936, 551),
  Prozent = c(26.31, 19.35, 32.60, 20.49, 14.20, 19.31, 3.57, 5.35, 25.84, 4.61, 10.50, 6.53, 32.40, 18.54)
)

ggplot(data = problems2_2012, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2012

measures2_2012 <- data.frame(
  Maßnahmen = c("Information", "Klärung der Situation", "Beratung", "Weitervermittlung", "Hilfe bei Anträgen und Korrespondenz",
                "Finanzhilfe/Sachleistung", "Sonstiges", "kein Angebot vorhanden"),
  n = c(1562, 1039, 2147, 755, 1002, 566, 181, 15),
  Prozent = c(50.45, 33.56, 69.35, 24.39, 32.36, 18.28, 5.85, 0.48)
)

ggplot(data = measures2_2012, aes(x = Maßnahmen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2012"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )








#Rohdaten 2013 einlesen (directory anpassen)

raw_2013 <- read.csv2("A:/Rohdaten/Rohdaten_2013.csv", encoding = "UTF-8")


#Beratungsgespräch 2013

talk_2013 <- raw_2013 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != '') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == "per Email/online"), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2013, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2013"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Geschlecht 2013

gender_2013 <- raw_2013 %>% select(Geschlecht) %>%
  filter(Geschlecht != '') %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2013, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2013"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2013

family_2013 <- raw_2013 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Alleinstehend (Einpersonenhaushalt)"), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Alleinerziehend (Ein-Eltern-Familie)"), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2013, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2013"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2013

age_2013 <- raw_2013 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == "bis 17"), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "60-64")) %>%
  mutate(Alter = replace(Alter, which(Alter == 9), "ab 65")) %>%
  mutate(Alter = replace(Alter, which(Alter == 10), "unbekannt")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2013, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2013"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2013

children_2013 <- raw_2013 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 4), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2013, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2013"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2013

education_2013 <- raw_2013 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == "abgeschlossene  Berufsausbildung"), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2013, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2013"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2013

migration_2013 <- rbind(
  
  raw_2013 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ' '), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "4"), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2013 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ' '), "kein Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '2'), "kein Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '3'), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2013

income1_2013 <- raw_2013 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
                                    Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges) %>%
  mutate(Einkommensart..Erwerbseinkommen = replace(Einkommensart..Erwerbseinkommen, which(Einkommensart..Erwerbseinkommen == 'x'), 1)) %>%
  mutate(Einkommensart..Erwerbseinkommen = replace(Einkommensart..Erwerbseinkommen, which(Einkommensart..Erwerbseinkommen == '0'), '')) %>%
  
  mutate(Einkommensart..ALG.I = replace(Einkommensart..ALG.I, which(Einkommensart..ALG.I == 'x'), 1)) %>%
  mutate(Einkommensart..ALG.I = replace(Einkommensart..ALG.I, which(Einkommensart..ALG.I == '0'), '')) %>%
  
  mutate(Einkommensart..ALG.II = replace(Einkommensart..ALG.II, which(Einkommensart..ALG.II == 'x'), 1)) %>%
  mutate(Einkommensart..ALG.II = replace(Einkommensart..ALG.II, which(Einkommensart..ALG.II == '0'), '')) %>%
  
  mutate(Einkommensart..SGB.XII = replace(Einkommensart..SGB.XII, which(Einkommensart..SGB.XII == 'x'), 1)) %>%
  mutate(Einkommensart..SGB.XII = replace(Einkommensart..SGB.XII, which(Einkommensart..SGB.XII == '0'), '')) %>%
  
  mutate(Einkommensart..Rente = replace(Einkommensart..Rente, which(Einkommensart..Rente == 'x'), 1)) %>%
  mutate(Einkommensart..Rente = replace(Einkommensart..Rente, which(Einkommensart..Rente == '0'), '')) %>%
  
  mutate(Einkommensart..Wohngeld = replace(Einkommensart..Wohngeld, which(Einkommensart..Wohngeld == 'x'), 1)) %>%
  mutate(Einkommensart..Wohngeld = replace(Einkommensart..Wohngeld, which(Einkommensart..Wohngeld == '0'), '')) %>%
  
  mutate(Einkommensart..Elterngeld = replace(Einkommensart..Elterngeld, which(Einkommensart..Elterngeld == 'x'), 1)) %>%
  mutate(Einkommensart..Elterngeld = replace(Einkommensart..Elterngeld, which(Einkommensart..Elterngeld == '0'), '')) %>%
  
  mutate(Einkommensart..Kinderzuschlag = replace(Einkommensart..Kinderzuschlag, which(Einkommensart..Kinderzuschlag == 'x'), 1)) %>%
  mutate(Einkommensart..Kinderzuschlag = replace(Einkommensart..Kinderzuschlag, which(Einkommensart..Kinderzuschlag == '0'), '')) %>%
  
  mutate(Art..Kinder..Erziehungsgeld.Unterhalt = replace(Art..Kinder..Erziehungsgeld.Unterhalt, which(Art..Kinder..Erziehungsgeld.Unterhalt == 'x'), 1)) %>%
  mutate(Art..Kinder..Erziehungsgeld.Unterhalt = replace(Art..Kinder..Erziehungsgeld.Unterhalt, which(Art..Kinder..Erziehungsgeld.Unterhalt == '0'), '')) %>%
  
  mutate(Einkommensart..Sonstiges = replace(Einkommensart..Sonstiges, which(Einkommensart..Sonstiges == '0'), '')) %>%
  mutate(Einkommensart..Sonstiges = replace(Einkommensart..Sonstiges, which(Einkommensart..Sonstiges == 'Nichts'), '')) %>%
  mutate(Einkommensart..Sonstiges = replace(Einkommensart..Sonstiges, which(Einkommensart..Sonstiges != ''), 1))

missing <- 76

income2_2013 <- rbind(
  
  income1_2013 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Art..Kinder..Erziehungsgeld.Unterhalt) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinder-/Erziehungsgeld/Unterhalt")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  income1_2013 %>% select(Einkommen = Einkommensart..Sonstiges) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen != ''), 1)) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Sonstiges")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2013, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2013"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )



#Problemlage 2013

problems1_2013 <- raw_2013 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
                                      Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  
  mutate(Problemlage..sozialrechtliche.Probleme = replace(Problemlage..sozialrechtliche.Probleme, which(Problemlage..sozialrechtliche.Probleme == 'x'), 1)) %>%
  mutate(Problemlage..sozialrechtliche.Probleme = replace(Problemlage..sozialrechtliche.Probleme, which(Problemlage..sozialrechtliche.Probleme == '0'), '')) %>%
  
  mutate(Problemlage..Umgang.mit.Behörden = replace(Problemlage..Umgang.mit.Behörden, which(Problemlage..Umgang.mit.Behörden == 'x'), 1)) %>%
  mutate(Problemlage..Umgang.mit.Behörden = replace(Problemlage..Umgang.mit.Behörden, which(Problemlage..Umgang.mit.Behörden == '0'), '')) %>%
  
  mutate(Problemlage..Arbeitslosigkeit = replace(Problemlage..Arbeitslosigkeit, which(Problemlage..Arbeitslosigkeit == 'x'), 1)) %>%
  mutate(Problemlage..Arbeitslosigkeit = replace(Problemlage..Arbeitslosigkeit, which(Problemlage..Arbeitslosigkeit == '0'), '')) %>%
  
  mutate(Problemlage..Sanktionen.nach.SGB.II = replace(Problemlage..Sanktionen.nach.SGB.II, which(Problemlage..Sanktionen.nach.SGB.II == 'x'), 1)) %>%
  mutate(Problemlage..Sanktionen.nach.SGB.II = replace(Problemlage..Sanktionen.nach.SGB.II, which(Problemlage..Sanktionen.nach.SGB.II == '0'), '')) %>%
  
  mutate(Problemlage..Schulden = replace(Problemlage..Schulden, which(Problemlage..Schulden == 'x'), 1)) %>%
  mutate(Problemlage..Schulden = replace(Problemlage..Schulden, which(Problemlage..Schulden == '0'), '')) %>%
  
  mutate(Problemlage..Energie..oder.Mietschulden = replace(Problemlage..Energie..oder.Mietschulden, which(Problemlage..Energie..oder.Mietschulden == 'x'), 1)) %>%
  mutate(Problemlage..Energie..oder.Mietschulden = replace(Problemlage..Energie..oder.Mietschulden, which(Problemlage..Energie..oder.Mietschulden == '0'), '')) %>%
  
  mutate(Problemlage..sontige.finanzielle.Prob... = replace(Problemlage..sontige.finanzielle.Prob..., which(Problemlage..sontige.finanzielle.Prob... == 'x'), 1)) %>%
  mutate(Problemlage..sontige.finanzielle.Prob... = replace(Problemlage..sontige.finanzielle.Prob..., which(Problemlage..sontige.finanzielle.Prob... == '0'), '')) %>%
  
  mutate(Problemlage..Partner...Erziehung..fam... = replace(Problemlage..Partner...Erziehung..fam..., which(Problemlage..Partner...Erziehung..fam... == 'x'), 1)) %>%
  mutate(Problemlage..Partner...Erziehung..fam... = replace(Problemlage..Partner...Erziehung..fam..., which(Problemlage..Partner...Erziehung..fam... == '0'), '')) %>%
  
  mutate(Problemlage..Psychische.Probleme = replace(Problemlage..Psychische.Probleme, which(Problemlage..Psychische.Probleme == 'x'), 1)) %>%
  mutate(Problemlage..Psychische.Probleme = replace(Problemlage..Psychische.Probleme, which(Problemlage..Psychische.Probleme == '0'), '')) %>%
  
  mutate(Problemlage..Krankheit = replace(Problemlage..Krankheit, which(Problemlage..Krankheit == 'x'), 1)) %>%
  mutate(Problemlage..Krankheit = replace(Problemlage..Krankheit, which(Problemlage..Krankheit == '0'), '')) %>%
  
  mutate(Problemlage..Alter.Pflegebedürftigkeit = replace(Problemlage..Alter.Pflegebedürftigkeit, which(Problemlage..Alter.Pflegebedürftigkeit == 'x'), 1)) %>%
  mutate(Problemlage..Alter.Pflegebedürftigkeit = replace(Problemlage..Alter.Pflegebedürftigkeit, which(Problemlage..Alter.Pflegebedürftigkeit == '0'), '')) %>%
  
  mutate(Problemlage..Schwangerschaft = replace(Problemlage..Schwangerschaft, which(Problemlage..Schwangerschaft == 'x'), 1)) %>%
  mutate(Problemlage..Schwangerschaft = replace(Problemlage..Schwangerschaft, which(Problemlage..Schwangerschaft == '0'), '')) %>%
  
  mutate(Problemlage..Behinderung = replace(Problemlage..Behinderung, which(Problemlage..Behinderung == 'x'), 1)) %>%
  mutate(Problemlage..Behinderung = replace(Problemlage..Behinderung, which(Problemlage..Behinderung == '0'), '')) %>%
  
  mutate(Problemlage..Bereich.Wohnen = replace(Problemlage..Bereich.Wohnen, which(Problemlage..Bereich.Wohnen == 'x'), 1)) %>%
  mutate(Problemlage..Bereich.Wohnen = replace(Problemlage..Bereich.Wohnen, which(Problemlage..Bereich.Wohnen == '0'), '')) %>%
  
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges == ' '), '')) %>% 
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges == '0'), '')) %>%  
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1))

missing <- 19

problems2_2013 <- rbind(
  
  problems1_2013 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Energie..oder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  problems1_2013 %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = problems2_2013, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2013"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2013

measures1_2013 <- raw_2013 %>% select(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden) %>%
  
  mutate(Maßnahmen..Information = replace(Maßnahmen..Information, which(Maßnahmen..Information == 'x'), 1)) %>%
  mutate(Maßnahmen..Information = replace(Maßnahmen..Information, which(Maßnahmen..Information == '0'), '')) %>%
  
  mutate(Maßnahmen..Klärung.der.Situtation = replace(Maßnahmen..Klärung.der.Situtation, which(Maßnahmen..Klärung.der.Situtation == 'x'), 1)) %>%
  mutate(Maßnahmen..Klärung.der.Situtation = replace(Maßnahmen..Klärung.der.Situtation, which(Maßnahmen..Klärung.der.Situtation == '0'), '')) %>%
  
  mutate(Maßnahmen..Beratung = replace(Maßnahmen..Beratung, which(Maßnahmen..Beratung == 'x'), 1)) %>%
  mutate(Maßnahmen..Beratung = replace(Maßnahmen..Beratung, which(Maßnahmen..Beratung == '0'), '')) %>%
  
  mutate(Maßnahmen..Weitervermittlung = replace(Maßnahmen..Weitervermittlung, which(Maßnahmen..Weitervermittlung == 'x'), 1)) %>%
  mutate(Maßnahmen..Weitervermittlung = replace(Maßnahmen..Weitervermittlung, which(Maßnahmen..Weitervermittlung == '0'), '')) %>%
  
  mutate(Maßnahmen..Hilfe.bei.Anträgen.Korres... = replace(Maßnahmen..Hilfe.bei.Anträgen.Korres..., which(Maßnahmen..Hilfe.bei.Anträgen.Korres... == 'x'), 1)) %>%
  mutate(Maßnahmen..Hilfe.bei.Anträgen.Korres... = replace(Maßnahmen..Hilfe.bei.Anträgen.Korres..., which(Maßnahmen..Hilfe.bei.Anträgen.Korres... == '0'), '')) %>%
  
  mutate(Maßnahmen..Finanzhilfe.Sachleistung = replace(Maßnahmen..Finanzhilfe.Sachleistung, which(Maßnahmen..Finanzhilfe.Sachleistung == 'x'), 1)) %>%
  mutate(Maßnahmen..Finanzhilfe.Sachleistung = replace(Maßnahmen..Finanzhilfe.Sachleistung, which(Maßnahmen..Finanzhilfe.Sachleistung == '0'), '')) %>%
  
  mutate(Sonstiges = replace(Sonstiges, which(Sonstiges == 'x'), 1)) %>%
  mutate(Sonstiges = replace(Sonstiges, which(Sonstiges == '0'), '')) %>%
  
  mutate(Maßnahmen..kein.Angebot.vorhanden = replace(Maßnahmen..kein.Angebot.vorhanden, which(Maßnahmen..kein.Angebot.vorhanden == 'x'), 1)) %>%
  mutate(Maßnahmen..kein.Angebot.vorhanden = replace(Maßnahmen..kein.Angebot.vorhanden, which(Maßnahmen..kein.Angebot.vorhanden == '0'), ''))

missing <- 54

measures2_2013 <- rbind (
  
  measures1_2013 %>% select(Maßnahme = Maßnahmen..Information) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  measures1_2013 %>% select(Maßnahme = Maßnahmen..Klärung.der.Situtation) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Klärung der Situation")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  measures1_2013 %>% select(Maßnahme = Maßnahmen..Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  measures1_2013 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  measures1_2013 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen und Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  measures1_2013 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  measures1_2013 %>% select(Maßnahme = Sonstiges) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme != ''), "Sonstiges")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  measures1_2013 %>% select(Maßnahme = Maßnahmen..kein.Angebot.vorhanden) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "kein Angebot vorhanden")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2013, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2013"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Rohdaten 2014 einlesen (directory anpassen)

raw_2014 <- read.csv2("A:/Rohdaten/Rohdaten_2014.csv", encoding = "UTF-8")
raw_2014 <- raw_2014[-1,]
raw_2014 <- raw_2014[c(1:3307),]

#Beratungsgespräch 2014

talk_2014 <- raw_2014 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != '') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2014, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2014"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Geschlecht 2014

gender_2014 <- raw_2014 %>% select(Geschlecht) %>%
  filter(Geschlecht != '') %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2014, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2014"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2014

family_2014 <- raw_2014 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2014, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2014"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2014

age_2014 <- raw_2014 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "60-64")) %>%
  mutate(Alter = replace(Alter, which(Alter == 9), "ab 65")) %>%
  mutate(Alter = replace(Alter, which(Alter == 10), "unbekannt")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2014, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2014"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2014

children_2014 <- raw_2014 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 4), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2014, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2014"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2014

education_2014 <- raw_2014 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2014, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2014"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2014

migration_2014 <- rbind(
  
  raw_2014 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    filter(Migrationshintergrund != 2) %>%
    filter(Migrationshintergrund != 4) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2014 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2014

income1_2014 <- raw_2014 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
                                    Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges) %>%
  mutate(Einkommensart..Sonstiges = replace(Einkommensart..Sonstiges, which(Einkommensart..Sonstiges != ''), 1)) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
        Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges)


missing <- as.numeric(income1_2014 %>% summarise(missing = first(n)))

income2_2014 <- rbind(
  
  raw_2014 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Art..Kinder..Erziehungsgeld.Unterhalt) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinder-/Erziehungsgeld/Unterhalt")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Einkommen = Einkommensart..Sonstiges) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen != ''), 1)) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Sonstiges")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2014, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2014"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2014

problems1_2014 <- raw_2014 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
                                      Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)


missing <- as.numeric(problems1_2014 %>% summarise(missing = first(n)))

problems2_2014 <- rbind(
  
  raw_2014 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Energie..oder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = problems2_2014, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2014"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2014

measures1_2014 <- raw_2014 %>% select(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden) %>%
  count(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden)

missing <- as.numeric(measures1_2014 %>% summarise(missing = first(n)))

measures2_2014 <- rbind (
  
  raw_2014 %>% select(Maßnahme = Maßnahmen..Information) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2014 %>% select(Maßnahme = Maßnahmen..Klärung.der.Situtation) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Klärung der Situation")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Maßnahme = Maßnahmen..Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen und Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Maßnahme = Sonstiges) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Sonstiges")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2014 %>% select(Maßnahme = Maßnahmen..kein.Angebot.vorhanden) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "kein Angebot vorhanden")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2014, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2014"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Rohdaten 2015 einlesen (directory anpassen)

raw_2015 <- read.csv2("A:/Rohdaten/Rohdaten_2015.csv", encoding = "UTF-8")
raw_2015 <- raw_2015[-1,]
raw_2015 <- raw_2015[c(1:3142),]

#Beratungsgespräch 2015

talk_2015 <- raw_2015 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != '') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2015, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2015"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Geschlecht 2015

gender_2015 <- raw_2015 %>% select(Geschlecht) %>%
  filter(Geschlecht != '') %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2015, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2015"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2015

family_2015 <- raw_2015 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2015, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2015"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2015

age_2015 <- raw_2015 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "60-64")) %>%
  mutate(Alter = replace(Alter, which(Alter == 9), "ab 65")) %>%
  mutate(Alter = replace(Alter, which(Alter == 10), "unbekannt")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2015, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2015"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2015

children_2015 <- raw_2015 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 4), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2015, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2015"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2015

education_2015 <- raw_2015 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2015, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2015"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2015

migration_2015 <- rbind(
  
  raw_2015 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    filter(Migrationshintergrund != 2) %>%
    filter(Migrationshintergrund != 4) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2015 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2015

income1_2015 <- raw_2015 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
                                    Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges) %>%
  mutate(Einkommensart..Sonstiges = replace(Einkommensart..Sonstiges, which(Einkommensart..Sonstiges != ''), 1)) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
        Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges)


missing <- as.numeric(income1_2015 %>% summarise(missing = first(n)))

income2_2015 <- rbind(
  
  raw_2015 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Art..Kinder..Erziehungsgeld.Unterhalt) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinder-/Erziehungsgeld/Unterhalt")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Einkommen = Einkommensart..Sonstiges) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen != ''), 1)) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Sonstiges")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2015, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2015"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2015

problems1_2015 <- raw_2015 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
                                      Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)


missing <- as.numeric(problems1_2015 %>% summarise(missing = first(n)))

problems2_2015 <- rbind(
  
  raw_2015 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Energie..oder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = problems2_2015, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2015"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2015

measures1_2015 <- raw_2015 %>% select(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden) %>%
  count(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden)

missing <- as.numeric(measures1_2015 %>% summarise(missing = first(n)))

measures2_2015 <- rbind (
  
  raw_2015 %>% select(Maßnahme = Maßnahmen..Information) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2015 %>% select(Maßnahme = Maßnahmen..Klärung.der.Situtation) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Klärung der Situation")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Maßnahme = Maßnahmen..Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen und Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Maßnahme = Sonstiges) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Sonstiges")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2015 %>% select(Maßnahme = Maßnahmen..kein.Angebot.vorhanden) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "kein Angebot vorhanden")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2015, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2015"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )



#Rohdaten 2016 einlesen (directory anpassen)

raw_2016 <- read.csv2("A:/Rohdaten/Rohdaten_2016.csv", encoding = "UTF-8")
raw_2016 <- raw_2016[-1,]
raw_2016 <- raw_2016[c(1:3482),]

#Beratungsgespräch 2016

talk_2016 <- raw_2016 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != '') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2016, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2016"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Geschlecht 2016

gender_2016 <- raw_2016 %>% select(Geschlecht) %>%
  filter(Geschlecht != '') %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2016, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2016"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2016

family_2016 <- raw_2016 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2016, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2016"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2016

age_2016 <- raw_2016 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "60-64")) %>%
  mutate(Alter = replace(Alter, which(Alter == 9), "ab 65")) %>%
  mutate(Alter = replace(Alter, which(Alter == 10), "unbekannt")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2016, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2016"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2016

children_2016 <- raw_2016 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 4), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2016, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2016"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2016

education_2016 <- raw_2016 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2016, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2016"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2016

migration_2016 <- rbind(
  
  raw_2016 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    filter(Migrationshintergrund != 2) %>%
    filter(Migrationshintergrund != 4) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2016 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2016

income1_2016 <- raw_2016 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
                                    Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges) %>%
  mutate(Einkommensart..Sonstiges = replace(Einkommensart..Sonstiges, which(Einkommensart..Sonstiges != ''), 1)) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
        Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges)


missing <- as.numeric(income1_2016 %>% summarise(missing = first(n)))

income2_2016 <- rbind(
  
  raw_2016 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Art..Kinder..Erziehungsgeld.Unterhalt) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinder-/Erziehungsgeld/Unterhalt")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Einkommen = Einkommensart..Sonstiges) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen != ''), 1)) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Sonstiges")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2016, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2016"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2016

problems1_2016 <- raw_2016 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                      Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)


missing <- as.numeric(problems1_2016 %>% summarise(missing = first(n)))

problems2_2016 <- rbind(
  
  raw_2016 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = problems2_2016, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2016"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2016

measures1_2016 <- raw_2016 %>% select(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden) %>%
  count(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden)

missing <- as.numeric(measures1_2016 %>% summarise(missing = first(n)))

measures2_2016 <- rbind (
  
  raw_2016 %>% select(Maßnahme = Maßnahmen..Information) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2016 %>% select(Maßnahme = Maßnahmen..Klärung.der.Situtation) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Klärung der Situation")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Maßnahme = Maßnahmen..Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen und Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Maßnahme = Sonstiges) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Sonstiges")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2016 %>% select(Maßnahme = Maßnahmen..kein.Angebot.vorhanden) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "kein Angebot vorhanden")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2016, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2016"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Rohdaten 2017 einlesen (directory anpassen)

raw_2017 <- read.csv2("A:/Rohdaten/Rohdaten_2017.csv", encoding = "UTF-8")
raw_2017 <- raw_2017[-1,]
raw_2017 <- raw_2017[c(1:3033),]

#Beratungsgespräch 2017

talk_2017 <- raw_2017 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != '') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2017, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2017"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Geschlecht 2017

gender_2017 <- raw_2017 %>% select(Geschlecht) %>%
  filter(Geschlecht != '') %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2017, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2017"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2017

family_2017 <- raw_2017 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2017, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2017"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2017

age_2017 <- raw_2017 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "60-64")) %>%
  mutate(Alter = replace(Alter, which(Alter == 9), "ab 65")) %>%
  mutate(Alter = replace(Alter, which(Alter == 10), "unbekannt")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2017, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2017"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2017

children_2017 <- raw_2017 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 4), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2017, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2017"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2017

education_2017 <- raw_2017 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2017, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2017"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2017

migration_2017 <- rbind(
  
  raw_2017 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    filter(Migrationshintergrund != 2) %>%
    filter(Migrationshintergrund != 4) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2017 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2017

income1_2017 <- raw_2017 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
                                    Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges) %>%
  mutate(Einkommensart..Sonstiges = replace(Einkommensart..Sonstiges, which(Einkommensart..Sonstiges != ''), 1)) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..SGB.XII, 
        Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Art..Kinder..Erziehungsgeld.Unterhalt, Einkommensart..Sonstiges)


missing <- as.numeric(income1_2017 %>% summarise(missing = first(n)))

income2_2017 <- rbind(
  
  raw_2017 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Art..Kinder..Erziehungsgeld.Unterhalt) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinder-/Erziehungsgeld/Unterhalt")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Einkommen = Einkommensart..Sonstiges) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen != ''), 1)) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Sonstiges")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2017, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2017"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2017

problems1_2017 <- raw_2017 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                      Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)


missing <- as.numeric(problems1_2017 %>% summarise(missing = first(n)))

problems2_2017 <- rbind(
  
  raw_2017 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = problems2_2017, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2017"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2017

measures1_2017 <- raw_2017 %>% select(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden) %>%
  count(Maßnahmen..Information, Maßnahmen..Klärung.der.Situtation, Maßnahmen..Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung, Sonstiges, Maßnahmen..kein.Angebot.vorhanden)

missing <- as.numeric(measures1_2017 %>% summarise(missing = first(n)))

measures2_2017 <- rbind (
  
  raw_2017 %>% select(Maßnahme = Maßnahmen..Information) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2017 %>% select(Maßnahme = Maßnahmen..Klärung.der.Situtation) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Klärung der Situation")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Maßnahme = Maßnahmen..Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen und Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Maßnahme = Sonstiges) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Sonstiges")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2017 %>% select(Maßnahme = Maßnahmen..kein.Angebot.vorhanden) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "kein Angebot vorhanden")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2017, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2017"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )



#Rohdaten 2018 einlesen (directory anpassen)

raw_2018 <- read.csv2("A:/Rohdaten/Rohdaten_2018.csv", encoding = "UTF-8")
raw_2018 <- raw_2018[c(2:2741),]

#Beratungsgespräch 2018

talk_2018 <- raw_2018 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != 0) %>%
  filter(Beratungsgespräch != '') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2018, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2018"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Geschlecht 2018

gender_2018 <- raw_2018 %>% select(Geschlecht) %>%
  filter(Geschlecht != '') %>%
  filter(Geschlecht != 0) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2018, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2018"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2018

family_2018 <- raw_2018 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  filter(Familiensituation != 0) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2018, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2018"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2018

age_2018 <- raw_2018 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  filter(Alter != 0) %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "60-64")) %>%
  mutate(Alter = replace(Alter, which(Alter == 9), "ab 65")) %>%
  mutate(Alter = replace(Alter, which(Alter == 10), "unbekannt")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2018, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2018"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2018

children_2018 <- raw_2018 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  filter(Kinderanzahl != 0) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 4), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2018, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2018"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2018

education_2018 <- raw_2018 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Bildungsabschluss != 0) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2018, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2018"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2018

migration_2018 <- rbind(
  
  raw_2018 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    filter(Migrationshintergrund != '') %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2018 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    filter(Migrationshintergrund != '') %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2018

income1_2018 <- raw_2018 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
                                    Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Einkommensart..Kindergeld, Einkommensart...Unterhalt..vorschuss, Einkommensart..Krankengeld) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
        Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Einkommensart..Kindergeld, Einkommensart...Unterhalt..vorschuss, Einkommensart..Krankengeld)


missing <- as.numeric(income1_2018 %>% summarise(missing = first(n)))

income2_2018 <- rbind(
  
  raw_2018 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..Aufstocker) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Aufstocker")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..Kindergeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kindergeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart...Unterhalt..vorschuss) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Unterhalt/-vorschuss")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Einkommen = Einkommensart..Krankengeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Krankengeld")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2018, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2018"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2018

problems1_2018 <- raw_2018 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                      Problemlage..Finanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Behinderung, Problemlage..Wohnen) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..Finanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Wohnen)


missing <- as.numeric(problems1_2018 %>% summarise(missing = first(n)))

problems2_2018 <- rbind(
  
  raw_2018 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Krankheit..auch.psychische.probleme.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Finanzielle.Schwierigkeiten) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Problemlage = Problemlage..Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
  
)

ggplot(data = problems2_2018, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2018"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2018

measures1_2018 <- raw_2018 %>% select(Maßnahmen..Information.Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung) %>%
  count(Maßnahmen..Information.Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Finanzhilfe.Sachleistung)

missing <- as.numeric(measures1_2018 %>% summarise(missing = first(n)))

measures2_2018 <- rbind (
  
  raw_2018 %>% select(Maßnahme = Maßnahmen..Information.Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information/Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2018 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen und Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2018 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2018, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2018"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )



#Rohdaten 2019 einlesen (directory anpassen)

raw_2019 <- read.csv2("A:/Rohdaten/Rohdaten_2019.csv", encoding = "UTF-8")
raw_2019 <- raw_2019[c(2:2987),]

#Beratungsgespräch 2019

talk_2019 <- raw_2019 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != 0) %>%
  filter(Beratungsgespräch != '') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 1), "persönlich")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 2), "telefonisch")) %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == 3), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2019, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2019"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Geschlecht 2019 (Der Einfachkeithalber die 9 'divers' Fälle nicht berücksichtigt)

gender_2019 <- raw_2019 %>% select(Geschlecht) %>%
  filter(Geschlecht != '') %>%
  filter(Geschlecht != 0) %>%
  filter(Geschlecht != 3) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 1), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == 2), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2019, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2019"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2019

family_2019 <- raw_2019 %>% select(Familiensituation) %>%
  filter(Familiensituation != '') %>%
  filter(Familiensituation != 0) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 1), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 2), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == 3), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2019, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2019"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2019

age_2019 <- raw_2019 %>% select(Alter = Alter.des.der.Klient.in) %>%
  filter(Alter != '') %>%
  filter(Alter != 0) %>%
  mutate(Alter = replace(Alter, which(Alter == 1), "<17")) %>%
  mutate(Alter = replace(Alter, which(Alter == 2), "18-20")) %>%
  mutate(Alter = replace(Alter, which(Alter == 3), "21-24")) %>%
  mutate(Alter = replace(Alter, which(Alter == 4), "25-29")) %>%
  mutate(Alter = replace(Alter, which(Alter == 5), "30-39")) %>%
  mutate(Alter = replace(Alter, which(Alter == 6), "40-49")) %>%
  mutate(Alter = replace(Alter, which(Alter == 7), "50-59")) %>%
  mutate(Alter = replace(Alter, which(Alter == 8), "60-64")) %>%
  mutate(Alter = replace(Alter, which(Alter == 9), "ab 65")) %>%
  mutate(Alter = replace(Alter, which(Alter == 10), "unbekannt")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2019, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2019"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2019

children_2019 <- raw_2019 %>% select(Kinderanzahl = Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != '') %>%
  filter(Kinderanzahl != 0) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 1), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 2), "1 Kind")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 3), "2 Kinder")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == 4), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2019, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2019"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2019

education_2019 <- raw_2019 %>% select(Bildungsabschluss) %>%
  filter(Bildungsabschluss != '') %>%
  filter(Bildungsabschluss != 0) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 1), "Schulabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 2), "abgeschlossene Berufsausbildung")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 3), "Studienabschluss")) %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss == 4), "kein Abschluss")) %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2019, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2019"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2019

migration_2019 <- rbind(
  
  raw_2019 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden) %>%
    filter(Migrationshintergrund != '') %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2019 %>% select(Migrationshintergrund = Migrationshintergrund.des.Partners) %>%
    filter(Migrationshintergrund != '') %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == 1), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == '0'), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2019

income1_2019 <- raw_2019 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
                                    Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Einkommensart..Kindergeld, Einkommensart...Unterhalt..vorschuss, Einkommensart..Krankengeld, Einkommensart..Asylbewerb.leist.gesetz) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
        Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Einkommensart..Kindergeld, Einkommensart...Unterhalt..vorschuss, Einkommensart..Krankengeld, Einkommensart..Asylbewerb.leist.gesetz)


missing <- as.numeric(income1_2019 %>% summarise(missing = first(n)))

income2_2019 <- rbind(
  
  raw_2019 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Aufstocker) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Aufstocker")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Elterngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Elterngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Kindergeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Kindergeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart...Unterhalt..vorschuss) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Unterhalt/-vorschuss")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Krankengeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Krankengeld")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2019 %>% select(Einkommen = Einkommensart..Asylbewerb.leist.gesetz) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == 1), "Asylbewerb.leist.gesetz")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
  
)




ggplot(data = income2_2019, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2019"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2019

problems1_2019 <- raw_2019 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                      Problemlage..sonstige.inanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Behinderung, Problemlage..Wohnen, Problemlage..Sprachprobleme) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sonstige.inanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Wohnen, Problemlage..Sprachprobleme)


missing <- as.numeric(problems1_2019 %>% summarise(missing = first(n)))

problems2_2019 <- rbind(
  
  raw_2019 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Krankheit..auch.psychische.probleme.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..sonstige.inanzielle.Schwierigkeiten) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Problemlage = Problemlage..Sprachprobleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sprachprobleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
  
)

ggplot(data = problems2_2019, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2019"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2019

measures1_2019 <- raw_2019 %>% select(Maßnahmen..Information.Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen, Maßnahmen..Hilfe.bei.Korrespondenz, Maßnahmen..Finanzhilfe.Sachleistung) %>%
  count(Maßnahmen..Information.Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen, Maßnahmen..Hilfe.bei.Korrespondenz, Maßnahmen..Finanzhilfe.Sachleistung)

missing <- as.numeric(measures1_2019 %>% summarise(missing = first(n)))

measures2_2019 <- rbind (
  
  raw_2019 %>% select(Maßnahme = Maßnahmen..Information.Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Information/Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2019 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Anträgen")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Korrespondenz) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Hilfe bei Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2019 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == 1), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2019, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2019"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Rohdaten 2020 einlesen (directory anpassen)

raw_2020 <- read.csv2("A:/Rohdaten/Rohdaten_2020.csv", encoding = "UTF-8")

#Beratungsgespräch 2020

talk_2020 <- raw_2020 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch != "Keine Angabe") %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == "per Email/online"), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2020, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")

#Geschlecht 2020 (Der Einfachkeithalber die 3 'divers' Fälle nicht berücksichtigt)

gender_2020 <- raw_2020 %>% select(Geschlecht = X1..Geschlecht) %>%
  filter(Geschlecht != "Divers") %>%
  filter(Geschlecht != "Keine Angabe") %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "Weiblich"), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "Männlich"), "männlich")) %>%
  count(Geschlecht) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(gender_2020, aes(x = "", y = n, fill = Geschlecht)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Geschlecht 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues") 


#Familiensituation 2020

family_2020 <- raw_2020 %>% select(Familiensituation = X2..Familiensituation) %>%
  filter(Familiensituation != "Keine Angabe") %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Alleinstehend (Einpersonenhaushalt)"), "alleinstehend")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Ehe/Partnerschaft/Familie"), "Ehe/Partnerschaft/Familie")) %>%
  mutate(Familiensituation = replace(Familiensituation, which(Familiensituation == "Alleinerziehend (Ein-Eltern-Familie)"), "alleinerziehend")) %>%
  count(Familiensituation) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(family_2020, aes(x = "", y = n, fill = Familiensituation)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Familiensituation 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Alter 2020

age_2020 <- raw_2020 %>% select(Alter = X3..Alter.des.der.Klient.in) %>%
  filter(Alter != "Keine Angabe") %>%
  mutate(Alter = replace(Alter, which(Alter == "bis 17"), "<17")) %>%
  count(Alter) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(age_2020, aes(x = "", y = n, fill = Alter)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Alter 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Kinderanzahl 2020

children_2020 <- raw_2020 %>% select(Kinderanzahl = X4..Anzahl.der.Kinder.im.Haushalt) %>%
  filter(Kinderanzahl != "Keine Angabe") %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == "Keine"), "keine")) %>%
  mutate(Kinderanzahl = replace(Kinderanzahl, which(Kinderanzahl == "Mehr als 2 Kinder"), "mehr als 2 Kinder")) %>%
  count(Kinderanzahl) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(children_2020, aes(x = "", y = n, fill = Kinderanzahl)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Kinderanzahl 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Bildungsabschluss 2020

education_2020 <- raw_2020 %>% select(Bildungsabschluss = X5..Bildungsabschluss) %>%
  filter(Bildungsabschluss != "Keine Angabe") %>%
  count(Bildungsabschluss) %>%
  mutate(Prozent = round((n/sum(n))*100,2))

ggplot(education_2020, aes(x = "", y = n, fill = Bildungsabschluss)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Bildungsabschluss 2020"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Migration 2020

migration_2020 <- rbind(
  
  raw_2020 %>% select(Migrationshintergrund = X6.1.Migrationshintergrund.des.Hilfes.) %>%
    filter(Migrationshintergrund != "Keine Angabe") %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "Ja"), "Migrationshintergrund des Hilfesuchenden")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "Nein"), "kein Migrationshintergrund des Hilfesuchenden")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2)),
  
  raw_2020 %>% select(Migrationshintergrund = X6.2.Migrationshintergrund.des.Partners) %>%
    filter(Migrationshintergrund != "Keine Angabe") %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "Ja"), "Migrationshintergrund des Partners")) %>%
    mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "Nein"), "kein Migrationshintergrund des Partners")) %>%
    count(Migrationshintergrund) %>%
    mutate(Prozent = round((n/sum(n))*100,2))
  
)


#Einkommen 2020

income1_2020 <- raw_2020 %>% select(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
                                    Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
                                    Einkommensart..Kindergeld, Einkommensart..Unterhalt..vorschuss, Einkommensart..Krankengeld, 
                                    Einkommensart..Asylbewerberleistungsges., Einkommensart..Kurzarbeitergeld) %>%
  count(Einkommensart..Erwerbseinkommen, Einkommensart..ALG.I, Einkommensart..ALG.II, Einkommensart..Aufstocker, 
        Einkommensart..SGB.XII, Einkommensart..Rente, Einkommensart..Wohngeld, Einkommensart..Elterngeld, Einkommensart..Kinderzuschlag,
        Einkommensart..Kindergeld, Einkommensart..Unterhalt..vorschuss, Einkommensart..Krankengeld, 
        Einkommensart..Asylbewerberleistungsges., Einkommensart..Kurzarbeitergeld)


missing <- as.numeric(income1_2020 %>% summarise(missing = last(n)))

income2_2020 <- rbind(
  
  raw_2020 %>% select(Einkommen = Einkommensart..Erwerbseinkommen) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Erwerbseinkommen")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..ALG.I) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "ALG I")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..ALG.II) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "ALG II")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Aufstocker) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Aufstocker")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..SGB.XII) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "SGB XII")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Rente) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Rente")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Wohngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Wohngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Elterngeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Elterngeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Kinderzuschlag) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Kinderzuschlag")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Kindergeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Kindergeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Unterhalt..vorschuss) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Unterhalt/-vorschuss")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Krankengeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Krankengeld")) %>%
    count(Einkommen) %>%
    arrange(desc(Einkommen)) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Asylbewerberleistungsges.) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Asylbewerb.leist.gesetz")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Einkommen = Einkommensart..Kurzarbeitergeld) %>%
    mutate(Einkommen = replace(Einkommen, which(Einkommen == "Ja"), "Kurzarbeitergeld")) %>%
    count(Einkommen) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
  
)

ggplot(data = income2_2020, aes(x = Einkommen, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 1, color="black", size=3.5) +
  labs(
    title = "Einkommensarten 2020"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Problemlage 2020

problems1_2020 <- raw_2020 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                      Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme)


missing <- as.numeric(problems1_2020 %>% summarise(missing = last(n)))

problems2_2020 <- rbind(
  
  raw_2020 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Problemlage = Problemlage..sonstige.finanzielle.Prob.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Problemlage = Problemlage..Sprachprobleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Sprachprobleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
  
)

ggplot(data = problems2_2020, aes(x = Problemlage, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Problemlagen 2020"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Maßnahmen 2020

measures1_2020 <- raw_2020 %>% select(Maßnahmen..Information...Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Hilfe.bei.Korrespondenz, Maßnahmen..Finanzhilfe.Sachleistung) %>%
  count(Maßnahmen..Information...Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Hilfe.bei.Korrespondenz, Maßnahmen..Finanzhilfe.Sachleistung)

missing <- as.numeric(measures1_2020 %>% summarise(missing = last(n)))

measures2_2020 <- rbind (
  
  raw_2020 %>% select(Maßnahme = Maßnahmen..Information...Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "Ja"), "Information/Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(), 
  
  raw_2020 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "Ja"), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2020 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "Ja"), "Hilfe bei Anträgen")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Korrespondenz) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "Ja"), "Hilfe bei Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head(),
  
  raw_2020 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "Ja"), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_head()
)

ggplot(data = measures2_2020, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2020"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )



#Rohdaten 2021 einlesen (directory anpassen)

raw_2021 <- read.csv2("A:/Rohdaten/Rohdaten_2021.csv", encoding = "UTF-8")

#Beratungsgespräch 2021

talk_2021 <- raw_2021 %>% select(Beratungsgespräch = Beratungsgespräch.findet.statt.) %>%
  filter(Beratungsgespräch !='') %>%
  mutate(Beratungsgespräch = replace(Beratungsgespräch, which(Beratungsgespräch == "per Email/online"), "E-Mail")) %>%
  count(Beratungsgespräch) %>%
  mutate(Prozent = round((n/sum(n))*100,2)) %>%
  arrange(desc(n))

ggplot(talk_2021, aes(x = "", y = n, fill = Beratungsgespräch)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + 
  geom_text(aes(label = paste0(Prozent, "%")), position = position_stack(vjust=0.5), color = "black") + 
  labs(
    title = "Beratungsgespräch 2021"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette="Blues")


#Geschlecht 2021 (Der Einfachkeithalber die 3 'divers' Fälle nicht berücksichtigt)

gender_2021 <- raw_2021 %>% select(Geschlecht) %>%
  filter(Geschlecht != "Divers") %>%
  filter(Geschlecht != '') %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "Weiblich"), "weiblich")) %>%
  mutate(Geschlecht = replace(Geschlecht, which(Geschlecht == "Männlich"), "männlich")) %>%
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

problems1_2021 <- raw_2021 %>% select(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                      Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                      Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
                                      Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                      Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme)


missing <- as.numeric(problems1_2021 %>% summarise(missing = first(n)))

problems2_2021 <- rbind(
  
  raw_2021 %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Umgang mit Behörde")) %>%
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
  
  raw_2021 %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Alter/Pflegebedürftigkeit")) %>%
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


#Maßnahmen 2021

measures1_2021 <- raw_2021 %>% select(Maßnahmen..Information...Beratung, Maßnahmen..Weitervermittlung,
                                      Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Hilfe.bei.Korrespondenz, Maßnahmen..Finanzhilfe.Sachleistung) %>%
  count(Maßnahmen..Information...Beratung, Maßnahmen..Weitervermittlung,
        Maßnahmen..Hilfe.bei.Anträgen.Korres..., Maßnahmen..Hilfe.bei.Korrespondenz, Maßnahmen..Finanzhilfe.Sachleistung)

missing <- as.numeric(measures1_2021 %>% summarise(missing = first(n)))

measures2_2021 <- rbind (
  
  raw_2021 %>% select(Maßnahme = Maßnahmen..Information...Beratung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "x"), "Information/Beratung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(), 
  
  raw_2021 %>% select(Maßnahme = Maßnahmen..Weitervermittlung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "x"), "Weitervermittlung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Anträgen.Korres...) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "x"), "Hilfe bei Anträgen")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Maßnahme = Maßnahmen..Hilfe.bei.Korrespondenz) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "x"), "Hilfe bei Korrespondenz")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail(),
  
  raw_2021 %>% select(Maßnahme = Maßnahmen..Finanzhilfe.Sachleistung) %>%
    mutate(Maßnahme = replace(Maßnahme, which(Maßnahme == "x"), "Finanzhilfe/Sachleistung")) %>%
    count(Maßnahme) %>%
    mutate(Prozent = round((n/(sum(n)-missing))*100,2)) %>%
    slice_tail()
)

ggplot(data = measures2_2021, aes(x = Maßnahme, y = Prozent)) + 
  geom_bar(stat="identity", fill="steelblue") + 
  coord_flip() +
  geom_text(aes(label = paste0(Prozent, "%")), vjust= 0.5, color="black", size=3.5) +
  labs(
    title = "Maßnahmen 2021"
  ) +
  theme(
    plot.title = element_text(color = "black", size = 15, face = "bold", hjust = 0.5)
  )


#Zeitreihen

#Beratungsgespräch 2010-2021

ts_talk <- data.frame(
  Jahr = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
  persönlich = c(talk_2010[1,3],talk_2011[1,3],talk_2012[1,3],talk_2013[1,3],talk_2014[1,3],talk_2015[1,3],
                 talk_2016[1,3],talk_2017[1,3],talk_2018[1,3],talk_2019[1,3],talk_2020[1,3],talk_2021[1,3]),
  telefonisch = c(talk_2010[2,3],talk_2011[2,3],talk_2012[2,3],talk_2013[2,3],talk_2014[2,3],talk_2015[2,3],
                  talk_2016[2,3],talk_2017[2,3],talk_2018[2,3],talk_2019[2,3],talk_2020[2,3],talk_2021[2,3]),
  EMail = c(talk_2010[3,3],talk_2011[3,3],talk_2012[3,3],talk_2013[3,3],talk_2014[3,3],talk_2015[3,3],
            talk_2016[3,3],talk_2017[3,3],talk_2018[3,3],talk_2019[3,3],talk_2020[3,3],talk_2021[3,3])
  
)

ts_talk2 <- ts_talk %>% pivot_longer(cols = persönlich:EMail, 
                                     names_to = "Beratung", 
                                     values_to = "Prozent")

COLORS <- c(persönlich = "blue", telefonisch = "red",  
            EMail = "green")

ggplot(ts_talk2, aes(x = Jahr, y = Prozent, group = Beratung, color = Beratung)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Beratungsgespräch",
    subtitle = "2010-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  ) 


#Geschlecht 2010-2021

ts_gender <- data.frame(
  Jahr = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
  weiblich = c(gender_2010[1,3],gender_2011[1,3],gender_2012[1,3],gender_2013[1,3],gender_2014[1,3],gender_2015[1,3],
               gender_2016[1,3],gender_2017[1,3],gender_2018[1,3],gender_2019[1,3],gender_2020[1,3],gender_2021[1,3]),
  männlich = c(gender_2010[2,3],gender_2011[2,3],gender_2012[2,3],gender_2013[2,3],gender_2014[2,3],gender_2015[2,3],
               gender_2016[2,3],gender_2017[2,3],gender_2018[2,3],gender_2019[2,3],gender_2020[2,3],gender_2021[2,3])
)

ts_gender2 <- ts_gender %>% pivot_longer(cols = weiblich:männlich, 
                                         names_to = "Geschlecht", 
                                         values_to = "Prozent")

COLORS <- c(weiblich = "red", männlich = "blue")

ggplot(ts_gender2, aes(x = Jahr, y = Prozent, group = Geschlecht, color = Geschlecht)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Geschlecht",
    subtitle = "2010-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  ) 


#Familienstand 2010-2021

ts_family <- data.frame(
  Jahr = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
  Ehe.Partnerschaft.Familie = c(family_2010[1,3],family_2011[1,3],family_2012[1,3],family_2013[1,3],family_2014[1,3],family_2015[1,3],
                                family_2016[1,3],family_2017[1,3],family_2018[1,3],family_2019[1,3],family_2020[1,3],family_2021[1,3]),
  alleinstehend = c(family_2010[2,3],family_2011[2,3],family_2012[2,3],family_2013[2,3],family_2014[2,3],family_2015[2,3],
                    family_2016[2,3],family_2017[2,3],family_2018[2,3],family_2019[2,3],family_2020[2,3],family_2021[2,3]),
  alleinerziehend = c(family_2010[3,3],family_2011[3,3],family_2012[3,3],family_2013[3,3],family_2014[3,3],family_2015[3,3],
                      family_2016[3,3],family_2017[3,3],family_2018[3,3],family_2019[3,3],family_2020[3,3],family_2021[3,3])
)

ts_family2 <- ts_family %>% pivot_longer(cols = Ehe.Partnerschaft.Familie:alleinerziehend, 
                                         names_to = "Familienstand", 
                                         values_to = "Prozent")

COLORS <- c(Ehe.Partnerschaft.Familie = "red", alleinstehend = "blue", alleinerziehend = "green")

ggplot(ts_family2, aes(x = Jahr, y = Prozent, group = Familienstand, color = Familienstand)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Familienstand",
    subtitle = "2010-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


#Alter 2012-2021

ts_age <- data.frame(
  
  Jahr = c(2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
  
  "bis 17" = c(age_2012[1,3],age_2013[1,3],age_2014[1,3],age_2015[1,3],
               age_2016[1,3],age_2017[1,3],age_2018[1,3],age_2019[1,3],age_2020[1,3],age_2021[1,3]),
  
  "18 bis 20" = c(age_2012[2,3],age_2013[2,3],age_2014[2,3],age_2015[2,3],
                  age_2016[2,3],age_2017[2,3],age_2018[2,3],age_2019[2,3],age_2020[2,3],age_2021[2,3]),
  
  "21 bis 24" = c(age_2012[3,3],age_2013[3,3],age_2014[3,3],age_2015[3,3],
                  age_2016[3,3],age_2017[3,3],age_2018[3,3],age_2019[3,3],age_2020[3,3],age_2021[3,3]),
  
  "25 bis 29" = c(age_2012[4,3],age_2013[4,3],age_2014[4,3],age_2015[4,3],
                  age_2016[4,3],age_2017[4,3],age_2018[4,3],age_2019[4,3],age_2020[4,3],age_2021[4,3]),
  
  "30 bis 39" = c(age_2012[5,3],age_2013[5,3],age_2014[5,3],age_2015[5,3],
                  age_2016[5,3],age_2017[5,3],age_2018[5,3],age_2019[5,3],age_2020[5,3],age_2021[5,3]),
  
  "40 bis 49" = c(age_2012[6,3],age_2013[6,3],age_2014[6,3],age_2015[6,3],
                  age_2016[6,3],age_2017[6,3],age_2018[6,3],age_2019[6,3],age_2020[6,3],age_2021[6,3]),
  
  "50 bis 59" = c(age_2012[7,3],age_2013[7,3],age_2014[7,3],age_2015[7,3],
                  age_2016[7,3],age_2017[7,3],age_2018[7,3],age_2019[7,3],age_2020[7,3],age_2021[7,3]),
  
  "60 bis 64" = c(age_2012[8,3],age_2013[8,3],age_2014[8,3],age_2015[8,3],
                  age_2016[3,3],age_2017[3,3],age_2018[3,3],age_2019[3,3],age_2020[3,3],age_2021[3,3]),
  
  "ab 65" = c(age_2012[9,3],age_2013[9,3],age_2014[9,3],age_2015[9,3],
              age_2016[9,3],age_2017[9,3],age_2018[9,3],age_2019[9,3],age_2020[9,3],age_2021[9,3]),
  
  "unbekannt" = c(age_2012[10,3],age_2013[10,3],age_2014[10,3],age_2015[10,3],
                  age_2016[10,3],age_2017[10,3],age_2018[10,3],age_2019[10,3],age_2020[10,3],age_2021[10,3])
)

ts_age2 <- ts_age %>% pivot_longer(cols = bis.17:unbekannt, 
                                   names_to = "Alter", 
                                   values_to = "Prozent")
COLORS <- c(
  bis.17 = "red",
  X18.bis.20 = "orange",
  X21.bis.24 = "yellow",
  X25.bis.29 = "white",
  X30.bis.39 = "blue",
  X40.bis.49 = "green",
  X50.bis.59 = "darkgreen",
  X60.bis.64 = "magenta",
  ab.65 = "grey",
  unbekannt = "black"
)

ggplot(ts_age2, aes(x = Jahr, y = Prozent, group = Alter, color = Alter)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Alter",
    subtitle = "2012-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )



#Kinderanzahl 2010-2021

ts_children <- data.frame(
  
  Jahr = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
  
  "1 Kind" = c(children_2010[1,3],children_2011[1,3],children_2012[1,3],children_2013[1,3],children_2014[1,3],children_2015[1,3],
               children_2016[1,3],children_2017[1,3],children_2018[1,3],children_2019[1,3],children_2020[1,3],children_2021[1,3]),
  
  "2 Kinder" = c(children_2010[2,3],children_2011[2,3],children_2012[2,3],children_2013[2,3],children_2014[2,3],children_2015[2,3],
                 children_2016[2,3],children_2017[2,3],children_2018[2,3],children_2019[2,3],children_2020[2,3],children_2021[2,3]),
  
  "keine" = c(children_2010[3,3],children_2011[3,3],children_2012[3,3],children_2013[3,3],children_2014[3,3],children_2015[3,3],
              children_2016[3,3],children_2017[3,3],children_2018[3,3],children_2019[3,3],children_2020[3,3],children_2021[3,3]),
  
  "mehr als 2 Kinder" = c(children_2010[4,3],children_2011[4,3],children_2012[4,3],children_2013[4,3],children_2014[4,3],children_2015[4,3],
                          children_2016[4,3],children_2017[4,3],children_2018[4,3],children_2019[4,3],children_2020[4,3],children_2021[4,3])
)

ts_children2 <- ts_children %>% pivot_longer(cols = X1.Kind:mehr.als.2.Kinder, 
                                             names_to = "Kinderanzahl", 
                                             values_to = "Prozent")
COLORS <- c(
  X1.Kind = "red",
  X2.Kinder = "blue",
  keine = "green",
  mehr.als.2.Kinder = "orange"
)

ggplot(ts_children2, aes(x = Jahr, y = Prozent, group = Kinderanzahl, color = Kinderanzahl)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Kinderanzahl",
    subtitle = "2010-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


#Bildungsabschluss 2010-2021

ts_education <- data.frame(
  
  Jahr = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
  
  "abgeschlossene Berufsausbildung" = c(education_2010[1,3],education_2011[1,3],education_2012[1,3],education_2013[1,3],education_2014[1,3],education_2015[1,3],
                                        education_2016[1,3],education_2017[1,3],education_2018[1,3],education_2019[1,3],education_2020[1,3],education_2021[1,3]),
  
  "kein Abschluss" = c(education_2010[2,3],education_2011[2,3],education_2012[2,3],education_2013[2,3],education_2014[2,3],education_2015[2,3],
                       education_2016[2,3],education_2017[2,3],education_2018[2,3],education_2019[2,3],education_2020[2,3],education_2021[2,3]),
  
  "Schulabschluss" = c(education_2010[3,3],education_2011[3,3],education_2012[3,3],education_2013[3,3],education_2014[3,3],education_2015[3,3],
                       education_2016[3,3],education_2017[3,3],education_2018[3,3],education_2019[3,3],education_2020[3,3],education_2021[3,3]),
  
  "Studienabschluss" = c(education_2010[4,3],education_2011[4,3],education_2012[4,3],education_2013[4,3],education_2014[4,3],education_2015[4,3],
                         education_2016[4,3],education_2017[4,3],education_2018[4,3],education_2019[4,3],education_2020[4,3],education_2021[4,3])
)

ts_education2 <- ts_education %>% pivot_longer(cols = abgeschlossene.Berufsausbildung:Studienabschluss, 
                                               names_to = "Bildungsabschluss", 
                                               values_to = "Prozent")
COLORS <- c(
  abgeschlossene.Berufsausbildung = "red",
  kein.Abschluss = "blue",
  Schulabschluss = "green",
  Studienabschluss = "orange"
)

ggplot(ts_education2, aes(x = Jahr, y = Prozent, group = Bildungsabschluss, color = Bildungsabschluss)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Bildungsabschluss",
    subtitle = "2010-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )



#Migrationshintergrund 2010-2021

ts_migration <- data.frame(
  
  Jahr = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
  
  "Hilfesuchender" = c(migration_2010[1,3],migration_2011[2,3],migration_2012[1,3],migration_2013[2,3],migration_2014[2,3],migration_2015[2,3],
                       migration_2016[2,3],migration_2017[2,3],migration_2018[2,3],migration_2019[2,3],migration_2020[2,3],migration_2021[2,3]),
  
  "Partner" = c(migration_2010[2,3],migration_2011[4,3],migration_2012[2,3],migration_2013[4,3],migration_2014[4,3],migration_2015[4,3],
                migration_2016[4,3],migration_2017[4,3],migration_2018[4,3],migration_2019[4,3],migration_2020[4,3],migration_2021[4,3])
)

ts_migration2 <- ts_migration %>% pivot_longer(cols = Hilfesuchender:Partner, 
                                               names_to = "Migrationshintergrund", 
                                               values_to = "Prozent")
COLORS <- c(
  Hilfesuchender = "red",
  Partner = "blue"
)

ggplot(ts_migration2, aes(x = Jahr, y = Prozent, group = Migrationshintergrund, color = Migrationshintergrund)) +
  geom_line(size = 0.9) +
  geom_point(color = "#0099f9", size = 2) +
  scale_color_manual(values = COLORS) +
  labs(
    title = "Migrationshintergrund",
    subtitle = "2010-2021"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )
