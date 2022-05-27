##### Häufigkeitstabellen für Artikel #####

#Vorbereitung
library (tidyverse)

raw_2021 <- read.csv2("A:/Rohdaten/Rohdaten_2021.csv", encoding = "UTF-8")

#Gesamtdatensatz 2021
prop.table(table(raw_2021$Familiensituation))
prop.table(table(raw_2021$Geschlecht))

table(raw_2021$Einkommensart..ALG.I)
prop.table(table(raw_2021$Einkommensart..ALG.II))

table(raw_2021$Familiensituation, raw_2021$Einkommensart..ALG.II)

table(raw_2021$Familiensituation, raw_2021$Problemlage..Arbeitslosigkeit)

###Nur Arbeitslose (ALG II)
ALGII_2021 <- raw_2021 %>% 
  filter(Einkommensart..ALG.II == 'x')

table(ALGII_2021$Geschlecht)
prop.table(table(ALGII_2021$Geschlecht))

table(ALGII_2021$Familiensituation, ALGII_2021$Geschlecht)

table(ALGII_2021$Familiensituation, ALGII_2021$Länge.Bezug.von.ALGII..Kinderz..)

###Nur Alleinerziehende
Alleinerz <- raw_2021 %>% 
  filter(Familiensituation == 'Alleinerziehend (Ein-Eltern-Familie)')

prop.table(table(Alleinerz$Schwierigkeiten.Erreichbarkeit))
prop.table(table(Alleinerz$Schwierigkeiten.Leistungsbewilligung))
prop.table(table(Alleinerz$Schwierigkeiten.mit.Antragstellung))

###Alleinerziehende ALGII Beziehende
Allein_ALGII <- raw_2021 %>% 
  filter(Familiensituation == 'Alleinerziehend (Ein-Eltern-Familie)' & Einkommensart..ALG.II == 'x')

prop.table(table(Allein_ALGII$Schwierigkeiten.Erreichbarkeit))
prop.table(table(Allein_ALGII$Schwierigkeiten.Leistungsbewilligung))
prop.table(table(Allein_ALGII$Schwierigkeiten.mit.Antragstellung))

table(Allein_ALGII$Länge.Bezug.von.ALGII..Kinderz.., Allein_ALGII$Migrationshintergrund.des.Hilfesuchenden)

###Alleinerziehende ALGII Beziehende mit Migrationshintergrund
Mig <- Allein_ALGII %>% 
  filter(Migrationshintergrund.des.Hilfesuchenden == 'x')

table(Mig$Länge.Bezug.von.ALGII..Kinderz..)

