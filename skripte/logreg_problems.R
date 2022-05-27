library(tidyverse)

#Rohdaten 2021 einlesen (directory anpassen)

raw_2021 <- read.csv2("A:/Rohdaten/Rohdaten_2021.csv", encoding = "UTF-8")

problem_set <- raw_2021 %>% select(Geschlecht, Familiensituation, Alter = Alter.des.der.Klient.in, Bildungsabschluss, 
                                   Kinder = Anzahl.der.Kinder.im.Haushalt, Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, 
                                   Einkommenssituation.verschlechtert, Corona.Schulden, Corona.Wohnschulden, Corona.Enegrieschulden, 
                                   Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Sanktionen.nach.SGB.II, 
                                   Problemlage..Arbeitslosigkeit, Problemlage..Sprachprobleme, Problemlage..Partner...Erziehung..fam..., 
                                   Problemlage..Behinderung, Problemlage..Alter.Pflegebedürftigkeit, Problemlage..Krankheit, Problemlage..Bereich.Wohnen,
                                   Problemlage..Energieschulden, Problemlage..Mietschulden, Problemlage..Schulden.allgemein,
                                   Problemlage..sonstige.finanzielle.Prob.) %>%
  filter (Geschlecht != "", Geschlecht != "Divers", Familiensituation != "", Kinder != "", Alter != "", Bildungsabschluss != "") %>%
  mutate(Kinder = replace(Kinder, which(Kinder != "Keine"), "Ja")) %>%
  mutate(Kinder = replace(Kinder, which(Kinder == "Keine"), "Nein")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Ja")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund != "Ja"), "Nein")) %>%
  mutate(Einkommenssituation.verschlechtert = replace(Einkommenssituation.verschlechtert, which(Einkommenssituation.verschlechtert == "Ja"), 1)) %>%
  mutate(Einkommenssituation.verschlechtert = replace(Einkommenssituation.verschlechtert, which(Einkommenssituation.verschlechtert != 1), 0))

problem_set[problem_set == "x"] <- 1
problem_set[problem_set == ""] <- 0

for (i in 7:24) {
  problem_set[i]  <- as.numeric(as.character(problem_set[,i]))
}

# Einkommenssituation verschlechtert
log_mod1 <- glm(Einkommenssituation.verschlechtert ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod1)

#Corona Schulden
log_mod2 <- glm(Corona.Schulden ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod2)

#Corona Wohnschulden
log_mod3 <- glm(Corona.Wohnschulden ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod3)

#Corona Energieschulden
log_mod4 <- glm(Corona.Enegrieschulden ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod4)

#Sozialrechtliche Probleme
log_mod5 <- glm(Problemlage..sozialrechtliche.Probleme ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod5)

#Umgang mit Behörden
log_mod6 <- glm(Problemlage..Umgang.mit.Behörden ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod6)

#Sanktionen nach SGB II
log_mod7 <- glm(Problemlage..Sanktionen.nach.SGB.II ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod7)

#Arbeitslosigkeit
log_mod8 <- glm(Problemlage..Arbeitslosigkeit ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod8)

#Sprachprobleme
log_mod8 <- glm(Problemlage..Sprachprobleme ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod8)

#Partner/Erziehung/famili?re Probleme
log_mod9 <- glm(Problemlage..Partner...Erziehung..fam... ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                family = binomial(logit), data = problem_set)
summary(log_mod9)

#Behinderung
log_mod10 <- glm(Problemlage..Behinderung ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod10)

#Alter/Pflegebedürftigkeit
log_mod11 <- glm(Problemlage..Alter.Pflegebedürftigkeit ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod11)

#Krankheit
log_mod12 <- glm(Problemlage..Krankheit ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod12)

#Bereich Wohnen
log_mod13 <- glm(Problemlage..Bereich.Wohnen ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod13)

#Energieschulden
log_mod14 <- glm(Problemlage..Energieschulden ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod14)

#Mietschulden
log_mod15 <- glm(Problemlage..Mietschulden ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod15)

#Schulden allgemein
log_mod16 <- glm(Problemlage..Schulden.allgemein ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod16)

#Sonstige finanzielle Probleme
log_mod17 <- glm(Problemlage..sonstige.finanzielle.Prob. ~ Kinder + Migrationshintergrund + Geschlecht + Familiensituation + Alter + Bildungsabschluss,
                 family = binomial(logit), data = problem_set)
summary(log_mod17)
