
library(readxl)
library(dplyr)
library(randomForest)
library(Hmisc)


### Daten vorbereiten für statistische Analysen
df <- bind_rows(raw_2021)
index <- c("x", "")  
values <- c(1,0)
mfh <- values[match(df$Maßnahmen..Finanzhilfe.Sachleistung, index)] 
df$finanzhilfe <- factor(mfh, order = TRUE)

mfh <- values[match(df$Maßnahmen..Finanzhilfe.Sachleistung, index)] 
df$finanzhilfe <- factor(mfh, order = TRUE)

mag <- values[match(df$Maßnahmen..Hilfe.bei.Anträgen.Korres..., index)] 
df$antraghilfe <- factor(mag, order = TRUE)

mkh <- values[match(df$Maßnahmen..Hilfe.bei.Korrespondenz, index)] 
df$korrespondenzhilfe <- factor(mkh, order = TRUE)

mwv <- values[match(df$Maßnahmen..Weitervermittlung, index)] 
df$weitervermittlung <- factor(mwv, order = TRUE)

mib <- values[match(df$Maßnahmen..Information...Beratung, index)] 
df$beratung <- factor(mib, order = TRUE)

index <- c("telefonisch", "per Email/online", "persönlich")  
values <- c(1,1,0)
ob <- values[match(df$Beratungsgespräch.findet.statt., index)] 
df$online_beratung <- factor(ob, order = TRUE)

index <- c("Keine", "1 Kind", "2 Kinder", "Mehr als 2 Kinder")  
values <- c(0,1,2,3)
df$kinder <- values[match(df$Anzahl.der.Kinder.im.Haushalt, index)] 

index <- c("Weiblich", "Männlich", "Divers")  
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
df$abschluss <- factor(ba, order = TRUE)

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

jca <- values[match(df$Schwierigkeiten.mit.Antragstellung, index)]
df$jobcenterantrag_schwierig <- factor(jca, order = TRUE)

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

sp <- values[match(df$Problemlage..Sprachprobleme, index)] 
df$sprachprob <- factor(sp, order = TRUE)

bp <- values[match(df$Problemlage..Umgang.mit.Behörden, index)] 
df$behoerden_probleme <- factor(bp, order = TRUE)

al <- values[match(df$Problemlage..Arbeitslosigkeit, index)] 
df$Arbeitslosigkeit <- factor(al, order = TRUE)

srp <- values[match(df$Problemlage..sozialrechtliche.Probleme, index)]
df$sozrechtprob <- factor(srp, order = TRUE)

sankt <- values[match(df$Problemlage..Sanktionen.nach.SGB.II, index)]
df$Sanktionen <- factor(sankt, order = TRUE)

famprob <- values[match(df$Problemlage..Partner...Erziehung..fam..., index)]
df$Familienprobleme <- factor(famprob, order = TRUE)

pflegebed <- values[match(df$Problemlage..Alter.Pflegebedürftigkeit, index)]
df$Pflegebeduerftigkeit <- factor(pflegebed, order = TRUE)

wohnen <- values[match(df$Problemlage..Bereich.Wohnen, index)]
df$Wohnprobleme <- factor(wohnen, order = TRUE)

schulden <- values[match(df$Problemlage..Schulden.allgemein, index)]
df$Schulden <- factor(schulden, order = TRUE)

index <- c("", "weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")  
values <- c(0,0,0,1,1)
alg24 <- values[match(df$Länge.Bezug.von.ALGII..Kinderz.., index)]
df$ALG_24plus <- factor(alg24, order = TRUE)

index <- c("", "weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")  
values <- c(0,0,0,0, 1)
alg36 <- values[match(df$Länge.Bezug.von.ALGII..Kinderz.., index)]
df$ALG_36plus <- factor(alg36, order = TRUE)

# Beobachtungen mit fehlenden Daten entfernen:
df <- df[!is.na(df$weiblich),]
df <- df[!is.na(df$finanzhilfe),]
df <- df[!is.na(df$kinder),]
df <- df[!is.na(df$ehe_familie),]
df <- df[!is.na(df$alter),]
df <- df[!is.na(df$abschluss),]
df <- df[!is.na(df$ausbildung_oder_studium),]
df <- df[!is.na(df$alg2),]
df <- df[!is.na(df$kindergeld),]
df <- df[!is.na(df$jobcenter_schwierig),]
df <- df[!is.na(df$jobcenterantrag_schwierig),]
df <- df[!is.na(df$corona_fin_schlechter),]
df <- df[!is.na(df$corona_wohnschulden),]
df <- df[!is.na(df$krankheit),]
df <- df[!is.na(df$behinderung),]
df <- df[!is.na(df$online_beratung),]
df <- df[!is.na(df$Wohnprobleme),]
df <- df[!is.na(df$Schulden),]
df <- df[!is.na(df$Pflegebeduerftigkeit),]
df <- df[!is.na(df$Familienprobleme),]
df <- df[!is.na(df$Sanktionen),]
df <- df[!is.na(df$Wohnprobleme),]
df <- df[!is.na(df$sozrechtprob),]
df <- df[!is.na(df$Arbeitslosigkeit),]
df <- df[!is.na(df$behoerden_probleme),]
df <- df[!is.na(df$sprachprob),]
df <- df[!is.na(df$ALG_36plus),]
df <- df[!is.na(df$migrationshintergrund),]

# Rentner*innen nicht miteinbeziehen in die Analyse
data_unter65 <- subset(df, alter < 9)

# Analyse mit Random Forest: Wer ist arbeitslos?
fit_RF <- randomForest(as.factor(Arbeitslosigkeit) ~ kinder + migrationshintergrund + weiblich + allein + alter + abschluss + behinderung + krankheit, data=data_unter65,importance=TRUE,ntree=5000)

# Analyse mit logistischer Regression: Wer ist arbeitslos?
log_mod <- glm(as.factor(Arbeitslosigkeit) ~ kinder + migrationshintergrund + weiblich + allein + alter + abschluss + behinderung + krankheit, data = data_unter65, family = binomial)
summary(log_mod)
