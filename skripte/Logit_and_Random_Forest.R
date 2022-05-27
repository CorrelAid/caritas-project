library(tidyverse)
library(readxl)
library(dplyr)
library(randomForest)

# Run data cleaning script
source("C:/Users/Caritas_Data_Cleaning_2010_to_2021.R")

# Convert to dataframe
df <- bind_rows(raw_2021)

# Pre-process variables
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
values <- c(0,0,1)
fam <- values[match(df$Familiensituation, index)] 
df$ehe_familie <- factor(fam, order = TRUE)

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
kg <- values[match(df$Einkommensart..Rente, index)] 
df$kindergeld <- factor(kg, order = TRUE)
jc <- values[match(df$Schwierigkeiten.Erreichbarkeit, index)] 
df$jobcenter_schwierig <- factor(jc, order = TRUE)
krank <- values[match(df$Problemlage..Krankheit, index)] 
df$Krankheit <- factor(krank, order = TRUE)
beh <- values[match(df$Problemlage..Behinderung, index)] 
df$Behinderung <- factor(beh, order = TRUE)
cfs <- values[match(df$Corona.Schulden, index)] 
df$corona_fin_schlechter <- factor(cfs, order = TRUE)
cws <- values[match(df$Corona.Wohnschulden, index)] 
df$corona_wohnschulden <- factor(cws, order = TRUE)

# Remove missing values (we lose 12.2% of observations):
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
df <- df[!is.na(df$Krankheit),]
df <- df[!is.na(df$Behinderung),]
df <- df[!is.na(df$online_beratung),]

### Finanzhilfe
# A) Logistic regression
logit_finanzhilfe <- glm(as.factor(finanzhilfe) ~ kinder 
                         + allein + weiblich + alter 
                         + schulabschluss 
                         + ausbildung_oder_studium 
                         + alg2 + kindergeld 
                         + jobcenter_schwierig + Krankheit 
                         + Behinderung 
                         + corona_fin_schlechter 
                         + corona_wohnschulden 
                         + online_beratung, data = df, 
                         family = "binomial")
summary(logit_finanzhilfe)

# B) Run random forest algorithm
fit_RF_fin <- randomForest(as.factor(finanzhilfe) ~ kinder 
                       + ehe_familie + weiblich + alter 
                       + schulabschluss + 
                         ausbildung_oder_studium + alg2 
                       + kindergeld + jobcenter_schwierig 
                       + Krankheit + Behinderung 
                       + corona_fin_schlechter 
                       + corona_wohnschulden 
                       + online_beratung, data=df,
                       importance=TRUE,ntree=5000)
# Compute variable importance for each variable
importance(fit_RF_fin,type=1)
# Produce variable importance plot
varImpPlot(fit_RF_fin, main="Figure 1: Random Forest - Variable Importance Plot - Finanzhilfe")


### Antraghilfe
# A) Logistic regression
logit_antraghilfe <- glm(as.factor(antraghilfe) ~ kinder 
                         + allein + weiblich + alter 
                         + schulabschluss 
                         + ausbildung_oder_studium 
                         + alg2 + kindergeld 
                         + jobcenter_schwierig + Krankheit 
                         + Behinderung 
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
                           + Krankheit + Behinderung 
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
                         + jobcenter_schwierig + Krankheit 
                         + Behinderung 
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
                           + Krankheit + Behinderung 
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
                         + jobcenter_schwierig + Krankheit 
                         + Behinderung 
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
                           + Krankheit + Behinderung 
                           + corona_fin_schlechter 
                           + corona_wohnschulden 
                           + online_beratung, data=df,
                           importance=TRUE,ntree=5000)
# Compute variable importance for each variable
importance(fit_RF_wv,type=1)
# Produce variable importance plot
varImpPlot(fit_RF_wv, main="Figure 4: Random Forest - Variable Importance Plot - Weitervermittlung")

