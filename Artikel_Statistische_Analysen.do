clear
import delimited "C:\Rohdaten_2021.csv", delimiter(";") 

/* Datenaufbereitung */

rename maãnahmenfinanzhilfesachleistung finhilfe

gen weiblich = 0 
replace weiblich = 1 if geschlecht == "Weiblich"

gen ehe_familie = 0 
replace ehe_familie = 1 if familiensituation == "Ehe/Partnerschaft/Familie"

gen kinder = 0
replace kinder = 1 if anzahlderkinderimhaushalt == "1 Kind"
replace kinder = 2 if anzahlderkinderimhaushalt == "2 Kinder"
replace kinder = 3 if anzahlderkinderimhaushalt == "Mehr als 2 Kinder"

gen alleinerziehend = 0
replace alleinerziehend = 1 if familiensituation == "Alleinerziehend (Ein-Eltern-Familie)"

gen alleinstehend = 0 
replace alleinstehend = 1 if familiensituation == "Alleinstehend (Einpersonenhaushalt)"

gen alg_24plus = 0 
replace alg_24plus = 1 if lãngebezugvonalgiikinderz == "25-36 Monate" | lãngebezugvonalgiikinderz == "mehr als 36 Monate"

gen alg_36plus = 0 
replace alg_36plus = 1 if lãngebezugvonalgiikinderz == "mehr als 36 Monate"

encode alterdesderklientin, generate(alter)
replace alter = 0 if alter == 9 //bis 17
replace alter = . if alter == 10 //Alter unbekannt

gen abschluss = 0 
replace abschluss = 1 if bildungsabschluss == "Schulabschluss" | bildungsabschluss == "Studienabschluss" | bildungsabschluss == "abgeschlossene  Berufsausbildung"

gen mig_hilfesuchender = 0 
replace mig_hilfesuchender = 1 if migrationshintergrunddeshilfesuc == "1"

gen krankheit = 0
replace krankheit = 1 if problemlagekrankheit == "x"

gen behinderung = 0 
replace behinderung = 1 if problemlagebehinderung == "x"

gen arbeitslos = 0 
replace arbeitslos = 1 if lãngebezugvonalgiikinderz == "weniger als 12 Monate" | lãngebezugvonalgiikinderz == "12-24 Monate" | lãngebezugvonalgiikinderz == "25-36 Monate" | lãngebezugvonalgiikinderz == "mehr als 36 Monate"

gen jobcenter_probleme = 0 
replace jobcenter_probleme = 1 if schwierigkeitenerreichbarkeit == "x"

gen keine_kinder = 0
replace keine_kinder = 1 if anzahlderkinderimhaushalt == "Keine"

gen viele_kinder = 0 
replace viele_kinder = 1 if anzahlderkinderimhaushalt == "Mehr als 2 Kinder"

gen alg2dauer = 0 
replace alg2dauer = 1 if lãngebezugvonalgiikinderz == "weniger als 12 Monate" 
replace alg2dauer = 2 if lãngebezugvonalgiikinderz == "12-24 Monate" 
replace alg2dauer = 3 if lãngebezugvonalgiikinderz == "25-36 Monate" 
replace alg2dauer = 4 if lãngebezugvonalgiikinderz == "mehr als 36 Monate"

bys alleinerziehend mig_hi*: su alg_24plus
bys alleinerziehend abschluss: su alg_24plus
bys alleinerziehend viele_kinder: su alg_24plus

/* Statistische Analysen */
// (1) Wer wird arbeitslos?
reg arbeitslos keine_kinder viele_kinder alter abschluss mig_hi* abschluss#mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit
// "Viele Kinder"-Effekt
reg arbeitslos keine_kinder viele_kinder alter abschluss mig_hi* abschluss#mig_hi* weiblich alleinstehend behinderung krankheit if alleinerziehend == 1

// (2) Wer ist langfristig arbeitslos?
reg alg_36plus keine_kinder viele_kinder alter abschluss mig_hi* abschluss#mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit
// "Viele Kinder"-Effekt
reg alg_36plus keine_kinder viele_kinder alter abschluss mig_hi* abschluss#mig_hi* weiblich alleinstehend behinderung krankheit if alleinerziehend == 1

// (3) Wer hat Probleme mit dem Jobcenter?
reg jobcenter_probleme keine_kinder viele_kinder alter abschluss mig_hi* abschluss#mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit
logit alg_24plus keine_kinder viele_kinder alter abschluss mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit
logit alg_36plus keine_kinder viele_kinder alter abschluss mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit
logit jobcenter_probleme keine_kinder viele_kinder alter abschluss mig_hi* abschluss#mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit

/* Weitere (für den Artikel nicht relevante) Analysen */
// Mit Interaktionsterm
ologit alg2dauer keine_kinder viele_kinder alter abschluss mig_hi* abschluss#mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit, nolog
tab alg2dauer
margins, dydx(alleinerziehend)

tabulate alg2dauer alleinerziehend
tab viele_kinder alleinerziehend

// Ohne Interaktionsterm 
ologit alg2dauer keine_kinder viele_kinder alter abschluss mig_hi* weiblich alleinstehend alleinerziehend behinderung krankheit, nolog
tab alg2dauer
margins, dydx(*)


