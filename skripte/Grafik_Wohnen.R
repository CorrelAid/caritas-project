library(RColorBrewer)

# Analyse: Problemlage Mietschulden ####

## Recoding der Datensätze ####

### Berlin ####

#### 2011 ####
problems_2011_bb_raw <- raw_2011 %>% select(Nummer.BL, X8_1.sozialrechtliche.Probleme, X8_2.Partner..Erziehung..familiäre.Probleme, X8_3.Umgang.mit.Behörde, X8_4.psychische.Probleme, X8_5.Arbeitslosigkeit, 
                                            X8_6.Krankheit, X8_7.Sanktionen.nach.SGB.II, X8_8.Alter.Pflegebedürftigkeit, X8_9.Schulden, X8_10.Schwangerschaft, X8_11.Energie..odder.Mietschulden,
                                            X8_12.Behinderung, X8_13.sonstige.finanzielle.Schwierigkeiten, X8_14.Probleme.im.Bereich.Wohnen, X8_15.Sonstiges) %>%
  filter(Nummer.BL == "3") %>%
  mutate(X8_15.Sonstiges = replace(X8_15.Sonstiges, which(X8_15.Sonstiges != ''), 1)) %>%
  count(X8_1.sozialrechtliche.Probleme, X8_2.Partner..Erziehung..familiäre.Probleme, X8_3.Umgang.mit.Behörde, X8_4.psychische.Probleme, X8_5.Arbeitslosigkeit, 
        X8_6.Krankheit, X8_7.Sanktionen.nach.SGB.II, X8_8.Alter.Pflegebedürftigkeit, X8_9.Schulden, X8_10.Schwangerschaft, X8_11.Energie..odder.Mietschulden,
        X8_12.Behinderung, X8_13.sonstige.finanzielle.Schwierigkeiten, X8_14.Probleme.im.Bereich.Wohnen, X8_15.Sonstiges) %>%
  arrange(desc(X8_15.Sonstiges))


problems_2011_bb <- rbind(
  
  problems_2011_bb_raw %>% select(Problemlage = X8_1.sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_2.Partner..Erziehung..familiäre.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_3.Umgang.mit.Behörde) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_4.psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_5.Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_6.Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_7.Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_8.Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_9.Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_10.Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_11.Energie..odder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_12.Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_13.sonstige.finanzielle.Schwierigkeiten) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_14.Probleme.im.Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2011_bb_raw %>% select(Problemlage = X8_15.Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head()
)

#test <- problems_2011_bb %>% 
#  rows_update(n = 30, )


problems_2011_bb[7,1] <- "Sanktionen nach SGB II"
problems_2011_bb[7,2] <- 0
problems_2011_bb[7,3] <- 0.00
problems_2011_bb[8,1] <- "Alter/Pflegebedürftigkeit"
problems_2011_bb[8,2] <- 0
problems_2011_bb[8,3] <- 0.00

#### 2013 ####

problems_2013_bb_raw <- raw_2013 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
                                            Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  filter(Bundesland == "Berlin") %>%
  
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
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)

problems_2013_bb <- rbind(
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Energie..oder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2013_bb_raw %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head()
)



#### 2014 ####
problems_2014_bb_raw <- raw_2014 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
                                            Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  filter(Bundesland == "Berlin") %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)


problems_2014_bb <- rbind(
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Energie..oder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2014_bb_raw %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head()
)
# Korrektur: Prozent
problems_2014_bb <- problems_2014_bb %>% mutate(Prozent = round((n/(sum(n)))*100,2))

#### 2015 ####

problems_2015_bb_raw <- raw_2015 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
                                            Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  filter(Bundesland == "Berlin") %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energie..oder.Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)


problems_2015_bb <- rbind(
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Energie..oder.Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energie- oder Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2015_bb_raw %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head()
)


#### 2016 ####

problems_2016_bb_raw <- raw_2016 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                            Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  filter(Bundesland == "Berlin") %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)


problems_2016_bb <- rbind(
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2016_bb_raw %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head()
)

#### 2017 ####

problems_2017_bb_raw <- raw_2017 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                            Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges) %>%
  filter(Bundesland == "Berlin") %>%
  mutate(Problemlage..Sonstiges = replace(Problemlage..Sonstiges, which(Problemlage..Sonstiges != ''), 1)) %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sontige.finanzielle.Prob..., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Psychische.Probleme, Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Schwangerschaft, Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sonstiges)

problems_2017_bb <- rbind(
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Psychische.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "psychische Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Schulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Schwangerschaft) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schwangerschaft")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..sontige.finanzielle.Prob...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2017_bb_raw %>% select(Problemlage = Problemlage..Sonstiges) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage != ''), 1)) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstiges")) %>%
    count(Problemlage) %>%
    arrange(desc(Problemlage)) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head()
)


#### 2018 ####

problems_2018_bb_raw <- raw_2018 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                            Problemlage..Finanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Behinderung, Problemlage..Wohnen) %>%
  filter(Bundesland == "Berlin") %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..Finanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Wohnen)


problems_2018_bb <- rbind(
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Krankheit..auch.psychische.probleme.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Finanzielle.Schwierigkeiten) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2018_bb_raw %>% select(Problemlage = Problemlage..Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail()
  
)

problems_2018_bb[10,1] <- "Mietschulden"
problems_2018_bb[10,2] <- 0
problems_2018_bb[10,3] <- 0.00

#### 2019 ####

problems_2019_bb_raw <- raw_2019 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                            Problemlage..sonstige.inanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Behinderung, Problemlage..Wohnen, Problemlage..Sprachprobleme) %>%
  filter(Bundesland == "Berlin") %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sonstige.inanzielle.Schwierigkeiten, Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit..auch.psychische.probleme., Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Wohnen, Problemlage..Sprachprobleme)

problems_2019_bb <- rbind(
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Krankheit..auch.psychische.probleme.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..sonstige.inanzielle.Schwierigkeiten) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2019_bb_raw %>% select(Problemlage = Problemlage..Sprachprobleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == 1), "Sprachprobleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail()
  
)

#### 2020 ####

problems_2020_bb_raw <- raw_2020 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                            Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme) %>%
  filter(Bundesland == "Berlin") %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme)

problems_2020_bb <- rbind(
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_head(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..sonstige.finanzielle.Prob.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2020_bb_raw %>% select(Problemlage = Problemlage..Sprachprobleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "Ja"), "Sprachprobleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail()
  
)

#### 2021 ####


problems_2021_bb_raw <- raw_2021 %>% select(Bundesland, Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
                                            Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
                                            Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
                                            Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
                                            Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme) %>%
  filter(Bundesland == "Berlin") %>%
  count(Problemlage..sozialrechtliche.Probleme, Problemlage..Umgang.mit.Behörden, Problemlage..Arbeitslosigkeit,
        Problemlage..Sanktionen.nach.SGB.II, Problemlage..Schulden.allgemein, Problemlage..Energieschulden, Problemlage..Mietschulden,
        Problemlage..sonstige.finanzielle.Prob., Problemlage..Partner...Erziehung..fam...,
        Problemlage..Krankheit, Problemlage..Alter.Pflegebedürftigkeit,
        Problemlage..Behinderung, Problemlage..Bereich.Wohnen, Problemlage..Sprachprobleme)


problems_2021_bb <- rbind(
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..sozialrechtliche.Probleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "sozialrechtliche Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Partner...Erziehung..fam...) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Partner/Erziehung/familiäre Probleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Umgang.mit.Behörden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Umgang mit Behörde")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Arbeitslosigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Arbeitslosigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Krankheit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Krankheit (auch psychische Probleme)")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Sanktionen.nach.SGB.II) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Sanktionen nach SGB II")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Alter.Pflegebedürftigkeit) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Alter/Pflegebedürftigkeit")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Schulden.allgemein) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Schulden allgemein")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Energieschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Energieschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Mietschulden) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Mietschulden")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Behinderung) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Behinderung")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..sonstige.finanzielle.Prob.) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Sonstige finanzielle Schwierigkeiten")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Bereich.Wohnen) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Probleme im Bereich Wohnen")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail(),
  
  problems_2021_bb_raw %>% select(Problemlage = Problemlage..Sprachprobleme) %>%
    mutate(Problemlage = replace(Problemlage, which(Problemlage == "x"), "Sprachprobleme")) %>%
    count(Problemlage) %>%
    mutate(Prozent = round((n/(sum(n)))*100,2)) %>%
    slice_tail()
  
)

#### Zusammenfassen von Kategorien #####

problems_2011_bb <- problems_2011_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2013_bb <- problems_2013_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2014_bb <- problems_2014_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2015_bb <- problems_2015_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2016_bb <- problems_2016_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2017_bb <- problems_2017_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% c("Krankheit", "psychische Probleme") ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2018_bb <- problems_2018_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    TRUE ~ Problemlage),
  ) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2019_bb <- problems_2019_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2020_bb <- problems_2020_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    Problemlage %in% "Krankheit" ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)

problems_2021_bb <- problems_2021_bb %>% 
  mutate(Problemlage = case_when(
    Problemlage %in% c("Energieschulden", "Mietschulden") ~ "Energie- oder Mietschulden",
    Problemlage %in% "Schulden allgemein" ~ "Schulden",
    Problemlage %in% "Krankheit" ~ "Krankheit (auch psychische Probleme)",
    TRUE ~ Problemlage)) %>%
  group_by(Problemlage) %>%
  summarize_all(sum)


## Zeitreihe (Berlin) ####

ts_problems_berlin <- list(problems_2011_bb, problems_2013_bb, problems_2014_bb, problems_2015_bb,
                           problems_2016_bb, problems_2017_bb, problems_2018_bb, problems_2019_bb,
                           problems_2020_bb, problems_2021_bb) %>%
  reduce(left_join, by = "Problemlage")


ts_problems_berlin <- ts_problems_berlin %>% select(Problemlage, contains("Prozent"))
colnames(ts_problems_berlin) <- c("Problemlage", "2011", "2013", "2014", "2015", "2016",
                                  "2017", "2018", "2019", "2020", "2021")

ts_problems_berlin2 <- ts_problems_berlin %>% pivot_longer(!Problemlage, 
                                                           names_to = "Jahr", 
                                                           values_to = "Prozent") %>%
  mutate(Problemlage = factor(Problemlage,
                              levels = c("Alter/Pflegebedürftigkeit", "Behinderung", 
                                         "Krankheit (auch psychische Probleme)", "Schwangerschaft", 
                                         "Arbeitslosigkeit", "Sanktionen nach SGB II", "sozialrechtliche Probleme", 
                                         "Umgang mit Behörde", "Schulden", "Energie- oder Mietschulden", 
                                         "Sonstige finanzielle Schwierigkeiten", "Probleme im Bereich Wohnen", 
                                         "Partner/Erziehung/familiäre Probleme", "Sonstiges")))
structure(ts_problems_berlin2$Problemlage)

problems_bb <- ts_problems_berlin2 %>%
  filter(Problemlage == "sozialrechtliche Probleme" |
           Problemlage == "Umgang mit Behörde" |
           Problemlage == "Energie- oder Mietschulden" |
           Problemlage == "Probleme im Bereich Wohnen") %>%
  ggplot(aes(x = Jahr, y = Prozent, group = Problemlage, color = Problemlage)) +
  geom_line(size = 1.1) + 
  theme_linedraw() +
  labs(title = "Problemlagen", subtitle = "Berlin | 2011-2021") +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)) +
  scale_color_brewer(palette = "Paired")
#ggsave(problems_bb, file = "problems_bb.jpeg", unit = "cm", height = 12, width = 21, dpi = 500)

