# Eurostat Data
# Age in crude age groups
# 2020 is break in series 




library(tidyverse)


setwd("~/CorrelAid/Referenzwerte_SILC/Age/ilc_pees01")


# Rename & Verkleinern

data_haushalt2 <- read.csv2("ilc_pees01_1_Data.csv",na="NA", sep=",", dec=",")

df = subset(data_haushalt2, select= -c(GEO, Flag.and.Footnotes))


# LEV_DEPR & WORKINT anpassen

df1 <- tidyr::pivot_wider(df, names_from = LEV_DEPR, values_from = Value)

df2 <- rename(df1, Drop1 = "Severe", Drop2 = "Non severe")

df2$Drop1 <- as.numeric(df2$Drop1)
df2$Drop2 <- as.numeric(df2$Drop2)

df3 <- mutate(df2, 
              Value = Drop1 + Drop2)

df3$Drop1 <- NULL
df3$Drop2 <- NULL


df4 <- tidyr::pivot_wider(df3, names_from = WORKINT, values_from = Value)

df5 <- rename(df4, Drop1 = "Not very low work intensity (0.2-1) and not applicable", Drop2 = "Very low work intensity (0-0.2)")

df6 <- mutate(df5, 
       Value = Drop1 + Drop2)

df6$Drop1 <- NULL
df6$Drop2 <- NULL
df6$SEX <- NULL

df7 <- df6%>% mutate(Altersgruppe = fct_recode(AGE,
                                                           "Jünger als 18" = "Less than 18 years",
                                                           "18 bis 49" = "From 18 to 49 years", 
                                                           "50 bis 64" = "From 50 to 64 years", 
                                                           "65 und älter" = "65 years or over"))



df8 <- tidyr::pivot_wider(df7, names_from = YN_RSKPOV, values_from = Value)

df9 <- rename(df8, Arm = "At risk of poverty", Nicht_arm = "Not at risk of poverty")
df10 <- mutate(df9, Total = Arm + Nicht_arm)

df10$Nicht_arm <- NULL

df11 <- df10 %>% group_by(TIME) %>% mutate(alle_arm = sum(Arm), alle_total = sum(Total))

df11$Total_Prozent <- (df11$Total / df11$alle_total) * 100 
df11$Arm_Prozent <- (df11$Arm / df11$alle_arm) * 100

df11$Arm <- NULL
df11$Total <- NULL
df11$alle_arm <- NULL
df11$alle_total <- NULL
df11$AGE <- NULL
df11$UNIT <- NULL

Alter <- df11

# Graphen erstellen


ggplot(Alter, aes(x = TIME, y = Total_Prozent, group = Altersgruppe, color = Altersgruppe)) +
  geom_line(size = 0.9) +
  scale_x_continuous(breaks = seq(2010, 2020, 2)) +
  scale_fill_discrete(limits= c("65 und älter", "50 bis 64", "18 bis 49", "Jünger als 18")) +
  geom_point(color = "black", size = 1.5) +
  labs(
    title = "Altersverteilung, alle Menschen",
    subtitle = "2010-2020",
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


 ggplot(Alter, aes(x = TIME, y = Arm_Prozent, group = Altersgruppe, color = Altersgruppe)) +
  geom_line(size = 0.9) +
   scale_x_continuous(breaks = seq(2010, 2020, 2)) +
   scale_fill_discrete(limits= c("65 und älter", "50 bis 64", "18 bis 49", "Jünger als 18")) +
  geom_point(color = "black", size = 1.5) +
  labs(
    title = "Altersverteilung, arme und armutsgefährdete Menschen",
    subtitle = "2010-2020",
    x = "Jahr", y = "% der Bevölkerungsgruppe"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )

# Das ist nicht spannend... 




