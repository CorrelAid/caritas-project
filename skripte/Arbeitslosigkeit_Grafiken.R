library (tidyverse)
library(gridExtra)


raw_2021 <- read.csv2("A:/Rohdaten/Rohdaten_2021.csv", encoding = "UTF-8")


# Caritas Sample vs. deutschlandweite Zahlen: Schulabschluss ####

Arbeitslosigkeit <- rep("Deutschlandweit", times = 2264)
Bildungsabschluss <- c(rep("kein Abschluss", times = 469), rep("min. Schulabschluss", times = 1795))
df1 <- as.data.frame(cbind(Arbeitslosigkeit, Bildungsabschluss))

unemployed_education2_2021 <- raw_2021 %>% select(Arbeitslosigkeit = Problemlage..Arbeitslosigkeit, Bildungsabschluss) %>%
  filter(Arbeitslosigkeit == 'x', Bildungsabschluss != '') %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss != "kein Abschluss"), "min. Schulabschluss")) %>%
  mutate(Arbeitslosigkeit = replace(Arbeitslosigkeit, which(Arbeitslosigkeit == "x"), "Caritas Sample"))

df2 <- bind_rows(unemployed_education2_2021, df1)

positions <- c("Caritas Sample", "Deutschlandweit")

ggplot(df2, aes(x = Arbeitslosigkeit, fill = Bildungsabschluss)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Arbeitslose ohne Schulabschluss"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )

# Caritas Sample vs. deutschlandweite Zahlen: Geschlecht ####

Arbeitslosigkeit <- rep("Deutschlandweit", times = 2614)
Geschlecht <- c(rep("Männlich", times = 1455), rep("Weiblich", times = 1159))
df1 <- as.data.frame(cbind(Arbeitslosigkeit, Geschlecht))

unemployed_gender_2021 <- raw_2021 %>% select(Arbeitslosigkeit = Problemlage..Arbeitslosigkeit, Geschlecht) %>%
  filter(Arbeitslosigkeit == 'x', Geschlecht != '', Geschlecht != "Divers") %>%
  mutate(Arbeitslosigkeit = replace(Arbeitslosigkeit, which(Arbeitslosigkeit == "x"), "Caritas Sample"))

df2 <- bind_rows(unemployed_gender_2021, df1)

positions <- c("Caritas Sample", "Deutschlandweit")

ggplot(df2, aes(x = Arbeitslosigkeit, fill = Geschlecht)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Arbeitslose nach Geschlecht"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )

# Caritas Sample vs. deutschlandweite Zahlen: Geschlecht und Schulabschluss ####

Arbeitslosigkeit <- rep("Deutschlandweit", times = 2265)
Geschlecht_Bildung <- c(rep("Männlich/ohne Abschluss", times = 265), 
                        rep("Männlich/mit Abschluss", times = 1009), 
                        rep("Weiblich/ohne Abschluss", times = 204),
                        rep("Weiblich/mit Abschluss", times = 787)
)
df1 <- as.data.frame(cbind(Arbeitslosigkeit, Geschlecht_Bildung))

unemployed_gender_educ_2021 <- raw_2021 %>% select(Arbeitslosigkeit = Problemlage..Arbeitslosigkeit, Geschlecht, Bildungsabschluss) %>%
  filter(Arbeitslosigkeit == 'x', Geschlecht != '', Geschlecht != "Divers", Bildungsabschluss != "") %>%
  mutate(Bildungsabschluss = replace(Bildungsabschluss, which(Bildungsabschluss != "kein Abschluss"), "min. Schulabschluss")) %>%
  mutate(Arbeitslosigkeit = replace(Arbeitslosigkeit, which(Arbeitslosigkeit == "x"), "Caritas Sample")) %>%
  count(Geschlecht, Bildungsabschluss)

Arbeitslosigkeit <- rep("Caritas Sample", times = sum(unemployed_gender_educ_2021[,3]))
Geschlecht_Bildung <- c(rep("Männlich/ohne Abschluss", times = unemployed_gender_educ_2021[1,3]), 
                        rep("Männlich/mit Abschluss", times = unemployed_gender_educ_2021[2,3]), 
                        rep("Weiblich/ohne Abschluss", times = unemployed_gender_educ_2021[3,3]),
                        rep("Weiblich/mit Abschluss", times = unemployed_gender_educ_2021[4,3])
)
df2 <- as.data.frame(cbind(Arbeitslosigkeit, Geschlecht_Bildung))

df2 <- bind_rows(df2, df1)

positions <- c("Caritas Sample", "Deutschlandweit")

ggplot(df2, aes(x = Arbeitslosigkeit, fill = Geschlecht_Bildung)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Arbeitslose nach Geschlecht und Schulabschluss"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )


# Länge ALGII-Bezug: Bildungsabschluss ####

algII_education_2021 <- raw_2021 %>% select(Bildungsabschluss, ALGII = Länge.Bezug.von.ALGII..Kinderz..) %>%
  filter(ALGII != '', Bildungsabschluss != '')

positions <- c("weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")

ggplot(algII_education_2021, aes(x = ALGII, fill = Bildungsabschluss)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Länge ALGII-Bezug nach Bildungsgrad (2021)"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),  
  )

# Länge ALGII-Bezug: Migrationshintergrund ####

algII_migration_2021 <- raw_2021 %>% select(Migrationshintergrund = Migrationshintergrund.des.Hilfesuchenden, ALGII = Länge.Bezug.von.ALGII..Kinderz..) %>%
  filter(ALGII != '') %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == "x"), "Migrationshintergrund")) %>%
  mutate(Migrationshintergrund = replace(Migrationshintergrund, which(Migrationshintergrund == ''), "kein Migrationshintergrund"))

positions <- c("weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")

ggplot(algII_migration_2021, aes(x = ALGII, fill = Migrationshintergrund)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Länge ALGII-Bezug nach Bildungsgrad (2021)"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )

# Länge ALGII-Bezug: Geschlecht ####

algII_gender_2021 <- raw_2021 %>% select(Geschlecht, ALGII = Länge.Bezug.von.ALGII..Kinderz..) %>%
  filter(ALGII != '', Geschlecht != '', Geschlecht != "Divers")

positions <- c("weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")

ggplot(algII_gender_2021, aes(x = ALGII, fill = Geschlecht)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Länge ALGII-Bezug nach Geschlecht (2021)"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )

# Länge ALGII-Bezug: Familiensituation ####

algII_children_2021 <- raw_2021 %>% select(Geschlecht, Familiensituation, ALGII = Länge.Bezug.von.ALGII..Kinderz..) %>%
  filter (ALGII != '', Familiensituation != '')

positions <- c("weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")

ggplot(algII_children_2021, aes(x = ALGII, fill = Familiensituation)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Länge ALGII-Bezug nach Familiensituation (2021)"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )

# Länge ALGII-Bezug: Familiensituation Geschlecht ####

algII_male_children_2021 <- algII_children_2021 %>% filter (Geschlecht == "Männlich")

positions <- c("weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")

ggplot(algII_male_children_2021, aes(x = ALGII, fill = Familiensituation)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Länge ALGII-Bezug nach Familiensituation (Männlich) (2021)"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )


algII_female_children_2021 <- algII_children_2021 %>% filter (Geschlecht == "Weiblich")

positions <- c("weniger als 12 Monate", "12-24 Monate", "25-36 Monate", "mehr als 36 Monate")

ggplot(algII_female_children_2021, aes(x = ALGII, fill = Familiensituation)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Länge ALGII-Bezug nach Familiensituation (Weiblich) (2021)"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )

# Anhang: Familiensituation nach Geschlecht ####

gender_children_2021 <- raw_2021 %>% select(Geschlecht, Familiensituation) %>%
  filter (Geschlecht != '', Geschlecht != "Divers", Familiensituation != '')

positions <- c("Männlich", "Weiblich")

ggplot(gender_children_2021, aes(x = Geschlecht, fill = Familiensituation)) + 
  scale_x_discrete(limits = positions) + 
  geom_bar(position ="fill") +
  labs(
    title = "Familiensituation nach Geschlecht (2021)"
  ) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
  )
