library(tidyverse)
library(grid)
library(gridExtra)
library(ggrepel)

raw_2021 <- read.csv2("A:/Rohdaten/Rohdaten_2021.csv", encoding = "UTF-8")


##### Balkendiagramm #####

farben <- setNames(c("#E6B8BF","#CC7A88","#B33E52", "#990F26", "#999999"), reihenfolge_laenge)

reihenfolge_laenge <- c("weniger als 12 Monate", "12-24 Monate","25-36 Monate","mehr als 36 Monate","kein ALG II-Bezug")

alleinerz_l <- raw_2021 %>%
  filter(Familiensituation == "Alleinerziehend (Ein-Eltern-Familie)") %>%
  mutate(Laenge = case_when(Einkommensart..ALG.II == 'x' & Länge.Bezug.von.ALGII..Kinderz.. != '' ~ Länge.Bezug.von.ALGII..Kinderz..,
                            Länge.Bezug.von.ALGII..Kinderz..== '' ~ "kein ALG II-Bezug",
                            TRUE ~ "keine Angabe")) %>%
  group_by(Laenge) %>%
  count()

alleinerz_l <- left_join(data.frame(Laenge = reihenfolge_laenge),
                         alleinerz_l,
                         by = "Laenge")


alleinerz_l$perc = alleinerz_l$n / sum(alleinerz_l$n) * 100


alleinerz_l2 <- alleinerz_l
alleinerz_l2$Laenge <- factor(alleinerz_l$Laenge,
                  levels = rev(reihenfolge_laenge))


balken_allein <- ggplot(data = alleinerz_l2, aes(x=Laenge, y=perc)) +
  geom_bar(stat = "identity", fill=farben) +
  coord_flip() +
  labs(x = "ALG II-Bezug", y = "Prozent",
       title = "Alleinerziehende") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 12)) +
  geom_label(aes(label = paste0(round(perc,0)," %"),
                 hjust = "right",
                 colour = Laenge),
             #color = "white",
             fontface = "bold",
             show.legend = FALSE) +
  scale_colour_manual(values = farben)

balken_allein

#####

andere_l <- raw_2021 %>%
  filter(Familiensituation != "Alleinerziehend (Ein-Eltern-Familie)") %>%
  mutate(Laenge = case_when(Einkommensart..ALG.II == 'x' & Länge.Bezug.von.ALGII..Kinderz.. != '' ~ Länge.Bezug.von.ALGII..Kinderz..,
                            Länge.Bezug.von.ALGII..Kinderz..== '' ~ "kein ALG II-Bezug",
                            TRUE ~ "keine Angabe")) %>%
  group_by(Laenge) %>%
  count()

andere_l <- left_join(data.frame(Laenge = reihenfolge_laenge),
                         andere_l,
                         by = "Laenge")


andere_l$perc = andere_l$n / sum(andere_l$n) * 100


andere_l2 <- andere_l
andere_l2$Laenge <- factor(andere_l$Laenge,
                              levels = rev(reihenfolge_laenge))


balken_andere <-ggplot(data = andere_l2, aes(x=Laenge, y=perc)) +
  geom_bar(stat = "identity", fill=farben) +
  coord_flip() +
  labs(x = "ALG II-Bezug", y = "Prozent",
       title = "Alleinstehende / In Partnerschaft") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(vjust = 0, size = 10),
        axis.title.y = element_text(vjust = 2, size = 12)) +
  geom_label(aes(label = paste0(round(perc,0)," %"),
                 hjust = "right",
                 colour = Laenge),
             #color = "white",
             fontface = "bold",
             show.legend = FALSE) +
  scale_colour_manual(values = farben)

#####

grid.arrange(balken_allein, balken_andere, nrow = 2,
             top = textGrob("Bezug von Arbeitslosengeld II nach Familienstand",gp=gpar(fontsize=14,font=2)))

###### Donut Charts (nicht verwendet) #####

### Alleinerziehende: ALG II Bezug
#Aggregation
alleinerz <- raw_2021 %>%
  filter(Familiensituation == "Alleinerziehend (Ein-Eltern-Familie)") %>%
  mutate(ALGII_Bezug = case_when(Einkommensart..ALG.II == 'x' ~ "ALG II",
                                 TRUE ~ "kein ALG II")) %>%
  group_by(ALGII_Bezug) %>%
  count()

# Compute percentages
alleinerz$perc = alleinerz$n / sum(alleinerz$n)

# Compute the cumulative percentages (top of each rectangle)
alleinerz$ymax = cumsum(alleinerz$perc)

# Compute the bottom of each rectangle
alleinerz$ymin = c(0, head(alleinerz$ymax, n=-1))

#Label
alleinerz$labelPosition <- (alleinerz$ymax + alleinerz$ymin) / 2
alleinerz$label <- paste0(alleinerz$ALGII_Bezug, ":\n", round(alleinerz$perc*100,0), "%")

#simple donut chart
ggplot(alleinerz, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=ALGII_Bezug)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=5) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  scale_fill_manual(values = c("#ba0f30", "#999999")) +
  theme(legend.position = "none")

### Alleinerziehende: Länge Bezug
reihenfolge_laenge <- c("weniger als 12 Monate", "12-24 Monate","25-36 Monate","mehr als 36 Monate","NA")

alleinerz_l <- raw_2021 %>%
  filter(Familiensituation == "Alleinerziehend (Ein-Eltern-Familie)") %>%
  mutate(Laenge = case_when(Einkommensart..ALG.II == 'x' & Länge.Bezug.von.ALGII..Kinderz.. != '' ~ Länge.Bezug.von.ALGII..Kinderz..,
                            Länge.Bezug.von.ALGII..Kinderz..== '' ~ "NA",
                            TRUE ~ "NA")) %>%
  group_by(Laenge) %>%
  count()

alleinerz_l <- left_join(data.frame(Laenge = reihenfolge_laenge),
                         alleinerz_l,
                         by = "Laenge")


# Compute percentages
alleinerz_l$perc = alleinerz_l$n / sum(alleinerz_l$n)

# Compute the cumulative percentages (top of each rectangle)
alleinerz_l$ymax = cumsum(alleinerz_l$perc)

# Compute the bottom of each rectangle
alleinerz_l$ymin = c(0, head(alleinerz_l$ymax, n=-1))

#Label
alleinerz_l$labelPosition <- (alleinerz_l$ymax + alleinerz_l$ymin) / 2
alleinerz_l$label <- paste0(alleinerz_l$Laenge, ":\n", round(alleinerz_l$perc*100,0), "%")

ggplot(alleinerz_l, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Laenge)) +
  geom_rect() +
  geom_label_repel(aes(x = 3.5, y = labelPosition, label = label),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  scale_fill_manual(values = c("#f37970", "#e43d40", "#ba0f30", "#999999","#fabec0")) +
  theme(legend.position = "none")
