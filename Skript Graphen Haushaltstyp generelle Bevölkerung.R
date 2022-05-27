# Graph Haushaltsgröße

# Eurostat data - HH-Type over years
library(tidyverse)
library(forcats)

# Angegeben ist jeweils der Prozentsatz, den diese Haushaltsform ausmacht. Es addiert sich nicht auf 100% auf, da 
# Haushalte mit 3 und mehr Erwachsenen nicht erfasst wurden. 

# Familienstand für arme Familien (weniger als 60% des Medianeinkiommens)

ggplot(Familienstand_Armutsgrenze, aes(x = year, y = Prozent, group = Familienstand, color = Familienstand)) +
  geom_line(size = 0.9) +
  geom_point(color = "black", size = 1.5) +
  labs(
    title = "Familienstand für arme Familien",
    subtitle = "2010-2020"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )

# Familienstand total
ggplot(Familienstand_Alle, aes(x = year, y = Prozent, group = Familienstand, color = Familienstand)) +
  geom_line(size = 0.9) +
  labs(
    title = "Familienstand für alle Familien",
    subtitle = "2010-2020"
  ) +
  theme(
    plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5)
  )


