# (0) Lade die notwendigen packages:
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(haven)
library(data.table)
library(labelled)
library(panelr)
library(stringr)
library(stargazer)
library(gridExtra)

##
# (1) Daten einlesen

shp14 <- readRDS("shp14.rds")
male_data <- readRDS("male_data.rds")
female_data <- readRDS("female_data.rds")
links_data <- readRDS("links_data.rds")
mitte_data <- readRDS("mitte_data.rds")
rechts_data <- readRDS("rechts_data.rds")
tiefer_stundenlohn <- readRDS("tiefer_stundenlohn.rds")
mittlerer_stundenlohn <- readRDS("mittlerer_stundenlohn.rds")
hoher_stundenlohn <- readRDS("hoher_stundenlohn.rds")
kleines_interesse <- readRDS("kleines_interesse.rds")
mittleres_interesse <- readRDS("mittleres_interesse.rds")
hohes_interesse <- readRDS("hohes_interesse.rds")


##
# (2) Deskriptive Datenanalyse

### voluntary_work
table(shp14$voluntary_work)
mean(shp14$voluntary_work == 0)
  mean(shp14$voluntary_work == 1)

### Gender
table(shp14$gender)
mean(shp14$gender == "male")
mean(shp14$gender == "female")

prop.table(table(shp14$voluntary_work[shp14$gender == "male"]))
prop.table(table(shp14$voluntary_work[shp14$gender == "female"]))



### left_right
table(shp14$left_right)
mean(shp14$left_right == "links")
mean(shp14$left_right == "mitte")
mean(shp14$left_right == "rechts")

### hourly_wage
table(shp14$hourly_wage3)
mean(shp14$hourly_wage3 == "tief")
mean(shp14$hourly_wage3 == "mittel")
mean(shp14$hourly_wage3 == "hoch")

### hours_worked
table(shp14$hours_worked)
mean(shp14$hours_worked == "wenig")
mean(shp14$hours_worked == "mittel")
mean(shp14$hours_worked == "viel")

### edu
table(shp14$edu)
mean(shp14$edu == 1)
mean(shp14$edu == 2)
mean(shp14$edu == 3) 

### age4
table(shp14$age4)
mean(shp14$age4 == "jung")
mean(shp14$age4 == "mittel")
mean(shp14$age4 == "alt")
mean(shp14$age4 == "rentner")

### management 
table(shp14$management)
mean(shp14$management == 0)
mean(shp14$management == 1)

### public_private
table(shp14$public_private)
mean(shp14$public_private == 0)
mean(shp14$public_private == 1)

### kids
table(shp14$kids)
mean(shp14$kids == "keine")
mean(shp14$kids == "einsbiszwei")
mean(shp14$kids == "mehralszwei")

### areas
table(shp14$areas)
mean(shp14$areas == "deutschschweiz")
mean(shp14$areas == "romandie")
mean(shp14$areas == "tessin")

### birth_ch 
table(shp14$birth_ch)
mean(shp14$birth_ch == 0)
mean(shp14$birth_ch == 1)

### church_frequency
table(shp14$church_frequency)
mean(shp14$church_frequency == 0)
mean(shp14$church_frequency == 1)

### political_interest3
table(shp14$political_interest3)
mean(shp14$political_interest3 == "klein")
mean(shp14$political_interest3 == "mittel")
mean(shp14$political_interest3 == "hoch")

### city_village2
table(shp14$city_village2)
mean(shp14$city_village2 == "dorf")
mean(shp14$city_village2 == "stadt")

### realtive Anteil an freiwilligen und nicht freiwillgen für jede Ausprägung aller Variablen.

# Gender
prop.table(table(shp14$voluntary_work[shp14$gender == "male"]))
prop.table(table(shp14$voluntary_work[shp14$gender == "female"]))

# Left_right
prop.table(table(shp14$voluntary_work[shp14$left_right == "links"]))
prop.table(table(shp14$voluntary_work[shp14$left_right == "mitte"]))
prop.table(table(shp14$voluntary_work[shp14$left_right == "rechts"]))

# Hourly_wage3
prop.table(table(shp14$voluntary_work[shp14$hourly_wage3 == "tief"]))
prop.table(table(shp14$voluntary_work[shp14$hourly_wage3 == "mittel"]))
prop.table(table(shp14$voluntary_work[shp14$hourly_wage3 == "hoch"]))

# Hours_worked
prop.table(table(shp14$voluntary_work[shp14$hours_worked == "wenig"]))
prop.table(table(shp14$voluntary_work[shp14$hours_worked == "mittel"]))
prop.table(table(shp14$voluntary_work[shp14$hours_worked == "viel"]))

# Edu
prop.table(table(shp14$voluntary_work[shp14$edu == 1]))
prop.table(table(shp14$voluntary_work[shp14$edu == 2]))
prop.table(table(shp14$voluntary_work[shp14$edu == 3]))

# Age4
prop.table(table(shp14$voluntary_work[shp14$age4 == "jung"]))
prop.table(table(shp14$voluntary_work[shp14$age4 == "mittel"]))
prop.table(table(shp14$voluntary_work[shp14$age4 == "alt"]))
prop.table(table(shp14$voluntary_work[shp14$age4 == "rentner"]))

# Management
prop.table(table(shp14$voluntary_work[shp14$management == 1]))
prop.table(table(shp14$voluntary_work[shp14$management == 0]))

# Public_private
prop.table(table(shp14$voluntary_work[shp14$public_private == 1]))
prop.table(table(shp14$voluntary_work[shp14$public_private == 0]))

# Kids
prop.table(table(shp14$voluntary_work[shp14$kids == "keine"]))
prop.table(table(shp14$voluntary_work[shp14$kids == "einsbiszwei"]))
prop.table(table(shp14$voluntary_work[shp14$kids == "mehralszwei"]))

# Areas
prop.table(table(shp14$voluntary_work[shp14$areas == "deutschschweiz"]))
prop.table(table(shp14$voluntary_work[shp14$areas == "romandie"]))
prop.table(table(shp14$voluntary_work[shp14$areas == "tessin"]))

# Birth_ch
prop.table(table(shp14$voluntary_work[shp14$birth_ch == 1]))
prop.table(table(shp14$voluntary_work[shp14$birth_ch == 0]))

# Church_frequency
prop.table(table(shp14$voluntary_work[shp14$church_frequency == 1]))
prop.table(table(shp14$voluntary_work[shp14$church_frequency == 0]))

# Political_interest3
prop.table(table(shp14$voluntary_work[shp14$political_interest3 == "klein"]))
prop.table(table(shp14$voluntary_work[shp14$political_interest3 == "mittel"]))
prop.table(table(shp14$voluntary_work[shp14$political_interest3 == "hoch"]))

# City_village2
prop.table(table(shp14$voluntary_work[shp14$city_village2 == "stadt"]))
prop.table(table(shp14$voluntary_work[shp14$city_village2 == "dorf"]))



### (2.1) plots


### Anteil Freiwilligenarbeiter nach Geschlecht
# Berechne die Anteile von Freiwilligenarbeit nach Geschlecht

volunteer_summary <- shp14 %>%
  mutate(gender = recode(gender, "female" = "Frauen", "male" = "Männer")) %>%
  group_by(gender) %>%
  summarise(
    Total = n(),
    Volunteers = sum(voluntary_work),
    NonVolunteers = Total - Volunteers,
    VolunteerRate = Volunteers / Total
  )

# Erstelle den Plot ohne Legende und mit y-Achse bis 100%
ggplot(volunteer_summary, aes(x = gender, y = VolunteerRate, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Anteil der Freiwilligenarbeit nach Geschlecht",
    x = "Geschlecht",
    y = "Anteil der Freiwilligenarbeit"
  ) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  theme_minimal() +
  theme(legend.position = "none")

### Verteilung der Freiwilligenarbeit und nicht freiwilligen nach Arbeitssunden in Erwerbstätigkeit

 ggplot(shp14, aes(x = hours_worked_contract, fill = factor(voluntary_work))) +
    geom_density(alpha = 0.5) +  # Alpha-Wert für Transparenz
    labs(
      title = "Verteilung der Arbeitsstunden nach Freiwilligenarbeit",
      x = "Anzahl der Arbeitsstunden",
      y = "Dichte",
      fill = "Freiwilligenarbeit"
    ) +
    scale_fill_manual(values = c("0" = "#FFEB3B", "1" = "#2196F3"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +  # Adjust colors 
    theme_minimal()
 

### Verteilung der Freiwilligenarbeit und nicht freiwilligen nach Stundenlohn

 ggplot(shp14, aes(x = hourly_wage, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5) +  # Alpha-Wert für Transparenz
   labs(
     title = "Verteilung des Stundenlohnes nach Freiwilligenarbeit",
     x = "Stundenlohn",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("0" = "#FFEB3B", "1" = "#2196F3"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +  # Adjust colors 
   theme_minimal()
 
 

 
 

 
 # Ermittlung der maximalen Dichtewerte für die y-Achse
 max_density <- max(
   ggplot_build(ggplot(subset(shp14, gender == "male"), aes(x = hourly_wage, fill = factor(voluntary_work))) + geom_density(alpha = 0.5))$data[[1]]$density,
   ggplot_build(ggplot(subset(shp14, gender == "female"), aes(x = hourly_wage, fill = factor(voluntary_work))) + geom_density(alpha = 0.5))$data[[1]]$density
 )
 
 # Plot für männliche Teilnehmer
 p1 <- ggplot(subset(shp14, gender == "male"), aes(x = hourly_wage, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5) +
   labs(
     title = "Verteilung des Stundenlohns nach Freiwilligenarbeit (Männer)",
     x = "Stundenlohn",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(
     values = c("0" = "#FFEB3B", "1" = "#2196F3"),
     labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")
   ) +
   coord_cartesian(ylim = c(0, max_density)) +  # Setzen des gleichen y-Achsenbereichs
   theme_minimal()
 
 # Plot für weibliche Teilnehmer
 p2 <- ggplot(subset(shp14, gender == "female"), aes(x = hourly_wage, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5) +
   labs(
     title = "Verteilung des Stundenlohns nach Freiwilligenarbeit (Frauen)",
     x = "Stundenlohn",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(
     values = c("0" = "#FFC107", "1" = "#03A9F4"),
     labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")
   ) +
   coord_cartesian(ylim = c(0, max_density)) +  # Setzen des gleichen y-Achsenbereichs
   theme_minimal()
 
 # Plots nebeneinander darstellen
 grid.arrange(p1, p2, ncol = 2)
 
 
 

 
 # Ermittlung der maximalen Dichtewerte für die y-Achse
 max_density <- max(
   ggplot_build(ggplot(subset(shp14, political_interest3 == "klein"), aes(x = hourly_wage, fill = factor(voluntary_work))) + geom_density(alpha = 0.5))$data[[1]]$density,
   ggplot_build(ggplot(subset(shp14, political_interest3 == "mittel"), aes(x = hourly_wage, fill = factor(voluntary_work))) + geom_density(alpha = 0.5))$data[[1]]$density,
   ggplot_build(ggplot(subset(shp14, political_interest3 == "hoch"), aes(x = hourly_wage, fill = factor(voluntary_work))) + geom_density(alpha = 0.5))$data[[1]]$density
 )
 

 
 
 library(gridExtra)
 


 
 
 
### Verteilung nach Jahreseinkommen
 
 ggplot(shp14, aes(x = empl_income_annual, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5) +  # Alpha-Wert für Transparenz
   labs(
     title = "Verteilung des Stundenlohnes nach Freiwilligenarbeit",
     x = "Stundenlohn",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("0" = "#FFEB3B", "1" = "#2196F3"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +  # Adjust colors 
   theme_minimal()

 ggplot(shp14, aes(x = empl_income_annual, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5) +  # Alpha-Wert für Transparenz
   labs(
     title = "Verteilung des Jahreseinkommens nach Freiwilligenarbeit",
     x = "Jahreseinkommen",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("0" = "#FFEB3B", "1" = "#2196F3"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +  # Farben anpassen
   scale_x_continuous(labels = scales::comma_format()) +  # Achsenbeschriftung als Zahlen
   scale_y_continuous(labels = scales::comma_format()) +  # Achsenbeschriftung als Zahlen
   theme_minimal()
 
 
### Verteilung der Freiwilligenarbeit und nicht freiwilligen nach Alter

 shp14$age <- as.numeric(as.character(shp14$age))
 
 ggplot(shp14, aes(x = age, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5, adjust = 1.5) +  # Alpha-Wert für Transparenz und Adjust für Glättung
   labs(
     title = "Verteilung des Alters nach Freiwilligenarbeit",
     x = "Alter",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("0" = "#FFEB3B", "1" = "#2196F3"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +
   theme_minimal()
 
 
 ggplot(shp14, aes(x = age, fill = interaction(factor(voluntary_work), factor(kids)))) +
   geom_density(alpha = 0.5, adjust = 1.5) +  # Alpha-Wert für Transparenz und Adjust für Glättung
   labs(
     title = "Verteilung des Alters nach Freiwilligenarbeit und Anzahl der Kinder",
     x = "Alter",
     y = "Dichte",
     fill = "Freiwilligenarbeit und Kinder"
   ) +
   scale_fill_manual(
     values = c(
       "0.keine" = "#FFEB3B", 
       "0.einsbiszwei" = "#FFC107", 
       "0.mehralszwei" = "#FF9800",
       "1.keine" = "#2196F3", 
       "1.einsbiszwei" = "#03A9F4", 
       "1.mehralszwei" = "#00BCD4"
     ),
     labels = c(
       "Nicht-Freiwilligenarbeitend, keine Kinder", 
       "Nicht-Freiwilligenarbeitend, 1-2 Kinder", 
       "Nicht-Freiwilligenarbeitend, mehr als 2 Kinder",
       "Freiwilligenarbeitend, keine Kinder", 
       "Freiwilligenarbeitend, 1-2 Kinder", 
       "Freiwilligenarbeitend, mehr als 2 Kinder"
     )
   ) +
   theme_minimal()
 
 ### Verteilung nach Alter mit Kategoreie Freiwilligenarbeit und Kinder
 
 
 # Plot für keine Kinder
 p1 <- ggplot(subset(shp14, kids == "keine"), aes(x = age, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5, adjust = 1.5) +
   labs(
     title = "Keine Kinder",
     x = "Alter",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(
     values = c("0" = "#FFEB3B", "1" = "#2196F3"),
     labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")
   ) +
   theme_minimal() +
   coord_cartesian(ylim = c(0, 0.05), xlim = c(20, 80))  # Anpassen der Achsen
 
 # Plot für ein bis zwei Kinder
 p2 <- ggplot(subset(shp14, kids == "einsbiszwei"), aes(x = age, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5, adjust = 1.5) +
   labs(
     title = "Ein bis zwei Kinder",
     x = "Alter",
     y = "",  # leere Beschriftung für y-Achse
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(
     values = c("0" = "#FFC107", "1" = "#03A9F4"),
     labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")
   ) +
   theme_minimal() +
   coord_cartesian(ylim = c(0, 0.05), xlim = c(20, 80))  # Anpassen der Achsen
 
 # Plot für mehr als zwei Kinder
 p3 <- ggplot(subset(shp14, kids == "mehralszwei"), aes(x = age, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5, adjust = 1.5) +
   labs(
     title = "Mehr als zwei Kinder",
     x = "Alter",
     y = "",  # leere Beschriftung für y-Achse
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(
     values = c("0" = "#FF9800", "1" = "#00BCD4"),
     labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")
   ) +
   theme_minimal() +
   coord_cartesian(ylim = c(0, 0.05), xlim = c(20, 80))  # Anpassen der Achsen
 
 # Plots nebeneinander darstellen
 grid.arrange(p1, p2, p3, ncol = 3)
 
 
 
### Verteilung nach Anzahl Kinder
 
 ggplot(shp14, aes(x = n_kids, fill = factor(voluntary_work))) +
   geom_density(alpha = 0.5, adjust = 2) +  # Alpha-Wert für Transparenz und Adjust für Glättung
   labs(
     title = "Verteilung der Anzahl Kinder nach Freiwilligenarbeit",
     x = " Anzahl Kinder",
     y = "Dichte",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("0" = "#FFEB3B", "1" = "#2196F3"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +
   theme_minimal()
 
 ggplot(shp14, aes(x = n_kids, fill = factor(voluntary_work))) +
   geom_histogram(position = "identity", binwidth = 1, alpha = 0.5) +  # Alpha-Wert für Transparenz und binwidth für die Breite der Balken
   labs(
     title = "Verteilung der Anzahl Kinder nach Freiwilligenarbeit",
     x = "Anzahl Kinder",
     y = "Häufigkeit",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("0" = "#FF6347", "1" = "#4682B4"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +
   theme_minimal() +
   theme(
     plot.title = element_text(hjust = 0.5),
     legend.title = element_text(size = 10),
     axis.title.x = element_text(margin = margin(t = 10)),
     axis.title.y = element_text(margin = margin(r = 10))
   )
 
 # Plot erstellen
 ggplot(shp14, aes(x = kids, fill = factor(voluntary_work))) +
   geom_bar(position = "dodge", alpha = 0.8) +
   labs(
     title = "Anzahl der Kinder nach Freiwilligenarbeit",
     x = "Anzahl der Kinder",
     y = "Häufigkeit",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("0" = "#FF6347", "1" = "#4682B4"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +
   theme_minimal() +
   theme(
     plot.title = element_text(hjust = 0.5),
     legend.title = element_text(size = 10),
     axis.title.x = element_text(margin = margin(t = 10)),
     axis.title.y = element_text(margin = margin(r = 10))
   )
 
 
 # Gruppieren nach "kids" und "voluntary_work" und die Anzahl berechnen
 grouped_data <- shp14 %>%
   group_by(kids, voluntary_work) %>%
   summarise(count = n()) %>%
   mutate(percent = count / sum(count) * 100)  # Prozentualen Anteil berechnen
 
 # Plot erstellen
 ggplot(grouped_data[grouped_data$voluntary_work == 1, ], aes(x = kids, y = percent, fill = factor(voluntary_work))) +
   geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
   labs(
     title = "Relative Anzahl der Freiwilligenarbeiter nach Anzahl der Kinder",
     x = "Anzahl der Kinder",
     y = "Prozent der Freiwilligenarbeiter",
     fill = "Freiwilligenarbeit"
   ) +
   scale_fill_manual(values = c("#4682B4"), labels = c("Freiwilligenarbeitend")) +
   theme_minimal() +
   theme(
     plot.title = element_text(hjust = 0.5),
     legend.title = element_text(size = 10),
     axis.title.x = element_text(margin = margin(t = 10)),
     axis.title.y = element_text(margin = margin(r = 10))
   )
 
### Verteilung nach politschem Interesse
 
 shp14_filtered <- shp14 %>% 
   group_by(left_right) %>%
   summarize(prop_voluntary_work = mean(voluntary_work))
 
 
 # Plot erstellen
 ggplot(shp14_filtered, aes(x = left_right, y = prop_voluntary_work)) +
   geom_bar(stat = "identity", fill = "#4682B4", alpha = 0.8) +
   labs(
     title = "Relativer Anteil der Freiwilligenarbeiter nach politischer Orientierung",
     x = "Politische Orientierung",
     y = "Relativer Anteil der Freiwilligenarbeiter"
   ) +
   scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
   theme_minimal() +
   theme(legend.position = "none")

### Verteilung nach left-right
   
   ggplot(shp14 %>% filter(voluntary_work == 1), aes(x = factor(left_right, levels = c("links", "mitte", "rechts")))) +
     geom_bar(aes(y = (..count..) / sum(..count..)), fill = "#4682B4", alpha = 0.8) +
     scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
     labs(
       title = "Relative Häufigkeit der Freiwilligenarbeit nach politischer Orientierung",
       x = "Politische Orientierung",
       y = "Prozent"
     ) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5),
       axis.title.x = element_text(margin = margin(t = 10)),
       axis.title.y = element_text(margin = margin(r = 10))
     )
   
   shp14_filtered1 <- shp14 %>% 
     group_by(political_interest3) %>%
     summarize(prop_voluntary_work = mean(voluntary_work))
   
   
   # Plot erstellen
   ggplot(shp14_filtered1, aes(x = political_interest3, y = prop_voluntary_work)) +
     geom_bar(stat = "identity", fill = "#4682B4", alpha = 0.8) +
     labs(
       title = "Relativer Anteil der Freiwilligenarbeiter nach politischem Interesse",
       x = "Politisches Interesse",
       y = "Relativer Anteil der Freiwilligenarbeiter"
     ) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
     theme_minimal() +
     theme(legend.position = "none")
 
   
   

   
   
  
   ####
   library(ggplot2)
   library(dplyr)
   
   # Berechnung der Gesamtanzahl der Personen je nach politischem Interesse
   total_counts <- shp14 %>%
     group_by(political_interest3) %>%
     summarise(total = n(), .groups = 'drop')
   
   # Berechnung der Anzahl der Freiwilligenarbeitenden je nach politischem Interesse
   voluntary_counts <- shp14 %>%
     filter(voluntary_work == 1) %>%
     group_by(political_interest3, left_right) %>%
     summarise(volunteers = n(), .groups = 'drop')
   
   # Join der beiden Datensätze und Berechnung der relativen Häufigkeiten
   relative_data <- left_join(voluntary_counts, total_counts, by = "political_interest3") %>%
     mutate(relative_frequency = volunteers / total)
   
   # Plot erstellen
   ggplot(relative_data, aes(x = political_interest3, y = relative_frequency, fill = left_right)) +
     geom_bar(stat = "identity", position = "stack", alpha = 0.8) +
     scale_fill_manual(values = c("links" = "#FF6347", "mitte" = "#4682B4", "rechts" = "#32CD32")) +
     scale_y_continuous(labels = scales::percent_format()) +
     labs(
       title = "Relative Häufigkeit der Freiwilligenarbeit nach politischem Interesse und politischer Orientierung",
       x = "Politisches Interesse",
       fill = "Politische Orientierung",
       y = "Prozent der Freiwilligenarbeitenden im Verhältnis zur Gesamtzahl der Personen"
     ) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5),
       legend.title = element_text(size = 10),
       axis.title.x = element_text(margin = margin(t = 10)),
       axis.title.y = element_text(margin = margin(r = 10))
     )
   
  
   
   

   
   
  
### Vergleich mit church_frequency
   
   # Plot erstellen
   ggplot(shp14, aes(x = factor(church_frequency), fill = factor(voluntary_work))) +
     geom_bar(position = "fill", alpha = 0.8) +
     scale_fill_manual(values = c("0" = "#FFA07A", "1" = "#ADD8E6"), labels = c("Nicht-Freiwilligenarbeitend", "Freiwilligenarbeitend")) +
     labs(
       title = "Relative Häufigkeit der Freiwilligenarbeit nach Kirchenfrequenz",
       x = "Kirchenfrequenz",
       fill = "Freiwilligenarbeit",
       y = "Prozent der Arbeiter"
     ) +
     scale_y_continuous(labels = scales::percent_format()) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5),
       legend.title = element_text(size = 10),
       axis.title.x = element_text(margin = margin(t = 10)),
       axis.title.y = element_text(margin = margin(r = 10))
     )
   
   
   
### Vergleich Geschlechter und Anzahl Kinder

   
   
  ##
   
   library(dplyr)
   library(ggplot2)
   
   # Aggregiere die Daten, um die relativen Anteile zu berechnen
   aggregated_data <- shp14 %>%
     group_by(gender, kids) %>%
     summarize(
       total = n(),
       voluntary = sum(voluntary_work == 1)
     ) %>%
     mutate(voluntary_percent = voluntary / total * 100)
   
   # Plot erstellen
   ggplot(aggregated_data, aes(x = kids, y = voluntary_percent, fill = gender)) +
     geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
     scale_fill_manual(values = c("male" = "#4682B4", "female" = "#FF6347")) +
     labs(
       title = "Relativer Anteil der Freiwilligenarbeitenden nach Geschlecht und Anzahl Kinder",
       x = "Anzahl Kinder",
       y = "Prozent der Freiwilligenarbeitenden",
       fill = "Geschlecht"
     ) +
     scale_y_continuous(labels = scales::percent_format(scale = 1)) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5),
       legend.title = element_text(size = 10),
       axis.title.x = element_text(margin = margin(t = 10)),
       axis.title.y = element_text(margin = margin(r = 10))
     )
   
   ##
   
   library(dplyr)
   library(ggplot2)
   
   # Aggregiere die Daten, um die relativen Anteile zu berechnen
   aggregated_data <- shp14 %>%
     group_by(gender, kids) %>%
     summarize(
       total = n(),
       voluntary = sum(voluntary_work == 1)
     ) %>%
     mutate(voluntary_percent = voluntary / total * 100)
   
   # Plot erstellen
   ggplot(aggregated_data, aes(x = kids, y = voluntary_percent, fill = gender)) +
     geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
     scale_fill_manual(values = c("male" = "#4682B4", "female" = "#FF6347")) +
     labs(
       title = "Relativer Anteil der Freiwilligenarbeitenden nach Geschlecht und Anzahl Kinder",
       x = "Anzahl Kinder",
       y = "Prozent der Freiwilligenarbeitenden",
       fill = "Geschlecht"
     ) +
     scale_y_continuous(labels = scales::percent_format(scale = 1)) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5),
       legend.title = element_text(size = 10),
       axis.title.x = element_text(margin = margin(t = 10)),
       axis.title.y = element_text(margin = margin(r = 10))
     )
   

### Vergleich mit edu und kids
   
   library(dplyr)
   library(ggplot2)
   
   # Filter auf Freiwilligenarbeitende
   voluntary_data <- shp14 %>%
     filter(voluntary_work == 1)
   
   # Aggregiere die Daten, um die relativen Anteile zu berechnen
   edu_kids_data <- voluntary_data %>%
     group_by(edu, kids) %>%
     summarize(total = n()) %>%
     mutate(total_percent = total / sum(total) * 100)
   
   # Plot erstellen
   ggplot(edu_kids_data, aes(x = kids, y = total_percent, fill = factor(edu))) +
     geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
     scale_fill_manual(values = c("1" = "#4682B4", "2" = "#FF6347", "3" = "#32CD32"),
                       labels = c("Sekundarstufe 1", "Sekundarstufe 2", "Tertiärabschluss")) +
     labs(
       title = "Relative Häufigkeit der Freiwilligenarbeit nach Bildungsstand und Anzahl der Kinder",
       x = "Anzahl der Kinder",
       y = "Prozent der Freiwilligenarbeitenden",
       fill = "Bildungsabschluss"
     ) +
     scale_y_continuous(labels = scales::percent_format(scale = 1)) +
     theme_minimal() +
     theme(
       plot.title = element_text(hjust = 0.5),
       legend.title = element_text(size = 10),
       axis.title.x = element_text(margin = margin(t = 10)),
       axis.title.y = element_text(margin = margin(r = 10))
     )
   
   

   
   
### Korrlationsatrix für alle verwendeten Variablen
   
   # Laden der Bibliothek für Korrelationsanalysen
   library(corrplot)
   
   # Auswahl der relevanten Variablen für die Korrelationsanalyse
   selected_variables <- shp14[, c("voluntary_work", "gender", "left_right", "hourly_wage3", "hours_worked", "edu", "age4", "management", "public_private", "kids", "areas", "birth_ch", "church_frequency", "political_interest3", "city_village2")]
   
   # Berechnung der Korrelationsmatrix
   correlation_matrix <- cor(selected_variables)
   
   # Visualisierung der Korrelationsmatrix
   corrplot(correlation_matrix, method = "color")
   
   
   # Laden der Bibliothek für Korrelationsanalysen
   library(corrplot)
   
   # Auswahl der relevanten Variablen für die Korrelationsanalyse
   selected_variables <- shp14[, c("voluntary_work", "political_position", "hourly_wage", "hours_worked_contract", "edu", "age", "management", "public_private", "n_kids", "birth_ch", "church_frequency", "political_interest")]
   
   # Berechnung der Korrelationsmatrix
   correlation_matrix <- cor(selected_variables)
   
   # Visualisierung der Korrelationsmatrix
   corrplot(correlation_matrix, method = "color")
   
   # Berechnung der Korrelationsmatrix
   correlation_matrix <- cor(selected_variables, use = "complete.obs")
   
   corrplot(correlation_matrix, method = "number", type = "upper", 
            tl.cex = 0.8, number.cex = 0.8, col = colorRampPalette(c("black", "white"))(200))
   
   
   
##
# (2) Regression für gesamte Stichprobe

reg_fit <- lm(data = shp14, formula = voluntary_work ~ empl_income_annual + hours_worked_contract
              + management + political_position)
summary(reg_fit)

reg_fit2 <- lm(data = shp14, formula = voluntary_work ~ relevel(factor(income), ref = "mittel") 
               + relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
               + gender + relevel(factor(age4), ref = "mittel") + management
               + relevel(factor(political_interest3), ref = "mittel") + public_private 
               + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
               + birth_ch)
summary(reg_fit2)

reg_fit2 <- lm(data = shp14, formula = voluntary_work ~ relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
               + gender + relevel(factor(age4), ref = "mittel") + management
               + relevel(factor(political_interest3), ref = "mittel") + public_private 
               + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
               + birth_ch + city_village2)
summary(reg_fit2)


reg_fit3 <- lm(data = shp14, formula = voluntary_work ~ relevel(factor(income), ref = "mittel") 
               +  relevel(factor(edu), ref = 2)
               + gender + relevel(factor(age4), ref = "mittel"))
summary(reg_fit3)


reg_fit4 <- lm(data = shp14, formula = voluntary_work ~ gender + relevel(factor(age4), ref = "mittel") + management
               + relevel(factor(left_right), ref = "mitte") + public_private)
summary(reg_fit4)


reg_fit5 <-  lm(data = shp14, formula = voluntary_work ~ relevel(factor(left_right), ref = "mitte")
                + relevel(factor(areas), ref = "deutschschweiz") + birth_ch)
summary(reg_fit5)

reg_fit6 <- lm(data = shp14, formula = voluntary_work ~ hours_worked)
summary(reg_fit6)

reg_fit7 <- lm(data = shp14, formula = voluntary_work ~ gender + relevel(factor(age4), ref = "mittel") + management)
summary(reg_fit7)

reg_fit8 <- lm(data = shp14, formula = voluntary_work ~ relevel(factor(income), ref = "mittel") 
               + relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2))
summary(reg_fit8)

reg_fit9 <- lm(data = shp14, formula = voluntary_work ~ relevel(factor(left_right), ref = "mitte"))
summary(reg_fit9)


##
# (3) Regressionen für die verschiedenen Kategorien Geschlecht, politische Einstellung, Stundenlohn

### Nur Männer
reg_male <- lm(data = male_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
               + relevel(factor(age4), ref = "mittel") + management
               + public_private 
               + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
               + birth_ch + church_frequency)
summary(reg_male)

reg1_male <- lm(data = male_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") 
                + relevel(factor(edu), ref = 2)
                + relevel(factor(age4), ref = "mittel") + management
                + relevel(factor(left_right), ref = "mitte") + public_private 
                + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency
                + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_male)

### Nur Frauen 

reg_female <- lm(data = female_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                 + relevel(factor(age4), ref = "mittel") + management
                 + public_private 
                 + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                 + birth_ch + church_frequency)
summary(reg_female)

reg1_female <- lm(data = female_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") 
                  + relevel(factor(edu), ref = 2)
                  + relevel(factor(age4), ref = "mittel") + management
                  + relevel(factor(left_right), ref = "mitte") + public_private 
                  + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                  + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency
                  + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_female)

### Nur links positionierte

reg_links <- lm(data = links_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                + relevel(factor(age4), ref = "mittel") + management
                + public_private 
                + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                + birth_ch + church_frequency)
summary(reg_links)

reg1_links <- lm(data = links_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                 + gender + relevel(factor(age4), ref = "mittel") + management
                 + public_private 
                 + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                 + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_links)

### Nur Mitte positionierte

reg_mitte <- lm(data = mitte_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                + relevel(factor(age4), ref = "mittel") + management
                + public_private 
                + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                + birth_ch + church_frequency)
summary(reg_mitte)

reg1_mitte <- lm(data = mitte_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                 + gender + relevel(factor(age4), ref = "mittel") + management
                 + public_private 
                 + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                 + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_mitte)

reg2_mitte <- lm(data = mitte_data, formula = voluntary_work ~  
                 + gender + relevel(factor(age4), ref = "mittel") 
                 + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                 + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency)
summary(reg2_mitte)

### Nur Rechts positionierte

reg_rechts <- lm(data = rechts_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                 + relevel(factor(age4), ref = "mittel") + management
                 + public_private 
                 + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                 + birth_ch + church_frequency)
summary(reg_rechts)

reg1_rechts <- lm(data = rechts_data, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                  + gender + relevel(factor(age4), ref = "mittel") + management
                  + public_private 
                  + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                  + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_rechts)

### Nur mit tiefem Stundenlohn

reg_tief_stdlohn <- lm(data = tiefer_stundenlohn, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                       + relevel(factor(age4), ref = "mittel") + management
                       + public_private 
                       + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                       + birth_ch + church_frequency + relevel(factor(political_interest3), ref = "mittel"))
summary(reg_tief_stdlohn)

reg1_tief_stdlohn <- lm(data = tiefer_stundenlohn, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                        + gender + relevel(factor(age4), ref = "mittel") + management
                        + relevel(factor(left_right), ref = "mitte") + public_private 
                        + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                        + birth_ch + church_frequency + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_tief_stdlohn)

### Nur mit mittlerem Stundenlohn

reg_mittlerer_stdlohn <- lm(data = mittlerer_stundenlohn, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                            + relevel(factor(age4), ref = "mittel") + management
                            + public_private 
                            + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                            + birth_ch + church_frequency)
summary(reg_mittlerer_stdlohn)

reg1_mittlerer_stdlohn <- lm(data = mittlerer_stundenlohn, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                             + gender + relevel(factor(age4), ref = "mittel") + management
                             + relevel(factor(left_right), ref = "mitte") + public_private 
                             + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                             + birth_ch + church_frequency + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_mittlerer_stdlohn)

### Nur mit hohem Stundenlohn

reg_hoher_stdlohn <- lm(data = hoher_stundenlohn, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                        + relevel(factor(age4), ref = "mittel") + management
                        + public_private 
                        + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                        + birth_ch + church_frequency)
summary(reg_hoher_stdlohn)

reg1_hoher_stdlohn <- lm(data = hoher_stundenlohn, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                         + gender + relevel(factor(age4), ref = "mittel") + management
                         + relevel(factor(left_right), ref = "mitte") + public_private 
                         + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                         + birth_ch + church_frequency + relevel(factor(political_interest3), ref = "mittel") + city_village2)
summary(reg1_hoher_stdlohn)

regression <- lm(data = hoher_stundenlohn, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") *   + relevel(factor(kids), ref = "einsbiszwei")+ relevel(factor(edu), ref = 2) * management
                 + gender + relevel(factor(age4), ref = "mittel") 
                 + relevel(factor(left_right), ref = "mitte")  
                 + relevel(factor(areas), ref = "deutschschweiz")
                 + birth_ch * church_frequency + management * public_private)

### nur mit kleinem politischen Interesse

reg1_kleines_interesse <- lm(data = kleines_interesse, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                         + gender + relevel(factor(age4), ref = "mittel") + management
                         + relevel(factor(left_right), ref = "mitte") + public_private 
                         + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                         + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency + city_village)
summary(reg1_kleines_interesse)

### nur mit mittlerem politischen Interesse

reg1_mittleres_interesse <- lm(data = mittleres_interesse, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                             + gender + relevel(factor(age4), ref = "mittel") + management
                             + relevel(factor(left_right), ref = "mitte") + public_private 
                             + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                             + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency + city_village)
summary(reg1_mittleres_interesse)

### nur mit hohem politischen Interesse

reg1_hohes_interesse <- lm(data = hohes_interesse, formula = voluntary_work ~  relevel(factor(hours_worked), ref = "mittel") + relevel(factor(edu), ref = 2)
                               + gender + relevel(factor(age4), ref = "mittel") + management
                               + relevel(factor(left_right), ref = "mitte") + public_private 
                               + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                               + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency + city_village)
summary(reg1_hohes_interesse)


### Regressionsoutput generieren

### Geschlecht

stargazer(reg1_male, reg1_female, type = "text", title = "Results", align = TRUE,
          column.labels = c("Männer", "Frauen"),
          dep.var.caption = "Abhängige Variable",
          dep.var.labels = "Freiwilligenarbeit")

stargazer(reg1_male, reg1_female, type = "html", title = "Results", align = TRUE,
          column.labels = c("Männer", "Frauen"),
          dep.var.caption = "Abhängige Variable",
          dep.var.labels = "Freiwilligenarbeit")

# Speichern Sie die HTML-Ausgabe in einer Datei
html_output <- capture.output(
  stargazer(reg1_male, reg1_female, type = "html", title = "Results1", align = TRUE,
            column.labels = c("Männer", "Frauen"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit")
)

# Schreiben Sie die Ausgabe in eine HTML-Datei
cat(html_output, file = "results1.html", sep = "\n")


### Politische Einstellung

# Speichern Sie die HTML-Ausgabe in einer Datei
html_output <- capture.output(
  stargazer(reg1_links, reg1_mitte, reg1_rechts, type = "html", title = "Results2", align = TRUE,
            column.labels = c("Links", "Mitte", "Rechts"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit")
)

# Schreiben Sie die Ausgabe in eine HTML-Datei
cat(html_output, file = "results2.html", sep = "\n")


### Stundenlohn

# Speichern Sie die HTML-Ausgabe in einer Datei
html_output <- capture.output(
  stargazer(reg1_tief_stdlohn, reg1_mittlerer_stdlohn, reg1_hoher_stdlohn, type = "html", title = "Results3", align = TRUE,
            column.labels = c("tiefer Stundenlohn", "mittlerer Stundenlohn", "hoher Stundenlohn"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit")
)

# Schreiben Sie die Ausgabe in eine HTML-Datei
cat(html_output, file = "results3.html", sep = "\n")


### Politisches Interesse

# Speichern Sie die HTML-Ausgabe in einer Datei
html_output <- capture.output(
  stargazer(reg1_kleines_interesse, reg1_mittleres_interesse, reg1_hohes_interesse, type = "html", title = "Results4", align = TRUE,
            column.labels = c("tiefes Interesse", "mittleres Interesse", "hohes Interesse"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit")
)

# Schreiben Sie die Ausgabe in eine HTML-Datei
cat(html_output, file = "results4.html", sep = "\n")





### mit fixest funktion

library(fixest)
library(dreamerr)

# Define the regression model using fixest::feols
reg1_male_feols <- feols(voluntary_work ~ relevel(factor(hours_worked), ref = "mittel")
                         + relevel(factor(edu), ref = 2)
                         + relevel(factor(age4), ref = "mittel") + management
                         + relevel(factor(left_right), ref = "mitte") + public_private 
                         + relevel(factor(kids), ref = "einsbiszwei") + relevel(factor(areas), ref = "deutschschweiz")
                         + birth_ch + relevel(factor(hourly_wage3), ref = "mittel") + church_frequency
                         + relevel(factor(political_interest3), ref = "mittel") + city_village2, 
                         data = male_data)

# Print the summary of the regression model
summary(reg1_male_feols)

# Use etable() to display the results, selecting only the important variables
etable(reg1_male_feols, drop = c("public_private", "city_village2", "birth_ch")) # Adjust the drop argument as needed

install.packages("dreamerr")

install.packages("stringmagic", type = "source")
install.packages("fixest")


Sys.which("make")

# Install the fixest package if it's not already installed
if (!require("fixest")) {
  install.packages("fixest")
}

# Load the fixest package
library("fixest")

# To install from CRAN:
install.packages("fixest")

# To install the latest stable development release:
install.packages("fixest", 
                 repos = c(ropensci = 'https://fastverse.r-universe.dev',
                           CRAN = 'https://cloud.r-project.org'))

# To install from CRAN:
install.packages("fixest")

# To install the latest stable development release:
install.packages("fixest", 
                 repos = c(ropensci = 'https://fastverse.r-universe.dev',
                           CRAN = 'https://cloud.r-project.org'))


