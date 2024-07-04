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

##
# (1) Setze das Arbeitsverzeichnis:
setwd("C:/Users/jan_b/OneDrive - Universitaet Bern/HS2024-JB-Yoga/Seminar Ökonometrie/Data")

##
# (2) Importiere den SHP-Datensatz des Jahres 2015 (Nutze die Importfunktion des Pakets "haven")
shp14 <- read_dta("shp15.dta")

##
# (3)  Verschaffe dir ein Ãœberblick Ã¼ber die Daten (str(), names())
unique(shp14$p14n35)
unique(shp14$p14n38)
unique(shp14$p14n34)
unique(shp14$age14)
unique(shp14$sex14)
unique(shp14$nbadul14)
unique(shp14$p14d29)
unique(shp14$p14p10)
unique(shp14$p14w34a)
unique(shp14$isced14)
unique(shp14$wstat14)
unique(shp14$x14i04)
unique(shp14$i14wyg)
unique(shp14$p14w32)
unique(shp14$ownkid14)
unique(shp14$canton14)
unique(shp14$nat_1_14)
unique(shp14$nbkid14)
unique(shp14$p14d160)
unique(shp14$region14)
unique(shp14$p15r02)
unique(shp15$)


shp14%>%group_by(p15r02)%>%summarize(n())
shp14%>%group_by(p15p01)%>%summarize(n())
shp14%>%group_by(com2_15)%>%summarize(n())


##
# (4) Variablen selektieren und umbenennen:
##  (i) Welche Faktoren beeinflussen das Einkommen und die Körpergrösse?
# 	    Selektiere die relevanten Einflussfaktoren. Und gib diesen
#       Variablen einen sprecherenden Namen.
#       
#       monatliches Einkommen (angestellt, brutto),
#       jährliches Einkommen (angestellt, brutto),
#       jährliches Einkommen (brutto)
#       Anzahl Arbeitsstunden gemäss Artbeitsvertrag,
#       Bildungsjahre auf der Grundlage der ISCED-Klassifikation,
#       Geschlecht,
#       Körpergrösse,
#       Alter,
#       Mangement,
#       

shp14 %<>% 
  rename(empl_income_annual = i15empyg,
         hours_worked_contract = p15w74,
         education = isced15,
         gender =  sex15,
         age = age15,
         management = p15w34a,
         voluntary_work = p15n35,
         voluntary_hours = p15n38,
         political_position = p15p10,
         working_status = wstat15,
         public_private = p15w32,
         n_kids = ownkid15,
         canton = canton15,
         nationality_1 = nat_1_15,
         n_children_hh = nbkid15,
         birth_ch = p15d160,
         region = region15,
         religion = p15r02,
         political_interest = p15p01,
         city_village = com2_15,
  )

## (ii) Behalte nur diese Variablen 

shp14 %<>% 
  select(empl_income_annual,hours_worked_contract,
         education,gender,age,management,
         voluntary_work,voluntary_hours,
         political_position,working_status,public_private,
         n_kids,canton,nationality_1,n_children_hh,
         birth_ch,region,religion,political_interest,city_village)

##
# (5) Sample selection:
## Beschränke den Datensatz auf Beobachtungen für die wir eine Information
#  haben für alle verwendeten Variablen. 

shp14 <- shp14[shp14$empl_income_annual >= 0 & 
                 shp14$hours_worked_contract >= 0 & 
                 shp14$education >= 0 & 
                 shp14$gender >= 0 & 
                 shp14$age >= 0 & 
                 shp14$management >= 0 & 
                 shp14$voluntary_work >= 0 & 
                 shp14$political_position >= 0 & 
                 shp14$working_status >= 0 &
                 shp14$public_private >= 0 &
                 shp14$n_kids >= 0 &
                 shp14$canton >= 0 &
                 shp14$nationality_1 >= 0 &
                 shp14$n_children_hh >= 0 &
                 shp14$birth_ch >= 0 &
                 shp14$region >= 0 &
                 shp14$religion >= 0 &
                 shp14$political_interest >= 0 , ]



##
# (6) Generieren und Umbennenen von neuen Variablen

### (i) Einkommen in 3 Blöcke einteilen
quantile(shp14$empl_income_annual, probs = c(0.33, 0.66))
shp14$income <- "mittel"
shp14$income[shp14$empl_income_annual < 50000] <- "tief"
shp14$income[shp14$empl_income_annual > 100000] <- "hoch"
table(shp14$income)

### (ii) Arbeitsstunden in 3 Blöcke einteilen
quantile(shp14$hours_worked_contract, probs = c(0.33, 0.66))
shp14$hours_worked <- "mittel" 
shp14$hours_worked[shp14$hours_worked_contract < 31] <- "wenig"
shp14$hours_worked[shp14$hours_worked_contract > 41] <- "viel"
table(shp14$hours_worked)

### (iii) Generiere eine Variable die den höchsten Bildungsabschluss in Form von
#  3 Kategorien zeigt. 
#  1 = bis Sekundarstufe 1
#  2 = Sekundarstufe 2
#  3 = Tertiärstufe

shp14$edu <- 0
shp14$edu[shp14$education >= 0 & shp14$education <= 20] <- 1
shp14$edu[shp14$education >= 21 & shp14$education <= 41] <- 2
shp14$edu[shp14$education >= 42 & shp14$education <= 60] <- 3
table(shp14$edu)

### (iv) Geschlecht andere Werte zuordnen

shp14$gender <- as.character(shp14$gender)
shp14$gender[shp14$gender == "1"] <- "male"
shp14$gender[shp14$gender == "2"] <- "female"
table(shp14$gender)

### (v) Alter in 3 Kategorien einteilen

quantile(shp14$age, probs = c(0.33, 0.66))
shp14$age <- as.character(shp14$age)

shp14$age4 <- "mittel"
shp14$age4[shp14$age < "35"] <- "jung"
shp14$age4[shp14$age > "50"] <- "alt"
shp14$age4[shp14$age > "65"] <- "rentner"
table(shp14$age4)

### (vi) hat Person Management Position oder nicht

shp14$management <- ifelse(shp14$management <= 1, 1, 0)
table(shp14$management)
## Bemerkung 1 ist Management Position und 0 sonst.

### (vii) Freiwilig engagiert oder nicht

shp14$voluntary_work <- ifelse(shp14$voluntary_work == 2, 0, shp14$voluntary_work)
table(shp14$voluntary_work)
## Bemerkung 1 ist freiwilig engagiert und 0 nicht.

### (ix) Partner 
# rausgenommen


### (x) neue Variable für politische Einstellung mit 3 Ausprägungen

shp14$left_right <- cut(shp14$political_position, breaks = c(0, 3, 6, 10), labels = c("links", "mitte", "rechts"), include.lowest = TRUE)
table(shp14$left_right)
## Bemerkung die jenigen mit 0 bis 3 sind links, die mit 4-6 mitte und 7-10 rechts.

### (xi) Arbeitsstatus 
## Bemerkung sind alle beschäftigt.

### (xii) öffentlichen oder privaten Sektor beschäftigt.

shp14$public_private <- ifelse(shp14$public_private == 1, 0, ifelse(shp14$public_private == 2, 1, shp14$public_private))
table(shp14$public_private)
## Bemerkung 0 ist im privaten Sektor und 1 im öffentlichen.

### (xiii) Anzahl Kinder in 3 Kategorien einteilen.

shp14$kids <- cut(shp14$n_kids, breaks = c(-Inf, 0, 2, Inf), labels = c("keine", "einsbiszwei", "mehralszwei"), include.lowest = TRUE)
table(shp14$kids)
## Bemerkung eingeteilt in 0, 1-2 und mehr als 2 Kinder

### (xiv) neue Variable areas generieren, welche Kantone in deutschschweiz, romandie und tessin einteilt.

shp14$areas <- ifelse(shp14$canton %in% c(8, 11, 13, 23), "romandie",
                      ifelse(shp14$canton == 21, "tessin", "deutschschweiz"))
table(shp14$areas)

### (xv) ist die erste Nationalität der Person schweiz oder nicht.

shp14$nationality_1 <- ifelse(shp14$nationality_1 == 8100, 1, 0)
table(shp14$nationality_1)
## Bemerkung 1 ist CH 0 sonst.

### (xvi) leben im Haushalt noch Kinder bis 17 oder nicht.
# rausgenommen

## BEmerkung 0 wenn keine Kinder im HH mehr leben und 1 wenn doch.

### (xvii) Person in der Schweiz geboren oder nicht.

shp14$birth_ch <- ifelse(shp14$birth_ch == 2, 0, ifelse(shp14$birth_ch == 1, 1, shp14$birth_ch))
table(shp14$birth_ch)
## Bemerkung 0 wenn Person nicht in CH geboren ist 1 sonst.

### (xviii) Stundenlohn generieren
# Berechnung des Stundenlohns gemäss Paper

shp14$hourly_wage <- (((shp14$empl_income_annual / 12) / 4) / shp14$hours_worked_contract)

### (xix) Stundenlohn in 3 Kategorien aufteilen

quantile(shp14$hourly_wage, probs = c(0.33, 0.66))
shp14$hourly_wage3 <- "mittel"
shp14$hourly_wage3[shp14$hourly_wage < "35"] <- "tief"
shp14$hourly_wage3[shp14$hourly_wage > "55"] <- "hoch"
table(shp14$hourly_wage3)

### (xx) Religion in Kategorien aufteilen
# 0 wenn nicht oder nicht aus religiösen Gründe in Kirche geht (Familienfeier), 1 sonst
unique(shp14$religion)
shp14%>%group_by(religion)%>%summarize(n())

shp14$church_frequency <- 0
shp14$church_frequency[shp14$religion > 2] <- 1
table(shp14$church_frequency)

### (xxi) Politisches Interesse in Kategorien einteilen
shp14$political_interest3 <- "mittel"
shp14$political_interest3[shp14$political_interest < 4] <- "klein"
shp14$political_interest3[shp14$political_interest > 6] <- "hoch"
table(shp14$political_interest3)

### (xxii) Region in Gruppen einteilen - Stadt und Agglo. Rest Dorf.
shp14$city_village2 <- "stadt"
shp14$city_village2[shp14$city_village > 1] <- "dorf"
table(shp14$city_village2)
table(shp14$city_village)


### erstes und letztes Perzentil entfernen für hourly_Wage und hours_worked um Ausreiser vermeiden.

# Berechnung der Perzentile für hourly_wage und hours_worked_contract unter Ausschluss von NA-Werten
hourly_wage_quantiles <- quantile(shp14$hourly_wage, probs = c(0.01, 0.99), na.rm = TRUE)
hours_worked_contract_quantiles <- quantile(shp14$hours_worked_contract, probs = c(0.01, 0.99), na.rm = TRUE)

# Filtern der Daten, um die untersten und obersten 1% der hourly_wage und hours_worked_contract zu entfernen
shp14 <- shp14[shp14$hourly_wage >= quantile(shp14$hourly_wage, probs = 0.01) &
                 shp14$hourly_wage <= quantile(shp14$hourly_wage, probs = 0.99) &
                 shp14$hours_worked_contract >= quantile(shp14$hours_worked_contract, probs = 0.01) &
                 shp14$hours_worked_contract <= quantile(shp14$hours_worked_contract, probs = 0.99), ]



##
# (6) Datensatz für bestimmte Gruppen

### (i) Nur für Männer

male_data <- subset(shp14, gender == "male")

### (ii) Nur für Frauen 

female_data <- subset(shp14, gender == "female")

### (iii) Nur für Personen mit linker politischer Einstellung

links_data <- subset(shp14, left_right == "links")

### (iv) Nur für Personen mit politischer Einstellung in der Mitte

mitte_data <- subset(shp14, left_right == "mitte")

### (v) Nur für Personen mit rechter politischer Einstellung

rechts_data <- subset(shp14, left_right == "rechts")

### (vi) Nur für Personen mit tiefem Stundenlohn

tiefer_stundenlohn <- subset(shp14, hourly_wage3 == "tief")

### (vii) Nur für Personen mit mittlerem Stundenlohn

mittlerer_stundenlohn <- subset(shp14, hourly_wage3 == "mittel")

### (viii) Nur für Personen mit hohem Stundennlohn

hoher_stundenlohn <- subset(shp14, hourly_wage3 == "hoch")

### (ix) Nur für Personen mit kleinem politischen Interesse

kleines_interesse <- subset(shp14, political_interest3 == "klein")

### (x) Nur für Personen mit mittlerem politischen Interesse

mittleres_interesse <- subset(shp14, political_interest3 == "mittel")

### (xi) Nur für Personen mit hohem politischen Interesse

hohes_interesse <- subset(shp14, political_interest3 == "hoch")

##
# (7) Datensätze speichern
saveRDS(shp14, file = "shp14.rds")
saveRDS(male_data, file = "male_data.rds")
saveRDS(female_data, file = "female_data.rds")
saveRDS(links_data, file = "links_data.rds")
saveRDS(mitte_data, file = "mitte_data.rds")
saveRDS(rechts_data, file = "rechts_data.rds")
saveRDS(tiefer_stundenlohn, file = "tiefer_stundenlohn.rds")
saveRDS(mittlerer_stundenlohn, file = "mittlerer_stundenlohn.rds")
saveRDS(hoher_stundenlohn, file = "hoher_stundenlohn.rds")
saveRDS(kleines_interesse, file = "kleines_interesse.rds")
saveRDS(mittleres_interesse, file = "mittleres_interesse.rds")
saveRDS(hohes_interesse, file = "hohes_interesse.rds")



