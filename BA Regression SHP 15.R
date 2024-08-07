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
library("fixest")

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
### relative Anteile Ausprägung

### voluntary_work
table(shp14$voluntary_work)
mean(shp14$voluntary_work == 0)
  mean(shp14$voluntary_work == 1)

### Gender
table(shp14$gender)
mean(shp14$gender == "male")
mean(shp14$gender == "female")


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

   
##
# (2) Regression für gesamte Stichprobe

reg_fit <- lm(data = shp14, formula = voluntary_work ~  
                gender +
                relevel(factor(left_right), ref = "mitte") + 
                relevel(factor(hourly_wage3), ref = "mittel") + 
                relevel(factor(political_interest3), ref = "mittel") +
                relevel(factor(hours_worked), ref = "mittel") +
                relevel(factor(edu), ref = 2) +
                relevel(factor(age4), ref = "mittel") + 
                management +
                public_private + 
                relevel(factor(kids), ref = "einsbiszwei") + 
                relevel(factor(areas), ref = "deutschschweiz") +
                birth_ch + 
                church_frequency +
                city_village2)
summary(reg_fit)


reg1_fit <- lm(data = shp14, formula = voluntary_work ~  
                gender +
                relevel(factor(left_right), ref = "mitte") + 
                relevel(factor(hourly_wage3), ref = "mittel") + 
                relevel(factor(political_interest3), ref = "mittel") +
                relevel(factor(edu), ref = 2) +
                relevel(factor(age4), ref = "mittel") + 
                management +
                public_private + 
                relevel(factor(kids), ref = "einsbiszwei") + 
                relevel(factor(areas), ref = "deutschschweiz") +
                birth_ch + 
                church_frequency +
                city_village2)
summary(reg1_fit)

reg2_fit <- lm(data = shp14, formula = voluntary_work ~  
                gender +
                relevel(factor(left_right), ref = "mitte") + 
                relevel(factor(political_interest3), ref = "mittel") +
                relevel(factor(edu), ref = 2) +
                relevel(factor(age4), ref = "mittel") + 
                management +
                public_private + 
                relevel(factor(kids), ref = "einsbiszwei") + 
                relevel(factor(areas), ref = "deutschschweiz") +
                birth_ch + 
                church_frequency +
                city_village2 +
                relevel(factor(hourly_wage3), ref = "mittel") * relevel(factor(hours_worked), ref = "mittel"))
summary(reg2_fit)


##
# (3) Regressionen für die verschiedenen Kategorien Geschlecht, politische Einstellung/Intresse, Stundenlohn

### Nur Männer

reg1_male <- lm(data = male_data, formula = voluntary_work ~  
                  relevel(factor(left_right), ref = "mitte") + 
                  relevel(factor(hourly_wage3), ref = "mittel") + 
                  relevel(factor(political_interest3), ref = "mittel") +
                  relevel(factor(hours_worked), ref = "mittel") +
                  relevel(factor(edu), ref = 2) +
                  relevel(factor(age4), ref = "mittel") + 
                  management +
                  public_private + 
                  relevel(factor(kids), ref = "einsbiszwei") + 
                  relevel(factor(areas), ref = "deutschschweiz") +
                  birth_ch + 
                  church_frequency +
                  city_village2)
summary(reg1_male)


### Nur Frauen 

reg1_female <- lm(data = female_data, formula = voluntary_work ~  
                    relevel(factor(left_right), ref = "mitte") + 
                    relevel(factor(hourly_wage3), ref = "mittel") + 
                    relevel(factor(political_interest3), ref = "mittel") +
                    relevel(factor(hours_worked), ref = "mittel") +
                    relevel(factor(edu), ref = 2) +
                    relevel(factor(age4), ref = "mittel") + 
                    management +
                    public_private + 
                    relevel(factor(kids), ref = "einsbiszwei") + 
                    relevel(factor(areas), ref = "deutschschweiz") +
                    birth_ch + 
                    church_frequency +
                    city_village2)
summary(reg1_female)


### Nur links positionierte

reg1_links <- lm(data = links_data, formula = voluntary_work ~  
                   gender + 
                   relevel(factor(hourly_wage3), ref = "mittel") + 
                   relevel(factor(political_interest3), ref = "mittel") +
                   relevel(factor(hours_worked), ref = "mittel") +
                   relevel(factor(edu), ref = 2) +
                   relevel(factor(age4), ref = "mittel") + 
                   management +
                   public_private + 
                   relevel(factor(kids), ref = "einsbiszwei") + 
                   relevel(factor(areas), ref = "deutschschweiz") +
                   birth_ch + 
                   church_frequency +
                   city_village2)

summary(reg1_links)


### Nur Mitte positionierte

reg1_mitte <- lm(data = mitte_data, formula = voluntary_work ~  
                   gender +
                   relevel(factor(hourly_wage3), ref = "mittel") +
                   relevel(factor(political_interest3), ref = "mittel") +
                   relevel(factor(hours_worked), ref = "mittel") + 
                   relevel(factor(edu), ref = 2) +
                   relevel(factor(age4), ref = "mittel") + 
                   management +
                   public_private + 
                   relevel(factor(kids), ref = "einsbiszwei") + 
                   relevel(factor(areas), ref = "deutschschweiz") +
                   birth_ch + 
                   church_frequency +
                   city_village2)

summary(reg1_mitte)




### Nur Rechts positionierte

reg1_rechts <- lm(data = rechts_data, formula = voluntary_work ~  
                    gender +
                    relevel(factor(hourly_wage3), ref = "mittel") +
                    relevel(factor(political_interest3), ref = "mittel") +
                    relevel(factor(hours_worked), ref = "mittel") + 
                    relevel(factor(edu), ref = 2) +
                    relevel(factor(age4), ref = "mittel") + 
                    management +
                    public_private + 
                    relevel(factor(kids), ref = "einsbiszwei") + 
                    relevel(factor(areas), ref = "deutschschweiz") +
                    birth_ch + 
                    church_frequency +
                    city_village2)

summary(reg1_rechts)


### Nur mit tiefem Stundenlohn

reg1_tief_stdlohn <- lm(data = tiefer_stundenlohn, formula = voluntary_work ~  
                          gender +
                          relevel(factor(left_right), ref = "mitte") +
                          relevel(factor(political_interest3), ref = "mittel") +
                          relevel(factor(hours_worked), ref = "mittel") + 
                          relevel(factor(edu), ref = 2) +
                          relevel(factor(age4), ref = "mittel") + 
                          management +
                          public_private + 
                          relevel(factor(kids), ref = "einsbiszwei") + 
                          relevel(factor(areas), ref = "deutschschweiz") +
                          birth_ch + 
                          church_frequency +
                          city_village2)

summary(reg1_tief_stdlohn)


### Nur mit mittlerem Stundenlohn

reg1_mittlerer_stdlohn <- lm(data = mittlerer_stundenlohn, formula = voluntary_work ~  
                               gender +
                               relevel(factor(left_right), ref = "mitte") +
                               relevel(factor(political_interest3), ref = "mittel") +
                               relevel(factor(hours_worked), ref = "mittel") + 
                               relevel(factor(edu), ref = 2) +
                               relevel(factor(age4), ref = "mittel") + 
                               management +
                               public_private + 
                               relevel(factor(kids), ref = "einsbiszwei") + 
                               relevel(factor(areas), ref = "deutschschweiz") +
                               birth_ch + 
                               church_frequency +
                               city_village2)

summary(reg1_mittlerer_stdlohn)


### Nur mit hohem Stundenlohn

reg1_hoher_stdlohn <- lm(data = hoher_stundenlohn, formula = voluntary_work ~  
                           gender +
                           relevel(factor(left_right), ref = "mitte") +
                           relevel(factor(political_interest3), ref = "mittel") +
                           relevel(factor(hours_worked), ref = "mittel") + 
                           relevel(factor(edu), ref = 2) +
                           relevel(factor(age4), ref = "mittel") + 
                           management +
                           public_private + 
                           relevel(factor(kids), ref = "einsbiszwei") + 
                           relevel(factor(areas), ref = "deutschschweiz") +
                           birth_ch + 
                           church_frequency +
                           city_village2)

summary(reg1_hoher_stdlohn)




### nur mit kleinem politischen Interesse



reg1_kleines_interesse <- lm(data = kleines_interesse, formula = voluntary_work ~  
                               gender +
                               relevel(factor(left_right), ref = "mitte") +
                               relevel(factor(hourly_wage3), ref = "mittel") +
                               relevel(factor(hours_worked), ref = "mittel") + 
                               relevel(factor(edu), ref = 2) +
                               relevel(factor(age4), ref = "mittel") + 
                               management +
                               public_private + 
                               relevel(factor(kids), ref = "einsbiszwei") + 
                               relevel(factor(areas), ref = "deutschschweiz") +
                               birth_ch + 
                               church_frequency +
                               city_village2)

summary(reg1_kleines_interesse)

### nur mit mittlerem politischen Interesse



reg1_mittleres_interesse <- lm(data = mittleres_interesse, formula = voluntary_work ~  
                                 gender +
                                 relevel(factor(left_right), ref = "mitte") +
                                 relevel(factor(hourly_wage3), ref = "mittel") +
                                 relevel(factor(hours_worked), ref = "mittel") + 
                                 relevel(factor(edu), ref = 2) +
                                 relevel(factor(age4), ref = "mittel") + 
                                 management +
                                 public_private + 
                                 relevel(factor(kids), ref = "einsbiszwei") + 
                                 relevel(factor(areas), ref = "deutschschweiz") +
                                 birth_ch + 
                                 church_frequency +
                                 city_village2)

summary(reg1_mittleres_interesse)


### nur mit hohem politischen Interesse



reg1_hohes_interesse <- lm(data = hohes_interesse, formula = voluntary_work ~  
                             gender +
                             relevel(factor(left_right), ref = "mitte") +
                             relevel(factor(hourly_wage3), ref = "mittel") +
                             relevel(factor(hours_worked), ref = "mittel") + 
                             relevel(factor(edu), ref = 2) +
                             relevel(factor(age4), ref = "mittel") + 
                             management +
                             public_private + 
                             relevel(factor(kids), ref = "einsbiszwei") + 
                             relevel(factor(areas), ref = "deutschschweiz") +
                             birth_ch + 
                             church_frequency +
                             city_village2)

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


# Modell mit geänderten Namen




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

ggsave("regression_results.png", reg1_male_feols, width = 8, height = 6, units = "in")



library(stargazer)

######## Relevante Regressionen




### Geschlecht und Gesamt

# Definiere die gewünschten Labels für die Variablen
covariate.labels <- c("Männer (Referenz: Frauen)", "Links eingestellt (Ref: Mitte)", "Rechts eingestellt (Ref: Mitte)", "hoher Stundenlohn (Ref: Mittel)", "tiefer Stundenlohn (Ref: Mittel)", "hohes Politikinteresse (Ref: Mittel)", "tiefes Politikinteresse (Ref: Mittel)")

# Erstelle eine Liste der zu entfernenden Variablen
variables_to_omit <- c("hours_worked", "edu", "age4", "management", "public_private", 
                       "kids", "areas", "birth_ch", "church_frequency", "city_village2")




# Führe stargazer aus und gebe nur die gewünschten Variablen aus
html_output <- capture.output(
  stargazer(reg1_male, reg1_female, reg_fit, type = "html", title = "Geschlecht", align = TRUE,
            column.labels = c("Männer", "Frauen", "Gesamt"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit",
            covariate.labels = covariate.labels,
            omit = variables_to_omit))
cat(html_output, file = "Geschlecht Teilausschnitt.html", sep = "\n")

# Neue Reihenfolge

covariate.labels <- c("Männer (Referenz: Frauen)", "Links eingestellt (Ref: Mitte)", "Rechts eingestellt (Ref: Mitte)", "hoher Stundenlohn (Ref: Mittel)", "tiefer Stundenlohn (Ref: Mittel)", "hohes Politikinteresse (Ref: Mittel)", "tiefes Politikinteresse (Ref: Mittel)",
                      "viele Arbeitsstunden (Ref: Mittel)", "wenige Arbeitsstunden (Ref: Mittel)", "Sek 1 Abschluss (Ref: Sek 2)" , "Tertiär Abschluss (Ref: Sek 2)", "Alt (Ref: Mittel)", "Jung (Ref: Mittel)", "Rentner (Ref: Mittel)",
                      "Mangement Position", "öffentlicher Sektor", "keine Kinder (Ref: 1-2 Kinder)", "mehr als zwei Kinder (Ref: 1-2 Kinder)", "Romandie (Ref: Deutschschweiz)", "Tessin (Ref: Deutschschweiz)", "in Schweiz geboren", "Kirchgänger aus religiösen Gründen",
                      "Stadt (Ref: Land)")

html_output <- capture.output(
  stargazer(reg1_male, reg1_female, reg_fit, type = "html", title = "Geschlecht", align = TRUE,
            column.labels = c("Männer", "Frauen", "Gesamt"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit",
            covariate.labels = covariate.labels))
  
cat(html_output, file = "Geschlecht.html", sep = "\n")

### Politische Einstellung

covariate.labels <- c("Männer (Referenz: Frauen)", "hoher Stundenlohn (Ref: Mittel)", "tiefer Stundenlohn (Ref: Mittel)", "hohes Politikinteresse (Ref: Mittel)", "tiefes Politikinteresse (Ref: Mittel)",
                      "viele Arbeitsstunden (Ref: Mittel)", "wenige Arbeitsstunden (Ref: Mittel)", "Sek 1 Abschluss (Ref: Sek 2)" , "Tertiär Abschluss (Ref: Sek 2)", "Alt (Ref: Mittel)", "Jung (Ref: Mittel)", "Rentner (Ref: Mittel)",
                      "Mangement Position", "öffentlicher Sektor", "keine Kinder (Ref: 1-2 Kinder)", "mehr als zwei Kinder (Ref: 1-2 Kinder)", "Romandie (Ref: Deutschschweiz)", "Tessin (Ref: Deutschschweiz)", "in Schweiz geboren", "Kirchgänger aus religiösen Gründen",
                      "Stadt (Ref: Land)")

html_output <- capture.output(
  stargazer(reg1_links, reg1_mitte, reg1_rechts, type = "html", title = "Politische Einstellung", align = TRUE,
            column.labels = c("Links", "Mitte", "Rechts"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit",
            covariate.labels = covariate.labels))

cat(html_output, file = "politische Einstellung.html", sep = "\n")


### Stundenlohn

covariate.labels <- c("Männer (Referenz: Frauen)", "Links eingestellt (Ref: Mitte)", "Rechts eingestellt (Ref: Mitte)", "hohes Politikinteresse (Ref: Mittel)", "tiefes Politikinteresse (Ref: Mittel)",
                      "viele Arbeitsstunden (Ref: Mittel)", "wenige Arbeitsstunden (Ref: Mittel)", "Sek 1 Abschluss (Ref: Sek 2)" , "Tertiär Abschluss (Ref: Sek 2)", "Alt (Ref: Mittel)", "Jung (Ref: Mittel)", "Rentner (Ref: Mittel)",
                      "Mangement Position", "öffentlicher Sektor", "keine Kinder (Ref: 1-2 Kinder)", "mehr als zwei Kinder (Ref: 1-2 Kinder)", "Romandie (Ref: Deutschschweiz)", "Tessin (Ref: Deutschschweiz)", "in Schweiz geboren", "Kirchgänger aus religiösen Gründen",
                      "Stadt (Ref: Land)")

html_output <- capture.output(
  stargazer(reg1_tief_stdlohn, reg1_mittlerer_stdlohn, reg1_hoher_stdlohn, type = "html", title = "Stundenlohn", align = TRUE,
            column.labels = c("tiefer Stundenlohn", "mittlerer Stundenlohn", "hoher Stundenlohn"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit",
            covariate.labels = covariate.labels))

cat(html_output, file = "Stundenlohn.html", sep = "\n")


### politisches Interesse

covariate.labels <- c("Männer (Referenz: Frauen)", "Links eingestellt (Ref: Mitte)", "Rechts eingestellt (Ref: Mitte)", "hoher Stundenlohn (Ref: Mittel)", "tiefer Stundenlohn (Ref: Mittel)",
                      "viele Arbeitsstunden (Ref: Mittel)", "wenige Arbeitsstunden (Ref: Mittel)", "Sek 1 Abschluss (Ref: Sek 2)" , "Tertiär Abschluss (Ref: Sek 2)", "Alt (Ref: Mittel)", "Jung (Ref: Mittel)", "Rentner (Ref: Mittel)",
                      "Mangement Position", "öffentlicher Sektor", "keine Kinder (Ref: 1-2 Kinder)", "mehr als zwei Kinder (Ref: 1-2 Kinder)", "Romandie (Ref: Deutschschweiz)", "Tessin (Ref: Deutschschweiz)", "in Schweiz geboren", "Kirchgänger aus religiösen Gründen",
                      "Stadt (Ref: Land)")

html_output <- capture.output(
  stargazer(reg1_kleines_interesse, reg1_mittleres_interesse, reg1_hohes_interesse, type = "html", title = "Politisches Interesse", align = TRUE,
            column.labels = c("tiefes Interesse", "mittleres Interesse", "hohes Interesse"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit",
            covariate.labels = covariate.labels))

cat(html_output, file = "politisches Interesse.html", sep = "\n")

### Interaktionsterm
covariate.labels <- c("Männer (Referenz: Frauen)", "Links eingestellt (Ref: Mitte)", "Rechts eingestellt (Ref: Mitte)", "hohes Politikinteresse (Ref: Mittel)", "tiefes Politikinteresse (Ref: Mittel)",
                      "Sek 1 Abschluss (Ref: Sek 2)" , "Tertiär Abschluss (Ref: Sek 2)", "Alt (Ref: Mittel)", "Jung (Ref: Mittel)", "Rentner (Ref: Mittel)",
                      "Mangement Position", "öffentlicher Sektor", "keine Kinder (Ref: 1-2 Kinder)", "mehr als zwei Kinder (Ref: 1-2 Kinder)", "Romandie (Ref: Deutschschweiz)", "Tessin (Ref: Deutschschweiz)", "in Schweiz geboren", "Kirchgänger aus religiösen Gründen",
                      "Stadt (Ref: Land)", "hoher Stundenlohn" , "tiefer Stundenlohn", "viele Arbeitsstunden", "wenige Arbeitsstunden", "hoher Stundenlohn X viele Arbeitsstunden", "tiefer Stundenlohn X viele Arbeitsstunden", "hoher Stundenlohn X wenige Arbeitsstunden", "tiefer Stundenlohn X wenige Arbeitsstunden")

html_output <- capture.output(
  stargazer(reg2_fit, type = "html", title = "Interatkion", align = TRUE,
            column.labels = c("Gesamt"),
            dep.var.caption = "Abhängige Variable",
            dep.var.labels = "Freiwilligenarbeit",
            covariate.labels = covariate.labels))
cat(html_output, file = "Interatkion.html", sep = "\n")
