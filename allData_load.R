## 
## Load all data from one sheet
## 
## last update: 2018-10-16
## 
## Author:  Mathis Lammert (lammert@cbs.mpg.de)
## 



# Load csv file containing all data
allData <- read.table("allData_metabSample_2018-11-12.csv", 
                      header = TRUE,
                      sep = ";", dec = ".",
                      na.strings = "NA",
                      stringsAsFactors = FALSE)


# Define factors 
allData$Gruppe <- factor(allData$Gruppe, levels = c("BR","GR"))


allData$dem_Schulabschluss = factor(allData$dem_Schulabschluss, 
                                   levels = c("kein Schulabschluss", "Sonderschule", "Haupt-/Volksschulabschluss", "Realschulabschluss", "(Fach-)Abitur", "Sonstige"))
allData$dem_Beruf_Abschluss = factor(allData$dem_Beruf_Abschluss, 
                                    levels = c("ohne Berufsabschluss", "in Berufsausbildung", "FH/Universität", "Meister", "Lehre/Fachschule", "Sonstiges"))

allData$Med_AD <- factor(allData$Med_AD, levels = c("seit OP nicht mehr", 
                                                          "seit OP Dosis verringert", 
                                                          "keine Änderung durch OP", 
                                                          "seit OP Dosis erhöht", 
                                                          "Einnahme erst nach OP begonnen")) 

allData$Med_AD_class <-  factor(allData$Med_AD_class, levels = c("tricyclic", "SSRI", "SNRI", "SSNRI", "SNDRI", "MAO-Inhib", "other"))

allData$BS_procedure <- factor(allData$BS_procedure, levels = c("RYGB", "SG"))

allData$dem_sex <- factor(allData$dem_sex, levels = c("female", "male"))





