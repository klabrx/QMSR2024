# ./scripts/prepRef.R

# Benötigte Pakete laden
library(dplyr)

# Schritt 1: Daten laden
# Laden der Referenztabelle für Größen
ref_groesse <- read.csv("./MSR/data/RefTab_Groesse.csv", sep = ";", stringsAsFactors = FALSE)

# Laden der Datei für Adressen (als RDS-Datei gespeichert)
ref_adressen <- readRDS("./MSR/data/sf_data.RDS") 

# Falls Sie die Daten aus einer RData-Datei laden möchten:
# load("./data/adr_2024.RData")
# ref_adressen <- sf_data

# Schritt 2: Daten bereinigen
# Tippfehler korrigieren in allen Spalten
replace_typo <- function(df, old_value, new_value) {
  df %>% mutate(across(everything(), ~ gsub(old_value, new_value, ., fixed = TRUE)))
}

# Tippfehler korrigieren in ref_adressen
ref_adressen <- replace_typo(ref_adressen, "Klosterwinl", "Klosterwinkel")

# Beispiel für weitere Bereinigungen, wenn erforderlich:
# ref_adressen <- replace_typo(ref_adressen, "another_old_value", "correct_value")

# Falls es eine Referenzdatei für Baujahresklassen gibt, laden und bereinigen:
# ref_baujahr <- read.csv("./data/RefTab_Baujahr.csv", sep = ";", stringsAsFactors = FALSE)
# ref_baujahr <- replace_typo(ref_baujahr, "old_value", "correct_value")

# Schritt 3: Speichern in einer RData-Datei
# Speichern der bereinigten DataFrames
save(ref_groesse, ref_adressen, file = "./MSR/data/ref.RData")

# Falls ref_baujahr hinzugefügt wurde:
# save(ref_groesse, ref_adressen, ref_baujahr, file = "./MSR/data/ref.RData")
