# ./scripts/prepRef.R

# Lade erforderliche Bibliotheken
library(dplyr)

# Definiere die Korrekturen für Schreibfehler
typo_correction <- function(data) {
  # Definiere die Korrekturen
  corrections <- list(
    "Klosterwinl" = "Klosterwinkel"
    # Weitere Korrekturen können hier hinzugefügt werden
  )
  
  # Wende die Korrekturen auf alle Spalten an
  for (col in names(data)) {
    for (wrong in names(corrections)) {
      correct <- corrections[[wrong]]
      data[[col]] <- gsub(wrong, correct, data[[col]], fixed = TRUE)
    }
  }
  
  return(data)
}

# Lade die Referenztabelle und wende Korrekturen an
reference_table <- read.csv("./MSR/data/ref_groesse.csv", sep = ";", stringsAsFactors = FALSE)

# Lade die Adresse-Daten
sf_data <- readRDS("./MSR/data/ref_adressen.RDS")

# Wende Korrekturen an
sf_data <- typo_correction(sf_data)

# Füge den WL_faktor hinzu
sf_data <- sf_data %>%
  mutate(WL_faktor = case_when(
    WL_2024 == "A" ~ 0.00,
    WL_2024 == "B" ~ -0.07,
    WL_2024 == "C" ~ -0.10,
    TRUE ~ NA_real_
  ))

# Speichere die Daten in einer RData-Datei
save(reference_table, sf_data, file = "./MSR/data/ref.RData")
