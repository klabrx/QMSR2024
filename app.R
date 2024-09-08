library(shiny)
library(shinyjs)
library(dplyr)
library(readr)

# Load the prepared data
load("./data/ref.RData")

# UI
ui <- fluidPage(
  useShinyjs(),
  
  # External CSS
  includeCSS("styles/styles.css"),
  
  # Header
  div(id = "result-header", "Bestimmung der ortsüblichen Vergleichsmiete, qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  
  # Main content
  div(id = "main-content",
      fluidRow(
        column(4, 
               h3("Angaben zur Wohnung"),
               numericInput("wohn_groesse_input", "Größe in m² lt. Mietvertrag", 
                            value = NA, min = min(ref_groesse$low), 
                            max = max(ref_groesse$hi), step = 0.1),
               div(class = "hover-container",
                   selectInput("adresse_input", "Adresse (zur Bestimmung der Wohnlage)", 
                               choices = c("", ref_adressen$STRASSE_HS), 
                               selected = NULL, 
                               multiple = FALSE),
                   div(class = "hover-text", "Bitte wählen Sie eine Adresse aus der Liste aus. Wenn die Adresse nicht vorhanden ist, überprüfen Sie die Schreibweise oder wenden Sie sich an den Support.")
               )
        ),
        column(8, 
               h3("Ergebnis"),
               uiOutput("report_ui")
        )
      )
  ),
  
  # Footer
  div(id = "result-footer", "Datenschutzerklärung | Impressum")
)

# Server
server <- function(input, output, session) {
  
  output$report_ui <- renderUI({
    
    # Debug print statements
    print(paste("Wohnungsgröße Input:", input$wohn_groesse_input))
    print(paste("Adresse Input:", input$adresse_input))
    
    # Initialize results
    wohn_groesse_result <- "<p>Größenangabe fehlt bzw. nicht zwischen 25 bis unter 150 m²</p>"
    von_adjusted <- med_adjusted <- bis_adjusted <- von_faktor <- med_faktor <- bis_faktor <- sum_von <- sum_med <- sum_bis <- nettokaltmiete_von <- nettokaltmiete_med <- nettokaltmiete_bis <- NA
    
    # Size calculation
    if (!is.na(input$wohn_groesse_input) && input$wohn_groesse_input >= min(ref_groesse$low) && input$wohn_groesse_input <= max(ref_groesse$hi)) {
      row <- ref_groesse[ref_groesse$low <= input$wohn_groesse_input & ref_groesse$hi > input$wohn_groesse_input, ]
      
      if (nrow(row) == 1) {
        wohn_groesse_result <- paste0("von ", row$low, " bis unter ", row$hi, " m²")
        von_adjusted <- round(row$low, 2)
        med_adjusted <- round(row$med, 2)
        bis_adjusted <- round(row$hi, 2)
        
        sum_von <- von_adjusted
        sum_med <- med_adjusted
        sum_bis <- bis_adjusted
      }
    }
    
    # Address calculation
    lage_result <- "<p>Adresse fehlt, Lagenbestimmung nicht möglich</p>"
    adresse <- input$adresse_input
    if (!is.null(adresse) && adresse != "" && !is.na(von_adjusted) && !is.na(med_adjusted) && !is.na(bis_adjusted)) {
      adresse_row <- ref_adressen[ref_adressen$STRASSE_HS == adresse, ]
      
      if (nrow(adresse_row) == 1) {
        wohnlage <- as.character(adresse_row$WL_2024)
        lagenfaktor <- adresse_row$WL_FAKTOR
        
        lage_result <- paste0(adresse, " (WL: ", wohnlage, ", ", 
                              ifelse(lagenfaktor == 0, "±0%", 
                                     ifelse(lagenfaktor > 0, paste0("+", lagenfaktor * 100, "%"), paste0(lagenfaktor * 100, "%"))), ")")
        
        # Calculate adjusted values
        von_faktor <- round(von_adjusted * lagenfaktor, 2)
        med_faktor <- round(med_adjusted * lagenfaktor, 2)
        bis_faktor <- round(bis_adjusted * lagenfaktor, 2)
        
        # Sum up values
        sum_von <- sum_von + von_faktor
        sum_med <- sum_med + med_faktor
        sum_bis <- sum_bis + bis_faktor
      }
    }
    
    # Calculate rent
    if (!is.na(input$wohn_groesse_input) && !is.na(sum_von) && !is.na(sum_med) && !is.na(sum_bis)) {
      nettokaltmiete_von <- round(sum_von * input$wohn_groesse_input, 2)
      nettokaltmiete_med <- round(sum_med * input$wohn_groesse_input, 2)
      nettokaltmiete_bis <- round(sum_bis * input$wohn_groesse_input, 2)
    }
    
    # Generate results table
    HTML(paste0("<table class='table'>
                 <tr><th>Merkmal</th><th>Angaben</th><th style='text-align: center;'>Von</th><th style='text-align: center;' class='highlight'>Ortsüblich</th><th style='text-align: center;'>Bis</th></tr>
                 <tr><td>Wohnungsgröße</td><td>", wohn_groesse_result, "</td><td style='text-align: center;'>", 
                von_adjusted, "</td><td style='text-align: center;' class='highlight'>", med_adjusted, "</td><td style='text-align: center;'>", bis_adjusted, "</td></tr>
                 <tr><td>Lage</td><td>", lage_result, "</td><td style='text-align: center;'>", 
                von_faktor, "</td><td style='text-align: center;' class='highlight'>", med_faktor, "</td><td style='text-align: center;'>", bis_faktor, "</td></tr>
                 <tr><td colspan='2'><b>Ortsübliche Vergleichsmiete:</b></td><td style='text-align: center;'><b>", 
                sum_von, "</b></td><td style='text-align: center;' class='highlight'><b>", sum_med, "</b></td><td style='text-align: center;'><b>", sum_bis, "</b></td></tr>
                 <tr><td colspan='2'><b>Nettokaltmiete für die angegebenen ", input$wohn_groesse_input, " m²:</b></td><td style='text-align: center;'><b>", 
                nettokaltmiete_von, "</b></td><td style='text-align: center;' class='highlight'><b>", nettokaltmiete_med, "</b></td><td style='text-align: center;'><b>", nettokaltmiete_bis, "</b></td></tr>
                 </table>"))
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
