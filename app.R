library(shiny)
library(shinyjs)
library(dplyr)

# Load the prepared data
load("./data/ref.RData")

# Define UI
ui <- fluidPage(
  useShinyjs(),
  
  # Include external CSS
  includeCSS("styles/styles.css"),
  
  # Fixed header
  div(id = "result-header", "Bestimmung der ortsüblichen Vergleichsmiete, qualifizierter Mietspiegel der Stadt Passau ab 2024"),
  
  # Main content
  div(id = "main-content",
      fluidRow(
        column(4, 
               h3("Angaben zur Wohnung"),
               # Input field for apartment size
               numericInput("wohn_groesse_input", "Größe in m² lt. Mietvertrag", 
                            value = NULL, min = 15, 
                            max = 150, step = 0.1),
               # Dropdown field for address selection
               selectInput("adresse_input", "Adresse (zur Bestimmung der Wohnlage)", 
                           choices = c("", ref_adressen$STRASSE_HS), 
                           selected = NULL, 
                           multiple = FALSE),
               # Dropdown field for Baujahresklasse selection
               selectInput("baujahr_input", "Baujahr", 
                           choices = c("", ref_baujahr$Baujahresklasse),
                           selected = NULL,
                           multiple = FALSE)
        ),
        column(8, 
               h3("Ergebnis"),
               # Dynamic report displayed with renderUI
               uiOutput("report_ui")
        )
      )
  ),
  
  # Fixed footer
  div(id = "result-footer", "Datenschutzerklärung | Impressum")
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive expression to calculate results
  results <- reactive({
    req(input$wohn_groesse_input)
    
    # Initialize variables
    von_adjusted <- med_adjusted <- bis_adjusted <- NA
    von_adjusted_address <- med_adjusted_address <- bis_adjusted_address <- NA
    von_adjusted_baujahr <- med_adjusted_baujahr <- bis_adjusted_baujahr <- NA
    
    # Check apartment size
    if (!is.na(input$wohn_groesse_input) && input$wohn_groesse_input > 15 && input$wohn_groesse_input <= 150) {
      row <- ref_groesse %>% 
        filter(von <= input$wohn_groesse_input & bis_unter > input$wohn_groesse_input)
      
      if (nrow(row) == 1) {
        von_adjusted <- row$low
        med_adjusted <- row$med
        bis_adjusted <- row$hi
      }
    }
    
    # Check address and factor
    if (!is.null(input$adresse_input) && input$adresse_input != "") {
      address_row <- ref_adressen %>% filter(STRASSE_HS == input$adresse_input)
      
      if (nrow(address_row) == 1) {
        factor <- address_row$WL_FAKTOR
        von_adjusted_address <- round(von_adjusted * (factor), 2)
        med_adjusted_address <- round(med_adjusted * (factor), 2)
        bis_adjusted_address <- round(bis_adjusted * (factor), 2)
        address_text <- paste0(input$adresse_input, ", Wohnlage ", address_row$WL_2024, ", Abschlag ", round(factor * 100, 2), "%")
      } else {
        address_text <- "Adresse nicht gefunden"
      }
    } else {
      address_text <- "Adresse fehlt"
    }
    
    # Check Baujahresklasse and factor
    if (!is.null(input$baujahr_input) && input$baujahr_input != "") {
      baujahr_row <- ref_baujahr %>% filter(Baujahresklasse == input$baujahr_input)
      
      if (nrow(baujahr_row) == 1) {
        baujahr_factor <- baujahr_row$Faktor
        von_adjusted_baujahr <- round(von_adjusted * (baujahr_factor), 2)
        med_adjusted_baujahr <- round(med_adjusted * (baujahr_factor), 2)
        bis_adjusted_baujahr <- round(bis_adjusted * (baujahr_factor), 2)
        baujahr_text <- paste0("Baujahr ", input$baujahr_input, ", Zu-/Abschlag ", round(baujahr_factor * 100, 2), "%")
      } else {
        baujahr_text <- "Baujahresklasse nicht gefunden"
      }
    } else {
      baujahr_text <- "Baujahresklasse fehlt"
    }
    
    # Compute totals
    ortsueblich_von <- round(sum(von_adjusted, von_adjusted_address, von_adjusted_baujahr, na.rm = TRUE), 2)
    ortsueblich_med <- round(sum(med_adjusted, med_adjusted_address, med_adjusted_baujahr, na.rm = TRUE), 2)
    ortsueblich_bis <- round(sum(bis_adjusted, bis_adjusted_address, bis_adjusted_baujahr, na.rm = TRUE), 2)
    
    list(
      von_adjusted = von_adjusted,
      med_adjusted = med_adjusted,
      bis_adjusted = bis_adjusted,
      von_adjusted_address = von_adjusted_address,
      med_adjusted_address = med_adjusted_address,
      bis_adjusted_address = bis_adjusted_address,
      address_text = address_text,
      von_adjusted_baujahr = von_adjusted_baujahr,
      med_adjusted_baujahr = med_adjusted_baujahr,
      bis_adjusted_baujahr = bis_adjusted_baujahr,
      baujahr_text = baujahr_text,
      ortsueblich_von = ortsueblich_von,
      ortsueblich_med = ortsueblich_med,
      ortsueblich_bis = ortsueblich_bis,
      nettokaltmiete_von = round(ortsueblich_von * input$wohn_groesse_input, 2),
      nettokaltmiete_med = round(ortsueblich_med * input$wohn_groesse_input, 2),
      nettokaltmiete_bis = round(ortsueblich_bis * input$wohn_groesse_input, 2)
    )
  })
  
  # Render UI for the results table
  output$report_ui <- renderUI({
    res <- results()
    
    # Dynamically get the size range for the input size
    size_row <- ref_groesse %>%
      filter(von <= input$wohn_groesse_input & bis_unter > input$wohn_groesse_input)
    
    size_from <- if (nrow(size_row) > 0) size_row$von else "–"
    size_to <- if (nrow(size_row) > 0) size_row$bis_unter else "–"
    
    # Format currency values
    format_currency <- function(value) {
      if (is.na(value) || value == "–") {
        return("–")
      } else {
        return(formatC(value, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
      }
    }
    
    HTML(paste0("<table class='table'>
               <tr><th>Merkmal</th><th>Angaben</th><th style='text-align: center;'>Von</th><th style='text-align: center;' class='highlight'>Ortsüblich</th><th style='text-align: center;'>Bis</th></tr>
               <tr><td>Wohnungsgröße</td><td>Von ", size_from, " bis unter ", size_to, " m²</td><td style='text-align: center;'>", 
                format_currency(res$von_adjusted), "</td><td style='text-align: center;' class='highlight'>", 
                format_currency(res$med_adjusted), "</td><td style='text-align: center;'>", 
                format_currency(res$bis_adjusted), "</td></tr>
               <tr><td>Adresse</td><td>", res$address_text, "</td><td style='text-align: center;'>", 
                format_currency(res$von_adjusted_address), "</td><td style='text-align: center;' class='highlight'>", 
                format_currency(res$med_adjusted_address), "</td><td style='text-align: center;'>", 
                format_currency(res$bis_adjusted_address), "</td></tr>
               <tr><td>Baujahr</td><td>", res$baujahr_text, "</td><td style='text-align: center;'>", 
                format_currency(res$von_adjusted_baujahr), "</td><td style='text-align: center;' class='highlight'>", 
                format_currency(res$med_adjusted_baujahr), "</td><td style='text-align: center;'>", 
                format_currency(res$bis_adjusted_baujahr), "</td></tr>
               <tr><td colspan='2'><b>Ortsübliche Vergleichsmiete:</b></td><td style='text-align: center;'>", 
                format_currency(res$ortsueblich_von), "</td><td style='text-align: center;' class='highlight'>", 
                format_currency(res$ortsueblich_med), "</td><td style='text-align: center;'>", 
                format_currency(res$ortsueblich_bis), "</td></tr>
               <tr><td colspan='2'><b>Nettokaltmiete für die angegebenen ", input$wohn_groesse_input, " m²:</b></td><td style='text-align: center;'>", 
                format_currency(res$nettokaltmiete_von), "</td><td style='text-align: center;' class='highlight'>", 
                format_currency(res$nettokaltmiete_med), "</td><td style='text-align: center;'>", 
                format_currency(res$nettokaltmiete_bis), "</td></tr>
               </table>"))
  })
  
  # Display notification if any mandatory field is missing
  observe({
    req(input$wohn_groesse_input, input$adresse_input, input$baujahr_input)
    
    if (is.null(input$wohn_groesse_input) || input$wohn_groesse_input == "" ||
        is.null(input$adresse_input) || input$adresse_input == "" ||
        is.null(input$baujahr_input) || input$baujahr_input == "") {
      shinyjs::alert("Pflichtangabe fehlt")
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
