library(shiny)
library(dplyr)
library(readr)

# Load the prepared data
load("./data/ref.RData")

# Define the UI
ui <- fluidPage(
  titlePanel("Mietindex Rechner"),
  sidebarLayout(
    sidebarPanel(
      selectInput("adresse", "Adresse", choices = c("", ref_adressen$STRASSE_HS)),
      selectInput("baujahr", "Baujahr", choices = c("", ref_baujahr$Baujahresklasse)),
      numericInput("wohn_groesse_input", "Wohnungsgröße (m²)", value = NULL, min = 15, max = 150),
      actionButton("calculate", "Berechnen")
    ),
    mainPanel(
      uiOutput("report_ui")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive expression for size and address selection
  results <- reactive({
    req(input$calculate)
    
    # Validate inputs
    validate(
      need(!is.null(input$wohn_groesse_input) && input$wohn_groesse_input >= 15 && input$wohn_groesse_input <= 150, 'Bitte gültige Wohnungsgröße eingeben.'),
      need(input$adresse != "", 'Bitte Adresse auswählen.'),
      need(input$baujahr != "", 'Bitte Baujahresklasse auswählen.')
    )
    
    # Get size range from ref_groesse
    size_row <- ref_groesse %>%
      filter(Von <= input$wohn_groesse_input, Bis > input$wohn_groesse_input) %>%
      select(low, med, hi)
    
    von_value <- size_row$low
    med_value <- size_row$med
    bis_value <- size_row$hi
    
    # Get address details
    address_details <- ref_adressen %>%
      filter(STRASSE_HS == input$adresse) %>%
      select(WL_FAKTOR) %>%
      as.numeric()
    
    # Get Baujahresklasse factor
    baujahr_factor <- ref_baujahr %>%
      filter(Baujahresklasse == input$baujahr) %>%
      select(Faktor) %>%
      as.numeric()
    
    # Apply address factor
    von_adjusted_address <- round(von_value * (1 + address_details), 2)
    med_adjusted_address <- round(med_value * (1 + address_details), 2)
    bis_adjusted_address <- round(bis_value * (1 + address_details), 2)
    
    # Apply baujahr factor
    von_adjusted_baujahr <- round(von_value * (1 + address_details) * (1 + baujahr_factor), 2)
    med_adjusted_baujahr <- round(med_value * (1 + address_details) * (1 + baujahr_factor), 2)
    bis_adjusted_baujahr <- round(bis_value * (1 + address_details) * (1 + baujahr_factor), 2)
    
    # Sum values
    ortsueblich_von <- sum(von_value, von_adjusted_address, von_adjusted_baujahr)
    ortsueblich_med <- sum(med_value, med_adjusted_address, med_adjusted_baujahr)
    ortsueblich_bis <- sum(bis_value, bis_adjusted_address, bis_adjusted_baujahr)
    
    # Calculate Nettokaltmiete
    nettokaltmiete_von <- round(ortsueblich_von * input$wohn_groesse_input, 2)
    nettokaltmiete_med <- round(ortsueblich_med * input$wohn_groesse_input, 2)
    nettokaltmiete_bis <- round(ortsueblich_bis * input$wohn_groesse_input, 2)
    
    list(
      size_row = size_row,
      address_row = list(
        address = input$adresse,
        factor = address_details
      ),
      baujahr_row = list(
        baujahr = input$baujahr,
        factor = baujahr_factor
      ),
      ortsueblich = list(
        von = ortsueblich_von,
        med = ortsueblich_med,
        bis = ortsueblich_bis
      ),
      nettokaltmiete = list(
        von = nettokaltmiete_von,
        med = nettokaltmiete_med,
        bis = nettokaltmiete_bis
      )
    )
  })
  
  output$report_ui <- renderUI({
    req(results())
    res <- results()
    
    tagList(
      tableOutput("result_table")
    )
  })
  
  output$result_table <- renderTable({
    res <- results()
    
    data.frame(
      Merkmal = c("Wohnungsgröße", "Adresse", "Baujahr", "Ortsübliche Vergleichsmiete:", "Nettokaltmiete für die angegebenen ", "m²:"),
      Angaben = c(
        paste("Von", res$size_row$low, "bis unter", res$size_row$hi, "m²"),
        paste(res$address_row$address, ", Wohnlage ", ifelse(res$address_row$factor == 0, "Abschlag +-0,00%", paste0("Abschlag ", round(res$address_row$factor * 100, 2), "%"))),
        paste("Baujahr ", res$baujahr_row$baujahr, ", Zu-/Abschlag ", round(res$baujahr_row$factor * 100, 2), "%"),
        paste("Von ", round(res$ortsueblich$von, 2), " ", round(res$ortsueblich$med, 2), " ", round(res$ortsueblich$bis, 2)),
        paste(round(res$nettokaltmiete$von, 2), " ", round(res$nettokaltmiete$med, 2), " ", round(res$nettokaltmiete$bis, 2))
      ),
      von = c("", round(res$size_row$low, 2), "", round(res$ortsueblich$von, 2), round(res$nettokaltmiete$von, 2)),
      ortsueblich = c("", round(res$size_row$med, 2), "", round(res$ortsueblich$med, 2), round(res$nettokaltmiete$med, 2)),
      bis = c("", round(res$size_row$hi, 2), "", round(res$ortsueblich$bis, 2), round(res$nettokaltmiete$bis, 2))
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
