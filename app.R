
# App
# Laden der notwendigen Bibliotheken
library(shiny)
library(bslib)
library(bsicons)
library(plotly) 
library(shinyWidgets)
library(tidyverse)
library(tm)
library(topicmodels)
library(textstem)
library(tidytext)
library(slam)

# Import der vortrainierten Modelle und Validierungsparameter
final_gbm    <- readRDS("gbm_modell_final.rds")      
final_rf_reg <- readRDS("rf_regr_modell_final.rds")   
lda_model    <- readRDS("lda_modell_final.rds")
vokabular    <- readRDS("trainings_vokabular.rds")
best_t       <- readRDS("optimaler_threshold.rds")   
manuelle_stoppwoerter <- readRDS("manuelle_stoppworte.rds")

# Definition der Themennamen für das Dashboard
themen_namen <- c(
  "Europarecht & Grenzwerte",
  "Gewährleistung & Täuschung",
  "Rückabwicklung & Nutzung",
  "Prozessuale Formalia",
  "Vorstandschaft & Strategie",
  "Emissionen & Prüfstand",
  "Abschalteinrichtungen"
)

source("utils.R") 
# Gestaltung des Headers mit Logo 
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center;",
    tags$img(src = "logo.png", height = "100px", style = "margin-right: 20px;"), 
    div(
      style = "display: flex; flex-direction: column;",
      span("LEGALPREDICT: DIESEL-ABGAS-ANALYTIK", style = "font-weight: bold; font-size: 19px; color: #2c3e50;"),
      span("MANDATSPRÜFUNG", style = "font-size: 17px; color: #6c757d; font-weight: 500;")
    )
  ),
  theme = bs_theme(
    version = 5, 
    bootswatch = "lux", 
    primary = "#2c3e50", 
    base_font = font_google("Inter"),
    bg = "#FFFFFF",  
    fg = "#2c3e50"
  ),
  
  header = tags$head(
    tags$style(HTML("
      body { background-color: #f4f7f9 !important; }
      .navbar { background-color: white !important; border-bottom: 1px solid #dee2e6; }
    "))
  ),
  # Gestaltung der Sidebar mit Eingabefeldern
  nav_panel(
    title = " ", 
    layout_sidebar(
      sidebar = sidebar(
        width = 350,
        title = span("SACHVERHALTSAUFNAHME", style = "font-size: 16px; font-weight: bold; color: #2c3e50;"),
        accordion(
          accordion_panel(
            "Fahrzeugparameter",
            icon = bs_icon("car-front"),
            numericInput("kaufpreis_input", "Kaufpreis (in €):", value = 12500, min = 0),
            numericInput("km_input", "Kilometerstand bei Kauf:", value = 41500, min = 0),
            selectInput("neuwagen", "Zustand bei Kauf:", choices = c("Neuwagen" = 1, "Gebrauchtwagen" = 0)),
            selectInput("motortyp", "Motor EA 189:", choices = c("Ja" = 1, "Nein" = 0))
            # Hubraum hier entfernt
          ),
          accordion_panel(
            "Rechtliche Parameter",
            icon = bs_icon("hammer"),
            helpText("Geben Sie hier den Sachverhalt oder die Klagebegründung ein:"),
            textAreaInput("tatbestand_text", "", rows = 8, placeholder = "Sachverhalt...")
          )
        ),
        actionButton("submit", "Rechtliche Ersteinschätzung generieren", 
                     class = "btn-dark w-100 mt-3", icon = icon("wand-magic-sparkles"))
      ),
  # Gestaltung übriges Layout     
      layout_column_wrap(
        width = 1,
        layout_column_wrap(
          width = 1/2,
          value_box(
            title = "Verfahrens-Tendenz",
            value = uiOutput("tendenz_text"), 
            showcase = bs_icon("graph-up-arrow"),
            theme = "light"
          ),
          uiOutput("euro_box") 
        ),
        
        card(
          card_header(span(bs_icon("speedometer2"), " Erfolgswahrscheinlichkeit")),
          plotlyOutput("gauge_plot", height = "250px")
        ),
        
        card(
          card_header(span(bs_icon("list-check"), " Identifizierte Risikofaktoren")),
          uiOutput("risk_analysis")
        )
      )
    )
  ),
  
  nav_spacer(),
  # Gestaltung des Footers 
  footer = tags$div(
    style = "padding: 10px; border-top: 1px solid #dee2e6; background-color: white; width: 100%; text-align: center;",
    tags$span("INTERNER GEBRAUCH: NUR FÜR AUTORISIERTE ANWALTSKANZLEIEN", 
              class = "text-muted small")
  )
) 

server <- function(input, output, session) {
  
  observeEvent(input$submit, {
    set.seed(123) 
    
    if (nchar(input$tatbestand_text) < 50) {
      sendSweetAlert(session, title = "Sachverhalt zu kurz", text = "Bitte geben Sie einen ausführlicheren Sachverhalt ein.", type = "error")
      return()
    }
    
# Textverarbeitung
    clean_text <- textverarbeitung(input$tatbestand_text, manuelle_stoppwoerter)
    
    tokens_uni <- tibble(word = unlist(strsplit(clean_text, " ")))
    tokens_bi  <- tibble(text = clean_text) %>% unnest_tokens(word, text, token = "ngrams", n = 2)
    tokens_all <- bind_rows(tokens_uni, tokens_bi) %>% filter(word %in% vokabular)
    
    counts <- tokens_all %>% count(word)
    dtm_mat <- matrix(0, nrow = 1, ncol = length(vokabular), dimnames = list("doc", vokabular))
    if(nrow(counts) > 0) dtm_mat[1, counts$word] <- counts$n
    test_dtm_ready <- as.simple_triplet_matrix(dtm_mat)
    
# Topic Modeling: Berechnung der Themenwahrscheinlichkeiten
    topic_probs <- posterior(lda_model, test_dtm_ready)$topics %>% as.data.frame()
    colnames(topic_probs) <- paste0("thema_", 1:7)
    
# Extraktion rechtlicher Risikofaktoren (Flags)
    raw_txt <- tolower(input$tatbestand_text)
    legal_flags <- data.frame(
      has_verjaehrung  = as.integer(str_detect(raw_txt, "verjähr")),
      has_ruecktritt   = as.integer(str_detect(raw_txt, "rücktritt")),
      has_sittenwidrig = as.integer(str_detect(raw_txt, "sittenwidrig")),
      has_update       = as.integer(str_detect(raw_txt, "software-?update|update|rückruf"))
    )
    
# Durchführung der GBM-Klassifikation
    input_gbm <- data.frame(
      kaufpreis_euro   = input$kaufpreis_input, 
      km_stand_kauf    = input$km_input,
      has_update       = legal_flags$has_update,
      motortyp         = as.numeric(input$motortyp),
      hubraum          = 0, # Dummy-Wert für das Modell (bleibt konstant)
      zustand          = as.numeric(input$neuwagen),
      has_verjaehrung  = legal_flags$has_verjaehrung,
      has_ruecktritt   = legal_flags$has_ruecktritt,
      has_sittenwidrig = legal_flags$has_sittenwidrig
    ) %>% bind_cols(topic_probs)
    
    prob_ja <- predict(final_gbm, newdata = input_gbm, type = "prob")$Ja
    
# Durchführung der Random-Forest-Regression
    input_regr <- data.frame(
      kaufpreis_euro = input$kaufpreis_input,
      km_stand_kauf  = input$km_input,
      motortyp       = as.numeric(input$motortyp),
      zustand        = as.numeric(input$neuwagen),
      has_update     = legal_flags$has_update
    ) %>% bind_cols(topic_probs)
    
    euro_prognose <- predict(final_rf_reg, newdata = input_regr)
    
    prob_val <- prob_ja * 100
    
# Ausgabe des Ergebnisses, je nach Wahrscheinlichkeit
    if (prob_val > 60) {
      aktuelle_farbe <- "success" 
      box_titel      <- "Vorauss. Schadensersatz"
      box_wert       <- paste0(format(round(euro_prognose, 2), big.mark="."), " €")
    } else {
      aktuelle_farbe <- "secondary" 
      box_titel      <- "Voraussichtlich kein Anspruch"
      box_wert       <- "0 €"
    }
    
    
    output$tendenz_text <- renderUI({ 
      if(prob_val > 80) {
        span("Hochgradig Erfolgversprechend", style = "font-size: 18px; font-weight: bold; color: #28a745;")
      } else if(prob_val > 60) {
        span("Überwiegend Wahrscheinlich", style = "font-size: 18px; font-weight: 500;")
      } else {
        span("Hohes Prozessrisiko", style = "font-size: 18px; color: #dc3545;")
      }
    })
    
    output$euro_box <- renderUI({
      value_box(title = box_titel, value = box_wert, showcase = bs_icon("currency-euro"), theme = aktuelle_farbe)
    })
    
    output$gauge_plot <- renderPlotly({
      plot_ly(type = "indicator", mode = "gauge+number", value = prob_val,
              gauge = list(axis = list(range = list(0, 100)),
                           bar = list(color = "#2c3e50"),
                           steps = list(list(range = c(0, 50), color = "#f8d7da"),
                                        list(range = c(50, 100), color = "#d4edda"))))
    })
    
    output$risk_analysis <- renderUI({
     # Den Namen des dominanten Themas aus der Liste ziehen
      idx <- which.max(topic_probs)
      tags$ul(
        tags$li(strong("Sittenwidrigkeit:"), if(legal_flags$has_sittenwidrig == 1) "Marker detektiert" else "Keine Hinweise"),
        tags$li(strong("Verjährung:"), if(legal_flags$has_verjaehrung == 1) span("Risiko erkannt", style="color:red;") else "Unkritisch"),
        tags$li(strong("Dominantes Thema:"), themen_namen[idx])
      )
    })
  })
}

shinyApp(ui, server)