library(tm)
library(stringr)
library(textstem)

# --- Textnormalisierung ---
normalize_raw <- function(x) {
  if (is.na(x)) return("")
  x <- tolower(x)
  x <- str_replace_all(x, "\u00A0", " ")
  x <- str_replace_all(x, "[\r\n\t]+", " ")
  x <- str_replace_all(x, "\\s+", " ")
  return(x)
}

# --- Juristische Marker (Regex) ---
extract_legal_flags <- function(text) {
  raw <- normalize_raw(text)
  data.frame(
    has_verjaehrung  = as.integer(str_detect(raw, regex("verjähr", ignore_case = TRUE))),
    has_ruecktritt   = as.integer(str_detect(raw, regex("rücktritt", ignore_case = TRUE))),
    has_sittenwidrig = as.integer(str_detect(raw, regex("sittenwidrig", ignore_case = TRUE))),
    has_update       = as.integer(str_detect(raw, regex("update|rückruf|nachbesserung", ignore_case = TRUE)))
  )
}

# --- Die große Textverarbeitungs-Pipeline ---
textverarbeitung <- function(text, custom_stopwords) {
  corpus <- VCorpus(VectorSource(text))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, content_transformer(function(x) str_replace_all(x, "[^[:alpha:][:space:]]", " ")))
  
  corpus <- tm_map(corpus, content_transformer(function(x) {
    x <- str_replace_all(x, "\\bkba\\b", "kraftfahrtbundesamt")
    x <- str_replace_all(x, "\\beur\\b", "euro")
    x <- str_replace_all(x, "\\babschalteinrichtungen\\b", "abschalteinrichtung")
    return(x)
  }))
  
  if(!missing(custom_stopwords)) corpus <- tm_map(corpus, removeWords, custom_stopwords)
  
  corpus <- tm_map(corpus, content_transformer(function(x) str_replace_all(x, "§§?\\s*\\d+[a-z]*(\\s*f{1,2}\\.)?", " ")))
  corpus <- tm_map(corpus, removeWords, stopwords("de"))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(lemmatize_strings))
  
  return(as.character(sapply(corpus, content)))
}