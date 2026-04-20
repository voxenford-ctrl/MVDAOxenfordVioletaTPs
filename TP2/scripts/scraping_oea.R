# =============================================================================
# scraping_oea.R
# Objetivo: scrapear los comunicados de prensa de la OEA (Ene-Abr 2026)
#           y guardar los resultados en /data.
# =============================================================================

library(here)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

# -----------------------------------------------------------------------------
# 1. Crear carpeta /data si no existe
# -----------------------------------------------------------------------------
data_dir <- here("TP2", "data")

if (!dir.exists(data_dir)) {
  message("Creando el directorio: ", data_dir)
  dir.create(data_dir, recursive = TRUE)
} else {
  message("El directorio ya existe: ", data_dir)
}

# -----------------------------------------------------------------------------
# 2. Definir meses y año
# -----------------------------------------------------------------------------
meses <- 1:4
anio  <- 2026
base_url <- "https://www.oas.org/es/centro_noticias/comunicados_prensa.asp"

# -----------------------------------------------------------------------------
# 3. Función: obtener links y títulos de una página mensual
# -----------------------------------------------------------------------------
scrape_links_mes <- function(mes, anio) {
  
  url <- paste0(base_url, "?nMes=", mes, "&nAnio=", anio)
  message("Scrapeando índice mes ", mes, ": ", url)
  
  pagina <- read_html(url)
  
  # Guardar HTML con timestamp para registro
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  html_path <- here("TP2", "data", paste0("indice_mes", mes, "_", timestamp, ".html"))
  xml2::write_html(pagina, html_path)
  message("HTML guardado: ", html_path)
  
  # Extraer títulos y links (selector identificado con SelectorGadget)
  nodos   <- html_nodes(pagina, "a.itemmenulink")
  titulos <- html_text(nodos, trim = TRUE)
  links   <- html_attr(nodos, "href")
  
  # Construir URLs absolutas
  links_abs <- ifelse(
    str_starts(links, "http"),
    links,
    paste0("https://www.oas.org/es/centro_noticias/", links)
  )
  
  Sys.sleep(3) # Respetar Crawl-delay del robots.txt
  
  tibble(mes = mes, titulo = titulos, url = links_abs)
}

# -----------------------------------------------------------------------------
# 4. Función: obtener el cuerpo de un comunicado individual
# -----------------------------------------------------------------------------
scrape_cuerpo <- function(url) {
  
  message("  Scrapeando comunicado: ", url)
  Sys.sleep(3) # Respetar Crawl-delay del robots.txt
  
  pagina <- tryCatch(
    read_html(url),
    error = function(e) {
      message("  ERROR: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(pagina)) return(NA_character_)
  
  # Selector del cuerpo del comunicado (identificado con SelectorGadget)
  cuerpo <- pagina |>
    html_node(".field-items") |>
    html_text(trim = TRUE)
  
  if (is.null(cuerpo) || length(cuerpo) == 0) return(NA_character_)
  
  return(cuerpo)
}

# -----------------------------------------------------------------------------
# 5. Iterar sobre los 4 meses
# -----------------------------------------------------------------------------
message("=== Iniciando scraping de índices mensuales ===")
tabla_links <- map_dfr(meses, scrape_links_mes, anio = anio)
message("Total de comunicados encontrados: ", nrow(tabla_links))

# -----------------------------------------------------------------------------
# 6. Scrapear el cuerpo de cada comunicado
# -----------------------------------------------------------------------------
message("=== Iniciando scraping de cuerpos de comunicados ===")
tabla_final <- tabla_links |>
  mutate(
    id     = row_number(),
    cuerpo = map_chr(url, scrape_cuerpo)
  ) |>
  select(id, titulo, cuerpo)

# -----------------------------------------------------------------------------
# 7. Guardar como .rds
# -----------------------------------------------------------------------------
rds_path <- here("TP2", "data", "comunicados_oea.rds")
saveRDS(tabla_final, rds_path)
message("Tabla guardada en: ", rds_path)
message("=== scraping_oea.R finalizado ===")
