# =============================================================================
# processing.R
# Objetivo: limpiar, lematizar y remover stopwords del texto scrapeado.
# =============================================================================

library(here)
library(dplyr)
library(stringr)
library(udpipe)

# -----------------------------------------------------------------------------
# 1. Crear carpeta /output si no existe
# -----------------------------------------------------------------------------
output_dir <- here("TP2", "output")

if (!dir.exists(output_dir)) {
  message("Creando el directorio: ", output_dir)
  dir.create(output_dir, recursive = TRUE)
} else {
  message("El directorio ya existe: ", output_dir)
}

# -----------------------------------------------------------------------------
# 2. Cargar los datos scrapeados
# -----------------------------------------------------------------------------
message("Cargando datos scrapeados...")
comunicados <- readRDS(here("TP2", "data", "comunicados_oea.rds"))
message("Comunicados cargados: ", nrow(comunicados))

# -----------------------------------------------------------------------------
# 3. Limpiar el texto del cuerpo
#    - Sacar puntuación, números y caracteres especiales
#    - Pasar a minúscula
# -----------------------------------------------------------------------------
message("Limpiando texto...")

comunicados <- comunicados |>
  mutate(
    cuerpo_limpio = cuerpo,
    cuerpo_limpio = str_to_lower(cuerpo_limpio),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[0-9]", " "),
    cuerpo_limpio = str_replace_all(cuerpo_limpio, "[[:punct:]]", " "),
    cuerpo_limpio = gsub("[^a-z ]", " ", cuerpo_limpio),
    cuerpo_limpio = str_squish(cuerpo_limpio)
  )

# -----------------------------------------------------------------------------
# 4. Descargar modelo de español para udpipe (solo la primera vez)
# -----------------------------------------------------------------------------
model_path <- here("TP2", "data", "spanish-gsd-ud-2.5-191206.udpipe")

if (!file.exists(model_path)) {
  message("Descargando modelo de español para udpipe...")
  udpipe_download_model(language = "spanish", model_dir = here("TP2", "data"))
} else {
  message("Modelo ya descargado.")
}

# Cargar el modelo
message("Cargando modelo udpipe...")
modelo <- udpipe_load_model(model_path)

# -----------------------------------------------------------------------------
# 5. Lematizar: quedarse con sustantivos, verbos y adjetivos
# -----------------------------------------------------------------------------
message("Lematizando textos (esto puede tardar unos minutos)...")

anotado <- udpipe_annotate(
  object   = modelo,
  x        = comunicados$cuerpo_limpio,
  doc_id   = comunicados$id
) |> as.data.frame()

# Filtrar solo sustantivos (NOUN), verbos (VERB) y adjetivos (ADJ)
lematizado <- anotado |>
  filter(upos %in% c("NOUN", "VERB", "ADJ")) |>
  mutate(lemma = str_to_lower(lemma)) |>
  select(doc_id, lemma)

# -----------------------------------------------------------------------------
# 6. Remover stopwords en español
# -----------------------------------------------------------------------------
message("Removiendo stopwords...")

# Stopwords básicas en español
stopwords_es <- c("ser", "estar", "haber", "tener", "hacer", "poder",
                  "deber", "ir", "ver", "dar", "saber", "querer",
                  "llegar", "pasar", "seguir", "encontrar", "llamar",
                  "venir", "pensar", "decir", "a", "ante", "bajo")

lematizado <- lematizado |>
  filter(!lemma %in% stopwords_es) |>
  filter(str_length(lemma) > 2)  # sacar palabras muy cortas

# -----------------------------------------------------------------------------
# 7. Guardar resultado
# -----------------------------------------------------------------------------
rds_path <- here("TP2", "output", "processed_text.rds")
saveRDS(lematizado, rds_path)
message("Texto procesado guardado en: ", rds_path)
message("=== processing.R finalizado ===")