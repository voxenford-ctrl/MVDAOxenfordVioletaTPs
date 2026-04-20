# =============================================================================
# metrics_figures.R
# Objetivo: calcular frecuencia de términos y generar gráfico de barras.
# =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

# -----------------------------------------------------------------------------
# 1. Cargar texto procesado
# -----------------------------------------------------------------------------
message("Cargando texto procesado...")
lematizado <- readRDS(here("TP2", "output", "processed_text.rds"))
message("Filas cargadas: ", nrow(lematizado))

# -----------------------------------------------------------------------------
# 2. Calcular frecuencia total de términos (DTM simplificada)
# -----------------------------------------------------------------------------
message("Calculando frecuencia de términos...")

frecuencia <- lematizado |>
  count(lemma, name = "frecuencia") |>
  arrange(desc(frecuencia))

# Ver los 20 más frecuentes para orientarse
message("Top 20 términos más frecuentes:")
print(head(frecuencia, 20))

# -----------------------------------------------------------------------------
# 3. Seleccionar 5 términos relevantes para el contexto de la OEA
# -----------------------------------------------------------------------------
terminos_seleccionados <- c("estado", "proceso", "electoral", 
                            "derecho", "permanente")

frecuencia_filtrada <- frecuencia |>
  filter(lemma %in% terminos_seleccionados)

message("Frecuencia de los 5 términos seleccionados:")
print(frecuencia_filtrada)

# -----------------------------------------------------------------------------
# 4. Generar gráfico de barras con ggplot2
# -----------------------------------------------------------------------------
message("Generando gráfico...")

grafico <- ggplot(frecuencia_filtrada, 
                  aes(x = reorder(lemma, frecuencia), y = frecuencia)) +
  geom_col(fill = "#2C7BB6") +
  coord_flip() +
  labs(
    title = "Frecuencia de términos clave en comunicados de la OEA",
    subtitle = "Enero - Abril 2026",
    x = "Término",
    y = "Frecuencia total"
  ) +
  theme_minimal()

# -----------------------------------------------------------------------------
# 5. Guardar figura
# -----------------------------------------------------------------------------
output_path <- here("TP2", "output", "frecuencia_terminos.png")
ggsave(output_path, plot = grafico, width = 8, height = 5, dpi = 300)
message("Figura guardada en: ", output_path)
message("=== metrics_figures.R finalizado ===")