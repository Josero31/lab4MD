# ============================================================
# Limpieza del dataset listings (Airbnb Austin, Texas)
# ============================================================

load("listings.RData")

cat("Dataset original: ", nrow(listings), "filas x", ncol(listings), "columnas\n")

# ----------------------------------------------------------
# 1. Eliminar columnas inútiles para análisis
#    - URLs (no aportan valor numérico/categórico)
#    - Columnas completamente NA
#    - IDs de scraping internos
# ----------------------------------------------------------
cols_eliminar <- c(
  "listing_url", "scrape_id", "picture_url",
  "host_url", "host_thumbnail_url", "host_picture_url",
  "neighbourhood_group_cleansed",   # 100% NA
  "calendar_updated",               # 100% NA
  "license"                         # ~99% NA
)
listings <- listings[, !(names(listings) %in% cols_eliminar)]

# ----------------------------------------------------------
# 2. Convertir precio y tasas a numérico
# ----------------------------------------------------------
# price: "$97.00" -> 97.00
listings$price <- as.numeric(gsub("[$,]", "", listings$price))

# host_response_rate: "100%" -> 100
listings$host_response_rate <- as.numeric(gsub("%", "", listings$host_response_rate))

# host_acceptance_rate: "90%" -> 90
listings$host_acceptance_rate <- as.numeric(gsub("%", "", listings$host_acceptance_rate))

# ----------------------------------------------------------
# 3. Convertir columnas booleanas "t"/"f" a TRUE/FALSE
# ----------------------------------------------------------
cols_bool <- c(
  "host_is_superhost",
  "host_has_profile_pic",
  "host_identity_verified",
  "has_availability",
  "instant_bookable"
)
for (col in cols_bool) {
  listings[[col]] <- listings[[col]] == "t"
}

# ----------------------------------------------------------
# 4. Convertir columnas de fecha (chr) a tipo Date
# ----------------------------------------------------------
cols_fecha <- c(
  "last_scraped",
  "host_since",
  "calendar_last_scraped",
  "first_review",
  "last_review"
)
for (col in cols_fecha) {
  listings[[col]] <- as.Date(listings[[col]])
}

# ----------------------------------------------------------
# 5. Renombrar neighbourhood_cleansed -> zip_code
#    (contiene códigos postales, no nombres de barrio)
# ----------------------------------------------------------
names(listings)[names(listings) == "neighbourhood_cleansed"] <- "zip_code"

# Limpiar columna neighbourhood (valores "Neighborhood highlights" son basura)
listings$neighbourhood[listings$neighbourhood == "Neighborhood highlights"] <- NA
listings$neighbourhood[listings$neighbourhood == ""] <- NA

# ----------------------------------------------------------
# 6. Convertir columnas de texto repetitivo a factor
# ----------------------------------------------------------
cols_factor <- c(
  "source", "host_response_time", "property_type",
  "room_type", "zip_code", "city"
)
for (col in cols_factor) {
  listings[[col]] <- as.factor(listings[[col]])
}

# ----------------------------------------------------------
# 7. Verificación final
# ----------------------------------------------------------
cat("\nDataset limpio: ", nrow(listings), "filas x", ncol(listings), "columnas\n")
cat("\n--- Tipos de datos ---\n")
print(sapply(listings, class))

cat("\n--- Resumen de valores faltantes por columna ---\n")
na_counts <- sort(colSums(is.na(listings)), decreasing = TRUE)
na_pct <- round(na_counts / nrow(listings) * 100, 1)
na_resumen <- data.frame(
  columna = names(na_counts),
  NAs = na_counts,
  porcentaje = paste0(na_pct, "%")
)
print(na_resumen[na_resumen$NAs > 0, ], row.names = FALSE)

cat("\n--- Vista previa de columnas numéricas clave ---\n")
print(summary(listings[, c("price", "host_response_rate", "host_acceptance_rate",
                            "accommodates", "bedrooms", "beds", "bathrooms",
                            "review_scores_rating", "reviews_per_month")]))

# ----------------------------------------------------------
# 8. Guardar dataset limpio
# ----------------------------------------------------------
save(listings, file = "listings_limpio.RData")
cat("\nArchivo guardado: listings_limpio.RData\n")
