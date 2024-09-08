
# Load and process the shapefile
sf_data <- st_read(shapefile_path)

# Check and convert the CRS (Coordinate Reference System) if needed
if (st_crs(sf_data)$epsg != 4326) {
  sf_data <- st_transform(sf_data, crs = 4326)
}

# Extract relevant columns and add sorting logic
ref_adressen <- sf_data %>%
  arrange(STRASSE, HNR, HNRZ) %>%
  select(
    STRASSE,         # Street name
    HAUSNUMMER,      # House number
    STRASSE_HS,      # Combined street and house number for display
    WL_2024,         # Wohnlage category
    geometry,        # Geometry for mapping
    HNR,             # Numeric part of house number for sorting
    HNRZ             # Suffix part of house number for sorting
  )