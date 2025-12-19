#' ---
#' title: "cs06: Wind Farm Suitability Analysis with Raster and Vector Data"
#' subtitle: "Aggregating High Wind Speed Areas by County using `terra` and `tigris`"
#' ---

# Load libraries
library(terra)
library(tidyverse)
library(sf)
library(tigris)
library(ggtext)

# Download county polygons for NYS
counties <- counties(state = "NY", cb = TRUE)

# Load wind data
wind <- rast("wind100.tif")

# Get elevation data, convert to raster, and resample to align with wind data
elev <- elevatr::get_elev_raster(locations = wind, z = 5, clip = "location") |>
  rast() |>
  resample(y = wind, method = "bilinear")

# Standardize CRS
st_crs(counties)
st_crs(wind)
#counties_proj <- st_transform(counties, crs = "+proj=lcc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
counties_proj <- st_transform(counties, crs = st_crs(wind))

# Identify high-wind areas
wind_masked <- mask(wind, counties_proj)
high_wind_raster <- wind_masked > 8

# Mask elevation data to NY extent
elev_masked <- mask(elev, counties_proj)

# Calculate slope and roughness from elevation data
slope <- terrain(elev_masked, v = "slope", unit = "degrees")
roughness <- terrain(elev_masked, v = "roughness")

# Check elevation, slope, and roughness data with quick plots
plot(elev_masked)
plot(slope)
plot(roughness)

# Find suitable areas for wind farms
suitable_rast <- high_wind_raster & (slope < 10) & (roughness < 100)
plot(suitable_rast)

# Summarize suitable area for each county 
pixel_area <- cellSize(suitable_rast, unit = "km")
suitable_land <- suitable_rast * pixel_area
county_summary <- terra::extract(suitable_land, counties_proj, fun = "sum", na.rm = TRUE)

counties_proj_sum <- counties_proj |> 
  bind_cols(county_summary) |>
  rename(suitable_area_km2 = wtk_conus_100m_mean_masked)

# Display table of suitable area for each county
counties_proj_sum |>
  select(NAME, suitable_area_km2) |>
  filter(suitable_area_km2 != 0) |>
  arrange(desc(suitable_area_km2)) |>
  mutate(suitable_area_km2  = round(suitable_area_km2, digits = 2)) |>
  st_drop_geometry()

# Create choropleth
wind_county_choropleth <- ggplot() +
  geom_sf(data = counties_proj_sum, aes(fill = suitable_area_km2)) +
  scale_fill_viridis_c(name = "Area with Wind Speed <br>> 8 m/s (km<sup>2</sup>)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal",
        legend.title = element_markdown()) +
  labs(title = "Potential Wind Farm Area by NY County", 
       subtitle = "Based on average wind speeds at 100 m height", 
       caption = "Data: NREL CONUS Wind Speed & US Census Bureau")

# Save figure
ggsave(wind_county_choropleth, filename = "wind-county-choropleth.png")
ggsave(wind_county_choropleth, filename = "wind-county-choropleth.pdf")
