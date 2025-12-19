#' ---
#' title: "cs04: Exploring Relational and Spatial Airport Data with Tidyverse"
#' subtitle: "Joining, Wrangling, and Mapping with `nycflights13`"

# Load packages
library(tidyverse)
# Load nycflights13 for flight and airport data
library(nycflights23)
# Load sf for simple features, providing standardized support for spatial vector data
library(sf)
# Load spData for world map data, useful for base maps
library(spData)

# Load data
data(flights)
data(airports)

# Explore the `nycflights23` Data Structures
glimpse(flights)
glimpse(airports)

# Note: link using faa code (either origin or dest in flights)
# spatial info (lat, lon) is only in the airports data

# Identify the Farthest Airport from NYC (Relational Data Wrangling)
farthest_airport <- flights |>
  filter(origin == "JFK") |>
  arrange(desc(distance)) |>
  slice_head() |>
  left_join(airports, join_by("dest" == "faa")) |>
  pull(name)

# Visualize Airports and Highlight the Farthest Destination (Simple Spatial Visualization)
# Create sf object from airports. Filter to remove non-JFK destination airports and add indicator column for farthest/JFK/other
airports_from_nyc <- airports |> 
  filter(faa %in% flights$dest | faa == "JFK") |>
  mutate(cat = case_when(name == farthest_airport ~ "Farthest Airport", faa == "JFK" ~ "JFK", 
                         name != farthest_airport & faa != "JFK" ~ "Other Destinations")) |>
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326")
# ^ Could have done this using TRUE ~ "Other Destinations as last condition in case_when

# Plot base map and airports
ggplot() +
  geom_sf(data = world, color = "white") +
  geom_sf(data = airports_from_nyc, aes(color = cat), alpha = 0.7, size = 2.5) +
  coord_sf(crs = 4326, xlim = c(-175, 1), ylim = c(0, 90)) +
  theme_minimal() +
  theme(axis.text.y = element_blank(), legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 90, by = 20)) +
  labs(title = "Global Airports with NYC Origins", 
       subtitle = "Farthest airport identified: Daniel K Inouye International Airport", 
       color = "Airport Type", caption = "Data: nycflights23 and spData") +
  scale_color_manual(values = c("red", "coral1", "navy"))
  
