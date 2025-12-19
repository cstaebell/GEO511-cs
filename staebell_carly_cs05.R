#' ---
#' title: "cs05: Analyzing Google Trends and the 2024 Solar Eclipse"
#' subtitle: "Joining, Wrangling, and Visualizing Spatial and Temporal Data"

## Pre-processing
# Load libraries
library(tidyverse)
library(sf)
library(tigris)      # For U.S. Census boundary data
  options(tigris_use_cache = TRUE)
library(ggpubr) # for multi-panel ggplot graphs

# Load data
eyes_hurt_state <- read_csv("eyes_hurt_map_state.csv", skip = 2) |>
  rename(state = "Region", state_hits = "eyes hurt: (4/3/24 - 4/10/24)")

eyes_hurt_time <- read_csv("eyes_hurt_timeline.csv", skip = 2) |>
  rename(date = "Day", us_hits = "eyes hurt: (United States)")

e_path <- st_read("upath_lo.kml")

# Download and prepare state spatial data
nonconus <- c("Guam", "Hawaii", "Alaska",
              "Commonwealth of the Northern Mariana Islands",
              "United States Virgin Islands", "American Samoa", "Puerto Rico")

states_sf <- states() |>
  filter(!NAME %in% nonconus) |>
  select(NAME, geometry)

plot(states_sf)

# Join search data with state spatial data
state_searches <- states_sf |>
  left_join(eyes_hurt_state, by = join_by("NAME" == "state"))

# Check if any states didn't match
states_sf |>
  anti_join(eyes_hurt_state, by = join_by("NAME" == "state"))

## Spatial Analysis and Visualization
# Crop and reproject the eclipse layer
st_crs(e_path)
st_crs(state_searches)
e_path2 <- st_transform(e_path, "EPSG:4269") |>
  st_crop(state_searches)

# Check eclipse path on map
ggplot() +
  geom_sf(data = state_searches) +
  geom_sf(data = e_path2)

# Calculate distance to eclipse path
dist <- as.numeric(st_distance(states_sf, e_path2) |>
  units::set_units(km))

state_searches2 <- state_searches |> 
  mutate(dist = dist, 
         intersect = ifelse(dist == 0, TRUE, FALSE))

# Visualize average search hits by state
state_map <- ggplot() +
  geom_sf(data = state_searches2, aes(fill = state_hits)) +
  geom_sf(data = e_path2, fill = NA, color = "red", linewidth = 1.25) +
  labs(title = "Google Trends for 'eyes hurt' following the 2024 Solar Eclipse", 
       subtitle = "Search intensity by state (week of April 3-10, 2024)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank()) +
  scale_fill_viridis_c(name = "Search Intensity")

# Visualize search interest over time
time_plot <- ggplot() +
  geom_line(data = eyes_hurt_time, aes(x = date, y = us_hits), linewidth = 1.25, color = "navy") +
  geom_vline(xintercept = as.Date("2024-04-08"), linetype = "dashed", linewidth = 1, color = "red") +
  annotate("text", x = as.Date("2024-04-17"), y = 85, label = "Solar Eclipse\n April 8, 2024", color = "red") +
  theme_minimal() +
  labs(title = "Google Search Trends for 'eyes hurt' in the U.S.", 
       subtitle = "March 1 - May 31, 2024", 
       x = "Date", 
       y = "Relative Search Interest (0-100)", 
       caption = "Data Sources: Google Trends, NASA SVS, U.S. Census Bureau (via tigris)") +
  coord_fixed(ratio = .2)

# Stack the plots vertically in one figure
ggarrange(state_map, time_plot, ncol = 1)

# Take a closer look at the relationship between search interest and distance to path
# Scatter plot with simple linear model shows inverse relationship between search intensity and distance
ggplot(data = state_searches2, aes(x = dist, y = state_hits)) +
  geom_point(size = 3, color = "darkblue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "darkred") +
  scale_y_continuous(limits = c(0, NA)) +
  theme_light() +
  labs(x = "Distance to Eclipse Path (km)", 
       y = "Relative Search Interest ('eyes hurt')",
       title = "Google Search Interest vs Distance to 2024 Eclipse Path", 
       subtitle = "Each point represents a U.S. state", 
       caption = "Data Sources: Google Trends, NASA SVS, U.S. Census Bureau (via tigris)")

# Boxplot comparing searches for states within the path vs. outside the path
ggplot(data = state_searches2) + 
  geom_boxplot(aes(x = intersect, y = state_hits), fill = "darkgray", alpha = 0.8) +
  labs(x = "Eclipse Path Status", 
       y = "Relative Search Interest ('eyes hurt')", 
       title = "Search Interest by Eclipse Path Intersection") +
  scale_x_discrete(labels = c("Outside of Path", "Within Path")) + 
  theme_light()
