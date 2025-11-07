# title: "cs10: Satellite Remote Sensing"
# subtitle: "Analyzing Satellite-Derived Land Surface Temperature and Land Cover"

# Setup and Data Download
library(terra)
library(tidyverse)
library(sf)
library(knitr)
library(kableExtra)
library(rasterVis)
library(ncdf4)

dir.create("data", showWarnings = FALSE)

lulc_url <- "https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MCD12Q1.051_aid0001.nc?raw=true"
lst_url <- "https://github.com/adammwilson/DataScienceData/blob/master/inst/extdata/appeears/MOD11A2.006_aid0001.nc?raw=true"

download.file(lulc_url, destfile = "data/MCD12Q1.051_aid0001.nc", mode = "wb")
download.file(lst_url, destfile = "data/MOD11A2.006_aid0001.nc", mode = "wb")

# Load and Explore the Data
nc_open("data/MCD12Q1.051_aid0001.nc")
lulc <- rast("data/MCD12Q1.051_aid0001.nc", subds = "Land_Cover_Type_1")
names(lulc)
plot(lulc[[13]], main = "MODIS Land Cover (Year 13)")

nc_open("data/MOD11A2.006_aid0001.nc")
lst <- rast("data/MOD11A2.006_aid0001.nc", subds = "LST_Day_1km")
names(lst)

# Convert and Label Land Cover
Land_Cover_Type_1 <- c(
  "Water" = 0, "Evergreen Needleleaf forest" = 1, "Evergreen Broadleaf forest" = 2,
  "Deciduous Needleleaf forest" = 3, "Deciduous Broadleaf forest" = 4, "Mixed forest" = 5,
  "Closed shrublands" = 6, "Open shrublands" = 7, "Woody savannas" = 8, "Savannas" = 9,
  "Grasslands" = 10, "Permanent wetlands" = 11, "Croplands" = 12, "Urban & built-up" = 13,
  "Cropland/Natural vegetation mosaic" = 14, "Snow & ice" = 15, "Barren/Sparsely vegetated" = 16
)

lcd <- data.frame(
  ID = Land_Cover_Type_1,
  landcover = names(Land_Cover_Type_1),
  stringsAsFactors = FALSE
)

lulc <- as.factor(lulc)

# Convert LST to Celsius
scoff(lst) = cbind(0.02, -273.15)
plot(lst)

# Part 1: Extract Time Series for a Point
buffalo_point <- st_as_sf(data.frame(lon = -78.79, lat = 43.00), 
                          coords = c("lon", "lat"), 
                          crs = 4326) |> 
  st_transform(dst = st_crs(lst))

buff_lst <- terra::extract(lst, buffalo_point) |>
  pivot_longer(cols = starts_with("LST")) |>
  mutate(time = time(lst)) |>
  select(time, value)

ggplot(buff_lst) +
  geom_point(aes(x = time, y = value)) +
  geom_smooth(aes(x = time, y = value), color = "blue", se = FALSE, span = 0.025) +
  labs(x = "date", y = "Land Surface Temperature (\u00B0C)", 
       title = "Time Series of LST Near Buffalo, NY")

# Part 2: Monthly Climatology

# Summarize the weekly data into monthly means using `tapp()`.
monthly_lst <- tapp(x = lst, index = "months", fun = "mean", na.rm = TRUE)
names(monthly_lst) <- c(month.name[2:12], month.name[1])

ggplot() +
  tidyterra::geom_spatraster(data = monthly_lst) +
  facet_wrap(~lyr) +
  scale_fill_gradient2(low = "blue", 
                       mid = "grey",
                       high = "red",
                       midpoint = (max(values(monthly_lst), na.rm = TRUE) 
                                   + min(values(monthly_lst), na.rm = TRUE))/2)

# Part 3: Compare LST by Land Cover Type
  # Explore how LST differs between Urban & built-up and Deciduous Broadleaf Forest areas.

lulc_resample <- resample(lulc[[13]], monthly_lst, method = "near")

# extract values from land use and temp rasters, join landcover labels, filter for specific landcover types
lulc_lst_df <- bind_cols(values(lulc_resample), values(monthly_lst)) |>
  left_join(lcd, by = c("Land_Cover_Type_1_13" = "ID")) |>
  filter(landcover == c("Urban & built-up", "Deciduous Broadleaf forest")) |>
  pivot_longer(cols = all_of(month.name)) |>
  mutate(month = factor(name, levels = month.name))

# plot monthly distributions by landcover type
ggplot(lulc_lst_df) +
  geom_violin(aes(x = month, y = value, fill = landcover)) +
  labs(x = "Month", y = "LST (\u00b0C)", title = "Monthly LST by Land Cover Type",
       fill = "Land Cover") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Reflection
# ✅ What seasonal patterns do you see in LST?  
 # LST is lowest in December, January, and February, then increases through the summer months
 # before decreasing through the fall months. The temperature of Lake Erie lags the land temp.

# ✅ How do urban and forested areas differ across months?  
 # Urban areas tend to have higher temperatures than forested ones, 
 # especially during the warmer months.

# ✅ What are some limitations of using MODIS LST for urban temperature studies?
 # Cloud cover may result in missing, incomplete, or inaccurate temperature data from MODIS. 
 # Earth-based observation methods may be a useful complement to MODIS data for urban
 # temperature studies.

