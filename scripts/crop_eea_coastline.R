library(sf)
library(dplyr)
library(rnaturalearth)

coast <- st_read("data/EEA_Coastline_Polygon_Shape/EEA_Coastline_20170228.shp")

# Step 2: Define the bounding box in lon/lat (WGS 84)
bbox_wgs84 <- st_bbox(c(
  xmin = 10.5,  # western longitude
  xmax = 13.5,  # eastern longitude
  ymin = 56.0,  # southern latitude
  ymax = 59.5   # northern latitude
), crs = 4326)  # WGS 84

# Step 3: Convert bbox to an sf polygon and reproject to match the coastline CRS
bbox_laea <- st_transform(st_as_sfc(bbox_wgs84), st_crs(coast))

# Step 4: Crop the coastline
sw_coast <- st_intersection(coast, bbox_laea)

sw_coast_wgs84 <- st_transform(sw_coast, 4326)

st_write(sw_coast_wgs84, "data/EEA_Coastline_Polygon_Shape_Swedish_west_coast/Swedish_West_Coast_WGS84.shp", delete_layer = TRUE)




### NE Lakes

# Load the full lakes dataset
lakes <- st_read("data/ne_10m_lakes/ne_10m_lakes.shp", quiet = TRUE)

# Load Sweden geometry
sweden <- ne_countries(scale = 10, country = "Sweden", returnclass = "sf")

lakes <- st_transform(lakes, st_crs(sweden))
lakes <- st_make_valid(lakes)
lakes <- lakes[st_is_valid(lakes), ]
sweden_lakes <- st_intersection(lakes, sweden)

st_write(sweden_lakes, "data/ne_10m_lakes/sweden_lakes.shp", delete_layer = TRUE)
