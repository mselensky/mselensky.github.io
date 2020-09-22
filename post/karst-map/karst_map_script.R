# This is the code I wrote to create an interactive global karst map using leaflet
# Check out my blog post for the full explanation! -->
# https://mselensky.github.io/post/karst-map/mapping-karst-around-the-world

# download shapefile from the following link and extract to your project folder:
  # https://produktcenter.bgr.de/terraCatalog/OpenSearch.do?search=ab3b15cb-a6c3-42ea-ae0c-0b417d698949&type=/Query/OpenSearch.do

# load required packages
pacman::p_load(tidyverse, randomcoloR, rgdal, leaflet, htmltools)

# determine number of layers we can work with
ogrListLayers("WHYMAP_WOKAM/shp")

# import WOKAM shape file layers 
cave_points <- readOGR("WHYMAP_WOKAM/shp", layer = "whymap_cave__v3_point") %>%
  as.data.frame() %>% fortify()
spring_points <- readOGR("WHYMAP_WOKAM/shp", layer = "whymap_spring__v3_point") %>%
  as.data.frame() %>% fortify()
karst_poly <- readOGR("WHYMAP_WOKAM/shp", layer = "whymap_karst__v1_poly")

# replace missing values with NA
cave_points <- cave_points %>%
  mutate_at(vars("c_depth_m", "c_length_k"), ~replace(., . < 0, NA)) %>%
  mutate(class = "cave")
spring_points <- spring_points %>%
  mutate_at(vars("s_low_qms", "s_high_qms"), ~replace(., . < 0, NA)) %>%
  mutate(class = "spring")

# merge shapefile point dataframes
merged_points <- full_join(cave_points, spring_points)

# generate color palette for karst topography based on surface rock expression
poly_colors <- colorFactor(c(distinctColorPalette(
  length(unique(levels(karst_poly@data[["RTypeLabel"]]))))), 
  domain = levels(karst_poly@data[["RTypeLabel"]]))

# generate color palette for "spring" and "cave" points
class_colors <- colorFactor(c("red", "#600734"), domain = merged_points$class)

# make an interactive map of global karst environments with leaflet
leaflet(merged_points) %>% 
  addProviderTiles(providers$Esri.WorldShadedRelief) %>%
  addPolygons(data = karst_poly, 
              color = ~poly_colors(RTypeLabel), 
              weight = 1, 
              fillOpacity = 0.6, 
              popup = ~htmlEscape(RTypeLabel)) %>%
  addCircleMarkers(~coords.x1, ~coords.x2, 
                   radius = 2, 
                   color = ~class_colors(class), 
                   popup = ~htmlEscape(name), fillOpacity = 1)

