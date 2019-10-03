library(readxl)
library(globe4r)
library(dplyr)

data <- read_xlsx("../datasets/MigratoryModel/CombinedMigratoryModel_GuthrieZone_Path - AllModels.xlsx")

model <- "Combined"

froms <- data %>% filter(Model == model,
                        PointOrder == 1)
tos <- data %>% filter(Model == model,
                       PointOrder == 2)

mapdata <- froms %>%
  inner_join(tos,
             by = "PathID",
             suffix = c(".from", ".to")) %>% 
  mutate(color = ifelse(ApomorphyType.from == "Unambiguous", "#fa7b23", "#28bfce"))

labeldata <- data %>% filter(Model == model) %>% group_by(GuthrieZone) %>% select(GuthrieZone, Latitude, Longitude)

globe <- create_globe() %>% 
  arcs_data(mapdata) %>% 
  arcs_start_lat("Latitude.from") %>% 
  arcs_start_lon("Longitude.from") %>% 
  arcs_end_lat("Latitude.to") %>% 
  arcs_end_lon("Longitude.to") %>%
  arcs_color("color") %>% 
  arcs_label("PathID") %>%
  # arcs_stroke("stroke") %>% 
  arcs_on_hover(func = "function(data) {var globe = get_globe(data.path);}") %>% 
  arcs_on_click(func = "function(data) {var globe = get_globe(data.path);}") %>% 
  labels_data(labeldata) %>% 
  labels_lat("Latitude") %>% 
  labels_lon("Longitude") %>% 
  labels_text("GuthrieZone") %>% 
  labels_include_dot(include = TRUE) %>% 
  labels_dot_radius(radius = 0.3) %>% 
  #scale_labels_size() %>% 
  #scale_labels_radius() %>% 
  globe_background("#fff") %>% 
  show_atmosphere(TRUE) %>%
  show_graticules(TRUE) %>%
  globe_img_url(url = "https://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74443/world.topo.200409.3x5400x2700.jpg")
  #globe_img_url(url = image_url("blue-marble"))

globe
