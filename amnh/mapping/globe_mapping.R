## Globe Visualizations of Bantu Migration Models


## Load in Libraries
library(readxl)
library(globe4r)
library(dplyr)

## Load in Model Data
data <- read_xlsx("../datasets/MigratoryModel/CombinedMigratoryModel_GuthrieZone_Path - AllModels.xlsx")

## Define Data Shaping and Mapping Functions
create_mapdata <- function(data, model){
  froms <- data %>% filter(Model == model,
                           PointOrder == 1)
  tos <- data %>% filter(Model == model,
                         PointOrder == 2)
  
  mapdata <- froms %>%
    inner_join(tos,
               by = "PathID",
               suffix = c(".from", ".to")) %>% 
    mutate(color = ifelse(ApomorphyType.from == "Unambiguous", "#fa7b23", "#28bfce"))
  return(mapdata)
}

create_labeldata <- function(data, model){
  labeldata <- data %>%
    filter(Model == model) %>% 
    group_by(GuthrieZone) %>%
    select(GuthrieZone, Latitude, Longitude)
  return(labeldata)
}

create_model_globe <- function(data, model, title = "", altitude = 0.6){
  output_globe <- create_globe() %>% 
    arcs_data(create_mapdata(data, model = model)) %>% 
    arcs_start_lat("Latitude.from") %>% 
    arcs_start_lon("Longitude.from") %>% 
    arcs_end_lat("Latitude.to") %>% 
    arcs_end_lon("Longitude.to") %>%
    arcs_color("color") %>% 
    arcs_label("PathID") %>%
    # arcs_stroke("stroke") %>% 
    arcs_on_hover(func = "function(data) {var globe = get_globe(data.path);}") %>% 
    arcs_on_click(func = "function(data) {var globe = get_globe(data.path);}") %>% 
    labels_data(create_labeldata(data, model = model)) %>% 
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
    globe_title(title) %>% 
    globe_pov(-9.16, 26.44, altitude = altitude, ms = 0) %>% 
    #globe_img_url(url = "https://eoimages.gsfc.nasa.gov/images/imagerecords/74000/74443/world.topo.200409.3x5400x2700.jpg")
    globe_img_url(url = image_url("blue-marble"))
  
  return(output_globe)
}


## Arrange in Grid
library(manipulateWidget)

## Create Globes
combined_globe <- create_model_globe(data, model = "Combined", title = "Combined", altitude = 0.7)
cult_globe <- create_model_globe(data, model = "Cultural", title = "Cultural")
lang_globe <- create_model_globe(data, model = "Language", title = "Language")
mtdna_globe <- create_model_globe(data, model = "Mitochondrial DNA", title = "Mitochondrial DNA")
ychr_globe <- create_model_globe(data, model = "Y-Chromosomal", title = "Y-Chromosomal")

## Create Widgets of Globes
w_comb <- combineWidgets(title = "Combined", combined_globe, nrow = 1, ncol = 1)
w_cult <- combineWidgets(title = "Cultural", cult_globe, nrow = 1, ncol = 1)
w_lang <- combineWidgets(title = "Language", lang_globe, nrow = 1, ncol = 1)
w_mtdna <- combineWidgets(title = "Mitochondrial DNA", mtdna_globe, nrow = 1, ncol = 1)
w_ychr <- combineWidgets(title = "Y-Chromosomal", ychr_globe, nrow = 1, ncol = 1)

## Combine Widgets into Grid
combineWidgets(#title = "Bantu Migration Models",
               nrow = 2,
               ncol = 2,
               w_cult,
               w_lang,
               w_mtdna,
               w_ychr) %>% 
  combineWidgets(w_comb, nrow = 1, ncol = 2)
