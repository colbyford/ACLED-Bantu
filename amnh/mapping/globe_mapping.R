## Globe Visualizations of Bantu Migration Models
## By: Colby T. Ford, Ph.D.

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
    mutate(color = case_when(ApomorphyType.from == "Unambiguous"  ~ "#fa7b23",
                             ApomorphyType.from == "Ambiguous" ~ "#28bfce",
                             is.na(ApomorphyType.from) ~ "#ff1493"))
  return(mapdata)
}

create_labeldata <- function(data, model){
  labeldata <- data %>%
    filter(Model == model) %>% 
    group_by(GuthrieZone) %>%
    select(GuthrieZone, Latitude, Longitude) %>%
    unique()
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

create_model_map <- function(data, model, title = "", basemapLayer = "Imagery", arrowFilled = TRUE){
  
  ## Reshape input data
  data <- create_mapdata(data, model)
  
  ## Setup for Swoopy.js
  esriPlugin <- htmlDependency("leaflet.esri", "1.0.3",
                               src = c(href = "https://cdn.jsdelivr.net/leaflet.esri/1.0.3/"),
                               script = "esri-leaflet.js"
  )
  
  swoopyPlugin <- htmlDependency("leaflet-swoopy", "3.4.1",
                                 src = c(href = "https://unpkg.com/leaflet-swoopy@3.4.1/build/"),
                                 script = "Leaflet.SwoopyArrow.js"
  )
  
  # swoopyPlugin <- htmlDependency("leaflet-swoopy", "3.4.1", 
  #                                src = c(href = "https://unpkg.com/leaflet-swoopy@3.4.1/build/"),
  #                                script = "Leaflet.SwoopyArrow.min.js"
  # )
  
  registerleafletPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  
  header <- "function(el, x) {"
  ## https://esri.github.io/esri-leaflet/api-reference/layers/basemap-layer.html
  basemap <- paste0("L.esri.basemapLayer('",basemapLayer,"').addTo(this);")
  swoopys <- ""
  for (i in 1:nrow(data)){
    fromLat <- data$Latitude.from[i]
    fromLong <- data$Longitude.from[i]
    toLat <- data$Latitude.to[i]
    toLong <- data$Longitude.to[i]
    color <- data$color[i]
    arrow <- if(arrowFilled){"true"}else{"false"}
    swoopyIter <- paste0("L.swoopyArrow([",fromLat,",",fromLong,"], [",toLat,",",toLong,"], {color: '",color,"', factor: 0.7, weight: 2, arrowFilled: ",arrow,"}).addTo(this);")
    #print(swoopyIter)
    swoopys <- paste0(swoopys, swoopyIter)
  }
  
  # fromLocs <- data %>% select(label.from, Latitude.from, Longitude.from)
  fromLocs <- data %>% select(GuthrieZone.from, Latitude.from, Longitude.from)
  colnames(fromLocs) <- c("location", "latitude", "longitude")
  # toLocs <- data %>% select(label.to, Latitude.to, Longitude.to)
  toLocs <- data %>% select(GuthrieZone.to, Latitude.to, Longitude.to)
  colnames(toLocs) <- c("location", "latitude", "longitude")
  allLocs <- fromLocs %>% rbind(toLocs) %>% unique()
  labels <- ""
  
  for (i in 1:nrow(allLocs)){
    fromLat <- allLocs$latitude[i]
    fromLong <- allLocs$longitude[i]
    toLat <- allLocs$latitude[i]
    toLong <- allLocs$longitude[i]
    loc <- allLocs$location[i]
    labelIter <- paste0("L.swoopyArrow([",fromLat,",",fromLong,"], [",toLat,",",toLong,"], {label: '",loc,"', labelColor: '#ffffff', labelFontSize: 12, iconAnchor: [20, 10], iconSize: [20, 16], factor: 0.7, weight: 0}).addTo(this);")
    #print(labelIter)
    labels <- paste0(labels, labelIter)
  }
  
  footer <- "}"
  
  renderText <- paste0(header, basemap, swoopys, labels, footer)
  #renderText <- paste0(header, swoopys, labels, footer)
  
  output_map <- leaflet(options = leafletOptions(attributionControl = FALSE,
                                                 zoomControl = FALSE)) %>%
    addProviderTiles(providers$Esri.WorldImagery) %>% 
    addScaleBar(position = "bottomleft") %>% 
    #setView(mean(data$Longitude.from), mean(data$Latitude.from), zoom = 5) %>%
    setView(25.083107, -12.058123,  zoom = 4) %>% 
    registerleafletPlugin(esriPlugin) %>%
    registerleafletPlugin(swoopyPlugin) %>% 
    removeScaleBar() %>%
    onRender(renderText)
  
  return(output_map)
}

### GLOBES
## Arrange in Grid
unique(data$Model)
## Create Globes
combined_globe <- create_model_globe(data, model = "Combined", altitude = 0.7)
cult_globe <- create_model_globe(data, model = "Cultural")
lang_globe <- create_model_globe(data, model = "Language")
mtdna_globe <- create_model_globe(data, model = "Mitochondrial DNA")
ychr_globe <- create_model_globe(data, model = "Y-Chromosomal")
dfe_globe <- create_model_globe(data, model = "de Filippo et al., 2011 (\"Early Split\")",)
dfl_globe <- create_model_globe(data, model = "de Filippo et al., 2011 (\"Late Split\")")
cur_globe <- create_model_globe(data, model = "Currie et al., 2013")
gro_globe <- create_model_globe(data, model = "Grollemund et al., 2015")
whi_globe <- create_model_globe(data, model = "Whiteley et al., 2018")

## Create Widgets of Globes
library(manipulateWidget)
w_comb <- combineWidgets(title = "Combined", combined_globe, nrow = 1, ncol = 1)
w_cult <- combineWidgets(title = "Cultural", cult_globe, nrow = 1, ncol = 1)
w_lang <- combineWidgets(title = "Language", lang_globe, nrow = 1, ncol = 1)
w_mtdna <- combineWidgets(title = "mtDNA", mtdna_globe, nrow = 1, ncol = 1)
w_ychr <- combineWidgets(title = "yChr", ychr_globe, nrow = 1, ncol = 1)
w_dfe <- combineWidgets(title = "Filippo (Early)", dfe_globe, nrow = 1, ncol = 1)
w_dfl <- combineWidgets(title = "Filippo (Late)", dfl_globe, nrow = 1, ncol = 1)
w_cur <- combineWidgets(title = "Currie", cur_globe, nrow = 1, ncol = 1)
w_gro <- combineWidgets(title = "Grollemund", gro_globe, nrow = 1, ncol = 1)
w_whi <- combineWidgets(title = "Whiteley", whi_globe, nrow = 1, ncol = 1)

comparison_models <- combineWidgets(nrow = 2,ncol = 2,
                                    w_dfe, w_dfl,
                                    w_cur, w_gro)

single_models <- combineWidgets(nrow = 2, ncol = 2,
                                w_cult, w_lang,
                                w_mtdna, w_ychr)


new_models <- combineWidgets(nrow = 2, ncol = 1,
                             w_comb, w_whi)

## Combine Widgets into Grid
combineWidgets(title = "Bantu Migration Models",
               comparison_models, single_models, new_models,
               nrow = 1, ncol = 3)


################################
## Using Leaflet + Swoopy
library(htmltools)
library(htmlwidgets)
library(leaflet)

## Create Globes
combined_map <- create_model_map(data, model = "Combined")
mapview::mapshot(combined_map,
                 file = "combined_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

cult_map <- create_model_map(data, model = "Cultural")
mapview::mapshot(cult_map,
                 file = "cult_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

lang_map <- create_model_map(data, model = "Language")
mapview::mapshot(lang_map,
                 file = "lang_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

mtdna_map <- create_model_map(data, model = "Mitochondrial DNA")
mapview::mapshot(mtdna_map,
                 file = "mtdna_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

ychr_map <- create_model_map(data, model = "Y-Chromosomal")
mapview::mapshot(ychr_map,
                 file = "ychr_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

dfe_map <- create_model_map(data, model = "de Filippo et al., 2011 (\"Early Split\")",)
mapview::mapshot(dfe_map,
                 file = "dfe_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

dfl_map <- create_model_map(data, model = "de Filippo et al., 2011 (\"Late Split\")")
mapview::mapshot(dfl_map,
                 file = "dfl_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

cur_map <- create_model_map(data, model = "Currie et al., 2013")
mapview::mapshot(cur_map,
                 file = "cur_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

gro_map <- create_model_map(data, model = "Grollemund et al., 2015")
mapview::mapshot(gro_map,
                 file = "gro_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)

whi_map <- create_model_map(data, model = "Whiteley et al., 2018")
mapview::mapshot(whi_map,
                 file = "whi_map.png",
                 remove_controls = c("zoomControl", "layersControl", "homeButton", "scaleBar"),
                 #vwidth = 1920, vheight = 1080,
                 zoom = 3)


## Create Widgets of Globes
library(manipulateWidget)
w_comb <- combineWidgets(title = "Combined", combined_map, nrow = 1, ncol = 1)
w_cult <- combineWidgets(title = "Cultural", cult_map, nrow = 1, ncol = 1)
w_lang <- combineWidgets(title = "Language", lang_map, nrow = 1, ncol = 1)
w_mtdna <- combineWidgets(title = "mtDNA", mtdna_map, nrow = 1, ncol = 1)
w_ychr <- combineWidgets(title = "yChr", ychr_map, nrow = 1, ncol = 1)
w_dfe <- combineWidgets(title = "Filippo (Early)", dfe_map, nrow = 1, ncol = 1)
w_dfl <- combineWidgets(title = "Filippo (Late)", dfl_map, nrow = 1, ncol = 1)
w_cur <- combineWidgets(title = "Currie", cur_map, nrow = 1, ncol = 1)
w_gro <- combineWidgets(title = "Grollemund", gro_map, nrow = 1, ncol = 1)
w_whi <- combineWidgets(title = "Whiteley", whi_map, nrow = 1, ncol = 1)

comparison_models <- combineWidgets(nrow = 2,ncol = 2,
                                    w_dfe, w_dfl,
                                    w_cur, w_gro)

single_models <- combineWidgets(nrow = 2, ncol = 2,
                                w_cult, w_lang,
                                w_mtdna, w_ychr)


new_models <- combineWidgets(nrow = 2, ncol = 1,
                             w_comb, w_whi)

## Combine Widgets into Grid
combineWidgets(title = "Bantu Migration Models",
               comparison_models, single_models, new_models,
               nrow = 1, ncol = 3)
