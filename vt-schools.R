library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(htmltools)
library(janitor)
library(scales)

vt_dist <- read_sf("https://opendata.arcgis.com/datasets/03147644b3db427e8117d9f7bf895a0b_56.geojson") %>%
  clean_names()

vt_schools <- read_sf("https://opendata.arcgis.com/datasets/efa79839b0f841ac92c821a7b8afeda5_3.geojson") %>%
  clean_names()

school_enroll <- read_csv("data/enrollment.csv") %>%
  clean_names() %>%
  filter(sy == max(sy))

by_union <- vt_dist %>%
  count(supername)

by_dist <- vt_dist %>%
  count(distname, supername)

schools_clean <- vt_schools %>%
  st_join(by_dist) %>%
  left_join(select(school_enroll, org_id, total), by = c("orgid" = "org_id")) %>%
  mutate(
    color = if_else(sector == "Public", "#263C4B", "#3F95B0"),
    label = paste0(
      "<b>", organizati, "</b></br>",
      city, "</br>",
      sector, " ", grtype, "</br>",
      "2021 enrollment: ", coalesce(comma(total, 1), "--"), "</br>",
      "District: ", distname, "</br>",
      "Supervisory Union: ", supername, "</br>"
      ),
    label = map(label, HTML)
    )


m <- leaflet() %>%
  addMapPane("poly", zIndex = 500) %>%
  addMapPane("point", zIndex = 600) %>%
  addPolygons(
    data = by_union,
    weight = 1.5,
    color = "white",
    fillColor = "green",
    fillOpacity = 0.3,
    label = ~supername,
    group = "Supervisory Union",
    options = pathOptions(pane = "poly"),
    highlightOptions = highlightOptions(
      weight = 5,
      opacity = 1,
      color = "#666",
      fillOpacity = 0.6
    )
  ) %>%
  addPolygons(
    data = by_dist,
    weight = 1.5,
    color = "white",
    fillColor = "purple",
    fillOpacity = 0.3,
    label = ~distname,
    group = "District",
    options = pathOptions(pane = "poly"),
    highlightOptions = highlightOptions(
      weight = 5,
      opacity = 1,
      color = "#666",
      fillOpacity = 0.6,
    )
  ) %>%
  addCircles(
    group = "Schools",
    data = schools_clean,
    color = ~color,
    opacity = 0.9,
    fillOpacity = 0.7,
    weight = 8,
    label = ~label,
    options = pathOptions(pane = "point"),
  ) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addSearchFeatures(
    targetGroups = c("Schools", "Supervisory Union", "District"),
    options = searchFeaturesOptions(
      zoom = 11,
      openPopup = TRUE,
      collapsed = FALSE,
      position = "topright",
      hideMarkerOnCollapse = TRUE,
      textPlaceholder = "Search schools and SUs"
    )
  ) %>%
  addResetMapButton() %>%
  addLegend(
    colors = unique(schools_clean$color),
    labels = c("Public", "Independent"),
    opacity = 0.7,
    position = "bottomright"
  ) %>%
  addLayersControl(
    position = "topleft",
    baseGroups = c("Supervisory Union", "District"),
    options = layersControlOptions(
      collapsed = FALSE
      )
    )

htmlwidgets::saveWidget(m, "site/vt-schools.html", selfcontained = FALSE)
