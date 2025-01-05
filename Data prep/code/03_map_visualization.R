#-------------------------------------------------------------------------------
# This code create map with burial sites
#-------------------------------------------------------------------------------
rm(list = ls())

library(dplyr)
library(readxl)
library(openxlsx)

# dir
dir1 <- "../intermediate/"

dt <- read_excel(paste0(dir1, "02_unified_factors.xlsx"))

# summary table
summary <- dt %>%
  select(Site, Period, ID, Confession) %>%
  unique() %>%
  filter(Site != "Kriveikiškiai") %>%
  group_by(Site, Period, Confession) %>%
  summarise(n = n(), .groups = "drop")

map <- FALSE
if (map) {
  library(ggmap)
  library(leaflet)
  library(purrr)
  library(ggrepel)
  library(osmdata)

  key <- "impute_key"
  register_google(key = key)

  sites_df_geocoded <- summary %>%
    mutate(Site = paste0(Site, ", Vilnius")) %>%
    mutate(geocode_result = map(Site, ~ geocode(.x))) %>%
    mutate(Site = gsub(", Vilnius", "", Site))

  sites_df <- sites_df_geocoded %>%
    mutate(
      lat = map_dbl(geocode_result, "lat"),
      lon = map_dbl(geocode_result, "lon")
    ) %>%
    select(-geocode_result) %>%
    mutate(Confession = ifelse(is.na(Confession),
      "Undetermined", Confession
    )) %>%
    filter(Site != "Verkiai")

  # fix
  sites_df <- sites_df %>%
    mutate(
      lon =
        case_when(
          Site ==
            "Franciscan Church of the Assumption of the Blessed Virgin Maria" ~
            25.279630,
          Site == "Čiurlionio str. 3" ~ 25.266740,
          Site == "Church of the Apostles Philip and Jacob" ~ 25.272160,
          TRUE ~ lon
        ),
      lat =
        case_when(
          Site ==
            "Franciscan Church of the Assumption of the Blessed Virgin Maria" ~
            54.680010,
          Site == "Čiurlionio str. 3" ~ 54.682610,
          Site == "Church of the Apostles Philip and Jacob" ~ 54.690670,
          TRUE ~ lat
        )
    ) %>%
    mutate(
      Site =
        ifelse(
          Site ==
            "Franciscan Church of the Assumption of the Blessed Virgin Maria",
          "Franciscan Church",
          Site
        )
    )


  bbox <- c(
    left = min(sites_df$lon) - 0.007,
    bottom = min(sites_df$lat) - 0.002,
    right = max(sites_df$lon) + 0.007,
    top = max(sites_df$lat) + 0.002
  )

  base_map <- get_map(bbox, maptype = "stamen_terrain_background")

  # Fetch OpenStreetMap data within the bounding box
  osm_highways <- opq(bbox = bbox) %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf()
  osm_water <- opq(bbox = bbox) %>%
    add_osm_feature(key = "natural", value = "water") %>%
    osmdata_sf()
  osm_rail <- opq(bbox = bbox) %>%
    add_osm_feature(
      key = "railway",
      value = c("rail")
    ) %>%
    osmdata_sf()
  osm_green <- opq(bbox = bbox) %>%
    add_osm_feature(
      key = "leisure",
      value = c(
        "park", "nature_reserve",
        "recreation_ground", "golf_course",
        "pitch", "garden"
      )
    ) %>%
    osmdata_sf()
  osm_greenspace <- opq(bbox = bbox) %>%
    add_osm_feature(key = "landuse", value = "grass") %>%
    osmdata_sf()

  osm_buildings <- opq(bbox = bbox) %>%
    add_osm_feature(key = "building") %>%
    osmdata_sf()

  # Create the map plot with the base map
  ggplot() +
    geom_sf(data = osm_rail$osm_lines, color = "grey79", size = 0.5) +
    geom_sf(
      data = osm_green$osm_multipolygons,
      fill = "darkolivegreen3", color = "darkolivegreen3",
      size = 0.5, alpha = 0.7
    ) +
    geom_sf(
      data = osm_green$osm_polygons,
      fill = "darkolivegreen3", color = "darkolivegreen3",
      size = 0.5, alpha = 0.7
    ) +
    geom_sf(
      data = osm_greenspace$osm_polygons,
      fill = "darkolivegreen3", color = "darkolivegreen3",
      size = 0.5, alpha = 0.7
    ) +
    geom_sf(
      data = osm_water$osm_multipolygons,
      fill = "lightskyblue1", color = "lightskyblue1"
    ) +
    geom_sf(
      data = osm_buildings$osm_polygons,
      fill = "tan", color = NA, alpha = 0.5
    ) +
    geom_sf(data = osm_highways$osm_lines, color = "grey79", size = 0.5) +
    coord_sf(
      xlim = c(min(sites_df$lon) - 0.007, max(sites_df$lon) + 0.007),
      ylim = c(min(sites_df$lat) - 0.002,
        top = max(sites_df$lat) + 0.002
      ), expand = FALSE
    ) +
    geom_point(
      data = sites_df,
      aes(x = lon, y = lat, shape = Confession, color = Period),
      size = 5
    ) +
    geom_text_repel(
      data = sites_df, aes(x = lon, y = lat, label = Site),
      fontface = "bold", size = 3, max.overlaps = 10,
      nudge_x = 0.001,
      nudge_y = 0.001
    ) +
    theme_minimal() +
    scale_shape_manual(
      values = c(
        "Catholic" = 15, "Orthodox" = 16,
        "Protestants" = 17, "Undetermined" = 18
      ),
      name = "Confession"
    ) +
    theme(
      legend.position = "right",
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.title = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 10)
    )
}
