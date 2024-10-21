## ----config, include=FALSE------------------------------------------------------------------------
## increase width for code output
.options_old <- options(width = 100)
## set up knitr defaults
NOT_CRAN <- identical(tolower(Sys.getenv("NOT_CRAN")), "true")
knitr::opts_chunk$set(purl = NOT_CRAN, eval = NOT_CRAN,
                      out.width = '100%', out.height = '560px')

## ----doc, include=FALSE, eval=TRUE----------------------------------------------------------------
## create alias
doc <- openrouteservice:::doc_link

## ----cran, eval=FALSE-----------------------------------------------------------------------------
#  install.packages("openrouteservice")

## ----installation, eval=FALSE---------------------------------------------------------------------
#  # install.packages("pak")
#  pak::pak("GIScience/openrouteservice-r")

## ----api_key, eval=FALSE--------------------------------------------------------------------------
#  library(openrouteservice)
#  
#  ors_api_key("<your-api-key>")

## ----directions-----------------------------------------------------------------------------------
library(openrouteservice)

coordinates <- list(c(8.34234, 48.23424), c(8.34423, 48.26424))

x <- ors_directions(coordinates)

## ----data_frame-----------------------------------------------------------------------------------
coordinates <- data.frame(lon = c(8.34234, 8.34423), lat = c(48.23424, 48.26424))

## ----leaflet--------------------------------------------------------------------------------------
library(leaflet)

leaflet() %>%
  addTiles() %>%
  addGeoJSON(x, fill=FALSE) %>%
  fitBBox(x$bbox)

## ----encodedpolyline------------------------------------------------------------------------------
x <- ors_directions(coordinates, format = "json")

geometry <- x$routes[[1]]$geometry
str(geometry)

## ----googlepolyline-------------------------------------------------------------------------------
library(googlePolylines)
str(decode(geometry))

## ----profiles-------------------------------------------------------------------------------------
ors_profile()

## ----bicycle--------------------------------------------------------------------------------------
x <- ors_directions(coordinates, profile="cycling-mountain")

leaflet() %>%
  addTiles() %>%
  addGeoJSON(x, fill=FALSE) %>%
  fitBBox(x$bbox)

## ----cycling_mountain, message=FALSE--------------------------------------------------------------
library("sf")

x <- ors_directions(coordinates, profile = "cycling-mountain", elevation = TRUE,
                    extra_info = "steepness", output = "sf")

height <- st_geometry(x)[[1]][, 3]

## ----segments-------------------------------------------------------------------------------------
points <- st_cast(st_geometry(x), "POINT")
n <- length(points)
segments <- cumsum(st_distance(points[-n], points[-1], by_element = TRUE))

## ----steepness------------------------------------------------------------------------------------
steepness <- x$extras$steepness$values
steepness <- rep(steepness[,3], steepness[,2]-steepness[,1])
steepness <- factor(steepness, -5:5)

palette = setNames(rev(RColorBrewer::brewer.pal(11, "RdYlBu")), levels(steepness))

## ----elevation_profile, fig.dim=c(10, 5), message=FALSE, out.height='100%'------------------------
library("ggplot2")
#library("ggforce")
library("units")

units(height) <- as_units("m")

df <- data.frame(x1 = c(set_units(0, "m"), segments[-(n-1)]),
                 x2 = segments,
                 y1 = height[-n],
                 y2 = height[-1],
                 steepness)

y_ran = range(height) * c(0.9, 1.1)

n = n-1

df2 = data.frame(x = c(df$x1, df$x2, df$x2, df$x1),
                 y = c(rep(y_ran[1], 2*n), df$y2, df$y1),
                 steepness,
                 id = 1:n)

ggplot() + theme_bw() +
  geom_segment(data = df, aes(x1, y1, xend = x2, yend = y2), linewidth = 1) +
  geom_polygon(data = df2, aes(x, y, group = id), fill = "white") +
  geom_polygon(data = df2, aes(x, y , group = id, fill = steepness)) +
  scale_fill_manual(values = alpha(palette, 0.8), drop = FALSE) +
  scale_x_units(unit = "km", expand = c(0,0)) +
  scale_y_units(expand = c(0,0), limits = y_ran) +
  labs(x = "Distance", y = "Height")

## ----bicycle-avoid--------------------------------------------------------------------------------
polygon = list(
    type = "Polygon",
    coordinates = list(
      list(
        c(8.330469, 48.261570),
        c(8.339052, 48.261570),
        c(8.339052, 48.258227),
        c(8.330469, 48.258227),
        c(8.330469, 48.261570)
      )
    ),
    properties = ""
  )

options <- list(
  avoid_polygons = polygon
)

x <- ors_directions(coordinates, profile="cycling-mountain", options=options)

leaflet() %>%
  addTiles() %>%
  addGeoJSON(polygon, color="#F00") %>%
  addGeoJSON(x, fill=FALSE) %>%
  fitBBox(x$bbox)

## ----isochrones_ranges----------------------------------------------------------------------------
library(mapview)

# embed data in the output file
mapviewOptions(fgb = FALSE)

coordinates <- data.frame(lon = c(8.34234, 8.34234), lat = c(48.23424, 49.23424))

## 30 minutes range split into 10 minute intervals
res <- ors_isochrones(coordinates, range = 1800, interval = 600, output = "sf")
res

values <- levels(factor(res$value))
ranges <- split(res, values)
ranges <- ranges[rev(values)]

names(ranges) <- sprintf("%s min", as.numeric(names(ranges))/60)

mapview(ranges, alpha.regions = 0.2, homebutton = FALSE, legend = FALSE)

## ----isochrones_colors----------------------------------------------------------------------------
locations = split(res, res$group_index)

locations <- lapply(locations, function(loc) {
  g <- st_geometry(loc)
  g[-which.min(values)] <- st_sfc(Map(st_difference,
                                      g[match(values[-which.min(values)], loc$value)],
                                      g[match(values[-which.max(values)], loc$value)]))
  st_geometry(loc) <- g
  loc
})

isochrones <- unsplit(locations, res$group_index)

pal <- setNames(heat.colors(length(values)), values)
mapview(isochrones, zcol = "value", col = pal, col.regions = pal,
        alpha.regions = 0.5, homebutton = FALSE)

## ----matrix---------------------------------------------------------------------------------------
coordinates <- list(
  c(9.970093, 48.477473),
  c(9.207916, 49.153868),
  c(37.573242, 55.801281),
  c(115.663757,38.106467)
)

# query for duration and distance in km
res <- ors_matrix(coordinates, metrics = c("duration", "distance"), units = "km")

# duration in hours
(res$durations / 3600) %>% round(1)

# distance in km
res$distances %>% round

## ----geocode--------------------------------------------------------------------------------------
## locations of Heidelberg around the globe
x <- ors_geocode("Heidelberg")

leaflet() %>%
  addTiles() %>%
  addGeoJSON(x) %>%
  fitBBox(x$bbox)

## set the number of results returned
x <- ors_geocode("Heidelberg", size = 1)

## search within a particular country
x <- ors_geocode("Heidelberg", boundary.country = "DE")

## structured geocoding
x <- ors_geocode(list(locality="Heidelberg", county="Heidelberg"))

## reverse geocoding
location <- x$features[[1L]]$geometry$coordinates

y <- ors_geocode(location = location, layers = "locality", size = 1)

## ----pois-----------------------------------------------------------------------------------------
geometry <- list(
  geojson = list(
    type = "Point",
    coordinates = c(8.8034, 53.0756)
  ),
  buffer = 500
)

ors_pois(
  request = 'pois',
  geometry = geometry,
  limit = 2000,
  sortby = "distance",
  filters = list(
    category_ids = 488,
    wheelchair = "yes"
  ),
  output = "sf"
)

## ----stats----------------------------------------------------------------------------------------
ors_pois(
  request = 'stats',
  geometry = geometry,
  limit = 2000,
  sortby = "distance",
  filters = list(category_ids = 488)
  )

## ----elevation------------------------------------------------------------------------------------
x <- ors_geocode("Königstuhl", output = "sf")

ors_elevation("point", st_coordinates(x))

## ----vehicles-------------------------------------------------------------------------------------
home_base <- data.frame(lon = 2.370658, lat = 48.721666)

vehicles = vehicles(
  id = 1:2,
  profile = "driving-car",
  start = home_base,
  end = home_base,
  capacity = 4,
  skills = list(c(1, 14), c(2, 14)),
  time_window = c(28800, 43200)
)

## ----jobs-----------------------------------------------------------------------------------------
locations <- list(
  c(1.98806, 48.705),
  c(2.03655, 48.61128),
  c(2.39719, 49.07611),
  c(2.41808, 49.22619),
  c(2.28325, 48.5958),
  c(2.89357, 48.90736)
)

jobs = jobs(
  id = 1:6,
  service = 300,
  amount = 1,
  location = locations,
  skills = list(1, 1, 2, 2, 14, 14)
)

## ----optimization---------------------------------------------------------------------------------
res <- ors_optimization(jobs, vehicles, options = list(g = TRUE))

## -------------------------------------------------------------------------------------------------
lapply(res$routes, with, {
  list(
    geometry = googlePolylines::decode(geometry)[[1L]],
    locations = lapply(steps, with, if (type=="job") location) %>%
      do.call(rbind, .) %>% data.frame %>% setNames(c("lon", "lat"))
  )
  }) -> routes

## Helper function to add a list of routes and their ordered waypoints
addRoutes <- function(map, routes, colors) {
  routes <- mapply(c, routes, color = colors, SIMPLIFY = FALSE)
  f <- function (map, route) {
    with(route, {
      labels <- sprintf("<b>%s</b>", 1:nrow(locations))
      markers <- awesomeIcons(markerColor = color, text = labels, fontFamily = "arial")
      map %>%
        addPolylines(data = geometry, lng = ~lon, lat = ~lat, col = ~color) %>%
        addAwesomeMarkers(data = locations, lng = ~lon, lat = ~lat, icon = markers)
    })
  }
  Reduce(f, routes, map)
}

leaflet() %>%
  addTiles() %>%
  addAwesomeMarkers(data = home_base, icon = awesomeIcons("home")) %>%
  addRoutes(routes, c("purple", "green"))

## ----cleanup, include=FALSE---------------------------------------------------
## restore user's options
options(.options_old)

