library(sf)
library(spData)

world
class(world)
names(world)
summary(world["lifeExp"])

plot(world)
plot(world[,"pop"])


#creating geometries
point <- st_point(c(5, 2))

linestring <- st_linestring(
  rbind(
    c(1, 5),
    c(4, 4),
    c(4, 1),
    c(2, 2),
    c(3, 2)
  )
)

polygon <- st_polygon(
  list(
    rbind(
      c(1, 5),
      c(2, 2),
      c(4, 1),
      c(4, 4),
      c(1, 5)
    )
  )
)

#plot them 
par(mfrow = c(3, 1),
    mar = c(2, 2, 2, 1))

plot(point,
     axes = TRUE,
     xlim = c(0, 6),
     ylim = c(0, 6),
     pch = 16,
     cex = 2,
     main = "POINT")

plot(linestring,
     axes = TRUE,
     xlim = c(0, 6),
     ylim = c(0, 6),
     lwd = 3,
     main = "LINESTRING")

plot(polygon,
     axes = TRUE,
     xlim = c(0, 6),
     ylim = c(0, 6),
     col = "lightblue",
     border = "black",
     lwd = 2,
     main = "POLYGON")

#creating multiple geometries

multipoint <- st_multipoint(
  rbind(
    c(5, 2),
    c(1, 3),
    c(3, 4),
    c(3, 2)
  )
)

multilinestring <- st_multilinestring(
  list(
    rbind(
      c(1, 5),
      c(4, 4),
      c(4, 1),
      c(2, 2),
      c(3, 2)
    ),
    rbind(
      c(1, 2), 
      c(2, 4)
    )
  )
)

multipolygon <- st_multipolygon(
  list(
    list(
      rbind(
        c(1, 5),
        c(2, 2),
        c(4, 1),
        c(4, 4),
        c(1, 5)
      )
    ),
    list(
      rbind(
        c(0, 2),
        c(1, 2),
        c(1, 3),
        c(0, 3),
        c(0, 2)
      )
    )
  )
)

#plot them 
par(mfrow = c(3, 1),
    mar = c(2, 2, 2, 1))

plot(multipoint,
     axes = TRUE,
     xlim = c(0, 6),
     ylim = c(0, 6),
     pch = 16,
     cex = 2,
     main = "MULTIPOINT")

plot(multilinestring,
     axes = TRUE,
     xlim = c(0, 6),
     ylim = c(0, 6),
     lwd = 3,
     main = "MULTILINESTRING")

plot(multipolygon,
     axes = TRUE,
     xlim = c(0, 6),
     ylim = c(0, 6),
     col = "lightblue",
     border = "black",
     lwd = 2,
     main = "MULTIPOLYGON")

#geometry collection
geometrycollection <- st_geometrycollection(
  list(
    st_multipoint(
      rbind(
        c(5, 2),
        c(1, 3),
        c(3, 4),
        c(3, 2)
      )
    ),
    st_linestring(
      rbind(
        c(1, 5),
        c(4, 4),
        c(4, 1),
        c(2, 2),
        c(3, 2)
      )
    )
  )
)

#plot it
par(mfrow = c(1, 1),
    mar = c(2, 2, 2, 1))

plot(geometrycollection,
     axes = TRUE,
     xlim = c(0, 6),
     ylim = c(0, 6),
     pch = 16,
     cex = 2,
     main = "MULTIPOINT")

# simple feature column

point1 <- st_point(c(5, 2))
point2 <- st_point(c(1, 3))
points_sfc <- st_sfc(point1, point2)
points_sfc

multilinestring1 <- st_multilinestring(list(rbind(c(1, 5), c(4, 4), c(4, 1), c(2, 2), c(3, 2)), 
                             rbind(c(1, 2), c(2, 4))))
point_multilinestring_sfc <- st_sfc(point1, multilinestring1)
point_multilinestring_sfc
st_geometry_type(point_multilinestring_sfc)

#crs

st_crs(points_sfc)

points_sfc_wgs <- st_sfc(point1, point2, crs = "EPSG:4326")
st_crs(points_sfc_wgs) # p

# create sf

rome_point <- st_point(c(12.4964, 41.9028))       # sfg object

rome_geom <- st_sfc(
  rome_point,
  crs = "EPSG:4326"
)                                                 # sfc object

rome_attrib <- data.frame(
  name = "Rome",
  temperature = 30,
  date = as.Date("2023-06-21")
)                                                 # data.frame object

rome_sf <- st_sf(
  rome_attrib,
  geometry = rome_geom
)

rome_sf

plot(st_geometry(world),
     col = "grey90",
     border = "grey70",
     axes = TRUE)

plot(rome_sf,
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 2)



class(world)
names(world)
world_small <- world[, c("name_long", "continent", "subregion")]
head(world_small)

world$pop_density <- world$pop / world$area_km2
head(world$pop_density)



rome_3857 <- st_transform(rome_sf, 3857)

rome_3857


plot(st_geometry(world),
     col = "grey90",
     border = "grey70",
     axes = TRUE)

plot(rome_sf,
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 2)

world_3857 <- st_transform(world, 3857)
plot(st_geometry(world_3857),
     col = "grey90",
     border = "grey70",
     axes = TRUE)


plot(rome_3857,
     add = TRUE,
     axes = TRUE,
     pch = 16,
     cex = 2)


# buffer

italy <- world[world$name_long == "Italy", ]

# S2 ON
sf_use_s2(TRUE)

rome_buffer_s2 <- st_buffer(
  rome_sf,
  dist = 100000
)

# S2 OFF
sf_use_s2(FALSE)

rome_buffer_deg <- st_buffer(
  rome_sf,
  dist = 1 #1 degree!!! using 100000 would be SUPER WRONG!
)

# Back ON
sf_use_s2(TRUE) #useless

# Projected
rome_3857 <- st_transform(rome_sf, 3857)
italy_3857 <- st_transform(italy, 3857)

rome_buffer_proj <- st_buffer(
  rome_3857,
  dist = 100000
)

#plot them
plot(st_geometry(italy),
     col = "grey95",
     border = "grey70",
     axes = TRUE,
     main = "Geographic + S2 ON")

plot(st_geometry(rome_buffer_s2),
     add = TRUE,
     col = rgb(0,0,1,0.3),
     border = "blue")

plot(st_geometry(rome_sf),
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 1.5)



plot(st_geometry(italy),
     col = "grey95",
     border = "grey70",
     axes = TRUE,
     main = "Geographic + S2 OFF")

plot(st_geometry(rome_buffer_deg),
     add = TRUE,
     col = rgb(1,0,0,0.3),
     border = "red")

plot(st_geometry(rome_sf),
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 1.5)



plot(st_geometry(italy_3857),
     col = "grey95",
     border = "grey70",
     axes = TRUE, 
     main = "Projected CRS (3857)")

plot(st_geometry(rome_buffer_proj),
     add = TRUE,
     col = rgb(0,1,0,0.3),
     border = "darkgreen")

plot(st_geometry(rome_3857),
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 1.5)


### which is the correct buffer?

plot(st_geometry(italy),
     col = "grey95",
     border = "grey70",
     axes = TRUE,
     main = "Geographic + S2 ON (asp=1)",
     asp=1)

plot(st_geometry(rome_buffer_s2),
     add = TRUE,
     col = rgb(0,0,1,0.3),
     border = "blue")

plot(st_geometry(rome_sf),
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 1.5)




plot(st_geometry(italy),
     col = "grey95",
     border = "grey70",
     axes = TRUE,
     main = "Geographic + S2 OFF (asp=1)",
     asp=1)

plot(st_geometry(rome_buffer_deg),
     add = TRUE,
     col = rgb(1,0,0,0.3),
     border = "red")

plot(st_geometry(rome_sf),
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 1.5)


# Projected with 32633
rome_32633 <- st_transform(rome_sf, 32633)
italy_32633 <- st_transform(italy, 32633)

rome_buffer_proj_loc <- st_buffer(
  rome_32633,
  dist = 100000
)

plot(st_geometry(italy_32633),
     col = "grey95",
     border = "grey70",
     axes = TRUE, 
     main = "Projected CRS (32633)")

plot(st_geometry(rome_buffer_proj_loc),
     add = TRUE,
     col = rgb(0,1,0,0.3),
     border = "darkgreen")

plot(st_geometry(rome_32633),
     add = TRUE,
     pch = 16,
     col = "red",
     cex = 1.5)


#compare the three things

rome_32633 <- st_transform(rome_sf, 32633)
# UTM (Universal Transverse Mercator) Zone 33N
# A local projected CRS covering much of Italy.
# Distances are expressed in meters and distortions are very small
# around Rome.

rome_buffer_32633 <- st_buffer(
  rome_32633,
  dist = 100000
)

rome_buffer_wgs84 <- st_transform(
  rome_buffer_32633,
  4326
)


#big plot
par(fig = c(0.0000001, 1, 0.0000001, 1))
plot(
  st_geometry(italy),
  col = "grey95",
  border = "grey70",
  axes = TRUE,
  main = "Geodesic vs UTM 33N"
)

box()

plot(st_geometry(rome_buffer_s2),
     add = TRUE,
     col = rgb(0,0,1,0.3),
     border = "blue")

plot(st_geometry(rome_sf),
     add = TRUE,
     pch = 16,
     col = "blue",
     cex = 1.5)

plot(st_geometry(rome_buffer_wgs84),
     add = TRUE,
     col = NA,
     border = "red")


#zoom
par(
  fig = c(0.01, 0.43, 0.01, 0.45),
  mar = c(0,0,0,0),
  new = TRUE
)



plot(
  st_geometry(italy),
  col = "grey95",
  border = "grey70",
  #axes = FALSE,
  xlab = "",
  ylab = "",
  main = "",
  xlim = c(11.5, 12.2),
  ylim = c(41.2, 42.3)
)

plot(st_geometry(rome_buffer_s2),
     add = TRUE,
     col = rgb(0,0,1,0.3),
     border = "blue")

plot(st_geometry(rome_sf),
     add = TRUE,
     pch = 16,
     col = "blue",
     cex = 1.5)

plot(st_geometry(rome_buffer_wgs84),
     add = TRUE,
     col = NA,
     border = "red")

box()


#### Raster ####

library(terra)
f <- system.file("ex/elev.tif", package = "terra")
r <- rast(f)
class(r)

r


plot(r)

values(r)[1:10]
r[3,32]
r[(3-1)*ncol(r) +32]

summary(r)

global(r, "mean", na.rm=TRUE) 
mean(values(r), na.rm=TRUE)

minmax(r)

r - global(r, "mean", na.rm=TRUE)[1,1]


#multilayer

s <- c(r, r*2, r*3)

s

s[[1]]          # first raster layer

s[[1]][10]      # 10th cell of the first layer

s[[2]][3,5]     # row 3, column 5 of the second layer

values(s)[1:5,] # first

global(s, "mean", na.rm=TRUE)    

mean(s)







countries <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
)

eu <- countries %>% 
  filter(region_un%in%c("Europe", "Asia", "Africa"))

tavg <- geodata::worldclim_global(
  var = "tavg",
  res = 10,
  path = tempdir()
)

temp <- mean(tavg)

temp_eu <- crop(
  temp,
  ext(-15, 40, 35, 72)
)

names(temp_eu)

data("world.cities", package = "maps")

cities <- st_as_sf(
  subset(world.cities, pop > 1500000),
  coords = c("long", "lat"),
  crs = 4326
)

cities_lab <- cbind(
  st_drop_geometry(cities),
  st_coordinates(cities)
)

ggplot() +
  tidyterra::geom_spatraster(
    data = temp_eu,
    aes(
      fill = mean
    ),
    alpha = 0.85
  ) +
  geom_sf(
    data = eu,
    fill = NA,
    color = "black",
    linewidth = 0.2
  ) +
  geom_sf(
    data = cities,
    aes(size=pop),
    color = "darkgreen",
    show.legend = FALSE
  ) +
  geom_text(
    data = cities_lab,
    aes(
      X,
      Y,
      label = name
    ),
    size = 3,
    nudge_y = 1
  ) +
  coord_sf(
    xlim = c(-15, 40),
    ylim = c(35, 72),
    expand = FALSE
  ) +
  labs(
    title = "Mean annual temperature and major European cities (1970-2000)"
  ) +
  scale_fill_gradient2(
    low = "#2166AC",
    mid = "#F7F7F7",
    high = "#B2182B",
    midpoint = 10,
    na.value = "aliceblue",
    name = "Temperature (°C)"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 18,
      hjust = 0.5
    ),
    legend.position = "bottom"
  )