#' Make a Casco Bay AOI Object

library(terra)
library(sf)
library(purrr)


casco_bbox <- list(x=c(-69.8387, -70.2528),
                   y = c(43.9439, 43.5834)) |>
  raster::extent() |> st_bbox(crs=4326) |> st_as_sfc() |>
  st_as_sf()

st_write(casco_bbox, "data/casco_aoi/casco_bay_aoi.shp", 
         delete_dsn = TRUE)

st_write(casco_bbox |> st_transform(crs = 32619),
         "data/casco_aoi/casco_bay_aoi_32619.shp", 
         delete_dsn = TRUE)
