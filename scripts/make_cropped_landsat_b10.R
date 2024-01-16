#https://catalog.data.gov/dataset/u-s-geological-survey-oceanographic-time-series-data-collection-of-measurements-of-current-velo
#https://catalog.data.gov/dataset/surface-measurements-of-partial-pressure-of-co2-pco2-ph-on-total-scale-water-temperature-salini
#https://libguides.library.umaine.edu/gis/region
library(terra)
library(sf)
library(purrr)

# the files
f <- list.files("data/landsat/b10/", full.names = FALSE) |>
  stringr::str_subset("011029", negate = TRUE)


#2021 iffy - nothing on search - check again
#but 12 30? for 2021
# check 2014, (it's 9/19)
# check 2017 (it's 9/11)
# check 2020 (it's 9/19)
# check 2023 (it's 9/20)


##
#the area
#path 11 row 030
##

casco_bbox <- list(x=c(-69.8387, -70.2528),
                     y = c(43.9439, 43.5834)) |>
  raster::extent() |> st_bbox(crs=4326) |> st_as_sfc()

casco_ext_32619 <- st_transform(casco_bbox, crs = 32619) |>
  st_bbox() |> ext()

casco_ext_4326 <- casco_bbox |>  st_bbox() |> ext()

# get coast
coastline <- st_read("data/maine_gov/Maine_State_Boundary_Polygon_Feature/Maine_State_Boundary_Polygon_Feature.shp")
casco_shoreline <- st_crop(coastline |> st_make_valid(), 
                        casco_bbox) 

casco_shoreline_32619 <- st_transform(coastline, crs = 32619) |>
  st_make_valid() |>
  st_crop(st_transform(casco_bbox, crs = 32619)) 

#plot(casco_shoreline$geometry)

##
# function to crop and transform
##

load_crop_transform_b10 <- function(filename, 
                                    extent = casco_ext_32619){
  # load and crop
  r <- rast(filename) 
  r <- crop(r, extent)
  
  # make into C
  r <- r*0.00341802+ 149 - 273.15
  
  # make into f
  r <- (r*9/5) + 32
  
  #give it a better value name
  d <- regmatches(filename, regexpr("\\d{8}", filename))
  
  names(r) <- paste0("SST_F_", d)
  
  #reproject
  #r |> project("epsg:4326")
  r
}

##
# function to read in a raster and
# write it out
##

read_write_rast <- function(a_file,
                indir = "data/landsat/b10/",
                outdir = "data/landsat/b10_cropped/",
                shore = casco_shoreline_32619){
  
  r <- load_crop_transform_b10(paste0(indir, a_file))
  
  # time info
  d <- regmatches(a_file, regexpr("\\d{8}", a_file))
  time(r) <- as.Date(d, format = "%Y%m%d")
  outfilename <- paste0(d, "_SST_F_B10.TIF")
    
  
  # mask out land
  r <- mask(r, 
       vect(shore$geometry|> 
              st_buffer(units::set_units(60, "m"))), 
       inverse=TRUE) |> 
    crop(st_bbox(shore))
  
  
  writeRaster(r, paste0(outdir, a_file), overwrite = TRUE)
}


# loop the whole thing after cleaning the dir
unlink("data/landsat/b10_cropped/*")
purrr::walk(f, read_write_rast)
unlink("data/landsat/b10_cropped/*.aux.json")

##
#check with a plot
##
a <- rast(list.files("data/landsat/b10_cropped/", 
                     full.names = TRUE))

a <- clamp(a, lower = 40, upper = 85, values = FALSE)
library(tidyterra)
ggplot() + geom_spatraster(data = a) + facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "B") +
  geom_sf(data = casco_shoreline_32619) +
  coord_sf(crs = 32619)

# for fun
#plet(a[[5]])
