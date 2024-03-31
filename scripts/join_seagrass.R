library(sf)
library(dplyr)

common_formatting <- . %>%
  rename_with(tolower) %>%
  mutate(id = as.character(id))
  


seagrass_2022 <- st_read("data/maine_gov_seagrass/MaineDEP_Casco_Bay_Seagrass_2022/MaineDEP_Casco_Bay_Seagrass_2022.shp") %>% common_formatting|>
  mutate(year = 2022)
seagrass_2018 <- st_read("data/maine_gov_seagrass/MaineDEP_Casco_Bay_Eelgrass_2018/MaineDEP_Casco_Bay_Eelgrass_2018.shp")%>% common_formatting|>
  mutate(year = 2018)
seagrass_2013 <- st_read("data/maine_gov_seagrass/MaineDEP_Casco_Bay_Eelgrass_2013/MaineDEP_Casco_Bay_Eelgrass_2013.shp")%>% common_formatting |>
  mutate(year = 2013)

seagrass_2010 <- st_read("data/maine_gov_seagrass/MaineDMR_-_Eelgrass-shp-2010/MaineDMR_-_Eelgrass.shp")|>
                           st_crop(seagrass_2022)|>
  mutate(id = 1:n()) %>% common_formatting|>
  mutate(year = 2010)

seagrass_1997 <- st_read("data/maine_gov_seagrass/MaineDMR_-_Eelgrass-1997-shp/MaineDMR_-_Eelgrass.shp")|>
  st_make_valid() |>
  st_crop(seagrass_2022)|>
  mutate(id = 1:n()) %>% common_formatting|>
  mutate(year = 1997)


cover_cats <- seagrass_2018 |>
  group_by(cover) |>
  slice(1L) |>
  as_tibble() |>
  select(cover, cover_pct) |>
  mutate(cover_no_pct = gsub("\\%", "", cover_pct),
         cover_no_pct = gsub(" to ", "-", cover_no_pct))


# fix cover pct in 1997, 2010, and 2022
seagrass_2022 <- seagrass_2022 |>
  rename(cover = orth_cover) |>
  select(-cover_pct) |>
  left_join(cover_cats) |>
  select(-cover_no_pct) 

seagrass_2010 <- seagrass_2010 |>
  left_join(cover_cats) |>
  select(-cover_no_pct) 


seagrass_1997 <- seagrass_1997 |>
  left_join(cover_cats) |>
  select(-cover_no_pct) 



joined_cover <- purrr::reduce(list(seagrass_2022,
                              seagrass_2018, 
                              seagrass_2013,
                              seagrass_2010,
                              seagrass_1997),
                              .f = bind_rows) |>
  select(year, name, acres, hectares, cover, cover_pct, year97)

saveRDS(joined_cover, "data/joined_seagrass_cover.Rds")
