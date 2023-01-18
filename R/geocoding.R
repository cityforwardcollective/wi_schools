library(tidyverse)
library(lubridate)
library(glue)
library(tidygeocoder)
library(sf)
library(terra)
library(wisconsink12)

# GEOCODING

files <- list.files("imports/school_dirs")
do_files <- files[which(str_detect(files, "2023"))]

dirs <- map_df(do_files, function(f) {
  fn <- paste0("imports/school_dirs/", f)
  
  date_f <- str_extract(f, "\\d{8}") |> 
    ymd()
  
  if (month(date_f) < 7) {
    school_year <- glue("{year(date_f)-1}-{year(date_f)-2000}")
  } else {
    school_year <- glue("{year(date_f)}-{year(date_f)+1-2000}")
  }
  
  d <- read_csv(fn, name_repair = janitor::make_clean_names) |> 
    mutate(school_year = school_year,
           district_code = ifelse(organisation_type == "Private school",
                                  "0000",
                                  lea_code),
           school_code = str_pad(school_code, side = "left", pad = "0", width = 4),
           dpi_true_id = glue("{district_code}_{school_code}")) |> 
    select(-c(school_code, lea_code, district_code)) |> 
    select(school_year, dpi_true_id, everything())
  
  if (!length(unique(d$dpi_true_id)) == nrow(d)) {
    stop("School codes aren't unique!")
  }
  return(d)
})


gc <- dirs |> 
  geocode(address = address, method = "census")

missed <- gc |> 
  filter(is.na(lat))

gc1 <- gc

for (i in 1:5) {
  gc1 <- bind_rows(gc1 |> filter(!is.na(lat)),
                  gc1 |> 
                    filter(is.na(lat)) |> 
                    select(-c(lat, long)) |> 
                    geocode(address = address, method = "census"))
}

gc_osm <- gc1 |> 
  filter(is.na(lat)) |> 
  select(-c(lat, long)) |> 
  geocode(address = address, method = "osm")

gc_osm1 <- gc_osm

for (i in 1:5) {
  gc_osm1 <- bind_rows(gc_osm1 |> filter(!is.na(lat)),
                       gc_osm1 |> 
                     filter(is.na(lat)) |> 
                     select(-c(lat, long)) |> 
                     geocode(address = address, method = "census"))
}

again <- gc_osm1 |> 
  filter(is.na(lat)) |> 
  select(-c(lat, long)) |> 
  geocode_combine(queries = list(list(method = "arcgis")),
                  global_params = list(address = "address"))

nrow(again |> filter(is.na(lat)))

all <- gc1 |> 
  filter(!is.na(lat)) |> 
  bind_rows(gc_osm1) |> 
  filter(!is.na(lat)) |> 
  bind_rows(again)

length(unique(all$dpi_true_id))
sum(is.na(all$lat))


# VISUALIZING

hq <- left_join(all, wisconsink12::report_cards |> 
                  filter(school_year == "2021-22") |> 
                  select(dpi_true_id, overall_score)) |> 
  filter(overall_score >= 70.0)


all_sf <- st_as_sf(hq, coords = c("long", "lat"), crs = 4326)

mke <- st_read("../shapefiles/milwaukee_citylimit/citylimit.shp") 

all_sf <- st_transform(all_sf, crs = st_crs(mke))

wi <- spData::us_states |> 
  filter(NAME == "Wisconsin") |> 
  st_transform(crs = st_crs(all_sf))

sv <- voronoi(vect(all_sf$geometry)) |> 
  st_as_sf() 

sv <- st_intersection(st_transform(wi, st_crs(mke)), sv)
mke_int <- sv |> 
  st_intersects(mke)

ind <- map_dbl(mke_int, function(i) {
  if (length(i) > 0) {
    return(i)
  } else {
    return(0)
  }
})

dd <- sv[which(ind == 1),] |> 
  mutate(area = as.numeric(st_area(geometry)) / 2.788e7)


dd |> 
  ggplot() +
  geom_sf(aes(fill = area)) +
  geom_sf(data = mke, fill = NA, color = "red") +
  theme_void()
