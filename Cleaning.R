# AC Survey Data Cleaning Pipeline - intended to be source()'d

library(dplyr)
library(tidyr)
library(sf)  
library(rnaturalearth) 
library(here)
library(lubridate)

# Data Import
# Alternate-format Intro question data from Qualtrics to be joined to primary df
df_alt <- read_csv(here("Qualtrics_split_numeric.csv"), show_col_types = FALSE) |> 
    slice(-c(1,2,3,4)) |> 
    select("ResponseId", starts_with("Intro_"))
df <- read_csv(here("Qualtrics_numeric.csv"), show_col_types = FALSE) |> 
    slice(-c(1,2,3,4)) |>  # first few rows are test data and headers
    left_join(df_alt, by = "ResponseId") |> 
    select("ResponseId", "Status", "Progress", "Duration (in seconds)",
           "LocationLatitude", "LocationLongitude", "Intro", "RecordedDate", 
           contains("_")) |>  # Qualtrics contains some other columns useless to analysis
    mutate(across(c(LocationLatitude, LocationLongitude), as.numeric),
           across(c(Progress, "Duration (in seconds)", contains("_")), as.integer)) |>
    filter(Status != "Spam") |> 
    select(-Status) |>  # now-useless column after filter
    rename(IntroList = Intro,
           Duration = "Duration (in seconds)")
rm(df_alt)  # keep our memory nice and focused
invisible(gc(verbose = FALSE))

# Data Augmentation (reverse geo-lookup)
world <- ne_countries(scale = "medium", returnclass = "sf") |>
  select(iso_a3, name, subregion, region_wb)
df <- df |>
  filter(!is.na(LocationLongitude), !is.na(LocationLatitude)) |>
  st_as_sf(coords = c("LocationLongitude", "LocationLatitude"), crs = 4326) |>
  st_join(world) |>
  st_drop_geometry() |> 
  select("ResponseId", "iso_a3", "name", "subregion", "region_wb") |> 
  right_join(df, by = "ResponseId") |> 
  rename(Subregion = subregion,
         CountryCode = iso_a3,
         CountryName = name,
         WBRegion = region_wb)  # no _ because it will make selector code easier later
rm(world)
invisible(gc(verbose = FALSE))

# Additional Features
df <- df |> 
    mutate(
        total_played = as.integer(rowSums(pick(starts_with("Intro_")), na.rm = TRUE)),
        RecordedDate = ymd_hms(RecordedDate),
        RecordedDay = day(RecordedDate),
        RecordedWeekday = wday(RecordedDate, label = TRUE)
    )
