library(tidyverse)
library(sf)

setwd("/Users/allisonelmer/Documents/GitHub/web-mapping/final-project")

snap_data <- st_read("snap/snap_retailers.json")

street_abbv <- c("Road" = "Rd", "Avenue" = "Ave", "Expressway" = "Expy",
                 "Street" = "St", "Lane" = "Ln", "Boulevard" = "Blvd",
                 "Highway" = "Hwy", "North" = "N", "South" = "S",
                 "West" = "W", "East" = "E")

match_retailers <- function(address1, address2) {
  address1 <- str_replace_all(address1, street_abbv)
  address2 <- str_replace_all(address2, street_abbv)
  
  if (str_to_lower(address1) == str_to_lower(address2)) {
    return(1)
  } else {
    return(0)
  }
}

data_cleaning <- function(df) {
  df <- df %>% 
    mutate(Address = gsub("# [a-zA-Z0-9]*", "", Address),
           across(ends_with(".Close"), ~{
             open_col <- str_replace(cur_column(), "\\.Close$", ".Open")
             case_when(
               df[[open_col]] == "Open 24 Hours" ~ "Open 24 Hours",
               df[[open_col]] == "Closed" ~ "Closed",
               . == "" ~ NA_character_,
               TRUE ~ .
             )
           }),
           across(ends_with(".Open"), ~{
             close_col <- str_replace(cur_column(), "\\.Open$", ".Close")
             case_when(
               df[[close_col]] == "Open 24 Hours" ~ "Open 24 Hours",
               . == "" ~ NA_character_,
               TRUE ~ .
             )
           })
    )
  
  names(df) <- gsub("\\.", "", names(df))
  
  return(df)
}

# iterate over grocery stores
grocery_data <- st_read("snap/groceries.geojson")

grocery_data <- data_cleaning(grocery_data)

grocery_data$accepts_snap <- NA

for (i in 1:length(grocery_data$CompanyName)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(grocery_data$Address[[i]], 
                                snap_data$Store_Street_Address[[j]])
    
    if (distance == 1) {
      grocery_data$accepts_snap[[i]] <- 1
      break
    } else {
      grocery_data$accepts_snap[[i]] <- 0
    }
  }
}

st_write(grocery_data, "groceries_snap.geojson")

# iterate over drinks and sweets
drinks_sweets <- st_read("snap/drinks_sweets.geojson")

for (i in 1:length(drinks_sweets$id)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(drinks_sweets$addr.housenumber[[i]], 
                                drinks_sweets$addr.street[[i]], 
                                snap_data$Store_Street_Address[[j]])
    if (distance == 1) {
      drinks_sweets$accepts_snap <- 1
      break
    } else {
      drinks_sweets$accepts_snap <- 0
    }
  }
}

st_write(drinks_sweets, "drinks_sweets_snap.geojson")

# iterate over farm
farm <- st_read("snap/farm.geojson")

farm$accepts_snap <- NA

for (i in 1:length(farm$id)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(farm$addr.housenumber[[i]], 
                                farm$addr.street[[i]], 
                                snap_data$Store_Street_Address[[j]])
    if (distance == 1) {
      farm$accepts_snap[[i]] <- 1
      break
    } else {
      farm$accepts_snap[[i]] <- 0
    }
  }
}

st_write(farm, "farm_snap.geojson")

# iterate over restaurants
restaurants <- st_read("snap/restaurants.geojson")

restaurants$accepts_snap <- NA

for (i in 1:length(restaurants$id)) {
  for (j in 1:length(snap_data$Record_ID)) {
    
    distance <- match_retailers(restaurants$addr.housenumber[[i]], 
                                restaurants$addr.street[[i]], 
                                snap_data$Store_Street_Address[[j]])
    if (distance == 1) {
      restaurants$accepts_snap[[i]] <- 1
      break
    } else {
      restaurants$accepts_snap[[i]] <- 0
    }
  }
}

st_write(restaurants, "restaurants_snap.geojson")

# iterate over specialty grocery stores
specialty <- st_read("snap/specialty.geojson")

specialty$accepts_snap <- NA

for (i in 1:length(specialty$id)) {
  for (j in 1:length(snap_data$Record_ID)) {

    distance <- match_retailers(specialty$addr.housenumber[[i]], 
                                specialty$addr.street[[i]], 
                                snap_data$Store_Street_Address[[j]])
    if (distance == 1) {
      specialty$accepts_snap[[i]] <- 1
      break
    } else {
      specialty$accepts_snap[[i]] <- 0
    }
  }
}

st_write(specialty, "snap/specialty_snap.geojson")
