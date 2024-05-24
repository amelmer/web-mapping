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

grocery_data$Type <- "grocery"

# iterate over convenience stores
convenience_data <- st_read("snap/convenience.geojson")

convenience_data <- data_cleaning(convenience_data)

convenience_data$accepts_snap <- NA

for (i in 1:length(convenience_data$CompanyName)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(convenience_data$Address[[i]], 
                                snap_data$Store_Street_Address[[j]])
    
    if (distance == 1) {
      convenience_data$accepts_snap[[i]] <- 1
      break
    } else {
      convenience_data$accepts_snap[[i]] <- 0
    }
  }
}

convenience_data$Type <- "convenience store"

# iterate over restaurants
restaurant_data <- st_read("snap/restaurants.geojson")

restaurant_data <- data_cleaning(restaurant_data)

restaurant_data$accepts_snap <- NA

for (i in 1:length(restaurant_data$CompanyName)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(restaurant_data$Address[[i]], 
                                snap_data$Store_Street_Address[[j]])
    
    if (distance == 1) {
      restaurant_data$accepts_snap[[i]] <- 1
      break
    } else {
      restaurant_data$accepts_snap[[i]] <- 0
    }
  }
}

restaurant_data$Type <- "restaurant"

# iterate over sweets
sweets_data <- st_read("snap/sweets.geojson")

sweets_data <- data_cleaning(sweets_data)

sweets_data$accepts_snap <- NA

for (i in 1:length(sweets_data$CompanyName)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(sweets_data$Address[[i]],
                                snap_data$Store_Street_Address[[j]])
    if (distance == 1) {
      sweets_data$accepts_snap <- 1
      break
    } else {
      sweets_data$accepts_snap <- 0
    }
  }
}

sweets_data$Type <- "sweets"

sweets_data <- sweets_data %>% 
  mutate(Type = ifelse(grepl("Costco", CompanyName) == TRUE, "grocery", Type)) %>% 
  filter(CompanyName != "Loop Neighborhood Market" & CompanyName != "Lucky Supermarkets" 
         & CompanyName != "McDonald's" & CompanyName != "Nordstrom" & CompanyName != "Panera Bread"
         & CompanyName != "Safeway" & CompanyName != "Smashburger" & CompanyName != "Denny's")

groceries_to_append <- sweets_data %>% 
  filter(Type != "sweets")

grocery_data <- grocery_data %>% 
  rbind(groceries_to_append)

sweets_data <- sweets_data %>% 
  filter(Type == "sweets")

# iterate over farms
farm_data <- st_read("snap/farms.geojson")

farm_data <- data_cleaning(farm_data)

farm_data$accepts_snap <- NA

for (i in 1:length(farm_data$CompanyName)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(farm_data$Address[[i]],
                                snap_data$Store_Street_Address[[j]])
    if (distance == 1) {
      farm_data$accepts_snap[[i]] <- 1
      break
    } else {
      farm_data$accepts_snap[[i]] <- 0
    }
  }
}

farm_data$Type <- "farm or specialty"

# iterate over food banks
charitable_data <- st_read("snap/charitable.geojson")

charitable_data <- charitable_data %>% 
  mutate(across(Company.Name:Sunday.Close, as.character))

charitable_data <- data_cleaning(charitable_data)

charitable_data$accepts_snap <- NA

for (i in 1:length(charitable_data$CompanyName)) {
  for (j in 1:length(snap_data$Record_ID)) {

    distance <- match_retailers(charitable_data$Address[[i]],
                                snap_data$Store_Street_Address[[j]])
    if (distance == 1) {
      charitable_data$accepts_snap[[i]] <- 1
      break
    } else {
      charitable_data$accepts_snap[[i]] <- 0
    }
  }
}

charitable_data$Type <- "charitable"

# export data
st_write(grocery_data, "groceries_snap.geojson")
st_write(convenience_data, "convenience_snap.geojson")
st_write(restaurant_data, "restaurant_snap.geojson")
st_write(sweets_data, "sweets_snap.geojson")
st_write(farm_data, "farm_snap.geojson")
st_write(charitable_data, "charitable_snap.geojson")
