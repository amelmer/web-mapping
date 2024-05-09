library(tidyverse)
library(sf)

snap_data <- st_read("snap_retailers.json")

match_retailers <- function(street_number, street_name, address) {
  street_name <- gsub("Road", "Rd", street_name)
  street_name <- gsub("Avenue", "Ave", street_name)
  street_name <- gsub("Expressway", "Expy", street_name)
  street_name <- gsub("Street", "St", street_name)
  street_name <- gsub("Lane", "Ln", street_name)
  street_name <- gsub("Boulevard", "Blvd", street_name)
  street_name <- gsub("Highway", "Hwy", street_name)
  street_name <- gsub("North", "N", street_name)
  street_name <- gsub("South", "S", street_name)
  street_name <- gsub("West", "W", street_name)
  street_name <- gsub("East", "E", street_name)
  
  address <- gsub("Road", "Rd", address)
  address <- gsub("Avenue", "Ave", address)
  address <- gsub("Expressway", "Expy", address)
  address <- gsub("Street", "St", address)
  address <- gsub("Lane", "Ln", address)
  address <- gsub("Boulevard", "Blvd", address)
  address <- gsub("Highway", "Hwy", address)
  address <- gsub("North", "N", address)
  address <- gsub("South", "S", address)
  address <- gsub("West", "W", street_name)
  address <- gsub("East", "E", street_name)
  
  if (c(paste0(street_number, " ", str_to_lower(street_name))) == str_to_lower(address)) {
    return(1)
  } else {
    return(0)
  }
}

# iterate over grocery stores
grocery_data <- st_read("groceries.geojson")

print(c(paste0(grocery_data$addr.housenumber[[20]], " ", grocery_data$addr.street[[20]])))
print(snap_data$Store_Street_Address[[10]])

grocery_data$accepts_snap <- NA

for (i in 1:length(grocery_data$id)) {
  for (j in 1:length(snap_data$Record_ID)) {
    distance <- match_retailers(grocery_data$addr.housenumber[[i]], 
                                grocery_data$addr.street[[i]], 
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
drinks_sweets <- st_read("drinks_sweets.geojson")

for (i in 1:length(drinks_sweets$id)) {
  for (j in 1:length(snap_data$features)) {
    snap_store <- snap_data$features[[j]]
    
    distance <- match_retailers(drinks_sweets$addr.housenumber[[i]], 
                                drinks_sweets$addr.street[[i]], 
                                snap_store$attributes$Store_Street_Address)
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
farm <- st_read("farm.geojson")

farm$accepts_snap <- NA

for (i in 1:length(farm$id)) {
  for (j in 1:length(snap_data$features)) {
    snap_store <- snap_data$features[[j]]
    
    distance <- match_retailers(farm$addr.housenumber[[i]], 
                                farm$addr.street[[i]], 
                                snap_store$attributes$Store_Street_Address)
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
restaurants <- st_read("restaurants.geojson")

restaurants$accepts_snap <- NA

for (i in 1:length(restaurants$id)) {
  for (j in 1:length(snap_data$features)) {
    snap_store <- snap_data$features[[j]]
    
    distance <- match_retailers(restaurants$addr.housenumber[[i]], 
                                restaurants$addr.street[[i]], 
                                snap_store$attributes$Store_Street_Address)
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
specialty <- st_read("specialty.geojson")

specialty$accepts_snap <- NA

for (i in 1:length(specialty$id)) {
  for (j in 1:length(snap_data$features)) {
    snap_store <- snap_data$features[[j]]
    
    distance <- match_retailers(specialty$addr.housenumber[[i]], 
                                specialty$addr.street[[i]], 
                                snap_store$attributes$Store_Street_Address)
    if (distance == 1) {
      specialty$accepts_snap[[i]] <- 1
      break
    } else {
      specialty$accepts_snap[[i]] <- 0
    }
  }
}

st_write(misc, "specialty_snap.geojson")

# iterate over miscellaneous
misc <- st_read("miscellaneous.geojson")

misc$accepts_snap <- NA

for (i in 1:length(misc$id)) {
  for (j in 1:length(snap_data$features)) {
    snap_store <- snap_data$features[[j]]
    
    distance <- match_retailers(misc$addr.housenumber[[i]], 
                                misc$addr.housenumber[[i]], 
                                snap_store$attributes$Store_Street_Address)
    if (distance == 1) {
      misc$accepts_snap[[i]] <- 1
      break
    } else {
      misc$accepts_snap[[i]] <- 0
    }
  }
}

st_write(misc, "miscellaneous_snap.geojson")
