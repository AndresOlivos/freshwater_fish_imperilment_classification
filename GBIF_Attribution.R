setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Input_datasets)")

library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(rgbif)
library(elevatr)
library(CoordinateCleaner)

# Zizka A, Silvestro D, Andermann T, Azevedo J, Duarte Ritter C, Edler D, 
# Farooq H, Herdean A, Ariza M, Scharn R, Svanteson S, Wengtrom N, Zizka V & 
# Antonelli A (2019) CoordinateCleaner: standardized cleaning of occurrence 
# records from biological collection databases. # Methods in Ecology and 
# Evolution, 10(5):744-751, doi:10.1111/2041-210X.13152, 
# https://github.com/ropensci/CoordinateCleaner

# read feature class with IUCN ranges
iucn_table = read.csv("full_species_table.csv",
                      header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

path_ranges = r"(C:\Users\olivoshj\Desktop\ArcGIS_Projects\Dam_Project_Aux\Ranges_SQLite.gpkg)"

iucn_ranges = vect(path_ranges,
                   layer="Extant_Native_Ranges_Clipped",
                   proxy=TRUE
                   )
iucn_ranges
salar_range = query(iucn_ranges, n=1, where=paste0("sci_name='", 
                                                   'Salmo salar',"'"))
plot(salar_range)

# download GBIF occurrence data

total = nrow(iucn_table)
pb = txtProgressBar(min = 1, max = total, style = 3)

gbif_data = lapply(iucn_table$sci_name, function(x) {
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  occ_search(scientificName = x,
             hasCoordinate = TRUE,
             hasGeospatialIssue = FALSE,
             occurrenceStatus="PRESENT",
             year = '1990,2024',
             fields = "all") %>% .$data
  } 
  ) %>% bind_rows()

## clean coordinates 

# prepare progress bar
total <- nrow(gbif_data)
pb <- txtProgressBar(min = 1, max = total, style = 3)

gbif_tested = clean_coordinates(x = gbif_data, 
                                species = "species",
                                tests = c("capitals", "centroids", "equal",
                                          "gbif", "institutions", "outliers", 
                                          "seas", "zeros","duplicates"),
                                value = "spatialvalid",
                                verbose=TRUE
                                )

write.csv(gbif_tested, "GBIF_Ocurrences_Tested.csv", row.names=FALSE)

summary(gbif_tested)

plot(gbif_tested)

gbif_clean = gbif_tested[gbif_tested$.summary, ]

# how many species and records remain
length(unique(gbif_clean$species))
length(gbif_clean$decimalLatitude)

# count removed occurrences
length(gbif_tested$decimalLatitude) - length(gbif_clean$decimalLatitude)

plot(gbif_clean)

## remove occurrences located out of their range
# (alternative pipeline to CoordinateCleaner's cc_iucn(), which didn't run)

gbif_sps <- unique(gbif_clean$species)

gbif_points <- vect(data.frame(species = gbif_clean$species,
                               lon = gbif_clean$decimalLongitude,
                               lat = gbif_clean$decimalLatitude),
                    geom = c("lon", "lat"), crs = "EPSG:4326")

# Initialize an empty list to store clipped points
clipped_list <- list()

# Loop through species and clip occurrences using corresponding IUCN range
# previously dissolved by species, keeping only extant native ranges with 
# high certainty labels

# prepare progress bar
total <- length(gbif_sps)
pb <- txtProgressBar(min = 1, max = total, style = 3)

for (sps in gbif_sps) {
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  
  # create objects for current species
  sps_occs = gbif_points[gbif_points$species == sps, ]
  sps_range = query(iucn_ranges, n=1, where=paste0("sci_name='",sps,"'"))
  
  # spatial clip
  clipped_occs = sps_occs[sps_range, ]
  
  # if anything got clipped, merge to list
  if (nrow(clipped_occs) > 0) {
    clipped_list[[as.character(sps)]] <- clipped_occs
    
  }
}

# collapse list of clipped points into a single vector
gbif_clipped = st_as_sf(vect(clipped_list))

# count records removed
length(gbif_points) - length(gbif_clipped)

# save final occurrences used in analysis
gbif_filtered = cbind(st_drop_geometry(gbif_clipped), st_coordinates(gbif_clipped))
write.csv(gbif_filtered, "GBIF_Ocurrences_Filtered.csv", row.names=FALSE)

## query elevation values from Amazon terrain tiles
# first clean memory to avoid problems while calculating elevations
rm(list = setdiff(ls(), c('gbif_clipped')))

# initialize an empty vector to store elevations
elev <- vector("numeric", length = nrow(gbif_clipped))

# prepare progress bar
total <- length(gbif_clipped$geometry)
pb <- txtProgressBar(min = 1, max = total, style = 3)

# iterate through occurrences querying elevation
for(i in seq_along(gbif_clipped$geometry)){
  
  setTxtProgressBar(pb, i)
  
  elev[i] <- get_elev_point(
    locations = gbif_clipped[i,], 
    prj = 4326, 
    src = "aws",  # uses pixels of ~100m
    z = 10
    )$elevation
}

print(i)

gbif_elevs <- cbind(gbif_clipped, elev)

gbif_elevs <- cbind(st_drop_geometry(gbif_elevs), st_coordinates(gbif_elevs))

write.csv(gbif_elevs, "GBIF_Occurrences_Elevation.csv", row.names = FALSE)

# get min, max, and mean elevation for each species

sps_elevs <- gbif_elevs %>%
  group_by(species) %>%
  summarize(
    Min_Elevation = min(elev, na.rm = TRUE),
            Max_Elevation = max(elev, na.rm = TRUE),
            Mean_Elevation = mean(elev, na.rm = TRUE)
    )

head(sps_elevs)
sum(is.na(sps_elevs$Mean_Elevation))

# write to csv table
write.csv(sps_elevs, "GBIF_Attributes.csv", row.names = FALSE)
