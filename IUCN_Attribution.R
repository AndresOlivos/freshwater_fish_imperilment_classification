library(rredlist)
library(dplyr)
library(utils)

#Sys.setenv(IUCN_REDLIST_KEY="CA9DzPzbfsgjQdD6nqoVCcRqJ2a3XtzbsDr9")

setwd("R:\\FWL\\Arismendi-Lab\\Andres\\Gilbert_Freshwater_Fish_Analysis\\Revised_Analysis_NatureCommunications\\Input_datasets\\Tabular_data_by_species")

iucn_table = read.csv("Ranges_Attributes.csv", header = T, stringsAsFactors = FALSE)
speciesranges = iucn_table$sci_name

fish_assessments <- rl_comp_groups("fishes", latest = TRUE)
?rredlist

## Get assessments for all fish species
# prepare progress bar
total <- sapply(list(fish_assessments$assessments), NROW)
pb <- txtProgressBar(min = 1, max = total, style = 3)

fish_data <- lapply(fish_assessments$assessments$assessment_id,
                     function(x) {
                       Sys.sleep(0.3)
                       setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
                       rl_assessment(x)
                     }
)

## ## Create habitats table while filtering based on target species list

fish_habitats = fish_data[sapply(fish_data, function(x) length(x$habitats) > 0)]

# prepare progress bar
total <- sapply(list(fish_habitats), NROW)
pb <- txtProgressBar(min = 1, max = total, style = 3)

habitat_table <- lapply(fish_habitats, function(x) {
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  x$habitats %>%
    mutate(species = x$taxon$scientific_name,
           code = gsub("_", ".", code),
           habitat = description$en,
           majorImportance = as.character(majorImportance),
           season = as.character(season),
           suitability=as.character(suitability),
           .keep= "unused"
           ) %>%
    filter(species %in% speciesranges)   # filter species of interest (freshwater species with IUCN ranges)
}
) %>% bind_rows()

#write.csv(habitat_table, "habitats.csv",row.names = FALSE, na="")


## Create measures table while filtering based on target species list

fish_measures = fish_data[sapply(fish_data, function(x) length(x$conservation_actions) > 0)]

# prepare progress bar
total <- sapply(list(fish_measures), NROW)
pb <- txtProgressBar(min = 1, max = total, style = 3)

measures_table <- lapply(fish_measures, function(x) {
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  x$conservation_actions %>%
    mutate(species = x$taxon$scientific_name,
           code = gsub("_", ".", code),
           title = as.character(description$en),
           .keep = 'none'
    ) %>%
    filter(species %in% speciesranges)   # filter species of interest (freshwater species with IUCN ranges)
}
) %>% bind_rows()

#write.csv(measures_table, "measures.csv",row.names = FALSE, na="")


## Create threats table while filtering based on target species list

fish_threats = fish_data[sapply(fish_data, function(x) length(x$threats) > 0)]

# prepare progress bar
total <- sapply(list(fish_threats), NROW)
pb <- txtProgressBar(min = 1, max = total, style = 3)

threats_table <- lapply(fish_threats, function(x) {
  setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
  x$threats %>%
    mutate(species = x$taxon$scientific_name,
           code = gsub("_", ".", code),
           title = as.character(description$en),
           timing = as.character(timing),
           scope = as.character(scope),
           severity = as.character(severity),
           score = as.character(score),
           .keep = 'none'
    ) %>%
    filter(species %in% speciesranges)   # filter species of interest (freshwater species with IUCN ranges)
}
) %>% bind_rows()

#write.csv(threats_table, "threats.csv",row.names = FALSE, na="")

### now use all '3' IUCN files to make individual rows for each species

#habitats<-na.omit(habitats)

habitats = habitats %>% distinct()
habitatscode = pivot_wider(habitats, 
                           id_cols=species, 
                           names_from = habitat, 
                           values_from = suitability, 
                           values_fill = NA, 
                           values_fn = list)

# aux function to replace special characters in column names
tidy_column_names <- function(column_names, characters_to_replace, replacement = "_") {
  # Convert characters_to_replace into a regex pattern
  pattern <- paste0("[", paste(characters_to_replace, collapse = ""), "]")
  
  # Replace all specified characters in column names
  cleaned_names <- sapply(column_names, function(name) {
    gsub(pattern, replacement, name)
  })
  
  return(cleaned_names)
}

replacers = c(" ","-","/","(",")")

# modify names for habitat columns
colnames(habitatscode) <- tidy_column_names(colnames(habitatscode), replacers, replacement = ".")

# remove dups
measures = measures %>% distinct()

# pivot measures by species
measurescode = pivot_wider(measures, 
                           id_cols=species, 
                           names_prefix = "m_", 
                           names_from = code, 
                           values_from = code, 
                           values_fill = NA, 
                           values_fn = list)

# remove dups
threats = threats %>% distinct()

# pivot threats by species
threatscode = pivot_wider(threats, 
                          id_cols=species, 
                          names_prefix = "t_", 
                          names_from = code, 
                          values_from = score, 
                          values_fill = NA, 
                          values_fn = list)

## merge IUCN table to input species table

IUCN_attributes =  full_join(full_species_table, habitatscode, by.x='sci_name', by.y='species') %>%
  full_join(., measurescode, by='species') %>%  
  full_join(., threatscode, by='species') %>%
  mutate_all(funs(type.convert(as.character(replace(., .=='NULL', NA)))))

## now reclass of IUCN attributes
Reclass = IUCN_attributes

Reclass$IUCN.temporary <- NA
Reclass$IUCN.permanent <- NA
Reclass$IUCN.nat.lentic <- NA
Reclass$IUCN.nat.lotic <- NA
Reclass$IUCN.artificial <- NA
Reclass$IUCN.other <- NA

#####
##ARTIFICIAL
Reclass$IUCN.artificial<-ifelse(is.na(Reclass$Artificial.Aquatic...Aquaculture.Ponds)!=T, -1,
                                ifelse(is.na(Reclass$Artificial.Aquatic...Canals.and.Drainage.Channels..Ditches)!=T, -1,
                                       ifelse(is.na(Reclass$Artificial.Aquatic...Excavations..open.)!=T, -1,
                                              ifelse(is.na(Reclass$Artificial.Aquatic...Irrigated.Land..includes.irrigation.channels.)!=T, -1,
                                                     ifelse(is.na(Reclass$Artificial.Aquatic...Karst.and.Other.Subterranean.Hydrological.Systems..human.made. )!=T, -1,
                                                            ifelse(is.na(Reclass$Artificial.Aquatic...Ponds..below.8ha. )!=T, -1,
                                                                   ifelse(is.na(Reclass$Artificial.Aquatic...Salt.Exploitation.Sites )!=T, -1,
                                                                          ifelse(is.na(Reclass$Artificial.Aquatic...Seasonally.Flooded.Agricultural.Land )!=T, -1,
                                                                                 ifelse(is.na(Reclass$Artificial.Aquatic...Wastewater.Treatment.Areas )!=T, -1,
                                                                                        ifelse(is.na(Reclass$Artificial.Aquatic...Water.Storage.Areas..over.8ha. )!=T, -1,
                                                                                               ifelse(is.na(Reclass$Artificial.Marine...Mari.Brackishculture.Ponds )!=T, -1,
                                                                                                      ifelse(is.na(Reclass$Artificial.Marine...Mariculture.Cages )!=T, -1,
                                                                                                             ifelse(is.na(Reclass$Artificial.Marine...Marine.Anthropogenic.Structures )!=T, -1,
                                                                                                                    ifelse(is.na(Reclass$Artificial.Terrestrial...Plantations)!=T, -1, NA))))))))))))))

######
##LENTIC
Reclass$IUCN.nat.lentic<-ifelse(is.na(Reclass$Wetlands..inland....Shrub.Dominated.Wetlands)!=T, -1,
                                ifelse(is.na(Reclass$Wetlands..inland....Bogs..Marshes..Swamps..Fens..Peatlands)!=T, -1,
                                       ifelse(is.na(Reclass$Wetlands..inland....Permanent.Freshwater.Lakes..over.8ha.)!=T, -1,
                                              ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Freshwater.Lakes..over.8ha.)!=T, -1,
                                                     ifelse(is.na(Reclass$Wetlands..inland....Permanent.Freshwater.Marshes.Pools..under.8ha.)!=T, -1,
                                                            ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Freshwater.Marshes.Pools..under.8ha.)!=T, -1,
                                                                   ifelse(is.na(Reclass$Wetlands..inland....Freshwater.Springs.and.Oases)!=T, -1,
                                                                          ifelse(is.na(Reclass$Wetlands..inland....Alpine.Wetlands..includes.temporary.waters.from.snowmelt.)!=T, -1,
                                                                                 ifelse(is.na(Reclass$Wetlands..inland....Geothermal.Wetlands)!=T, -1,
                                                                                        ifelse(is.na(Reclass$Wetlands..inland....Permanent.Inland.Deltas)!=T, -1,
                                                                                               ifelse(is.na(Reclass$Wetlands..inland....Permanent.Saline..Brackish.or.Alkaline.Lakes)!=T, -1,
                                                                                                      ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Saline..Brackish.or.Alkaline.Lakes.and.Flats)!=T, -1,
                                                                                                             ifelse(is.na(Reclass$Wetlands..inland....Permanent.Saline..Brackish.or.Alkaline.Marshes.Pools)!=T, -1,
                                                                                                                    ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Saline..Brackish.or.Alkaline.Marshes.Pools)!=T, -1,
                                                                                                                           ifelse(is.na(Reclass$Wetlands..inland....Karst.and.Other.Subterranean.Hydrological.Systems..inland.)!=T, -1,
                                                                                                                                  ifelse(is.na(Reclass$Marine.Coastal.Supratidal...Coastal.Freshwater.Lakes)!=T, -1,
                                                                                                                                         NA))))))))))))))))


#####
##LOTIC
Reclass$IUCN.nat.lotic<-ifelse(is.na(Reclass$Wetlands..inland....Permanent.Rivers.Streams.Creeks..includes.waterfalls.)!=T, -1,
                               ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Irregular.Rivers.Streams.Creeks)!=T, -1, NA))

##OTHER
Reclass$IUCN.other<-ifelse(is.na(Reclass$Unknown)!=T, -1,
                           ifelse(is.na(Reclass$Back.Slope)!=T, -1,
                                  ifelse(is.na(Reclass$Caves.and.Subterranean.Habitats..non.aquatic....Caves)!=T, -1,
                                         ifelse(is.na(Reclass$Desert...Hot)!=T, -1,
                                                ifelse(is.na(Reclass$Desert...Temperate)!=T, -1,
                                                       ifelse(is.na(Reclass$Foreslope..Outer.Reef.Slope.)!=T, -1,
                                                              ifelse(is.na(Reclass$Forest...Subtropical.Tropical.Moist.Lowland)!=T, -1,
                                                                     ifelse(is.na(Reclass$Forest...Subtropical.Tropical.Swamp)!=T, -1,
                                                                            ifelse(is.na(Reclass$Grassland...Subarctic)!=T, -1,
                                                                                   ifelse(is.na(Reclass$Grassland...Subtropical.Tropical.Seasonally.Wet.Flooded)!=T, -1,
                                                                                          ifelse(is.na(Reclass$Grassland...Tundra)!=T, -1,
                                                                                                 ifelse(is.na(Reclass$Inter.Reef.Rubble.Substrate)!=T, -1,
                                                                                                        ifelse(is.na(Reclass$Inter.Reef.Soft.Substrate)!=T, -1,
                                                                                                               ifelse(is.na(Reclass$Lagoon)!=T, -1,
                                                                                                                      ifelse(is.na(Reclass$Marine.Coastal.Supratidal...Coastal.Brackish.Saline.Lagoons.Marine.Lakes)!=T, -1,
                                                                                                                             ifelse(is.na(Reclass$Marine.Coastal.supratidal...Coastal.Caves.Karst)!=T, -1,
                                                                                                                                    ifelse(is.na(Reclass$Marine.Coastal.Supratidal...Coastal.Sand.Dunes)!=T, -1,
                                                                                                                                           ifelse(is.na(Reclass$Marine.Coastal.Supratidal...Sea.Cliffs.and.Rocky.Offshore.Islands)!=T, -1,
                                                                                                                                                  ifelse(is.na(Reclass$Marine.Intertidal...Mangrove.Submerged.Roots)!=T, -1,
                                                                                                                                                         ifelse(is.na(Reclass$Marine.Intertidal...Mud.Flats.and.Salt.Flats)!=T, -1,
                                                                                                                                                                ifelse(is.na(Reclass$Marine.Intertidal...Rocky.Shoreline)!=T, -1,
                                                                                                                                                                       ifelse(is.na(Reclass$Marine.Intertidal...Salt.Marshes..Emergent.Grasses.)!=T, -1,
                                                                                                                                                                              ifelse(is.na(Reclass$Marine.Intertidal...Sandy.Shoreline.and.or.Beaches..Sand.Bars..Spits..Etc)!=T, -1,
                                                                                                                                                                                     ifelse(is.na(Reclass$Marine.Intertidal...Shingle.and.or.Pebble.Shoreline.and.or.Beaches)!=T, -1,
                                                                                                                                                                                            ifelse(is.na(Reclass$Marine.Intertidal...Tidepools)!=T, -1,
                                                                                                                                                                                                   ifelse(is.na(Reclass$Marine.Neritic...Coral.Reef)!=T, -1,
                                                                                                                                                                                                          ifelse(is.na(Reclass$Marine.Neritic...Estuaries)!=T, -1,
                                                                                                                                                                                                                 ifelse(is.na(Reclass$Marine.Neritic...Macroalgal.Kelp)!=T, -1,
                                                                                                                                                                                                                        ifelse(is.na(Reclass$Marine.Neritic...Pelagic)!=T, -1,
                                                                                                                                                                                                                               ifelse(is.na(Reclass$Marine.Neritic...Seagrass..Submerged.)!=T, -1,
                                                                                                                                                                                                                                      ifelse(is.na(Reclass$Marine.Neritic...Subtidal.Loose.Rock.pebble.gravel)!=T, -1,
                                                                                                                                                                                                                                             ifelse(is.na(Reclass$Marine.Neritic...Subtidal.Muddy)!=T, -1,
                                                                                                                                                                                                                                                    ifelse(is.na(Reclass$Marine.Neritic...Subtidal.Rock.and.Rocky.Reefs)!=T, -1,
                                                                                                                                                                                                                                                           ifelse(is.na(Reclass$Marine.Neritic...Subtidal.Sandy)!=T, -1,
                                                                                                                                                                                                                                                                  ifelse(is.na(Reclass$Marine.Neritic...Subtidal.Sandy.Mud)!=T, -1,
                                                                                                                                                                                                                                                                         ifelse(is.na(Reclass$Marine.Oceanic...Bathypelagic..1000.4000m.)!=T, -1,
                                                                                                                                                                                                                                                                                ifelse(is.na(Reclass$Marine.Oceanic...Epipelagic..0.200m.)!=T, -1,
                                                                                                                                                                                                                                                                                       ifelse(is.na(Reclass$Marine.Oceanic...Mesopelagic..200.1000m.)!=T, -1,
                                                                                                                                                                                                                                                                                              ifelse(is.na(Reclass$Other)!=T, -1,
                                                                                                                                                                                                                                                                                                     ifelse(is.na(Reclass$Outer.Reef.Channel)!=T, -1,
                                                                                                                                                                                                                                                                                                            ifelse(is.na(Reclass$Rocky.areas..eg..inland.cliffs..mountain.peaks.)!=T, -1,
                                                                                                                                                                                                                                                                                                                   NA)))))))))))))))))))))))))))))))))))))))))



#####
##PERMANENT FRESHWATER
Reclass$IUCN.permanent<-ifelse(is.na(Reclass$Artificial.Aquatic...Aquaculture.Ponds)!=T, -1,
                               ifelse(is.na(Reclass$Artificial.Aquatic...Excavations..open.)!=T, -1,
                                      ifelse(is.na(Reclass$Artificial.Aquatic...Karst.and.Other.Subterranean.Hydrological.Systems..human.made. )!=T, -1,
                                             ifelse(is.na(Reclass$Artificial.Aquatic...Ponds..below.8ha. )!=T, -1,
                                                    ifelse(is.na(Reclass$Artificial.Aquatic...Wastewater.Treatment.Areas )!=T, -1,
                                                           ifelse(is.na(Reclass$Artificial.Aquatic...Water.Storage.Areas..over.8ha. )!=T, -1,
                                                                  ifelse(is.na(Reclass$Wetlands..inland....Bogs..Marshes..Swamps..Fens..Peatlands)!=T, -1,
                                                                         ifelse(is.na(Reclass$Wetlands..inland....Permanent.Freshwater.Lakes..over.8ha.)!=T, -1,
                                                                                ifelse(is.na(Reclass$Wetlands..inland....Permanent.Freshwater.Marshes.Pools..under.8ha.)!=T, -1,
                                                                                       ifelse(is.na(Reclass$Wetlands..inland....Freshwater.Springs.and.Oases)!=T, -1,
                                                                                              ifelse(is.na(Reclass$Wetlands..inland....Permanent.Inland.Deltas)!=T, -1,
                                                                                                     ifelse(is.na(Reclass$Wetlands..inland....Permanent.Saline..Brackish.or.Alkaline.Lakes)!=T, -1,
                                                                                                            ifelse(is.na(Reclass$Wetlands..inland....Permanent.Saline..Brackish.or.Alkaline.Marshes.Pools)!=T, -1,
                                                                                                                   ifelse(is.na(Reclass$Wetlands..inland....Karst.and.Other.Subterranean.Hydrological.Systems..inland.)!=T, -1,
                                                                                                                          ifelse(is.na(Reclass$Marine.Coastal.Supratidal...Coastal.Freshwater.Lakes)!=T, -1,
                                                                                                                                 ifelse(is.na(Reclass$Wetlands..inland....Permanent.Rivers.Streams.Creeks..includes.waterfalls.)!=T, -1,
                                                                                                                                        NA))))))))))))))))







#####
##TEMPORARY FRESHWATER
Reclass$IUCN.temporary<-ifelse(is.na(Reclass$Artificial.Aquatic...Irrigated.Land..includes.irrigation.channels.)!=T, -1,
                               ifelse(is.na(Reclass$Artificial.Aquatic...Seasonally.Flooded.Agricultural.Land )!=T, -1,
                                      ifelse(is.na(Reclass$Artificial.Aquatic...Salt.Exploitation.Sites )!=T, -1,
                                             ifelse(is.na(Reclass$Artificial.Aquatic...Canals.and.Drainage.Channels..Ditches)!=T, -1,
                                                    ifelse(is.na(Reclass$Artificial.Terrestrial...Plantations)!=T, -1,
                                                           ifelse(is.na(Reclass$Wetlands..inland....Shrub.Dominated.Wetlands)!=T, -1,
                                                                  ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Freshwater.Lakes..over.8ha.)!=T, -1,
                                                                         ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Freshwater.Marshes.Pools..under.8ha.)!=T, -1,
                                                                                ifelse(is.na(Reclass$Wetlands..inland....Alpine.Wetlands..includes.temporary.waters.from.snowmelt.)!=T, -1,
                                                                                       ifelse(is.na(Reclass$Wetlands..inland....Geothermal.Wetlands)!=T, -1,
                                                                                              ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Saline..Brackish.or.Alkaline.Lakes.and.Flats)!=T, -1,
                                                                                                     ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Saline..Brackish.or.Alkaline.Marshes.Pools)!=T, -1,
                                                                                                            ifelse(is.na(Reclass$Wetlands..inland....Seasonal.Intermittent.Irregular.Rivers.Streams.Creeks)!=T, -1, 
                                                                                                                   NA)))))))))))))

#####
##Conservation Actions
Reclass$M_Habitat <- NA # Combine IUCN 1 and 2
Reclass$M_Species <- NA # IUCN 3
Reclass$M_SocialPolicy <- NA #Combine IUCN 4, 5, 6

#####

Reclass$M_Habitat<-ifelse(is.na(Reclass$m_1.1)!=T, -1,
                          ifelse(is.na(Reclass$m_1.2)!=T, -1,
                                 ifelse(is.na(Reclass$m_2.1)!=T, -1,
                                        ifelse(is.na(Reclass$m_2.2)!=T, -1,
                                               ifelse(is.na(Reclass$m_2.3)!=T, -1,
                                                      NA)))))

Reclass$M_Species<-ifelse(is.na(Reclass$m_3.1.1)!=T, -1,
                          ifelse(is.na(Reclass$m_3.1.2)!=T, -1,
                                 ifelse(is.na(Reclass$m_3.1.3)!=T, -1,
                                        ifelse(is.na(Reclass$m_3.2)!=T, -1,
                                               ifelse(is.na(Reclass$m_3.3.1)!=T, -1,
                                                      ifelse(is.na(Reclass$m_3.3.2)!=T, -1,
                                                             ifelse(is.na(Reclass$m_3.4.1)!=T, -1,
                                                                    ifelse(is.na(Reclass$m_3.4.2)!=T, -1,
                                                                           NA))))))))

Reclass$M_SocialPolicy<-ifelse(is.na(Reclass$m_4.1)!=T, -1,
                               ifelse(is.na(Reclass$m_4.2)!=T, -1,
                                      ifelse(is.na(Reclass$m_4.3)!=T, -1,
                                             ifelse(is.na(Reclass$m_5.1.1)!=T, -1,
                                                    ifelse(is.na(Reclass$m_5.1.2)!=T, -1,
                                                           ifelse(is.na(Reclass$m_5.1.3)!=T, -1,
                                                                  ifelse(is.na(Reclass$m_5.1.4)!=T, -1,
                                                                         ifelse(is.na(Reclass$m_5.2)!=T, -1,
                                                                                ifelse(is.na(Reclass$m_5.3)!=T, -1,
                                                                                       ifelse(is.na(Reclass$m_5.4.1)!=T, -1,
                                                                                              ifelse(is.na(Reclass$m_5.4.2)!=T, -1,
                                                                                                     ifelse(is.na(Reclass$m_5.4.3)!=T, -1,
                                                                                                            ifelse(is.na(Reclass$m_5.4.4)!=T, -1,
                                                                                                                   ifelse(is.na(Reclass$m_6.2)!=T, -1,
                                                                                                                          ifelse(is.na(Reclass$m_6.2)!=T, -1,
                                                                                                                                 ifelse(is.na(Reclass$m_6.3)!=T, -1,
                                                                                                                                        ifelse(is.na(Reclass$m_6.4)!=T, -1,
                                                                                                                                               ifelse(is.na(Reclass$m_6.5)!=T, -1,
                                                                                                                                                      NA))))))))))))))))))

#####
##THREATS

#####
##Threats
Reclass$T_Develop <- NA # IUCN 1,4,6,9
Reclass$T_NatResource <- NA # IUCN 2,3,5,7.1 and 7.3
Reclass$T_Dams<-NA #7.2
Reclass$T_InvSpp<-NA #8
Reclass$T_NatDisaster<-NA #10, 11
Reclass$T_Other<-NA #12 #Will calculate, but probably not use

#####
Reclass$T_Develop<-ifelse(is.na(Reclass$t_1.1)!=T, -1,
                          ifelse(is.na(Reclass$t_1.2)!=T, -1,
                                 ifelse(is.na(Reclass$t_1.3)!=T, -1,
                                        ifelse(is.na(Reclass$m_4.1)!=T, -1,
                                               ifelse(is.na(Reclass$m_4.2)!=T, -1,
                                                      ifelse(is.na(Reclass$t_4.3)!=T, -1,
                                                             ifelse(is.na(Reclass$t_6.1)!=T, -1,
                                                                    ifelse(is.na(Reclass$t_6.2)!=T, -1,
                                                                           ifelse(is.na(Reclass$t_6.3)!=T, -1,
                                                                                  ifelse(is.na(Reclass$t_9.1.1)!=T, -1,
                                                                                         ifelse(is.na(Reclass$t_9.1.2)!=T, -1,
                                                                                                ifelse(is.na(Reclass$t_9.1.3)!=T, -1,
                                                                                                       ifelse(is.na(Reclass$t_9.2.1)!=T, -1,
                                                                                                              ifelse(is.na(Reclass$t_9.2.2)!=T, -1,
                                                                                                                     ifelse(is.na(Reclass$t_9.2.3)!=T, -1,
                                                                                                                            ifelse(is.na(Reclass$t_9.3.1)!=T, -1,
                                                                                                                                   ifelse(is.na(Reclass$t_9.3.2)!=T, -1,
                                                                                                                                          ifelse(is.na(Reclass$t_9.3.3)!=T, -1,
                                                                                                                                                 ifelse(is.na(Reclass$t_9.3.4)!=T, -1,
                                                                                                                                                        ifelse(is.na(Reclass$t_9.4)!=T, -1,
                                                                                                                                                               ifelse(is.na(Reclass$t_9.5.1)!=T, -1,
                                                                                                                                                                      ifelse(is.na(Reclass$t_9.5.4)!=T, -1,
                                                                                                                                                                             ifelse(is.na(Reclass$t_9.6.1)!=T, -1,
                                                                                                                                                                                    ifelse(is.na(Reclass$t_9.6.2)!=T, -1,
                                                                                                                                                                                           ifelse(is.na(Reclass$t_9.6.4)!=T, -1,
                                                                                                                                                                                                  NA)))))))))))))))))))))))))

######

Reclass$T_NatResource<-ifelse(is.na(Reclass$t_2.1.1)!=T, -1,
                              ifelse(is.na(Reclass$t_2.1.2)!=T, -1,
                                     ifelse(is.na(Reclass$t_2.1.3)!=T, -1,
                                            ifelse(is.na(Reclass$t_2.1.4)!=T, -1,
                                                   ifelse(is.na(Reclass$t_2.2.1)!=T, -1,
                                                          ifelse(is.na(Reclass$t_2.2.2)!=T, -1,
                                                                 ifelse(is.na(Reclass$t_2.2.3)!=T, -1,
                                                                        ifelse(is.na(Reclass$t_2.3.1)!=T, -1,
                                                                               ifelse(is.na(Reclass$t_2.3.2)!=T, -1,
                                                                                      ifelse(is.na(Reclass$t_2.3.3)!=T, -1,
                                                                                             ifelse(is.na(Reclass$t_2.3.4)!=T, -1,
                                                                                                    ifelse(is.na(Reclass$t_2.4.1)!=T, -1,
                                                                                                           ifelse(is.na(Reclass$t_2.4.2)!=T, -1,
                                                                                                                  ifelse(is.na(Reclass$t_2.4.3)!=T, -1,
                                                                                                                         ifelse(is.na(Reclass$t_3.1)!=T, -1,
                                                                                                                                ifelse(is.na(Reclass$t_3.2)!=T, -1,
                                                                                                                                       ifelse(is.na(Reclass$t_3.3)!=T, -1,
                                                                                                                                              ifelse(is.na(Reclass$t_5.1.1)!=T, -1,
                                                                                                                                                     ifelse(is.na(Reclass$t_5.1.2)!=T, -1,
                                                                                                                                                            ifelse(is.na(Reclass$t_5.2.2)!=T, -1,
                                                                                                                                                                   ifelse(is.na(Reclass$t_5.2.4)!=T, -1,
                                                                                                                                                                          ifelse(is.na(Reclass$t_5.3.1)!=T, -1,
                                                                                                                                                                                 ifelse(is.na(Reclass$t_5.3.2)!=T, -1,
                                                                                                                                                                                        ifelse(is.na(Reclass$t_5.3.3)!=T, -1,
                                                                                                                                                                                               ifelse(is.na(Reclass$t_5.3.4)!=T, -1,
                                                                                                                                                                                                      ifelse(is.na(Reclass$t_5.3.5)!=T, -1,
                                                                                                                                                                                                             ifelse(is.na(Reclass$t_5.4.1)!=T, -1,
                                                                                                                                                                                                                    ifelse(is.na(Reclass$t_5.4.2)!=T, -1,
                                                                                                                                                                                                                           ifelse(is.na(Reclass$t_5.4.3)!=T, -1,
                                                                                                                                                                                                                                  ifelse(is.na(Reclass$t_5.4.4)!=T, -1,
                                                                                                                                                                                                                                         ifelse(is.na(Reclass$t_5.4.5)!=T, -1,
                                                                                                                                                                                                                                                ifelse(is.na(Reclass$t_5.4.6)!=T, -1,
                                                                                                                                                                                                                                                       ifelse(is.na(Reclass$t_7.1.1)!=T, -1,
                                                                                                                                                                                                                                                              ifelse(is.na(Reclass$t_7.1.2)!=T, -1,
                                                                                                                                                                                                                                                                     ifelse(is.na(Reclass$t_7.1.3)!=T, -1,
                                                                                                                                                                                                                                                                            ifelse(is.na(Reclass$t_7.3)!=T, -1,
                                                                                                                                                                                                                                                                                   NA))))))))))))))))))))))))))))))))))))

#####

Reclass$T_Dams<-ifelse(is.na(Reclass$t_7.2.1)!=T, -1,
                       ifelse(is.na(Reclass$t_7.2.2)!=T, -1,
                              ifelse(is.na(Reclass$t_7.2.3)!=T, -1,
                                     ifelse(is.na(Reclass$t_7.2.4)!=T, -1,
                                            ifelse(is.na(Reclass$t_7.2.5)!=T, -1,
                                                   ifelse(is.na(Reclass$t_7.2.6)!=T, -1,
                                                          ifelse(is.na(Reclass$t_7.2.7)!=T, -1,
                                                                 ifelse(is.na(Reclass$t_7.2.8)!=T, -1,
                                                                        ifelse(is.na(Reclass$t_7.2.9)!=T, -1,
                                                                               ifelse(is.na(Reclass$t_7.2.10)!=T, -1,
                                                                                      ifelse(is.na(Reclass$t_7.2.11)!=T, -1,
                                                                                             NA)))))))))))

#####

Reclass$T_InvSpp<-ifelse(is.na(Reclass$t_8.1.1)!=T, -1,
                         ifelse(is.na(Reclass$t_8.1.2)!=T, -1,
                                ifelse(is.na(Reclass$t_8.2)!=T, -1,
                                       ifelse(is.na(Reclass$t_8.2.1)!=T, -1,
                                              ifelse(is.na(Reclass$t_8.2.2)!=T, -1,
                                                     ifelse(is.na(Reclass$t_8.3)!=T, -1,
                                                            ifelse(is.na(Reclass$t_8.4.1)!=T, -1,
                                                                   ifelse(is.na(Reclass$t_8.4.2)!=T, -1,
                                                                          ifelse(is.na(Reclass$t_8.5.1)!=T, -1,
                                                                                 ifelse(is.na(Reclass$t_8.6)!=T, -1,
                                                                                        NA))))))))))

######

Reclass$T_NatDisaster<-ifelse(is.na(Reclass$t_10.1)!=T, -1,
                              ifelse(is.na(Reclass$t_10.3)!=T, -1,
                                     ifelse(is.na(Reclass$t_11.1)!=T, -1,
                                            ifelse(is.na(Reclass$t_11.2)!=T, -1,
                                                   ifelse(is.na(Reclass$t_11.3)!=T, -1,
                                                          ifelse(is.na(Reclass$t_11.4)!=T, -1,
                                                                 ifelse(is.na(Reclass$t_11.5)!=T, -1,
                                                                        NA)))))))


#####
Reclass$T_Other<-ifelse(is.na(Reclass$t_12.1)!=T, -1,
                        NA)

Reclass_table<-Reclass[,c("species","category","genus","class","order_",
                          "family","IUCN.temporary","IUCN.permanent",
                          "IUCN.nat.lentic","IUCN.nat.lotic","IUCN.artificial",
                          "IUCN.other","M_Habitat","M_Species","M_SocialPolicy",
                          "T_Develop","T_NatResource","T_Dams", "T_InvSpp",
                          "T_NatDisaster")]

write.csv(Reclass_table, "IUCN_Attributes.csv", row.names=FALSE, na='')