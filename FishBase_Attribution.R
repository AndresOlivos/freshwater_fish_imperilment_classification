# FishBase attribution for species in IUCN database

library(rfishbase)
library(tidyr)
library(OneR)
library(dplyr)

setwd("R:\\FWL\\Arismendi-Lab\\Andres\\Gilbert_Freshwater_Fish_Analysis\\Revised_Analysis_NatureCommunications\\Input_datasets\\Tabular_data_by_species")

iucn_table <-read.csv("Ranges_Attributes.csv", header = T, stringsAsFactors = FALSE)

specieslist<-as.vector(iucn_table$sci_name)
length(unique(specieslist))

# calculate number of species not found in FishBase
sps_validation = unique(validate_names(specieslist))

# get species data attributes from FishBase
speciesdata<-species(specieslist)

# get stocks attributes from FishBase
stockdata<-stocks(specieslist)

# get ecology attributes from FishBase
ecologydata <- ecology(specieslist)

# get swimming attributes
swimdata <- swimming(specieslist)

str(speciesdata)
length(unique(speciesdata$Species))
str(stockdata)
length(unique(stockdata$Species))
str(ecologydata)
length(unique(ecologydata$Species))
str(swimdata)
length(unique(swimdata$Species))

names(iucn_table)[names(iucn_table)=="sci_name"]<-"Species"

fishbasetable = merge(iucn_table, speciesdata, by='Species') %>% 
  merge(stockdata, by='Species', all=TRUE) %>% 
  merge(ecologydata, by='Species', all=TRUE) %>% 
  merge(swimdata, by='Species', all=TRUE)

Reclass = fishbasetable

## remove exact duplicates
nrow(Reclass)
nrow(unique())

##Reclass Fishbase attributes

# substrate (organic, gravels, rock)
Reclass$Organic.material<-NA
names(Reclass)

Reclass$Organic.material <- ifelse(Reclass$Vegetation == -1, -1, 
                                   ifelse(Reclass$Stems == -1, -1,
                                          ifelse(Reclass$Roots == -1, -1,
                                                 ifelse(Reclass$Driftwood == -1, -1,
                                                        ifelse(Reclass$Macrophyte == -1, -1,
                                                               ifelse(Reclass$SeaGrassBeds == -1, -1,
                                                                      ifelse(Reclass$Vegetation == 0, 0, NA)))))))
length(unique(Reclass$Species))

Reclass$Coarse.sand.or.finer<-NA
Reclass$Coarse.sand.or.finer <- ifelse(Reclass$SoftBottom == -1, -1, 
                                       ifelse(Reclass$Sand == -1, -1,
                                              ifelse(Reclass$Coarse == -1, -1,
                                                     ifelse(Reclass$Fine == -1, -1,
                                                            ifelse(Reclass$Silt == -1, -1,
                                                                   ifelse(Reclass$Mud == -1, -1,
                                                                          ifelse(Reclass$Ooze == -1, -1,
                                                                                 ifelse(Reclass$Detritus == -1, -1,
                                                                                        ifelse(Reclass$Burrow == -1, -1,
                                                                                               ifelse(Reclass$SoftBottom == 0, 0, NA))))))))))
print(Reclass$Coarse.sand.or.finer)

Reclass$Rocky.bottom<-NA
Reclass$Rocky.bottom <- ifelse(Reclass$HardBottom == -1, -1, 
                               ifelse(Reclass$Rocky == -1, -1,
                                      ifelse(Reclass$Rubble == -1, -1,
                                             ifelse(Reclass$Gravel == -1, -1,
                                                    ifelse(Reclass$Crevice == -1, -1,
                                                           ifelse(Reclass$SoftBottom == 0, 0, NA))))))
print(Reclass$Rocky.bottom)

# species length attributes

Reclass$Length
Reclass$LTypeMaxM
unique(Reclass$LTypeMaxM)

Reclass$CommonLength
Reclass$LTypeComM

SLmax<-subset(Reclass, Reclass$LTypeMaxM =='SL')
TLmax<-subset(Reclass, Reclass$LTypeMaxM =='TL')
FLmax<-subset(Reclass, Reclass$LTypeMaxM =='FL')
OTmax<-subset(Reclass, Reclass$LTypeMaxM =='OT')
NGmax<-subset(Reclass, Reclass$LTypeMaxM =='NG')

SLintervals <- paste(levels(bin(log(SLmax$Length), nbins=3, method="cluster")), collapse="")
hist(log(SLmax$Length), main=paste("SLIntervals:", SLintervals))

TLintervals <- paste(levels(bin(log(TLmax$Length), nbins=3, method="cluster")), collapse="")
hist(log(TLmax$Length), main=paste("TLIntervals:", TLintervals))

FLintervals <- paste(levels(bin(log(FLmax$Length), nbins=3, method="cluster")), collapse="")
hist(log(FLmax$Length), main=paste("FLIntervals:", FLintervals))

Reclass$LengthCategory<-NA
Reclass$LengthCategory <- ifelse((Reclass$LTypeMaxM =='SL' & log(Reclass$Length) <2.06), "small", 
                                 ifelse((Reclass$LTypeMaxM =='SL' & log(Reclass$Length) <3.22), "medium",
                                        ifelse((Reclass$LTypeMaxM =='SL' & log(Reclass$Length) >=3.22), "large",
                                               ifelse((Reclass$LTypeMaxM =='TL' & log(Reclass$Length) <2.51), "small", 
                                                      ifelse((Reclass$LTypeMaxM =='TL' & log(Reclass$Length) <3.76), "medium",
                                                             ifelse((Reclass$LTypeMaxM =='TL' & log(Reclass$Length) >=3.76), "large",
                                                                    ifelse((Reclass$LTypeMaxM =='FL' & log(Reclass$Length) <2.99), "small", 
                                                                           ifelse((Reclass$LTypeMaxM =='FL' & log(Reclass$Length) <4.18), "medium",
                                                                                  ifelse((Reclass$LTypeMaxM =='FL' & log(Reclass$Length) >=4.18), "large",
                                                                                         NA)))))))))
unique(Reclass$LengthCategory)

#TempMin	TempMax	TempRef	TempPreferred	TempPref25	TempPref50	TempPref75	TempPrefRef
Reclass$UseTemp<-NA
Reclass$UseTemp<-ifelse(is.na(Reclass$TempPreferred)!=T, Reclass$TempPreferred,
                        ifelse(is.na(rowMeans(Reclass[,c('TempMin', 'TempMax')], na.rm=TRUE))!=T, rowMeans(Reclass[,c('TempMin', 'TempMax')], na.rm=TRUE), NA))



Reclass$RangeTemp<-NA
Reclass$RangeTemp<-Reclass$TempMax-Reclass$TempMin
Reclass$RangeTempOther<-Reclass$TempPref75-Reclass$TempPref25

print(Reclass$UseTemp)
#print(Reclass$RangeTemp)

Reclass$TRange<-NA
Reclass$TRange<-ifelse(is.na(Reclass$RangeTemp)!=T, Reclass$RangeTemp,
                       ifelse(is.na(Reclass$RangeTempOther)!=T, Reclass$RangeTempOther, NA))

# trophic level
sort(colnames(Reclass))
Reclass$TrophicLevel<-NA

Reclass$TrophicLevel<-ifelse(is.na(Reclass$DietTroph)!=T, Reclass$DietTroph,
                             ifelse(is.na(Reclass$DietTLu)!=T, Reclass$DietTLu, 
                                    ifelse(is.na(Reclass$FoodTroph)!=T, Reclass$FoodTroph, NA)))

# adult mode
unique(Reclass$AdultMode)
Reclass$AdultMode <- ifelse(Reclass$AdultMode == "Anguilliform", "anguilliform",
                            Reclass$AdultMode)

# adult mode
unique(Reclass$BodyShapeI)
Reclass$BodyShapeI <- ifelse(Reclass$BodyShapeI == "Elongated", "elongated",
                            Reclass$BodyShapeI)

# Importance
unique(Reclass$Importance)
Reclass$Importance <- ifelse(Reclass$Importance == " ", NA,
                             Reclass$Importance)

# bait use
unique(Reclass$UsedasBait)
Reclass$UsedasBait <- ifelse(Reclass$UsedasBait == " ", NA,
                             Reclass$UsedasBait)

# electrogenic
unique(Reclass$Electrogenic)
Reclass$Electrogenic <- ifelse(Reclass$Electrogenic == "Weakly discharging",
                               "weakly discharging",
                               ifelse(Reclass$Electrogenic == "Electrosensing only",
                                      "electrosensing only",
                                      Reclass$Electrogenic))

# Anadromy/catadromy
Reclass$AnaCat <- ifelse(Reclass$AnaCat %in% c(" ",
                                               "unknown",
                                               "anadromous?",
                                               "amphidromous?"),
                         NA,
                         Reclass$AnaCat)

##"pHMin" "pHMax" "dHMin" "dHMax"

Reclass$pH_Range<-NA
Reclass$pHMax<-ifelse(Reclass$pHMax>14,NA,Reclass$pHMax)
##One pH is over 70, another is 25 - removing since the pH scale should be 0 to 14
Reclass$pH_Range<-ifelse(is.na(rowMeans(Reclass[,c('pHMax', 'pHMin')], na.rm=TRUE))!=T, (Reclass$pHMax-Reclass$pHMin), NA)
Reclass$dH_Range<-NA
max(na.omit(Reclass$dHMax))
Reclass$dHMax<-ifelse(Reclass$dHMax>2000,NA,Reclass$dHMax)
##Carlhubbsia kidderi appears to have a typo or alternate unit with a value of >2000
Reclass$dH_Range<-ifelse(is.na(rowMeans(Reclass[,c('dHMax', 'dHMin')], na.rm=TRUE))!=T, (Reclass$dHMax-Reclass$dHMin), NA)
print(max(na.omit(Reclass$dH_Range)))
print(max(na.omit(Reclass$pH_Range)))

Reclass_table<-Reclass[,c("Species","Importance","UsedforAquaculture",
                      "UsedasBait","Aquarium","GameFish","BodyShapeI",
                      "Fresh","Brack","Saltwater","DemersPelag","AnaCat",
                      "DepthRangeShallow","DepthRangeDeep","LongevityWild",
                      "Vulnerability","LengthCategory","Weight","CITES_Code",
                      "Dangerous","Electrogenic","FeedingType","TrophicLevel",
                      "pHMin","pHMax","pH_Range","dHMin","dHMax","dH_Range",
                      "EnvTemp","UseTemp","TRange","AdultMode",
                      "Organic.material","Coarse.sand.or.finer","Rocky.bottom")]

# merge duplicate species keeping non-NA values for each column, when available
# for one of the dups
no_dups <- Reclass_table %>%
  arrange(rowSums(is.na(.))) %>%  # Sort by number of NA values in each row.
  distinct(Species, .keep_all = TRUE)   # Keep the first row for each unique ID.

write.csv(no_dups, "FishBase_Attributes.csv", row.names=FALSE, 
          na = c(NA, '<NA>'))
