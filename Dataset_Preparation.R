################################################################################
############################# Dataset merging ##################################
################################################################################

library(dplyr)
library(randomForest)
library(ggplot2)
library(janitor)
library(reshape2)

setwd(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Input_datasets\Tabular_data_by_species)")

ranges = read.csv("Ranges_Attributes.csv",
                header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

iucn = read.csv("IUCN_Attributes.csv",
                header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

fishbase = read.csv("FishBase_Attributes.csv",
                    header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

worldclim = read.csv("WorldClim_Attributes.csv",
                     header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

worldpop = read.csv("WorldPop_Attributes.csv",
                    header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

worldbank = read.csv("WorldBank_Attributes.csv",
                     header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

gdw = read.csv("GDW_Attributes.csv",
               header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

waste = read.csv("HydroWASTE_Attributes.csv",
                 header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

gloric = read.csv("GloRiC_Attributes.csv",
                  header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

probav = read.csv("PROBAV_Attributes.csv",
                  header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

hfp = read.csv("HFP_Attributes.csv",
               header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

gbif = read.csv("GBIF_Attributes.csv",
                header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

wdpa = read.csv("WDPA_Attributes.csv",
               header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

## Clean extra fields in each table
colnames(ranges)
ranges<-subset(ranges, select = -c(Freshwater_Percent))
colnames(ranges)[colnames(ranges) == "order_"] <- "order"
colnames(iucn)
colnames(fishbase)
colnames(worldclim)
colnames(worldpop)
worldpop<-subset(worldpop, select = -c(EarlyPop,LatePop,Clipped_AreaSqk))
colnames(worldbank)
worldbank<-subset(worldbank, select = -c(GDP_yearCount))
colnames(gdw)
colnames(waste)
waste<-subset(waste, select = -c(WASTE_count))
colnames(gloric)
colnames(probav)
colnames(hfp)
colnames(gbif)
colnames(wdpa)

## Merge all tables
datasets = list(ranges,iucn,fishbase,worldclim,worldpop,worldbank,gdw,waste,
                gloric,probav,hfp,gbif,wdpa)

# first standardize species column names
sps_cols = c("sci_name","species","Species")

for (i in seq_along(datasets)) {
  colnames(datasets[[i]])[colnames(datasets[[i]]) %in% sps_cols] <- "species"
}

# merge
alldata = Reduce(function(x, y) 
  merge(x, y[!duplicated(y$species), ], by = "species", 
        all.x = TRUE, all.y = FALSE),
  datasets)

colnames(alldata)
length(alldata$species)

# drop species that are not really freshwater according to FishBase 
# (drops 1,166 sps)

alldata<- subset(alldata,alldata$Fresh!= 0)
alldata$Fresh <- NULL

length(alldata$species)

# drop extinct species -- 3 species with ranges labeled as extant in IUCN data

alldata<- subset(alldata,alldata$category!= 'EX') # Extinct

# assign numeric values to IUCN categories

alldata %>% count(category)

alldata$num_category[alldata$category == "DD" ] <- 0
alldata$num_category[alldata$category == "LC" ] <- 1
alldata$num_category[alldata$category == "NT" ] <- 2
alldata$num_category[alldata$category == "VU" ] <- 3
alldata$num_category[alldata$category == "EN" ] <- 4
alldata$num_category[alldata$category == "CR" ] <- 5
alldata$num_category[alldata$category == "EX" ] <- 6

# count na's by row
alldata$na_count <- apply(alldata, 1, function(x) sum(is.na(x)))
hist(alldata$na_count)
min(alldata$na_count)
max(alldata$na_count)

write.csv(alldata, 
         r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Attribute_table\Native_FW_Fish_Attributes.csv)", 
         row.names=FALSE, na = "")

rm(list = setdiff(ls(), c("alldata")))

################################################################################
#################### Dataset preparation for RF ################################
################################################################################
# alldata = read.csv(r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_datasets\Attribute_table\Extant_Native_FW_Fishes_Attributes.csv)", 
#                     header=TRUE, stringsAsFactors = FALSE, strip.white=TRUE)

str(alldata)

# work with copy
rfdata = alldata

# check count of species with missing values
barplot(sort(colSums(is.na(rfdata)),decreasing=T),las = 2, cex.names = 0.5)

# filter to variables with most NAs
barplot(sort(colSums(is.na(rfdata)),decreasing=T)[1:40],las = 2, cex.names = 0.5)

# drop variables missing for >1,000 species 
rfdata<-subset(rfdata, select = -c(CITES_Code,AdultMode,LongevityWild,
                                   IUCN.artificial,DepthRangeDeep,dH_Range,
                                   DepthRangeShallow,M_Species,T_NatDisaster,
                                   IUCN.other,dHMin,Weight,dHMax,AnaCat,
                                   M_SocialPolicy,pH_Range,pHMax,pHMin,T_InvSpp,
                                   FeedingType,TrophicLevel,IUCN.temporary,
                                   TRange,UseTemp,Importance,T_Dams,M_Habitat,
                                   Rocky.bottom,Organic.material,
                                   Coarse.sand.or.finer,T_Develop,
                                   IUCN.nat.lentic,T_NatResource,Min_Elevation,
                                   Max_Elevation,IUCN.nat.lotic))

# drop columns with response variables
rfdata = subset(rfdata, select = -c(Vulnerability))

# clean some columns with a single value or too many levels for RF
rfdata = subset(rfdata, select = -c(IUCN.permanent)) # single level
rfdata = subset(rfdata, select = -c(species,class,genus,family)) # too many levs

# alter value types for RF
str(rfdata)

rfdata = rfdata %>% 
  mutate_if(is.integer,as.numeric) %>%
  mutate_if(is.character,as.factor) %>%
  mutate_if(is.logical,as.factor)

is.na(rfdata)<-sapply(rfdata,is.infinite)

# convert numerical to categorical for some columns
factor_cols = c("Reach_type","Class_geom","Class_hydr","Class_phys",
                "Brack","Saltwater","GameFish")


rfdata[factor_cols] <- lapply(rfdata[factor_cols], as.factor)

str(rfdata)

# generate imputations by median/mode
# ?na.roughfix
rfdata <- na.roughfix(rfdata)

########################### Correlation tests ##################################
########################### Auxiliary functions ################################

# Function to get high correlation columns

get_high_correlations <- function(df, target_col, col_list, threshold = 0.85) {
  # Ensure columns exist
  if (!(target_col %in% names(df))) {
    stop("Target column not found in dataframe.")
  }
  
  valid_cols <- col_list[col_list %in% names(df) & col_list != target_col]
  
  # Compute correlations
  cor_results <- sapply(valid_cols, function(col) {
    cor(df[[target_col]], df[[col]], use = "complete.obs")
  })
  
  # Filter based on threshold
  cor_filtered <- cor_results[abs(cor_results) > threshold]
  
  # Return as data frame
  result_df <- data.frame(
    column = names(cor_filtered),
    correlation = cor_filtered,
    row.names = NULL
  )
  
  # Sort by absolute correlation descending
  result_df <- result_df[order(-abs(result_df$correlation)), ]
  
  return(result_df)
}

# Function to plot correlation matrix

plot_correlation_matrix <- function(df, columns, txtsize) {
  # Subset the dataframe with selected columns
  data_subset <- df[, columns]
  
  # Compute correlation matrix
  cor_matrix <- cor(data_subset, use = "complete.obs")
  
  # Melt the correlation matrix for ggplot2
  cor_melt <- reshape2::melt(cor_matrix)
  
  # Plot using ggplot2
  ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1),
                         name = "Correlation") +
    geom_text(aes(label = round(value, 2)), color = "black", size = txtsize) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1,
                                     size=txtsize+4),
          axis.text.y = element_text(size=txtsize+4)) +
    coord_fixed() +
    labs(title = "Correlation Matrix", x = "", y = "")
}

plot_correlation_matrix_cat <- function(df, columns, txtsize) {
  # Subset the dataframe with selected columns
  data_subset <- df[, columns]
  
  # Compute correlation matrix
  cor_matrix <- chisq.test(data_subset)
  
  # Melt the correlation matrix for ggplot2
  cor_melt <- reshape2::melt(cor_matrix)
  
  # Plot using ggplot2
  ggplot(cor_melt, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1),
                         name = "Correlation") +
    geom_text(aes(label = round(value, 2)), color = "black", size = txtsize) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1,
                                     size=txtsize+4),
          axis.text.y = element_text(size=txtsize+4)) +
    coord_fixed() +
    labs(title = "Correlation Matrix", x = "", y = "")
}
################################################################################

num_columns <- names(rfdata)[sapply(rfdata, is.numeric)]
num_columns <- setdiff(num_columns, c('num_category','Area_Sqk'))
num_columns

## assess correlations to range areas
get_high_correlations(rfdata, "Area_Sqk", num_columns, threshold = 0.2)

## standardize variables with high correlation with range area

rfdata$GloRiC_km_p<-(rfdata$GloRiC_km/rfdata$Area_Sqk)
rfdata$Lake_wet_p<-(rfdata$Lake_wet/rfdata$Area_Sqk)*1000
rfdata$Symp_count_p<-(rfdata$Symp_count/rfdata$Area_Sqk)*10000
rfdata$Class_count_p<-(rfdata$Class_count/rfdata$Area_Sqk)*10000
rfdata$POP_SERVED_p<-(rfdata$POP_SERVED/rfdata$Area_Sqk)

# adjust values and names of GDW attributes

##############
####################### merge gdw datasets into one ###########################################
rfdata$Dams_count <- rfdata$smallDams_count + rfdata$largeDams_count
rfdata$Dams_AREA_SKM <- rfdata$largeDams_AREA_SKM + rfdata$smallDams_AREA_SKM
rfdata$Dams_CATCH_SKM <- rfdata$smallDams_CATCH_SKM + rfdata$largeDams_CATCH_SKM
rfdata$Dams_DIS_AVG_LS <- rfdata$largeDams_DIS_AVG_LS + rfdata$smallDams_DIS_AVG_LS

rfdata$Dams_p<-(rfdata$Dams_count/rfdata$Area_Sqk)*100000
rfdata$RA_Dams_p<-(rfdata$Dams_AREA_SKM/rfdata$Area_Sqk)*10000
rfdata$CAB_Dams_p<-(rfdata$Dams_CATCH_SKM/rfdata$Area_Sqk)*100
rfdata$DB_Dams_p<-(rfdata$Dams_DIS_AVG_LS/rfdata$Area_Sqk)

summary(rfdata$Dams_p)
summary(rfdata$RA_Dams_p)
summary(rfdata$CAB_Dams_p)
summary(rfdata$DB_Dams_p)

rfdata<-subset(rfdata, select = -c(largeDams_count,largeDams_AREA_SKM,
                                   largeDams_CATCH_SKM,largeDams_DIS_AVG_LS,
                                   smallDams_count,smallDams_AREA_SKM,
                                   smallDams_CATCH_SKM,smallDams_DIS_AVG_LS,
                                   Dams_count,Dams_AREA_SKM,
                                   Dams_CATCH_SKM,Dams_DIS_AVG_LS
                                   ))

## less than .5 cor
rfdata$Ramsar_count_p<-(rfdata$Ramsar_count/rfdata$Area_Sqk)*10000
rfdata$Intro_count_p<-(rfdata$Intro_count/rfdata$Area_Sqk)*10000
rfdata$WASTE_DIS_p<-(rfdata$WASTE_DIS/rfdata$Area_Sqk)
rfdata$Perimeter_p<-(rfdata$Perimeter_k/rfdata$Area_Sqk)

summary(rfdata$Ramsar_count_p)
summary(rfdata$Intro_count)
summary(rfdata$WASTE_DIS_p)
summary(rfdata$Perimeter_p)

# drainage area related-variables ##############################################
rfdata<-subset(rfdata, select = -c(GloRiC_km,Lake_wet,
                                   Class_count,
                                   Symp_count,POP_SERVED,
                                   Ramsar_count,Intro_count,
                                   WASTE_DIS,Perimeter_k))

# Re run correlation test with transformed variables
num_columns <- names(rfdata)[sapply(rfdata, is.numeric)]
num_columns <- setdiff(num_columns, c('num_category','Area_Sqk'))

get_high_correlations(rfdata, "Area_Sqk", num_columns, threshold = 0.2)

## Examine correlationships within and among predictor datasets
# Correlation among WorldBank variables
plot_correlation_matrix(rfdata, c('LatestGDP','EarlyGDP','LatestPC',
                                   'EarlyPC','GDPpercent','GDPslope',
                                   'PCpercent','PCslope'),txtsize = 3)

# remove highly correlated variables (>0.9)
# keeping slopes, since less correlated with remaining variables
rfdata = subset(rfdata, select = -c(LatestGDP,EarlyGDP,LatestPC,
                                    PCpercent,PCslope))

# test again
plot_correlation_matrix(rfdata, c('EarlyPC','GDPpercent','GDPslope'
                                  ),txtsize = 3)

# correlation among IUCN range variables
#East and west limits are 89% correlated. North and South are 83%...
plot_correlation_matrix(rfdata, c('North_Limit','South_Limit',
                                  'West_Limit','East_Limit'),txtsize = 3)

rfdata = subset(rfdata, select = -c(North_Limit,West_Limit))

# Correlation among IUCN ranges overlap variables
plot_correlation_matrix(rfdata, c('Symp_count_p',
                                  'Intro_count_p',
                                  'Extir_count'),
                        txtsize = 3)

rfdata = subset(rfdata, select = -c(Intro_count_p))

cor(rfdata$GloRiC_km_p,rfdata$Class_count_p)

# correlation among GloRiC variables
plot_correlation_matrix(rfdata,  c("CMI_indx","Class_count_p", 
                                   "Lake_wet_p","Log_Q_avg","Log_Q_var",
                                   "Stream_pow","Temp_min","GloRiC_km_p"), 2)

# sum of lake and wetland kms and stream network kms are 93% correlated
# classes count is less correlated with other variables, so removing stream km
rfdata = subset(rfdata, select = -c(GloRiC_km_p,Lake_wet_p))

# bioclim variables
bioclim_cols <- names(rfdata)[grepl("bio_", names(rfdata))]
plot_correlation_matrix(rfdata, bioclim_cols,txtsize=2)

# remove highly correlated variables
# keeping: bio_4, 8, 16, 17, 18, 19
rfdata = subset(rfdata, select = -c(bio_1,bio_2,bio_3,bio_5,bio_6,bio_7,bio_9,
                                    bio_10,bio_11,bio_12,bio_13,bio_14,bio_15))

bioclim_cols <- names(rfdata)[grepl("bio_", names(rfdata))]
plot_correlation_matrix(rfdata, bioclim_cols,2)

# land cover variables
lc_cols <- names(rfdata)[grepl("LC_", names(rfdata))]
plot_correlation_matrix(rfdata, lc_cols,2)

# human footprint variables
cor(rfdata$hfp_1993,rfdata$hfp_2009)
rfdata$hfp_1993<-NULL

# WorldPop variables
plot_correlation_matrix(rfdata, c('EarlyPD','LatePD','PDslope',
                                  'PDpercent'), txtsize = 3)

rfdata = subset(rfdata, select = -c(EarlyPD,LatePD))

# HydroWASTE variables
cor(rfdata$POP_SERVED_p,rfdata$WASTE_DIS_p)

# remove waste discharge
rfdata$WASTE_DIS_p <- NULL

# Correlation among WDPA data
cor(rfdata$Protected_Percent,rfdata$Ramsar_count_p)

# Correlation among GDW data
plot_correlation_matrix(rfdata, c('Dams_p','RA_Dams_p',
                                  'CAB_Dams_p','DB_Dams_p'),
                        txtsize = 3)

rfdata$DB_Dams_p <- NULL

# print correlation matrix to see where we are
num_columns <- names(rfdata)[sapply(rfdata, is.numeric)]
plot_correlation_matrix(rfdata,num_columns,0)

get_high_correlations(rfdata, "Temp_min", num_columns, threshold = 0.2)
rfdata$Temp_min<-NULL

# check other colorful correlations


################################################################################
get_high_correlations(rfdata, "Class_count_p", num_columns, threshold = 0.2)
get_high_correlations(rfdata, "Symp_count_p", num_columns, threshold = 0.2)
rfdata$Symp_count_p <- NULL

get_high_correlations(rfdata, "CAB_Dams_p", num_columns, threshold = 0.2)
get_high_correlations(rfdata, "PDslope", num_columns, threshold = 0.2)
get_high_correlations(rfdata, "Log_Q_var", num_columns, threshold = 0.2)
get_high_correlations(rfdata, "Perimeter_p", num_columns, threshold = 0.2)
rfdata$Perimeter_p <- NULL
get_high_correlations(rfdata, "POP_SERVED_p", num_columns, threshold = 0.2)

# last check
num_columns <- names(rfdata)[sapply(rfdata, is.numeric)]
plot_correlation_matrix(rfdata,num_columns,0)


## all remaining variables are less than 85% correlated with each other and 
# less than 50% correlated with range area

rfdataN<-dplyr::select_if(rfdata, is.numeric)
cortable<-cor(rfdataN)
write.csv((cortable), "num_correlations_RF_table.csv", row.names=TRUE)


rfdataC<-dplyr::select_if(rfdata, is.factor)

cramer_v <- function(x, y) {
  cm <- table(x, y)
  n <- sum(cm)
  r <- nrow(cm)
  k <- ncol(cm)
  
  chi2 <- chisq.test(cm)$statistic
  chi2corr <- max(0, chi2 - (k - 1) * (r - 1) / (n - 1))
  
  kcorr <- k - (k - 1)^2 / (n - 1)
  rcorr <- r - (r - 1)^2 / (n - 1)
  
  return(sqrt(chi2corr / n) / min(kcorr - 1, rcorr - 1))
}

# Function to compare all pairs using chi-squared test
cramers_table_pairs <- function(df) {
  vars <- names(df)
  n <- length(vars)
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      var1 <- as.factor(df[[i]])
      var2 <- as.factor(df[[j]])
      # Try to run chi-squared test, handle errors (e.g., zero counts)
      test_result <- cramer_v(var1,var2)
      
      print(test_result)
      
      }
    }
  }

cramers_table_pairs(rfdataC) # no categorical covariates > cramers v abs(.7)

rfdataC<-dplyr::select_if(rfdata, is.factor)

# drop drainage area field
rfdata = subset(rfdata, select = -c(Area_Sqk))

########################### final adjustements before RF #######################

# drop data deficient from further analysis
rfdata<- subset(rfdata,rfdata$num_category!= 0) # Data deficient

# drop unused levels from the raw category
rfdata$category <- droplevels(rfdata$category)

sort(unique(rfdata$num_category))
rfdata %>% count(num_category)

# Binary response variable: 
# Vulnerable is an "imperiled" category in binary model
rfdata$bin_category<-ifelse(rfdata$num_category < 3, 1, 0)
rfdata %>% count(bin_category)

# Where 1 is non-imperiled and 0 is imperiled -- 
# for the sake of aesthetics in PD plots 

rfdata$num_category <- factor(rfdata$num_category, ordered = TRUE)
rfdata$bin_category<-as.factor(rfdata$bin_category)

str(rfdata)

names(Filter(is.factor, rfdata))
names(Filter(is.numeric, rfdata))

order_count <- rfdata %>% count(order)
barplot(height=order_count$n, names=order_count$order, las=2, cex.names=0.45)

write.csv(rfdata, 
          r"(R:\FWL\Arismendi-Lab\Andres\Gilbert_Freshwater_Fish_Analysis\Revised_Analysis_NatureCommunications\Output_data\Attribute_table\Native_FW_Fish_Attributes_Prepared.csv)", 
          row.names=TRUE, na = "")

save(rfdata, file = "rfdata.RData")
rm(list = setdiff(ls(), c("rfdata")))


