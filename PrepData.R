#Coastal NOAA post-storm prep data for IRR analysis
#EBG started 2/2021; revised through 5/2012

#get the libraries
library(tidyverse)
library(stringr)

#DATA RETRIEVAL

#First check to see if data directory exists
direxist <- dir.exists("data")

if (direxist == TRUE) {
  #If dir exists, check to see if files exist
  RDV3 <- file.exists("data/ReleaseData_v3.csv")
  RDQ <- file.exists("data/ReleaseDataQuads.csv")
  RDNCE <- file.exists("data/ReleaseDataNCE.csv")
  if (RDV3 == FALSE){
    #If its not there, get it 
    download.file("https://zenodo.org/record/5172799/files/ReleaseData_v3.csv", "data/ReleaseData_v3.csv")
  }
  if (RDQ == FALSE) {
    #If its not there, get it 
    download.file("https://zenodo.org/record/5172799/files/ReleaseDataQuads.csv", "data/ReleaseDataQuads.csv")
  }
  if (RDNCE == FALSE) {
    #If its not there, get it 
    download.file("https://zenodo.org/record/5172799/files/ReleaseDataNCE.csv", "data/ReleaseDataNCE.csv")
  } 
  
} else {
  #If dir does not exist, make it and put the files in.
  dir.create("data")
  download.file("https://zenodo.org/record/5172799/files/ReleaseData_v3.csv", "data/ReleaseData_v3.csv")
  download.file("https://zenodo.org/record/5172799/files/ReleaseDataQuads.csv", "data/ReleaseDataQuads.csv")
  download.file("https://zenodo.org/record/5172799/files/ReleaseDataNCE.csv", "data/ReleaseDataNCE.csv")
}


#ANALYSIS
#Now, define a fn to Separate and Pivot the impact, for each category, 
#into their own dataframes

SepPivotI <- function(key, value) {
  data %>%
    select(userId:image, impactType) %>%
    add_column(question = value) %>%
    mutate(key = str_detect(impactType, value)) %>%
    select (-c(impactType)) %>%
    mutate_all(funs(str_replace(., "TRUE", "1"))) %>%
    mutate_all(funs(str_replace(., "FALSE", "0"))) %>%
    pivot_wider(names_from = userId, values_from = key)
}


#define a fn for terrain to do the same
SepPivotT <- function(key, value) {
  data %>%
    select(userId:image, terrianType) %>%
    add_column(question = value) %>%
    mutate(key = str_detect(terrianType, value)) %>%
    select (-c(terrianType)) %>%
    mutate_all(funs(str_replace(., "TRUE", "1"))) %>%
    mutate_all(funs(str_replace(., "FALSE", "0"))) %>%
    pivot_wider(names_from = userId, values_from = key)
}

#define function to prepare data
PrepDataFn <- function(data_csv) {
  ###### Split and Pivot the data into individual dataframes
  
  water <- data %>%
    select(userId:allWater) %>%
    add_column(question = "allWater") %>%
    mutate_all(funs(str_replace(., "TRUE", "1"))) %>%
    mutate_all(funs(str_replace(., "FALSE", "0"))) %>%
    pivot_wider(names_from = userId, values_from = allWater)
  
  development <- data %>%
    select(userId:image, devType) %>%
    add_column(question = "devType") %>%
    mutate_all(funs(str_replace(., "undeveloped", "0"))) %>%
    mutate_all(funs(str_replace(., "developed", "1"))) %>%
    pivot_wider(names_from = userId, values_from = devType)
  
  washover <- data %>%
    select(userId:image, washoverType) %>%
    add_column(question = "washType") %>%
    mutate_all(funs(str_replace(., "noWashover", "0"))) %>%
    mutate_all(funs(str_replace(., "washover", "1"))) %>%
    pivot_wider(names_from = userId, values_from = washoverType)
  
  damage <- data %>%
    select(userId:image, dmgType) %>%
    add_column(question = "dmgType") %>%
    mutate_all(funs(str_replace(., "noDamage", "0"))) %>%
    mutate_all(funs(str_replace(., "damage", "1"))) %>%
    pivot_wider(names_from = userId, values_from = dmgType)
  
  
  #Impact
  swash <- SepPivotI(swash, "swash")
  collision <- SepPivotI(collision, "collision")
  overwash <- SepPivotI(overwash, "overwash")
  inundation <- SepPivotI(inundation, "inundation")
  
  #A bespoke solution for 'noImpact' dataframe
  noImpact <- data %>%
    select(userId:image, impactType) %>%
    add_column(question = "No_Impact") %>%
    mutate(No_Impact = str_detect(impactType, "NaN")) %>%
    select (-c(impactType)) %>%
    mutate_all(funs(str_replace(., "TRUE", "1"))) %>%
    mutate_all(funs(str_replace(., "FALSE", "0"))) %>%
    pivot_wider(names_from = userId, values_from = No_Impact)
  
  #Terrain; don;t need water since we have already all water above
  sandy <- SepPivotT(sandyCoastline, "sandyCoastline")
  marsh <- SepPivotT(marsh, "marsh")
  inland <- SepPivotT(inland, "inland")
  river <- SepPivotT(river, "river")
  estuary <- SepPivotT(river, "estuary")
  
  #stack all dataframes:
  all_pivot <- rbind(
    development,
    washover,
    damage,
    swash,
    collision,
    overwash,
    inundation,
    noImpact,
    water,
    sandy,
    river,
    marsh,
    inland,
    estuary
  )
  
  
}


#######Prep the data for exp1 and 2
#import the data
data <- read_csv("data/ReleaseData_v3.csv")

all_pivot_exp12 <- PrepDataFn(data)

#Add in the experiment number

experimentCount <- rep(
  c(rep(c(1),times=900), 
    rep(c(2),times=600)),times = 14)


#concatenate the data and the experiment count
all_pivot_exp12_count <- cbind(all_pivot_exp12,experimentCount)

#save the dataframe
write.csv(all_pivot_exp12_count,'data/all_pivot_count_exp12.csv')


#######Prep the data for Exp 3

subset <- read_csv("SubsetList.csv")

Exp3Subset <- all_pivot_exp12_count %>%
  filter(image %in% c(subset$image) )

#save the dataframe
write.csv(Exp3Subset,'data/all_pivot_count_subset.csv')

#######Prep the data for exp3 Quads

data <- read_csv("data/ReleaseDataQuads.csv")

all_pivot_quads <- PrepDataFn(data)

#save the dataframe
write.csv(all_pivot_quads,'data/all_pivot_count_quads.csv')

#######Prep the data for Exp 4 subset (NCE)

subsetNCE <- read_csv("SubsetListNCE.csv")

Exp4Subset <- all_pivot_exp12_count %>%
  filter(image %in% c(subsetNCE$image) )

#save the dataframe
write.csv(Exp4Subset,'data/all_pivot_count_subset_NCE.csv')

#######Prep the data for Exp 4 subset (NCE), nonexpert

data <- read_csv("data/ReleaseDataNCE.csv")

all_pivot_NCE <- PrepDataFn(data)

#save the dataframe
write.csv(all_pivot_NCE,'data/all_pivot_count_NCE.csv')
