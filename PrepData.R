#Coastal NOAA post-storm prep data for IRR analysis
#EBG 2/2021

#get the libraries
library(tidyverse)
library(stringr)

#import the data (eventually replace w/ Zenodo URL)
data <- read_csv("data/ReleaseData3.csv")

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

###### Separate and Pivot the impact and terrain type for each category into dataframes

#define a fn
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

#Impact
swash <- SepPivotI(swash,"swash")
collision <- SepPivotI(collision,"collision")
overwash <- SepPivotI(overwash,"overwash")
inundation <- SepPivotI(inundation,"inundation")

#A bespoke solution for 'noImpact' dataframe
noImpact <-data %>%
  select(userId:image, impactType) %>%
  add_column(question = "No_Impact") %>%
  mutate(No_Impact = str_detect(impactType, "NaN")) %>%
  select (-c(impactType)) %>%
  mutate_all(funs(str_replace(., "TRUE", "1"))) %>%
  mutate_all(funs(str_replace(., "FALSE", "0"))) %>%
  pivot_wider(names_from = userId, values_from = No_Impact)


#define a fn for terrain
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

#Terrain; don;t need water since we have already all water above
sandy <- SepPivotT(sandyCoastline,"sandyCoastline")
marsh <- SepPivotT(marsh,"marsh")
inland <- SepPivotT(inland,"inland")
river <- SepPivotT(river,"river")

#stack all dataframes:
all_pivot <- rbind(development,
                   washover,
                   damage,
                   swash, collision, overwash, inundation, noImpact, 
                   water, sandy, river, marsh, inland)

#Add in the experiment number

experimentCount <- rep(
  c(rep(c(1),times=900), 
    rep(c(2),times=600)),times = 13)


#concatenate the data and the experiment count
all_pivot_count <- cbind(all_pivot,experimentCount)

#save the dataframe
write.csv(all_pivot_count,'all_pivot_count_v3.csv')