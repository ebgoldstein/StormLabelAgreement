#CHIIP IRR analysis
#EBG 2/2021

#get the libraries
library(irr)
library(tidyverse)
library(stringr)

#import the data (eventually replace w/ Zenodo URL)
CHIIPdata <- read_csv("ReleaseData2.csv")

###### Split and Pivot the data into individual dataframes

water <- CHIIPdata %>%
  select(userId:allWater) %>%
  add_column(question = "allWater") %>%
  mutate_all(funs(str_replace(., "TRUE", "1"))) %>%
  mutate_all(funs(str_replace(., "FALSE", "0"))) %>%
  pivot_wider(names_from = userId, values_from = allWater)

development <- CHIIPdata %>%
  select(userId:image, devType) %>%
  add_column(question = "devType") %>%
  mutate_all(funs(str_replace(., "undeveloped", "0"))) %>%
  mutate_all(funs(str_replace(., "developed", "1"))) %>%
  pivot_wider(names_from = userId, values_from = devType)

washover <- CHIIPdata %>%
  select(userId:image, washoverType) %>%
  add_column(question = "washType") %>%
  mutate_all(funs(str_replace(., "noWashover", "0"))) %>%
  mutate_all(funs(str_replace(., "washover", "1"))) %>%
  pivot_wider(names_from = userId, values_from = washoverType)

damage <- CHIIPdata %>%
  select(userId:image, dmgType) %>%
  add_column(question = "dmgType") %>%
  mutate_all(funs(str_replace(., "noDamage", "0"))) %>%
  mutate_all(funs(str_replace(., "damage", "1"))) %>%
  pivot_wider(names_from = userId, values_from = dmgType)

###### Separate and Pivot the impact and terrain type for each category into dataframes

#define a fn
SepPivotI <- function(key, value) {
  CHIIPdata %>%
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
noImpact <-CHIIPdata %>%
  select(userId:image, impactType) %>%
  add_column(question = "No_Impact") %>%
  mutate(No_Impact = str_detect(impactType, "NaN")) %>%
  select (-c(impactType)) %>%
  mutate_all(funs(str_replace(., "TRUE", "1"))) %>%
  mutate_all(funs(str_replace(., "FALSE", "0"))) %>%
  pivot_wider(names_from = userId, values_from = No_Impact)


#define a fn for terrain
SepPivotT <- function(key, value) {
  CHIIPdata %>%
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
  c(rep(c(1),times=100),
    rep(c(2),times=100),
    rep(c(3),times=100),
    rep(c(4),times=100),
    rep(c(5),times=100),
    rep(c(6),times=100),
    rep(c(7),times=100),
    rep(c(8),times=100),
    rep(c(9),times=100)), times = 13)

#concatenate the data and the experiment count
all_pivot_count <- cbind(all_pivot,experimentCount)

####### IRR calculation

#find unique number of images
All_Images <- length(unique(unlist(all_pivot_count[c("image")])))

#make a dataframe to hold the results
IRR_results <- data.frame(matrix(ncol = 2, nrow = (nrow(all_pivot_count)/All_Images)))
colnames(IRR_results) <- c("Question", "Krippendorf_alpha")

#Loop through data, each category at a time. Record the question, and calculcate IRR stat.
#Then add output to a new table

for ( i in seq(from=1, to=(nrow(all_pivot_count)-99), by=All_Images))
{
  #Table counter
  j = (i-1+All_Images)/All_Images
  working_subset <- all_pivot_count[i:(i+(All_Images-1)),]
  IRR_results[j,1] <- working_subset[1, 3]
  
  subsetDF <- working_subset %>%
    select("1":"13") 
  
  subsetMat <- data.matrix(subsetDF)
  
  subsetMat <- t(subsetMat)
  
  Alphalist <- kripp.alpha(subsetMat)
  
  IRR_results[j,2] <- Alphalist$value
  
}

#table 1
IRR_results


