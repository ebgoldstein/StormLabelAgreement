#CHIIP IRR analysis
#EBG 2/2021

#get the libraries
library(irr)
library(tidyverse)
library(stringr)

#import the data (replace w/ Zenodo)
CHIIPdata <- read_csv("ReleaseData2.csv")

###### Split and Pivot the data into individual dataframes

water <- CHIIPdata %>%
  select(userId:allWater) %>%
  pivot_wider(names_from = userId, values_from = allWater)

development <- CHIIPdata %>%
  select(userId:image, devType) %>%
  pivot_wider(names_from = userId, values_from = devType)

washover <- CHIIPdata %>%
  select(userId:image, washoverType) %>%
  pivot_wider(names_from = userId, values_from = washoverType)

damage <- CHIIPdata %>%
  select(userId:image, dmgType) %>%
  pivot_wider(names_from = userId, values_from = dmgType)

###### Sep. and Pivot the impact and terrain type for each category

#define a fn
SepPivotI <- function(key, value) {
  CHIIPdata %>%
    select(userId:image, impactType) %>%
    mutate(key = str_detect(impactType, value)) %>%
    select (-c(impactType)) %>%
    pivot_wider(names_from = userId, values_from = key)
}

#Impact
noImpact <- SepPivotI(No_Impact,"NaN")
swash <- SepPivotI(swash,"swash")
collision <- SepPivotI(collision,"collision")
overwash <- SepPivotI(overwash,"overwash")
inundation <- SepPivotI(inundation,"inundation")
  
#define a fn for terrain
SepPivotT <- function(key, value) {
  CHIIPdata %>%
    select(userId:image, terrianType) %>%
    mutate(key = str_detect(terrianType, value)) %>%
    select (-c(terrianType)) %>%
    pivot_wider(names_from = userId, values_from = key)
}

#Terrain
waterT <- SepPivotT(Water,"NaN")
sandy <- SepPivotT(sandyCoastline,"sandyCoastline")
marsh <- SepPivotT(marsh,"marsh")
inland <- SepPivotT(inland,"inland")
river <- SepPivotT(river,"river")

####### IRR (stop)

#Example:

#subset data
a <- damage %>%
  select("1":"7") %>%
  slice_head(n = 300)

#calculate IRR
kappam.light(a)

