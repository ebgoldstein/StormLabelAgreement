##############
#Move Coastal Images
# You can ignore this if you don't want to look at the images.
# you need to run the CoastalIRR.R script first, so that you have the correct csv saved
#EBG 2/2021

#get the libraries
library(tidyverse)
library(stringr)

#load the pivotd data
all_pivot_count <- read_csv("all_pivot_count.csv")


########
#Move images to folders based on counts for both washover deposit and overwash votes.
#this is for manual, visual inspection..

# Isolate experiment 1, washtype, and overwash
Exp123 <- all_pivot_count %>%
  filter(experimentCount < 4) %>%
  filter(question == "overwash" | question == "washType") %>%
  select("NOAA_flight":"7") %>%
  mutate(`1` = as.numeric(`1`),`2` = as.numeric(`2`),`3` = as.numeric(`3`),
         `4` = as.numeric(`4`),`5` = as.numeric(`5`),`6` = as.numeric(`6`),
         `7` = as.numeric(`7`))

#Counts for each
Exp123$OWcount <- rowSums(Exp1[,4:10])

#MAKE FOLDERS
#washovr deposit
dir.create("washType")
dir.create("washType/0")
dir.create("washType/1")
dir.create("washType/2")
dir.create("washType/3")
dir.create("washType/4")
dir.create("washType/5")
dir.create("washType/6")
dir.create("washType/7")

#Overwash impact
dir.create("overwash")
dir.create("overwash/0")
dir.create("overwash/1")
dir.create("overwash/2")
dir.create("overwash/3")
dir.create("overwash/4")
dir.create("overwash/5")
dir.create("overwash/6")
dir.create("overwash/7")

#Located the directory of teh images, whichc an be downlaoded with psi-collect or
#by clicking the links in storms.noaa.gov.
# regardless, you will need:
#Florence — 20180917a_jpgs
#Michael - 20181011a_jpgs
#isaias - 20200804a_jpgs
# i put these three directories in a single folder 'IRRpics'... e.g. IRRpics/20180917a_jpgs
psi_loc = "/Volumes/Passport/IRRpics/"

#Add correct directories as a column to the dataframe
#add destination directory and source directory and file
Exp123dest <- Exp123 %>%
  mutate(dest =  str_c(question,"/", as.character(OWcount))) %>%
  mutate_all(~gsub("Florence_|Isaias_|Michael_", "", .)) %>% #remove the storm name from flight directory name
  mutate(src =  str_c(psi_loc, NOAA_flight,"/jpgs/", as.character(image))) 


filelist <- Exp123dest$src
destlist <- file.path(Exp123dest$dest, Exp123dest$image)

#MOVE IMAGES
#cycle through dataframe and move files
file.copy(filelist, destlist)
