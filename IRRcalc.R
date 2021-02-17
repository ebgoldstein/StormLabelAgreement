#Coastal NOAA post-storm IRR analysis
#EBG 2/2021

#get the libraries
library(irr)
library(tidyverse)
library(stringr)

#load the pivotd data
all_pivot_count <- read_csv("all_pivot_count.csv")

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




