#Coastal NOAA post-storm IRR analysis
#EBG started 2/2021; revised through 5/2012

#get the libraries
library(irr)
library(tidyverse)
library(stringr)



# this is the FN to do the IRR calculations:
ImageIRR <- function(data,maxLabeler) {
  
  ####### IRR calculation
  
  #find unique number of images
  All_Images <- length(unique(unlist(data[c("image")])))
  
  #make a dataframe to hold the results
  IRR_results <- data.frame(matrix(ncol = 2, nrow = (nrow(data)/All_Images)))
  colnames(IRR_results) <- c("Question", "Krippendorf_alpha")
  
  #Loop through data, each category at a time. Record the question, and calculcate IRR stat.
  #Then add output to a new table
  
  for ( i in seq(from=1, to=(nrow(data)-99), by=All_Images))
  {
    #Table counter
    j = (i-1+All_Images)/All_Images
    working_subset <- data[i:(i+(All_Images-1)),]
    IRR_results[j,1] <- working_subset[1, 3]
    
    subsetDF <- working_subset %>%
      select("1":maxLabeler) 
    
    subsetMat <- data.matrix(subsetDF)
    
    subsetMat <- t(subsetMat)
    
    Alphalist <- kripp.alpha(subsetMat)
    
    IRR_results[j,2] <- Alphalist$value
    
  }
  
  #table 1
  IRR_results
  
}


####NOW LOAD THE DATA AND RUN FOR THE EXPERIMENTS

#load the pivoted data
all_pivot_count <- read_csv("data/all_pivot_count_exp12.csv", 
                            col_types = cols(`14` = col_double(), 
                                             `15` = col_double(), 
                                             `16` = col_double(), 
                                             `18` = col_double(), 
                                             `19` = col_double(),
                                             `20` = col_double()))


#remove the first column
all_pivot_count <- all_pivot_count %>%
  select(-c(X1)) 


####EXP1
#subset for Exp1
Exp1data <- all_pivot_count %>%
  filter(experimentCount == 1)

#run the fn for exp1
IRR_results1 <- ImageIRR(Exp1data,"20")
write.csv(IRR_results1,'data/IRRresults_EXP1.csv')

####EXP2
#subset for Exp2
Exp2data <- all_pivot_count %>%
  filter(experimentCount == 2)

#run the fn for exp2
IRR_results2 <- ImageIRR(Exp2data,"20")
write.csv(IRR_results2,'data/IRRresults_EXP2.csv')

####EXP3
#load the pivoted quad data
QuadData <- read_csv("data/all_pivot_count_quads.csv")

#remove the first column
QuadData <- QuadData %>%
  select(-c(X1)) 

#run the fn for Quads
IRR_results3 <- ImageIRR(QuadData,"4")
write.csv(IRR_results3,'data/IRRresults_EXP3.csv')


