#Coastal NOAA post-storm IRR analysis
#EBG started 2/2021; revised through 6/2021

#get the libraries
library(irr)
library(tidyverse)
library(stringr)
library(gridExtra)


# this is the FN to do the IRR calculations:
ImageIRR <- function(data,maxLabeler) {
  
  ####### IRR calculation
  
  #find unique number of images
  All_Images <- length(unique(unlist(data[c("image")])))
  
  #make a dataframe to hold the results
  IRR_results <- data.frame(matrix(ncol = 4, nrow = (nrow(data)/All_Images)))
  colnames(IRR_results) <- c("Question", "Krippendorff's alpha", "Percent Agreement", "Percent w/ atleast 1 Yes")
  
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
    
    ####
    #get percent agreement. The NAs prevent irr::agree from working.
    #first get non- NAs per row — which is the number of raters
    raters <- rowSums(!is.na(subsetDF))

    #calculate the total number of votes in each row
    votes <- rowSums(subsetDF, na.rm=TRUE)
    
    #dataframe
    ADF <- data_frame(raters,votes)
    
    #find where votes = 0 (agree no) and NAPR==TV ==  (agree yes)
    agreeframe <- ADF %>% 
      count(raters == votes | votes == 0)
    
    IRR_results[j,3] <- 100* (agreeframe[2,2] / nrow(subsetDF))
    ####
    
    #Percent w/ 1 yes vote.
    #find where votes > 0 (atleast 1 yes)
    oneyesframe <- ADF %>% 
      count(votes > 0)
    
    IRR_results[j,4] <- 100* (oneyesframe[2,2] / nrow(subsetDF))
    ####
    
  }
  
  #table 1
  IRR_results
  
}


####NOW LOAD THE DATA AND RUN FOR THE EXPERIMENTS

################################

#load the pivoted data for exp 1 and 2
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

####EXP3subset
#load the pivoted quad data
SubsetData <- read_csv("data/all_pivot_count_subset.csv")

#remove the first column
SubsetData <- SubsetData %>%
  select(-c(X1)) 

#run the fn for subset
IRR_resultsSubset3 <- ImageIRR(SubsetData,"20")
write.csv(IRR_resultsSubset3,'data/IRRresults_EXP3_subset.csv')

####EXP4subset
#load the pivoted quad data
SubsetData4 <- read_csv("data/all_pivot_count_subset_NCE.csv")

#remove the first column
SubsetData4 <- SubsetData4 %>%
  select(-c(X1)) 

#run the fn for subset
IRR_resultsSubset4 <- ImageIRR(SubsetData4,"20")
write.csv(IRR_resultsSubset4,'data/IRRresults_EXP4_subset.csv')

################################################################
####EXP3Q
#load the pivoted quad data
QuadData <- read_csv("data/all_pivot_count_quads.csv")

#remove the first column
QuadData <- QuadData %>%
  select(-c(X1)) 

#run the fn for Quads
IRR_results3 <- ImageIRR(QuadData,"4")
write.csv(IRR_results3,'data/IRRresults_EXP3_quads.csv')

################################################################
####EXP4_NCE
#load the pivoted quad data
NCEData <- read_csv("data/all_pivot_count_NCE.csv")

#remove the first column
NCEData <- NCEData %>%
  select(-c(X1)) 

#run the fn for Quads
IRR_results4 <- ImageIRR(NCEData,"4")
write.csv(IRR_results4,'data/IRRresults_EXP4.csv')

####################################
####################################
####################################

#Plot the IRR Data as bar chart

#Add Exp # 
#Add in the experiment number and concatenate the data 
#and the experiment number
IRR_results1 <- cbind(IRR_results1,Exp =rep(c(1),times=14))
IRR_results2 <- cbind(IRR_results2,Exp = rep(c(2),times=14))
IRR_results3s <- cbind(IRR_resultsSubset3,Exp =rep(c('cs'),times=14))
IRR_results3q <- cbind(IRR_results3,Exp = rep(c('cq'),times=14))
IRR_resultsCE <- cbind(IRR_resultsSubset4,Exp = rep(c('ce'),times=14))
IRR_resultsNCE <- cbind(IRR_results4,Exp = rep(c('nce'),times=14))

###CLEAN THE DATA UP A BIT:
#add the missing categories... exp 3s did not have pics from these categories.
IRR_results3s[12,1] <- 'marsh'
IRR_results3s[13,1] <- 'estuary'
IRR_results3s[14,1] <- 'inland'

#there was no estuary category in Exp 1, but that means the IRR score is 1...not correct.
IRR_results1[14,2] <- NA

#non experty swash was slightly negative (worse than chance)... 
IRR_resultsNCE[4,2] <- NA

# no rivers in Exp 2 so instead of KA ==1 nad PA ~=0, they should be NA
IRR_results2[11,2] <- NA
IRR_results2[11,3] <- NA

# no rivers in Exp 2, Exp3s, 3q, 4, so instead of KA ==1, they should be NA
IRR_results3s[11,2] <- NA
IRR_results3q[11,2] <- NA
IRR_resultsCE[11,2] <- NA
IRR_resultsNCE[11,2] <- NA

# combine the experiments into groups.
IRR12 <- rbind(IRR_results1,IRR_results2)
IRR33 <- rbind(IRR_results3s,IRR_results3q)
IRR44 <- rbind(IRR_resultsCE,IRR_resultsNCE)

# Add the correct labels to the questions
IRR12 <- IRR12 %>%
  mutate(Question = replace(Question, Question == 'washType', 'Washover?')) %>%
  mutate(Question = replace(Question, Question == 'sandyCoastline', 'Sandy Coastline?')) %>%
  mutate(Question = replace(Question, Question == 'dmgType', 'Damage?')) %>%
  mutate(Question = replace(Question, Question == 'swash', 'Swash?')) %>%
  mutate(Question = replace(Question, Question == 'collision', 'Collision?')) %>%
  mutate(Question = replace(Question, Question == 'overwash', 'Overwash?')) %>%
  mutate(Question = replace(Question, Question == 'inundation', 'Inundation?')) %>%
  mutate(Question = replace(Question, Question == 'No_Impact', 'No Impact?')) %>%
  mutate(Question = replace(Question, Question == 'allWater', 'All Water?')) %>%
  mutate(Question = replace(Question, Question == 'devType', 'Buidings?'))%>%
  mutate(Question = replace(Question, Question == 'river', 'River?')) %>%
  mutate(Question = replace(Question, Question == 'marsh', 'Marsh?')) %>%
  mutate(Question = replace(Question, Question == 'inland', 'Inland?'))%>%
  mutate(Question = replace(Question, Question == 'estuary', 'Estuary?')) 

IRR33 <- IRR33 %>%
  mutate(Question = replace(Question, Question == 'washType', 'Washover?')) %>%
  mutate(Question = replace(Question, Question == 'sandyCoastline', 'Sandy Coastline?')) %>%
  mutate(Question = replace(Question, Question == 'dmgType', 'Damage?')) %>%
  mutate(Question = replace(Question, Question == 'swash', 'Swash?')) %>%
  mutate(Question = replace(Question, Question == 'collision', 'Collision?')) %>%
  mutate(Question = replace(Question, Question == 'overwash', 'Overwash?')) %>%
  mutate(Question = replace(Question, Question == 'inundation', 'Inundation?')) %>%
  mutate(Question = replace(Question, Question == 'No_Impact', 'No Impact?')) %>%
  mutate(Question = replace(Question, Question == 'allWater', 'All Water?')) %>%
  mutate(Question = replace(Question, Question == 'devType', 'Buidings?'))%>%
  mutate(Question = replace(Question, Question == 'river', 'River?')) %>%
  mutate(Question = replace(Question, Question == 'marsh', 'Marsh?')) %>%
  mutate(Question = replace(Question, Question == 'inland', 'Inland?'))%>%
  mutate(Question = replace(Question, Question == 'estuary', 'Estuary?')) 


IRR33$Exp <- as.character(IRR33$Exp)

IRR33  <- IRR33 %>%
  mutate(Exp = replace(Exp, Exp == 'cs', 'Subset')) %>%
  mutate(Exp = replace(Exp, Exp == 'cq', 'Quadrants')) 

IRR44 <- IRR44 %>%
  mutate(Question = replace(Question, Question == 'washType', 'Washover?')) %>%
  mutate(Question = replace(Question, Question == 'sandyCoastline', 'Sandy Coastline?')) %>%
  mutate(Question = replace(Question, Question == 'dmgType', 'Damage?')) %>%
  mutate(Question = replace(Question, Question == 'swash', 'Swash?')) %>%
  mutate(Question = replace(Question, Question == 'collision', 'Collision?')) %>%
  mutate(Question = replace(Question, Question == 'overwash', 'Overwash?')) %>%
  mutate(Question = replace(Question, Question == 'inundation', 'Inundation?')) %>%
  mutate(Question = replace(Question, Question == 'No_Impact', 'No Impact?')) %>%
  mutate(Question = replace(Question, Question == 'allWater', 'All Water?')) %>%
  mutate(Question = replace(Question, Question == 'devType', 'Buidings?'))%>%
  mutate(Question = replace(Question, Question == 'river', 'River?')) %>%
  mutate(Question = replace(Question, Question == 'marsh', 'Marsh?')) %>%
  mutate(Question = replace(Question, Question == 'inland', 'Inland?'))%>%
  mutate(Question = replace(Question, Question == 'estuary', 'Estuary?')) 


IRR44$Exp <- as.character(IRR44$Exp)

IRR44  <- IRR44 %>%
  mutate(Exp = replace(Exp, Exp == 'ce', 'Expert')) %>%
  mutate(Exp = replace(Exp, Exp == 'nce', 'Non Expert')) 

# set the Q order via levels.
irrLVLS <- c('Buidings?', 'Damage?', 'Washover?', 'No Impact?', 
             'Swash?', 'Collision?', 'Overwash?', 'Inundation?', 
             'All Water?', 'Sandy Coastline?', 'Marsh?', 'River?',
             'Estuary?', 'Inland?')

IRR12$Question <- factor(IRR12$Question,levels = rev(irrLVLS))
IRR33$Question <- factor(IRR33$Question,levels = rev(irrLVLS))
IRR44$Question <- factor(IRR44$Question,levels = rev(irrLVLS))

#blue is subset, red is Quads
IRR33$Exp <- factor(IRR33$Exp,levels = c( 'Quadrants','Subset'))

#blue is CE, red is NCE
#IRR44$Exp <- factor(IRR44$Exp,levels = c( 'Quadrants','Subset'))

######### PLOT exp12
PivotIRR12 <- IRR12 %>%
  pivot_longer(cols = `Krippendorff's alpha`:`Percent Agreement`, 
               names_to = "Statval", values_to = "Value")

ggplot(data=PivotIRR12, aes(x=Question,y=Value,fill=factor(Exp, levels = c(2,1)))) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() + 
  facet_grid(~Statval, scales = "free") +
  labs(fill = "Experiment") + 
  guides(fill = guide_legend(reverse = TRUE)) 

######### PLOT exp3

PivotIRR33 <- IRR33 %>%
  pivot_longer(cols = `Krippendorff's alpha`:`Percent Agreement`, 
               names_to = "Statval", values_to = "Value")

ggplot(data=PivotIRR33, aes(x=Question,y=Value,fill=factor(Exp))) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() + 
  facet_grid(~Statval, scales = "free") +
  labs(fill = "Experiment") + 
  guides(fill = guide_legend(reverse = TRUE)) 

######### PLOT exp4

PivotIRR44 <- IRR44 %>%
  pivot_longer(cols = `Krippendorff's alpha`:`Percent Agreement`, 
               names_to = "Statval", values_to = "Value")

ggplot(data=PivotIRR44, aes(x=Question,y=Value,fill=factor(Exp))) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() + 
  facet_grid(~Statval, scales = "free") +
  labs(fill = "Experiment") + 
  guides(fill = guide_legend(reverse = TRUE)) 



