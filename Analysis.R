#Data analysis for coastal IRR work
# you need to run the CoastalIRR.R script first, 
# so that you have the correct csv saved
##EBG 2/2021

#get the libraries
library(tidyverse)


#load the pivoted data
all_pivot_count <- read_csv("all_pivot_count.csv")

#remove the first column
all_pivot_count <- all_pivot_count %>%
  select(-c(X1))

######
#Washover and Overwash bar charts

# Isolate experiment 1
Exp1 <- all_pivot_count %>%
  filter(experimentCount == 1) %>%
  select("NOAA_flight":"7") %>%
  mutate(`1` = as.numeric(`1`),`2` = as.numeric(`2`),`3` = as.numeric(`3`),
         `4` = as.numeric(`4`),`5` = as.numeric(`5`),`6` = as.numeric(`6`),
         `7` = as.numeric(`7`))

#Counts for each
Exp1$VoteCount <- rowSums(Exp1[,4:10])

#Bar chart facet for all Qs
ggplot(Exp1,
       aes(VoteCount)) + geom_bar() + facet_wrap(~question)

###JUST OVERWASH AND WASHOVER
#Bar chart facet for OW and washover dep.
ggplot(Exp1 %>%
         filter(question == "overwash" | question == "washType"),
       aes(VoteCount)) + geom_bar() + facet_wrap(~question)


#Bar chart dodge for OW and washover dep.
ggplot(Exp1 %>%
         filter(question == "overwash" | question == "washType"), 
       aes(VoteCount)) + geom_bar(aes(fill = question),position = "dodge") 

#####################

#Pivot to just look at counts for all the parameters
Exp1Votes <- Exp1 %>%
  select(-c(`1`,`2`,`3`,`4`,`5`,`6`,`7`)) %>%
  pivot_wider(names_from = question, values_from = VoteCount)

#development vs washover... too see if
#there is more confusion in washover(wash 1-6) when dev is present (1-7)
ggplot(Exp1Votes, aes(x = devType, y = washType)) +
  geom_jitter()

#overwash vs washover... generally correlated
ggplot(Exp1Votes, aes(x = overwash, y = washType)) +
  geom_jitter()





