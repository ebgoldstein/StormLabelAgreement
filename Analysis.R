#Data analysis for coastal IRR work
# you ned to run the CoastalIRR.R script first, 
# so that you have the correct csv saved
##EBG 2/2021

#get the libraries
library(tidyverse)


#load the pivotd data
all_pivot_count <- read_csv("all_pivot_count.csv")

######
#Washover and Overwash bar charts

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

#Bar chart facet
ggplot(Exp123, aes(OWcount)) + geom_bar() + facet_wrap(~question)

#Bar chart dodge
#ggplot(Exp123, aes(OWcount)) + geom_bar(aes(fill = question),position = "dodge") 

