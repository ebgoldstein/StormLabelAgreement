#Data analysis for coastal IRR work
# you need to run the PrepData.R script first, 
# so that you have the correct csv saved
##EBG 2/2021

#get the libraries
library(tidyverse)


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

####### Question bar charts <- Figure 3

# Isolate experiment 1
Exp1 <- all_pivot_count %>%
  filter(experimentCount == 1) %>%
  select("NOAA_flight":"7") %>%
  mutate(`1` = as.numeric(`1`),`2` = as.numeric(`2`),`3` = as.numeric(`3`),
         `4` = as.numeric(`4`),`5` = as.numeric(`5`),`6` = as.numeric(`6`),
         `7` = as.numeric(`7`))

#And only the big exp:
FullyCrossed <- rep(
  c(rep(c(1),times=300), 
    rep(c(0),times=600)),times = 13)

#concatenate the data and the experiment count
Exp1 <- cbind(Exp1All,FullyCrossed) %>%
  filter(FullyCrossed == 1)


#Counts for each
Exp1$VoteCount <- rowSums(Exp1[,4:10])

#remove estuary, which was not tagged in exp 1, and replace label name
Exp1 <- Exp1 %>%
  filter(question != "estuary") %>%
  mutate(question = replace(question, question == "devType", "Buildings?")) %>%
  mutate(question = replace(question, question == "allWater", "All Water?")) %>%
  mutate(question = replace(question, question == "collision", "Collision?")) %>%
  mutate(question = replace(question, question == "dmgType", "Damage?")) %>%
  mutate(question = replace(question, question == "inland", "Inland?")) %>%
  mutate(question = replace(question, question == "inundation", "Inundation?")) %>%
  mutate(question = replace(question, question == "marsh", "Marsh?")) %>%
  mutate(question = replace(question, question == "No_Impact", "No Impact?")) %>%
  mutate(question = replace(question, question == "overwash", "Overwash?")) %>%
  mutate(question = replace(question, question == "river", "River?")) %>%
  mutate(question = replace(question, question == "sandyCoastline", "Sandy Coastline?")) %>%
  mutate(question = replace(question, question == "swash", "Swash?"))  %>%
  mutate(question = replace(question, question == "washType", "Washover?"))


Exp1$question <- factor(Exp1$question, levels=c("Buildings?","Damage?","Washover?","No Impact?",
                                                "Swash?","Collision?","Overwash?","Inundation?",
                                                "Sandy Coastline?","Marsh?","Inland?","River?", "All Water?"))


#Bar chart facet for all Qs
ggplot(Exp1,
       aes(VoteCount)) + geom_bar() + facet_wrap(~question)

#look at counts of votes
TableOfCounts <- Exp1 %>% 
  group_by(question, VoteCount) %>% 
  summarise(n = n())


#################### Disagreement decay chart... not used

#sequentially look at disagreement
Exp1$add1 <- ifelse(Exp1$`1` == Exp1$`2`, 0, 1)
Exp1$add2 <- ifelse(Exp1$`1` == Exp1$`2` &  
                      Exp1$`2`== Exp1$`3`, 0, 1)
Exp1$add3 <- ifelse(Exp1$`1` == Exp1$`2` &  
                      Exp1$`2`== Exp1$`3` &  
                      Exp1$`3`== Exp1$`4`, 0, 1)
Exp1$add4 <- ifelse(Exp1$`1` == Exp1$`2` &  
                      Exp1$`2`== Exp1$`3` &  
                      Exp1$`3`== Exp1$`4` &  
                      Exp1$`4`== Exp1$`5`, 0, 1)

Exp1$add5 <- ifelse(Exp1$`1` == Exp1$`2` &  
                      Exp1$`2`== Exp1$`3` &  
                      Exp1$`3`== Exp1$`4` &  
                      Exp1$`4`== Exp1$`5` &  
                      Exp1$`5`== Exp1$`6`, 0, 1)

Exp1$add6 <- ifelse(Exp1$`1` == Exp1$`2` &  
                      Exp1$`2`== Exp1$`3` &  
                      Exp1$`3`== Exp1$`4` &  
                      Exp1$`4`== Exp1$`5` &  
                      Exp1$`5`== Exp1$`6` &  
                      Exp1$`6`== Exp1$`7`, 0, 1)


#group by Q and flight, sum by column

DISS <- Exp1 %>% 
  group_by(NOAA_flight) %>%
  group_by(question) %>%
  summarise(across(add1:add6, sum)) %>%
  pivot_longer(!question, names_to = "Added", values_to = "count")

ggplot(DISS, aes(x = Added, y = count, color = question)) +
  ylim(0, 300) +
  geom_line(aes(group = question)) +
  geom_point()


