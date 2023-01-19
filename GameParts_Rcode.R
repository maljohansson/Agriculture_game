# Agriculture game 
# Script by Malin Johansson 

# the R code (not shiny app)

# last modified Jan 18, 2023

# Delete all saved objects
#rm(list=ls())

# install packages before loading libraries

# libraries
library(truncnorm) # to  draw from the truncated normal distribution
library(kableExtra) # for making nicer tables
library(tidyverse)

# assign treatments to player ####

##  The player will be randomly assign to win/loose the game to manipulate the perception of equity
# The next line samples two numbers to assign treatment groups
TreatmentNum <- rbinom(n = 2, 
                       size = 1, 
                       p = 0.5)

# These lines convert numbers to words for treatments
EarnTreatment <- if_else(TreatmentNum[1] == 1, "Winner", "Loser")
VarTreatment <- if_else(TreatmentNum[2] == 1, "HiVar", "LoVar")

# Generate neighbour scores
# the code below generates scores based on NeighbourNumber, which can be set/changed below
NeighbourNumber <- 9

# It also relies on Player score, which should be inherited from the player's interactions with the game. 
# I set a value of 73 below so that the code works, but you'll want to annotate this out or delete it when the full model runs. 
PlayerScore <- 73

# To generate neighbour scores, we use a normal distribution truncated to avoid going beyond lower and upper limits, 
# so let's store variables with these values for your game: Highest and lowest possible score
HighestPossScore <- 100
LowestPossScore <- 0


# generate a SD based on PlayerScore and binomial SD, but the details don't matter a lot; 
# I used this because I wanted to change how big the SD was depending on how near to the upper or lower limit they got
SDSuccess <- sqrt((PlayerScore/100) * (1 - (PlayerScore/100))/ 15) * 100

# the following generates the neighbour scores in a tibble (same as data frame); 
# note the function comes from the truncnorm library
NeighbourScores <- tibble(
  Scores = rtruncnorm(n = NeighbourNumber,
                      a = LowestPossScore,
                      b = HighestPossScore,
                      mean = if_else(EarnTreatment == "Winner", 
                                     PlayerScore - 0.8 * SDSuccess,
                                     PlayerScore + 0.8 * SDSuccess),
                      sd = if_else(VarTreatment == "HiVar",
                                   2.2 * SDSuccess,
                                   0.4 * SDSuccess)))


# Make a table with NeighbourScores and PlayerScores
ScoreTable <- tibble(.rows = NeighbourNumber) %>% 
  mutate(Text = "Neighbour") %>% # this creates a column with text reading neighbour
  mutate(NeighbourNumber = seq(1:NeighbourNumber)) %>% 
  bind_cols(., NeighbourScores) %>% 
  mutate(Name = paste(Text, NeighbourNumber)) %>% 
  select(Name, Scores) %>% 
  bind_rows(., tibble(Name = "Player", Scores = PlayerScore)) %>% 
  arrange(desc(Scores))


# Next is bare-bones code for making nice tables, but I guess there are better options in Shiny
ScoreTable %>%
  kbl() %>%
  kable_styling()

# You can try rerunning this code several times to see how it works depending on the randomly assigned treatments. 
# It generally (but not always) puts winners near or at the top and vice versa, but there's some randomness, 
# we should store the actual rank performance of players as well as their treatement for possible stats analysis

# This table should be updated for each season (but only change slightly since the player will always be a winner or looser no matter of their stratedgy)







# Number of fields  ####
# Could be adjusted to less if it is too many
NumberofFields <- 12


# pest densities
LowestPestDensity <- 0
HighestPestDensity <- 100








## Pest invasion, season 1 #### 
# generate pests for season 1
# in this table all the data will be stored and I need help with how to store it into a google cloud 
# or similar place for each player playing the game
FarmerChoices <- tibble(.rows = NumberofFields) %>% 
  mutate(FieldNumber = 1:12) %>% 
  mutate(FieldRow = rep(1:4,each = 3)) %>% # generate row numbers
  mutate(FieldCol = rep(1:3,4)) %>% # generate column numbers
  mutate(PestDensity = rtruncnorm(n = NumberofFields,
                                  a = LowestPestDensity,
                                  b = HighestPestDensity,
                                  mean = 20,
                                  sd = 8))




# define color range for pest density
col.range=c(0,40)

# Produce a plot that tells farmers the pest density
# This will be showed to the player so he or she can choose what control to use
theme_set(theme_bw())
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow, fill = PestDensity)) +
  geom_tile() +
  geom_label(aes(label = FieldNumber), 
             colour = "white") + # adds field numbers
  # scale_fill_gradient(low = "green", high = "black") +
  scale_fill_gradientn(colours=terrain.colors(2), limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# These are the options the player can choice as treatment for each square
FarmerOptions <- c("Synthetic","Bio_choice", "Bio_recomm","Nothing")

# In the real game, the farmer will provide choices based on pest densities
# But the text below generates random choices
# This needs to be done by the shiny app

# The idea is to treat the crops 2 times/season --> play 3 seasons


# Store farmer choices in a object (will be done via Shiny eventually)
FarmerChoices <- FarmerChoices %>% 
  mutate(FarmerChoice = sample(FarmerOptions, 
                               size = NumberofFields,
                               replace = TRUE,
                               prob = c(0.3, 0.3, 0.3, 0.1)))

# Now generate consequences for farmer based on some settings, which can be changed below
# efficiency of each control
SyntheticEff <- 0.2
Bio_choiceEff <- 0.3
BiorecommEff <- rtruncnorm(n = 1,
                           a = 0.3,
                           b = 0.5,
                           mean = 0.4,
                           sd = 0.06)
NothingEff <- 1


# add the pest densities after treatment to the data table
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDens_afterTreat_R1 = if_else(FarmerChoice == "Synthetic", 
                                          PestDensity * SyntheticEff, #conseq of choosing synthetic
                                          if_else(FarmerChoice == "Bio_choice", 
                                                  PestDensity * Bio_choiceEff, # conseq of choosing Bio_choice
                                                  if_else(FarmerChoice == "Bio_recomm", 
                                                          PestDensity * BiorecommEff, # conseq of choosing Bio_rec
                                                          PestDensity * NothingEff)) #conseq of nothing
  ))









# cost/subsidy due to treatment ####
# change values for each session
# these are the implications of each control
SyntheticCost <- -20 
Bio_choiceSub <- 10
Bio_recommSub <- 30
NothingSub <- 50


# which will later be added to the neighbor score table
# however, it will still not affect too much the players score since he/she 
# has already been assigned to win or to loose
FarmerChoicesCost <- FarmerChoices %>% 
  mutate(Cost = if_else(
    FarmerChoice == "Synthetic", SyntheticCost,
    if_else(FarmerChoice == "Bio_choice", Bio_choiceSub,
            if_else(FarmerChoice == "Bio_recomm",
                    Bio_recommSub,
                    NothingSub))))


## Detection of pest and treatment season 1 
# Now can plot the field again to illustrate consequences
# this can also be moved to after pest reproduction and let the player chose pest control one more time before pest reproduces
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow,   #set x and y
             fill = PestDens_afterTreat_R1)) +
  geom_tile() +
  geom_text(aes(label = FieldNumber), 
            colour = "red",
            vjust = "top") + # adds field numbers
  geom_text(aes(label = FarmerChoice), # adds farmer choice
            colour = "red",
            vjust = "bottom")+ # could annotate out or use symbols instead
  #  scale_fill_gradient(low = "green", high = "black") +
  scale_fill_gradientn(colours=terrain.colors(2), limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) #these lines get rid of axis text



# reproduction of pest ####
# varying depending on species/weather  
# could also vary within season and between season if we want
Pest_repEff <- rtruncnorm(n = 1,
                          a = 1.1,
                          b = 1.4,
                          mean = 1.15,
                          sd = 0.06)


## Reproduction of pest season 1
# pest density after reproduction and after treatment added to the data table
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDens_afterRep = PestDens_afterTreat_R1 * Pest_repEff)

# pest average
Global_PestRep_Average <-mean(FarmerChoices$PestDens_afterRep)

# adding pest to each cell as pest dispersal
# this is simbolising the movement of pest since it can spread between squares
FarmerChoices %>% 
  mutate(Local_Pest_dispersal = PestDens_afterRep *0.8 + Global_PestRep_Average * 0.2 )  


# update field after pest reproduction and movement before next season
# Now can plot the field again to illustrate consequences
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow, 
             fill = PestDens_afterRep)) +
  geom_tile() +
  geom_text(aes(label = FieldNumber), 
            colour = "red",
            vjust = "top") + # adds field numbers
  geom_text(aes(label = FarmerChoice), # adds farmer choice
            colour = "red",
            vjust = "bottom")+ # could annotate out or use symbols instead
  # scale_fill_gradient(low = "green", high = "black") +   #scale_fill_manual --> google, might need to replace
  scale_fill_gradientn(colours=terrain.colors(2),limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) #these lines get rid of axis text





# Season 2 ####
# basically the same code as before, but adding the new players option and pest density to the data table

# generate pests for season 2
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDensity2 = rtruncnorm(n = NumberofFields,
                                  a = LowestPestDensity,
                                  b = HighestPestDensity,
                                  mean = 20,
                                  sd = 8))


# Produce a plot that tells farmers the pest density for season 2
theme_set(theme_bw())
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow, fill = PestDensity2)) +
  geom_tile() +
  geom_label(aes(label = FieldNumber), 
             colour = "white") + # adds field numbers
  # scale_fill_gradient(low = "green", high = "black") +
  scale_fill_gradientn(colours=terrain.colors(2), limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# Store farmer choices in a object (will be done via Shiny eventually)
# choices for season 2
FarmerChoices <- FarmerChoices %>% 
  mutate(FarmerChoice2 = sample(FarmerOptions, 
                               size = NumberofFields,
                               replace = TRUE,
                               prob = c(0.3, 0.3, 0.3, 0.1)))

# Consequeses of pest control treatment in season 2
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDens_afterTreat_R2 = if_else(FarmerChoice == "Synthetic", 
                                          PestDensity2 * SyntheticEff, #conseq of choosing synthetic
                                          if_else(FarmerChoice == "Bio_choice", 
                                                  PestDensity2 * Bio_choiceEff, # conseq of choosing Bio_choice
                                                  if_else(FarmerChoice == "Bio_recomm", 
                                                          PestDensity2 * BiorecommEff, # conseq of choosing Bio_rec
                                                          PestDensity2 * NothingEff)) #conseq of nothing
  ))


# it should save the new farmer decisions in a new column
FarmerChoicesCost2 <- FarmerChoices %>% 
  # mutate(RandNumber = runif(nrow(FarmerChoices))) %>% 
  mutate(Cost = if_else(
    FarmerChoice == "Synthetic", SyntheticCost,
    if_else(FarmerChoice == "Bio_choice", Bio_choiceSub,
            if_else(FarmerChoice == "Bio_recomm",
                    Bio_recommSub,
                    NothingSub))))


## Detetction of pest and treatment season 2 
# Now can plot the field again to illustrate consequences
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow,   #set x and y
             fill = PestDens_afterTreat_R2)) +
  geom_tile() +
  geom_text(aes(label = FieldNumber), 
            colour = "red",
            vjust = "top") + # adds field numbers
  geom_text(aes(label = FarmerChoice), # adds farmer choice
            colour = "red",
            vjust = "bottom")+ # could annotate out or use symbols instead
  #  scale_fill_gradient(low = "green", high = "black") +
  scale_fill_gradientn(colours=terrain.colors(2), limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) #these lines get rid of axis text



# we can have this randomized for each season since weather and other things can influence
# reproduction of pest ####
Pest_repEff <- rtruncnorm(n = 1,
                          a = 1.1,
                          b = 1.4,
                          mean = 1.15,
                          sd = 0.06)

## Reproduction of pest season 2
# pest density after reproduction and after treatment 
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDens_afterRep2 = PestDens_afterTreat_R2 * Pest_repEff)

# pest average
Global_PestRep_Average2 <-mean(FarmerChoices$PestDens_afterRep2)

# adding pest to each cell as pest dispersal
FarmerChoices %>% 
  mutate(Local_Pest_dispersal2 = PestDens_afterRep2 *0.8 + Global_PestRep_Average2 * 0.2 )  


# Field output season 2 with new pest densities
# Now can plot the field again to illustrate consequences
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow, 
             fill = PestDens_afterRep2)) +
  geom_tile() +
  geom_text(aes(label = FieldNumber), 
            colour = "red",
            vjust = "top") + # adds field numbers
  geom_text(aes(label = FarmerChoice), # adds farmer choice
            colour = "red",
            vjust = "bottom")+ # could annotate out or use symbols instead
  # scale_fill_gradient(low = "green", high = "black") +   #scale_fill_manual --> google, might need to replace
  scale_fill_gradientn(colours=terrain.colors(2),limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) #these lines get rid of axis text





# Season 3 ####
# basically the same code again

# generate pests for season 3
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDensity3 = rtruncnorm(n = NumberofFields,
                                   a = LowestPestDensity,
                                   b = HighestPestDensity,
                                   mean = 20,
                                   sd = 8))


# Produce a plot that tells farmers the pest density for season 3
theme_set(theme_bw())
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow, fill = PestDensity3)) +
  geom_tile() +
  geom_label(aes(label = FieldNumber), 
             colour = "white") + # adds field numbers
  # scale_fill_gradient(low = "green", high = "black") +
  scale_fill_gradientn(colours=terrain.colors(2), limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


# Store farmer choices in a object (will be done via Shiny eventually)
# choices for season 3
FarmerChoices <- FarmerChoices %>% 
  mutate(FarmerChoice3 = sample(FarmerOptions, 
                                size = NumberofFields,
                                replace = TRUE,
                                prob = c(0.3, 0.3, 0.3, 0.1)))

# Consequeses of pest control treatment in season 3
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDens_afterTreat_R3 = if_else(FarmerChoice == "Synthetic", 
                                          PestDensity3 * SyntheticEff, #conseq of choosing synthetic
                                          if_else(FarmerChoice == "Bio_choice", 
                                                  PestDensity3 * Bio_choiceEff, # conseq of choosing Bio_choice
                                                  if_else(FarmerChoice == "Bio_recomm", 
                                                          PestDensity3 * BiorecommEff, # conseq of choosing Bio_rec
                                                          PestDensity3 * NothingEff)) #conseq of nothing
  ))



# it should save the new farmer decisions in a new column
FarmerChoicesCost3 <- FarmerChoices %>% 
  # mutate(RandNumber = runif(nrow(FarmerChoices))) %>% 
  mutate(Cost = if_else(
    FarmerChoice == "Synthetic", SyntheticCost,
    if_else(FarmerChoice == "Bio_choice", Bio_choiceSub,
            if_else(FarmerChoice == "Bio_recomm",
                    Bio_recommSub,
                    NothingSub))))


## Detetction of pest and treatment season 3
# Now can plot the field again to illustrate consequences
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow,   #set x and y
             fill = PestDens_afterTreat_R3)) +
  geom_tile() +
  geom_text(aes(label = FieldNumber), 
            colour = "red",
            vjust = "top") + # adds field numbers
  geom_text(aes(label = FarmerChoice), # adds farmer choice
            colour = "red",
            vjust = "bottom")+ # could annotate out or use symbols instead
  #  scale_fill_gradient(low = "green", high = "black") +
  scale_fill_gradientn(colours=terrain.colors(2), limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) #these lines get rid of axis text



# we can have this randomized for each season since weather and other things can influence
# reproduction of pest ####
Pest_repEff <- rtruncnorm(n = 1,
                          a = 1.1,
                          b = 1.4,
                          mean = 1.15,
                          sd = 0.06)

## Reproduction of pest season 3
# pest density after reproduction and after treatment 
FarmerChoices <- FarmerChoices %>% 
  mutate(PestDens_afterRep3 = PestDens_afterTreat_R3 * Pest_repEff)

# pest average
Global_PestRep_Average3 <-mean(FarmerChoices$PestDens_afterRep3)

# adding pest to each cell as pest dispersal
FarmerChoices %>% 
  mutate(Local_Pest_dispersal3 = PestDens_afterRep3 *0.8 + Global_PestRep_Average3 * 0.2 )  

colnames(FarmerChoices)

# Field output season 3 with new pest densities
# Now can plot the field again to illustrate consequences
FarmerChoices %>% 
  ggplot(aes(x = FieldCol, y = FieldRow, 
             fill = PestDens_afterRep3)) +
  geom_tile() +
  geom_text(aes(label = FieldNumber), 
            colour = "red",
            vjust = "top") + # adds field numbers
  geom_text(aes(label = FarmerChoice), # adds farmer choice
            colour = "red",
            vjust = "bottom")+ # could annotate out or use symbols instead
  # scale_fill_gradient(low = "green", high = "black") +   #scale_fill_manual --> google, might need to replace
  scale_fill_gradientn(colours=terrain.colors(2),limits=col.range) + #look her
  scale_y_reverse() + # puts row 1 at the top instead of the bottom
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) #these lines get rid of axis text



# just to check the categories that has been saved in the data frame
colnames(FarmerChoices)



