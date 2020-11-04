source("Election Simulation Functions.R")
#probabilities of a biden win 
#obtained from https://www.actionnetwork.com/politics/election-odds-predictions-swing-states-florida-georgia-ohio-pennsylvania
# and https://www.oddsshark.com/politics/2020-usa-presidential-odds-futures

#these are the probabilities as of 3pm on Wednesday, 11/4

#if you see a probability on a site that is given in odds (American System), use the function, convert_odds(), 
#if any of these states are won, change the probability to 1
pa <- .587
ga <- .28
mi <- .85
wi <- 1 #won this state
az <- .75 #AP has called this race as a biden win, but stayed conservative

#electoral votes associated with each state
pa_elec <- 20
ga_elec <- 16 
mi_elec <- 16
wi_elec <- 10
az_elec <- 11

biden_elec <- 238 #assumes Nevada Win (no odds were available)
trump_elec <- 228 #assumees NC win (really likely)

state_names <- c("pa", "ga", "mi", "wi", "az")
swing_odds <- c(pa, ga, mi, wi, az)
names(swing_odds) <- state_names

swing_elec <- c(pa_elec, ga_elec, mi_elec, wi_elec, az_elec)
names(swing_elec) <- state_names


chances <- game_election(40000) %>%  #see functions script for details
  group_by(Winner) %>% 
  summarise(n = n()) %>%  
  ungroup() %>%  
  mutate(win_percent = (n/sum(n))*100)


color_scheme <- c("Trump Wins" = "red", "Biden Wins" =  "blue", "Tie" = "green")
ggplot(chances)+
  geom_col(aes(Winner, win_percent, fill = Winner), color = "black")+
  geom_text(aes(Winner, win_percent+5, label = paste0(win_percent, "%")))+
  scale_fill_manual(values =color_scheme)+ 
  theme(legend.position = "none")+
  theme_bw()+
  labs(x = "Winner", y = "Percent Chance of Winning")


  
