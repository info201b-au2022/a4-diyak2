library(tidyverse)
library(dplyr)

# The functions might be useful for A4
source("~/info201/assignments/a4-diyak2/source/a4-helpers.R")

## Section 2

# The function gets the highest jail population in all the states and then arranges 
# it in descending order to get the top 5 states that have the highest population
# and the bottom 5 that have the lowest total population count 
incarceration_df <- get_data()

highest_jail_pop_in__states <- function() {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    filter(p > 0) %>%
    select(state, p) 
  return(t)  
}
high_pop <- highest_jail_pop_in__states() %>%
    arrange(desc(p))

state_top_5_pop <- head(high_pop, 5)

state_bottom_5_pop <- tail(high_pop, 5)

# Answers the next question about states and their Black population
# Finds the state in the US with the highest Black population in jail 
# Finds the top 5 states with the highest Black population and bottom 5 states with 
# the lowest Black population in jail 

all_state_highest_black_pop <- incarceration_df %>%
  group_by(state) %>%
  summarise(p = sum(black_pop_15to64, na.rm = TRUE)) %>%
  filter(p > 0) %>%
  select(state, p) 

state_highest_black_pop <- head(all_state_highest_black_pop, 1)
state_top_5_black_pop <- head(all_state_highest_black_pop, 5)
state_bottom_5_black_pop <- tail(all_state_highest_black_pop, 5)

print("The top 5 states with highest pop")
print(state_top_5_pop)

print("The bottom 5 states with lowest pop")
print(state_bottom_5_pop)

print("The state in the US with the highest Black population")
print(all_state_highest_black_pop)

print("The top 5 states with highest Black pop")
print(state_top_5_black_pop)

print("The bottom 5 states with lowest Black pop")
print(state_bottom_5_black_pop)
  
## Section 3
# Growth of the U.S. Prison Population
# This function gets the total jail population for years 1970-2018  
 get_year_jail_pop <- function() {
  t <- incarceration_df %>%
    group_by(year) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(year, p) 
  return(t)  
}
#print("Jail population for every year")
#View(get_year_jail_pop())


# This function returns the bar chart that plots the increase in the total jail population 
 # in the US over the years 1970-2018
library(ggplot2)

plot_jail_pop_for_us <- function()  {
  jail_pop_for_us <- get_year_jail_pop()
  jail_chart <- ggplot(jail_pop_for_us, aes(x=year, y=p)) +
    ggtitle("Increase of Jail Population in U.S. (1970-2018)") +
    xlab("Years") + ylab("Total Jail Population") +
    geom_bar(stat = "identity") + scale_y_continuous(labels = scales::comma) +
    labs(caption = "This chart shows the trend in the US jail population over the years")

  print(jail_chart)
}

plot_jail_pop_for_us()

## Section 4  
# Growth of Prison Population by State 
#  This function shows the growth of jail population over the years by a specific 
# state or states that are passed in the function 

get_jail_pop_by_states <- function (states) {
  states_data <- incarceration_df[incarceration_df$state %in% states,]
  t <- states_data %>%
    group_by(state, year) %>%
    summarise(p = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(state, year, p) 
  return(t)
}

#View(get_jail_pop_by_states((c("WA", "OR", "CA"))))

# This plots three lines that show the growth in jail population for each of the 
# the three states given and shows that over the time frame from 1970-2018 for each
library(ggplot2)
plot_jail_pop_by_states  <- function (states) {
  jail_pop_for_states <- get_jail_pop_by_states(states)
  state_line_chart <- jail_pop_for_states %>%
    ggplot( aes(x=year, y=p, group=state, color=state)) +
    ggtitle("Jail Population in U.S. (1970-2018) for States") +
    geom_line() +
    xlab("Years") + ylab("Total Jail Population") +
    labs(caption = "This chart shows the trend in the US jail population over the years for given states")
  dev.off()
  print(state_line_chart)
}
plot_jail_pop_by_states(c("WA", "OR", "CA"))

## Section 5

# Comparison of Jail Population by Race 

# This function is intended to compare the total White population in jail to the total Black population
#in jail in order to reveal how the totals have changes over the time frame of 1970-2018, and see
# what population has had the most significant change and when

#These functions get the total White and Black population from 1970-2018 in jail
# in order to compare them 

get_white_jail_pop <- function() {
  t <- incarceration_df %>%
    group_by(year) %>%
    summarise(p = sum(white_pop_15to64, na.rm = TRUE)) %>%
    select(year, p) 
  return(t)  
}

white_jail_pop <- get_white_jail_pop()
colnames(white_jail_pop) <- c('year','White_Pop')  

get_black_jail_pop <- function() {
  t <- incarceration_df %>%
    group_by(year) %>%
    summarise(p = sum(black_pop_15to64, na.rm = TRUE)) %>%
    select(year, p) 
  return(t)  
}

black_jail_pop <- get_black_jail_pop()
colnames(black_jail_pop) <- c('year','Black_Pop')  

# This is too see this data in one dataframe in order to plot it 
jail_population <- merge(x = white_jail_pop, y = black_jail_pop, by = "year")
#view(jail_population)

#Shows the trend in the white population from 1970-2018 in jail compared 
# to the Black population in jail from 1970-2018

dev.off()
jail_pop_chart <- ggplot(jail_population, aes(year)) + 
  ggtitle("Jail Population Title") +
  geom_line(aes(y = White_Pop, colour = "White_Pop")) + 
  geom_line(aes(y = Black_Pop, colour = "Black_Pop")) + 
  scale_y_continuous(name="Population Count", labels = scales::comma) +
  labs(caption = "This chart shows the trend in ...") +
  guides(color = guide_legend(title = "Populations"))

print(jail_pop_chart)

## Section 6
#Map of the Total Jail Population in the US by State
# This function is intended to get the states across the US and 
# show the total jail population from 1970-2018

# This function gets the jail population by every state 
us_states_jail_pop <- function(region) {
  t <- incarceration_df %>%
    group_by(state) %>%
    summarise(Population = sum(total_jail_pop, na.rm = TRUE)) %>%
    select(state, Population) 
  return(t)  
}

diff_regions_populations <- us_states_jail_pop()

View(diff_regions_populations)

# Map shows the the comparison of the jail population throughout every state 
dev.off()

library(sf)
library(leaflet)
library(scales)
library(ggmap)
library(usmap)

# Makes the map showing the distribution across states 
us_map <- plot_usmap(data = diff_regions_populations, values = "Population", labels = TRUE, label_color = "white")

us_map <- us_map +
          ggtitle(label = "Jail Population Midwest vs. West") +
          labs(caption = "This chart shows the trend in the jail population over the years in the Midwest and the West of the US") +
          scale_fill_continuous(low = "white", high ="darkblue", 
                        name = "Population",label = scales::comma,
                        limits = c(0,3000000)) +
          theme(legend.position = "right")

print(us_map)

