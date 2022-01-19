# Adam Kippenhan ajk8sb 04/30/2021 finalPlots.R
# Code for DS3003 Final Project

library(plotly)
library(tidyverse)

# read in the OECD data on time spent working per country, remove duplicate employment type data for applicable countries and select data concerning country name, year and working time only
workData = read.csv("/Users/adam/Documents/School Files/University of Virginia/Second Year/Spring Semester/DS3003/finalProject/ANHRS_21042021234045409.csv") %>% distinct(`Country`, `Time`, .keep_all=TRUE) %>% 
                                                                                                                                                                 select(2,8,15) %>% 
                                                                                                                                                                 filter(`Country` != "Turkey") %>% 
                                                                                                                                                                 rename(`AvgHrsWorked`=`Value`)
# adjust country names to match in both datasets
workData$Country[workData$Country=="Korea"] <- "South Korea"
workData$Country[workData$Country=="Russian Federation"] <- "Russia"
workData$Country[workData$Country=="Slovak Republic"] <- "Slovakia"


# make workData plot here

# make a dataframe for each year in workData
workData %>% filter(`Time`==2015) -> workData2015
workData %>% filter(`Time`==2016) -> workData2016
workData %>% filter(`Time`==2017) -> workData2017
workData %>% filter(`Time`==2018) -> workData2018
workData %>% filter(`Time`==2019) -> workData2019

# read in World Happiness Report data from 2015 to 2019, filter for countries in the workData dataset and select columns with relevant data
happinessData2015 = read.csv("/Users/adam/Documents/School Files/University of Virginia/Second Year/Spring Semester/DS3003/finalProject/2015.csv") %>% filter(`Country` %in% workData$Country) %>% select(1,2,3,4,8,9,11)
happinessData2015$Time=2015 # add a time column to specify the year of the data for animation purposes later on

happinessData2016 = read.csv("/Users/adam/Documents/School Files/University of Virginia/Second Year/Spring Semester/DS3003/finalProject/2016.csv") %>% filter(`Country` %in% workData$Country) %>% select(1,2,3,4,9,10,12)
happinessData2016$Time=2016 # add a time column to specify the year of the data for animation purposes later on

happinessData2017 = read.csv("/Users/adam/Documents/School Files/University of Virginia/Second Year/Spring Semester/DS3003/finalProject/2017.csv") %>% filter(`Country` %in% workData$Country) 
happinessData2017$Time=2017 # add a time column to specify the year of the data for animation purposes later on

# create a regions dataset to allow for regions in happiness data for 2017 to 2019
regions = happinessData2016 %>% select(1,2)
happinessData2017 <- inner_join(regions, happinessData2017, by='Country') 
happinessData2017 %>% select(1,2,3,4,9,10,11,14) -> happinessData2017

happinessData2018 = read.csv("/Users/adam/Documents/School Files/University of Virginia/Second Year/Spring Semester/DS3003/finalProject/2018.csv") %>% filter(`Country.or.region` %in% workData$Country) %>%
                                                                                                                                                       rename(`Happiness.Rank`=`Overall.rank`,
                                                                                                                                                              `Country`=`Country.or.region`, 
                                                                                                                                                              `Happiness.Score`=`Score`, 
                                                                                                                                                              `Health..Life.Expectancy.`=`Healthy.life.expectancy`,
                                                                                                                                                              `Freedom`=`Freedom.to.make.life.choices`)
happinessData2018$Time=2018 # add a time column to specify the year of the data for animation purposes later on
happinessData2018 <- inner_join(regions, happinessData2018, by='Country') 
happinessData2018 %>% select(1,2,3,4,7,8,9,11) -> happinessData2018

happinessData2019 = read.csv("/Users/adam/Documents/School Files/University of Virginia/Second Year/Spring Semester/DS3003/finalProject/2019.csv") %>% filter(`Country.or.region` %in% workData$Country) %>%
                                                                                                                                                       rename(`Happiness.Rank`=`Overall.rank`,
                                                                                                                                                              `Country`=`Country.or.region`, 
                                                                                                                                                              `Happiness.Score`=`Score`, 
                                                                                                                                                              `Health..Life.Expectancy.`=`Healthy.life.expectancy`,
                                                                                                                                                              `Freedom`=`Freedom.to.make.life.choices`)
happinessData2019$Time=2019 # add a time column to specify the year of the data for animation purposes later on
happinessData2019 <- inner_join(regions, happinessData2019, by='Country') 
happinessData2019 %>% select(1,2,3,4,7,8,9,11) -> happinessData2019

# join the work and happiness data together for each year by Country
workAndHappinessData2015 <- inner_join(workData2015, happinessData2015, by="Country") %>% select(-2) %>% rename(`Time`=`Time.y`)
workAndHappinessData2016 <- inner_join(workData2016, happinessData2016, by="Country") %>% select(-2) %>% rename(`Time`=`Time.y`)
workAndHappinessData2017 <- inner_join(workData2017, happinessData2017, by="Country") %>% select(-2) %>% rename(`Time`=`Time.y`)
workAndHappinessData2018 <- inner_join(workData2018, happinessData2018, by="Country") %>% select(-2) %>% rename(`Time`=`Time.y`)
workAndHappinessData2019 <- inner_join(workData2019, happinessData2019, by="Country") %>% select(-2) %>% rename(`Time`=`Time.y`)

# bind work and happiness datasets for each year into one dataset and move AvgHrsWorked column to position 3 using dplyr::relocate
workAndHappinessData <- bind_rows(workAndHappinessData2015, workAndHappinessData2016, workAndHappinessData2017, workAndHappinessData2018, workAndHappinessData2019) %>% relocate(`AvgHrsWorked`, .after=3)

scoreVSWorkingPlot <- workAndHappinessData %>%
  plot_ly (
    x = ~`Happiness.Score`, # select x-axis data
    y = ~`AvgHrsWorked`, # select y-axis data
    size = ~`Happiness.Rank`*-1, # size each point in the plot inversely by happiness rank,
    marker = ~list(sizeref=.03), # adjust relative sizes of each point in the plot
    color = ~`Region`, # color each point by the region corresponding to the country it represents
    frame = ~`Time`,# animate plot by year from 2015 to 2019
    text = ~paste(Country,), # allow each country to be identified when hovering over each point
    hoverinfo = "text", 
    type = 'scatter', # specify the type of plot
    mode = 'markers', 
    fill = ' ') %>%
      layout(xaxis=list(title='Happiness Score'), # add titles for x and y-axes and the legend
             yaxis=list(title='Average Annual Time Spent Working (hours)'),
             legend=list(title=list(text='<b>Region</b>'))) # "<b>" is used to bold the text
