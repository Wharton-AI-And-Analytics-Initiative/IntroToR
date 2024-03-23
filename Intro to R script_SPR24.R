#Your working directory is where R will pull your data files from and where it will save any files you create, like this script
#use this code to check what R has your working directory set as
getwd()
#set your working directory to your downloads folder
#the code below in green should be your own personal file path, so it shouldn't have 'Users/jsamper', as that is my personal file path name
setwd("/Users/jsamper/Downloads/WA R Intro-selected")

#ignore the code below
setwd("/Users/jsamper/Documents/Intro to R")

#################################################

install.packages("tidyverse")
library(tidyverse)
#when you see the stop sign in the upper right corner of the console, it means R is working on your previous command. Wait until the sign disappears before telling R to run a new line of code.
#if you see a + in the bottom left of the console, it means you didn't finish the last block of code you wrote and R is waiting for more information. When you see a > sign, R is ready to run a new command. 


#importing data as a csv file and an excel file
univs <- read_csv('world all university rank and rank score.csv')
view(univs)
#R doesn't have a function to import an excel file. You need to download a package to do so. 
install.packages("readxl")
library(readxl)
uniexcel <- read_excel("world all university rank and rank score.xls")
view(uniexcel)
#in the lines of code above, 'univs' and 'uniexcel' are names I assigned to these new data frames.

################################################

#Using a new dataset on flights departing NYC in 2013, we will go over data cleaning and manipulation using the tidyr and dplyr packages
install.packages("nycflights13")
library(nycflights13)
#the nycflights13 library has the data stored as a tibble called 'flights'. It's easier to use this data as a dataframe, so let's read the data into a data frame below.
flightsdf <- flights
#take a look at the data types in the global environment - we have integers, numeric, and character variables

##############################################

#DATA CLEANING using tidyr
#checking to see how many missing values are in the flights data frame
sum(is.na(flightsdf))
#creating a new data frame in which only rows with no missing values are included
flightscomplete <- flightsdf[complete.cases(flightsdf), ]
#checking our work
sum(is.na(flightscomplete))
#this new data frame has no missing values

##############################################

#DATA MANIPULATION using dplyr - basic tasks are to filter, arrange, select, mutate values
#FILTERING allows you to subset observations based on their values
view(flightsdf)
jan1 <- filter(flightsdf, month ==1, day ==1)
jan1
view(jan1)
#jan 1 is now a data frame only containing data on flights that departed on january 1st

#you can filter data by multiple arguments
#Below is the code to see all flights that departed in November or December
novdec <- filter(flightsdf, month ==11 | month ==12)
#another way to write this same function is below. This code is saying to select every row where x is one of the values in y
novdec2 <- filter(flightsdf, month %in% c(11,12))
novdec2

####################################################

#ARRANGE - you can change the order the rows are shown in using the arrange function
arrange(flightsdf, month)
arrange(flightsdf, flight)

#you can provide more than one column name, or variable, to arrange by. Each additional column will be used to break ties in the values of the preceding columns
arrange(flightsdf, month, day)

#By default, R sorts in ascending order. You can use 'desc()' to sort row values in descending order
arrange(flightsdf, desc(day))

###################################################

#SELECT function
select(flightsdf, origin)
#within the select command, there are several functions you can use to tailor your select command, like 'starts_with()', 'ends_with()', 'contains()', '#'matches()', and 'num_range()'

##################################################

#MUTATE
#we can create new variables from existing variables.
#start out by creating a new data frame that only contains the variables you are interested in
view(flightsdf)
flights2 <- select(flightsdf,
                   year:day,
                   ends_with("delay"),
                   distance,
                   air_time
                   )
#you can confirm the data frame was created correctly using the 'view' command
view(flights2)
#using the mutate function, we can create new variables 'gain' and 'speed' from existing variables 
#once these new variables are created, you can refer to them in future analyses
mutate(flights2,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
       )
#you may have noticed that flights2 in the global environment says it still only has 7 variables. This is because we didn't add the mutation to the flights2 data frame. If we don't specify where to put the mutated values, R will default to creating a tibble with the new variables, which won't be seen in the data frame.
#below is the same code as above but we are updating the command to apply to the flights2 data frame.
flights2 <- mutate(flights2,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60
       )
#now if you look at the global environment, you will see 9 variables in flights2

#grouped mutations - finding a group (destinations) 
# that exceed a threshold (1095 - the number of days in 3 years)
popular_dests <- flightsdf %>%
  group_by(dest) %>%
  filter(n() > 1095)
popular_dests

#################################################

#EXPLORATORY DATA ANALYSIS - these steps are up to the analyst
#we will be using a new dataset - the diamonds dataset is part of the ggplot 2 package which we downloaded as part of the tidyverse
data(diamonds)
view(diamonds)

###############################################

#checking for missing values in the data frame
sum(is.na(diamonds))

#we are interested in knowing how many diamonds 
# we have for each cut, cut being an ordinal, categorical (factor) variable
diamonds %>%
  count(cut)

#range will tell us the minimum and maximum values we have for carat size
range(diamonds$carat)

#mean gives us the average for carat size
mean(diamonds$carat)

#if you have missing values, you can remove them with 'na.rm'  when calculating descriptives like the average so they don't skew the value
mean(diamonds$price, na.rm = TRUE)

#################################################

#EDA VISUALIZATIONS - histograms, scatterplots, changing colors/using themes
#histograms
#first histogram here is the default code displaying the count of different diamonds with different carat sizes
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat))
#you can adjust the binwidth to account for more or less cases in each bin. This changes the shape of the histogram
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

#bar graphs
#a bar graph can show values (like an average or a count) for levels of a factor variable
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))

#let's focus on only the smaller diamonds, 3 carats or less
#this line of code is creating a new data frame only containing data for diamonds that have 3 carats or less
smaller <- diamonds %>%
  filter(carat < 3)

#the code below is for a frequency polygon in which the count for 2 variables is being displayed: carat and cut
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

##################################################

#CUSTOMIZING YOUR VISUALIZATIONS
#to create a meaningful figure, you need to customize it with proper labels, font sizes, font colors, and data colors
#To practice customizations, we will use a Harry Potter dataset. We want to visualize the number of characters from the films that have been sorted into each Hogwart's House
hpcharacters <- read.csv("Characters.csv")

#creating a generic bar chart with no customizations
bar1 <- ggplot(data=hpcharacters, aes(x=House)) +
  geom_bar(stat = "count")
bar1
#this chart contains missing values and schools we are not interested in


#our first task is to figure out how many characters in the dataset were in each house during their time at Hogwarts
count(hpcharacters, House)

#in this next segment of code, we use a 'drop_na' function to remove any characters with missing values from the 'House' variable (indicating these characters are likely muggles)
#we also use a 'filter' function to only include those who were sorted into Hogwarts Houses
bar2 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House)) +
  geom_bar(stat = "count")
bar2
#better! We now only see the information we are interested
# in. But, it's not very visually appealing.


#adding color is great tool for customization. Let's start by assigning colors to each level of the 'house' variable
bar3 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count") +
  scale_fill_manual (values=c("red",
                              "yellow",
                              "blue",
                              "green"))
bar3

#While representative of the Hogwarts houses, these colors aren't quite the right shade. When you're trying to use a specific shade, you can hand select colors using hexadecimal values
#you may have noticed when you created bar3 above that a legend was created. This legend is unnecessary as the groups are already properly labeled along the x-axis. We can remove the legend by specifying a value of 'F', meaning false, to the 'show.legend' function
bar4 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  scale_fill_manual (values=c("#740001",
                              "#ecb939",
                              "#0e1a40",
                              "#1a472a"))
bar4

#the gray background takes away from the clarity of the figure. There's also no chart title, and the axes titles aren't necessary as this information can be contained within the chart title.
bar5 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  labs(title = "Count of Characters Sorted into Each Hogwarts House",
       x = " ",
       y = " ") +
  scale_fill_manual (values=c("#740001",
                              "#ecb939",
                              "#222f5b",
                              "#2a623d")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5))
bar5

#The no label look is clearer, but R's default with font is to make it gray. Let's increase the size of the axes and change the font color to a salient black
bar6 <- hpcharacters %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Hufflepuff", "Ravenclaw", "Slytherin")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  labs(title = "Count of Characters Sorted into Each Hogwarts House",
       x = " ",
       y = " ") +
  scale_fill_manual (values=c("#740001",
                              "#ecb939",
                              "#222f5b",
                              "#2a623d")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))
bar6

#In this last step, we want to re-order the Houses so they are in descending order of count value. To do this, we need to restructure the 'house' variable
#I don't want to alter the existing characters data frame, so I will create a new data frame that is a perfect copy of the original
hpcharacters2 <- hpcharacters
#in order to change the ordering of the Houses, I first need to confirm the 'House' variable is being stored as a factor, or categorical, variable
class(hpcharacters2$House)
#As we can see in the console, the 'House' variable is stored as a character variable, meaning R doesn't know it has a grouped structure to it. We need to tell R it is a factor variable and specify what the groups, or levels, are.
hpcharacters2$House <- as.factor(hpcharacters2$House)
#when I specify the levels of the factor variable below, I am entering them in the order in which I want R to display them in the visualization
hpcharacters2$House <- factor(hpcharacters2$House, levels = c("Gryffindor", "Slytherin", "Ravenclaw", "Hufflepuff"))
str(hpcharacters2$House)

#in this final figure, the groups are reordered to appear in descending order according to count value
bar7 <- hpcharacters2 %>%
  drop_na(House) %>%
  filter(House %in% c("Gryffindor", "Slytherin", "Ravenclaw", "Hufflepuff")) %>%
  ggplot(aes(x=House, fill=House)) +
  geom_bar(stat = "count",
           show.legend = F) +
  labs(title = "Count of Characters Sorted into Each Hogwarts House",
       x = " ",
       y = " ") +
  scale_fill_manual (values=c("#740001",
                              "#2a623d",
                              "#222f5b",
                              "#ecb939")) +
  theme(panel.background = element_blank(),
        plot.title=element_text(hjust=0.5),
        axis.text.x = element_text(color = "black", size = 12),
        axis.text.y = element_text(color = "black", size = 12))
bar7
#Done! This last figure is drastically different than the first one we made, highlighting the importance of customizing your figures.
