# HW 3
# Graphics Skills "take 2" - this time with ggplot2.
# Due Thursday February 19th by midnight 
# This .R file should contain your code

# We use the same data as in HW2 and we recreate the same four plots
# but this time using the ggplot2 package.

# Any variables that you are asked to create are the same as in HW2 so you
# can copy your code over, the plotting functions are different.

# First of all you need to install the ggplot2 package.
# You only need to do that once, so the install command is commented out 
# here to make sure you don't run it multiple times but make sure to install it once:
# install.packages("ggplot2")
library("ggplot2")
# And one more package:
# install.packages("plyr")
library("plyr")

# Before getting started you should aquaint yourself with the ggplot2 package.
# Visit these websites and look at the plots (nice, eh?)
# http://www.r-bloggers.com/quick-introduction-to-ggplot2/
# http://www.cookbook-r.com/Graphs/
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/

# Now open the manual pages and keep them open while you work:
# http://docs.ggplot2.org/current/

# STOP: Did you look at the examples in the links above?  Notice
# that the syntax of ggplot2 is different from what we've seen.
# The ggplot() or qplot() command is used to set up the
# plotting environment and then you call specific functions
# by literally adding them to the function call with a "+" and a new call.

# So if you are doing a boxplot you would write somethings like:
# ggplot(data) + geom_boxplot()
# see here : http://docs.ggplot2.org/current/geom_boxplot.html

# Look at a couple of more examples before going on.

##############################
## PART I
## (PART II is at the bottom, short and simple) 
##############################
# Remember, the three datasets are:
# a. SO2012Ctry which is a data frame with information about 
# each country that had an athlete participate in the olympics

# b. London2012ALL_ATHLETES.csv - a csv file which contains data
# on individual atheletes who participated in the 2012 Olympics

# c. wr1500m - a data frame containing information about the 
# world record in the 1500 meter men's race

##############################
# PLOT 1. World Record in Men's 1500 meter run
# When watching the summer Olympics, we might be curious
# about how much faster today's runners are compared to
# runners 50 or 100 years ago. 

# load the data
load("WR1500MeterMen.rda")

# The name of the object loaded is wr1500m
# The time (in the column "times") in these data are recorded in seconds, 
# and they are seconds over 3 minutes. 
# So a time of 70 is really 4 minutes and 10 seconds.

# Q1a. How many world records does this data frame contain?

n.wr <- nrow(wr1500m)


# Q1b. Use R commands to find out who currently holds the world
# record in the men's 1500 meter.

wr.name <- levels(droplevels(wr1500m[wr1500m$year == max(wr1500m$year), ]$athlete))


# Let's look at the relationship between date and time.
# Q1c. What type of variable (numeric (continuous or discrete), nominal ordinal)
# are year and times? (no need to use R code to answer this question)

### year : discrete
### times : continuous


# When we are examining a variable to see how it changes in time,
# we typically make a line plot, with time on the x-axes and 
# the (x,y) values connected with line segments.

# Q2a. Begin by making a step plot of year by times for these data.
# Don't bother to make the plot pretty yet; we will get to that later.
# But do add 180 to the times so that they are accurate measurements in seconds,
# store that in a new variable and add to the data frame.
# Hint: which geom_* function creates a step plot?

times_sec <- wr1500m$times + 180
wr1500m <- data.frame(wr1500m, times_sec = times_sec)

# Your ggplot / qplot command:
ggplot(wr1500m, aes(year, times_sec)) +
  geom_step()


# Q2b. Redo the plot using a date that incorporates the month as 
# well as the year. For example, in Sep 1904 the world record 
# was broken by James Lightbody. Use a date of 1904.75 for this
# date. If any month is NA, use 0.5 for the fraction.
# Create a new variable, new_year, with the date in this format but
# first find and set all missing months to 0.5
# Add new_year to the dataframe.

new_month <- wr1500m$month
new_month[is.na(new_month)] <- 6
new_month <- new_month / 12
new_year <- wr1500m$year + new_month
wr1500m <- data.frame(wr1500m, new_year = new_year)

# Your qplot command:
qplot(new_year, times_sec, data = wr1500m, geom = "step")


# Q3. The current world record was set in 1998. If we want to
# show that this record still stands in 2015, we could add a 
# horizontal line segment to the plot from 1998 to 2015 at the 
# 1998 record time.  
# Hint: which geom_* function adds a line segment?
# Hint: look at xlim() and theme().

new_year.1998 = wr1500m[wr1500m$year == 1998, ]$new_year
wr_1998 <- wr1500m[wr1500m$year == 1998, ]$times_sec

# Your ggplot command:
ggplot(wr1500m, aes(new_year, times_sec)) +
  geom_step() +
  xlim(1892, 2030) +
  geom_segment(x = new_year.1998, y = wr_1998, xend = 2015.5, yend = wr_1998)



# Q4. There are two times where the record stood for several
# years - in 1944 and 1998. Let's make it easier to see these
# dates and let's include the name of the athlete who set
# the record.  This additional reference information makes
# our plot richer.
# Add two *green* vertical lines. One at 1944 and the other at 1998.
# Add the runner's name in *blue* next to each vertical line.
# Remember, do not type in the athlete's name. Instead, use subsetting
# of wr1500m$athlete to access it.
# Hint: geom_vline(), annotate().

new_year.1944 = wr1500m[wr1500m$year == 1944, ]$new_year
wr_1944 <- wr1500m[wr1500m$year == 1944, ]$times_sec

# Your ggplot command
ggplot(wr1500m, aes(new_year, times_sec)) +
  geom_step() +
  xlim(1892, 2030) +
  geom_segment(x = new_year.1998, y = wr_1998, xend = 2015.5, yend = wr_1998) +
  geom_vline(xintercept = new_year.1944, colour = "green") +
  geom_vline(xintercept = new_year.1998, colour = "green") +
  annotate("text", new_year.1944 + 11, wr_1944 + 2, label = levels(droplevels(wr1500m[wr1500m$year == 1944, ]$athlete)), colour = "blue") +
  annotate("text", new_year.1998 + 15, wr_1998 + 2, label = levels(droplevels(wr1500m[wr1500m$year == 1998, ]$athlete)), colour = "blue")


# Q5. Now we are ready to add other contextual information.
# Remake the plot as before but now adding axis labels and a title.
# This is the FINAL version of the plot of world record times.
# Hint : labs()

# Your ggplot commands
ggplot(wr1500m, aes(new_year, times_sec)) +
  geom_step() +
  xlim(1892, 2030) +
  geom_segment(x = new_year.1998, y = wr_1998, xend = 2015.5, yend = wr_1998) +
  geom_vline(xintercept = new_year.1944, colour = "green") +
  geom_vline(xintercept = new_year.1998, colour = "green") +
  annotate("text", new_year.1944 + 11, wr_1944 + 2, label = levels(droplevels(wr1500m[wr1500m$year == 1944, ]$athlete)), colour = "blue") +
  annotate("text", new_year.1998 + 15, wr_1998 + 2, label = levels(droplevels(wr1500m[wr1500m$year == 1998, ]$athlete)), colour = "blue") +
  labs(title = "World Record in Men's 1500m Race", x = "Year", y = "Times (sec)")


################################
# PLOT 2
# A lot of medal counting goes on during the Olympics.
# We might wonder about the relationship between number of medals
# a country has and the size of its population and its wealth.
# We collected data from various sources (ManyEyes, Guardian,
# ISO) to create this data frame with GDP, population, and other information
# about each country that participated in the Olympics.

# The data frame SO2012Ctry contains this information.
# It can be loaded into R with

load("SummerOlympics2012Ctry.rda")

#Q6 Take a look at the variables in this data frame.
# What kind of variable is GDP and population?

### GDP : continuous
### population : discrete

# What about Total?

### Total : discrete


# To examine the relationship between these three variables,
# we could consider making a scatter plot of GDP against population
# and use plotting symbols that are proportional in size to
# the number of medals. 

# To begin, make a plot of GDP against population. Your ggplot command:
ggplot(SO2012Ctry, aes(GDP, pop)) +
  geom_point()


#Q7. Let's examine GDP per person (create this new variable yourself)
# and population.
# Use a log scale for both axes and create circles whose area is
# proportional to the total number of medals.
# Do not log the variables directly.
# Hint: use the options log and size.

GDP_per_person <- SO2012Ctry$GDP / SO2012Ctry$pop
SO2012Ctry <- data.frame(SO2012Ctry, GDP_per_person = GDP_per_person)

# Your ggplot command
ggplot(SO2012Ctry, aes(GDP_per_person, pop)) +
  geom_point(aes(size = sqrt(SO2012Ctry$Total))) +
  coord_trans(x = "log10", y = "log10")


# We skip Q8 this time the plot above is already fine.
# Q8. It appears that the countries with no medals are circles too....

# Q9. Make the plot information rich by adding axis labels, 
# title, and label 5 of the more interesting points
# with the country name.
# Hint: use annotate(), geom_text(), maybe other functions.

SO2012Ctry.won_medals <- SO2012Ctry[SO2012Ctry$Total > 0, ]
SO2012Ctry.no_medals <- SO2012Ctry[SO2012Ctry$Total == 0, ]

### 1. The country with the most medals
### 2. The country that won medals but with the lowest GDPPP
### 3. The country that won medals but with the lowest population
### 4. The country that has no medal but with the highest GDPPP
### 5. The country that has no medal but with the highest population

SO2012Ctry.most_medals <- SO2012Ctry[SO2012Ctry$Total == max(SO2012Ctry$Total), ]
SO2012Ctry.won_medals.lowest_gdppp <- SO2012Ctry.won_medals[SO2012Ctry.won_medals$GDP_per_person == min(SO2012Ctry.won_medals$GDP_per_person), ]
SO2012Ctry.won_medals.lowest_pop <- SO2012Ctry.won_medals[SO2012Ctry.won_medals$pop == min(SO2012Ctry.won_medals$pop), ]
SO2012Ctry.no_medals.highest_gdppp <- SO2012Ctry.no_medals[SO2012Ctry.no_medals$GDP_per_person == max(SO2012Ctry.no_medals$GDP_per_person), ]
SO2012Ctry.no_medals.highest_pop <- SO2012Ctry.no_medals[SO2012Ctry.no_medals$pop == max(SO2012Ctry.no_medals$pop), ]

top5 <- list(SO2012Ctry.most_medals, SO2012Ctry.won_medals.lowest_gdppp, SO2012Ctry.won_medals.lowest_pop, SO2012Ctry.no_medals.highest_gdppp, SO2012Ctry.no_medals.highest_pop)

# Your ggplot command:
gg <- ggplot(SO2012Ctry, aes(GDP_per_person, pop)) +
  geom_point(aes(size = sqrt(SO2012Ctry$Total))) +
  coord_trans(x = "log10", y = "log10") +
  labs(title = "Relationship between GDPPP, Population and Olympic Medals of Countries", x = "GDPPP (log)", y = "Population (log)")
for (country in top5) {
  gg <- gg + annotate("text", country$GDP_per_person, country$pop, label = country$Country)
}
gg


######################################
# PLOT 3.
# Plotting points on maps can help us see geographic relationships

#Q10. Install the maps library and load it into your R session.
# Make a map of the world where the countries are filled with a light grey color.
library("maps")
# Hint: look at map_data() and geom_polygon() in the ggplot2 manual.

# Your ggplot commands:
world = map_data("world")
ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey")

# Q11. Now add circles to the map where
# the circles are proportional in area to the number of medals
# won by the country. 
# pull out the contries that won at least one medal (you will need at least
# the contries longitude, latitude and Total.)
# Consider using the colors "grey40" and "grey90" for the map and "gold" for the circles.
# Hint: look at the function [geom_point()] and the parameters [aes] and [size]

wonMedal <- SO2012Ctry[SO2012Ctry$Total > 0, ]

# Your ggplot commands here.
ggplot() +
  geom_polygon(data = world, aes(long, lat, group = group), fill = "grey40", colour = "grey90") +
  geom_point(data = wonMedal, aes(longitude, latitude, size = sqrt(Total)), colour = "gold")


## Not needed
#Q12. Remake the plot and fill ......


## That was the FINAL version of this plot

##############################################
# PLOT 4
# During the news coverage of the Olympics it was noted that this
# Olympics had by far the greatest number of women competing and
# that some countries had female athletes competing for the first time.

# The csv file called London2012ALL_ATHLETES.csv
# contains information about every athlete who competed in the Olympics.
# It can be loaded into R as a data frame with the following call:

load("London2012ALL_ATHLETES.rda")

# There is one observation for each athlete. 
# (Actually, about 20 athletes have two records if they
# competed in different sporting events. Let's not worry about that.)

# Skip over this question....
#Q13. We are interested in the relationship between Sport and Sex. 

#Q14. Make a barplot of Sport and Sex that emphasizes the 
# important differences. To do this, first make a table of 
# Sex by Sport.
# Hint: Find the geom_*() function that makes a barplot then
# find the option that allows you to put bars side-by-side (study the manual page)

# make barplot with ggplot
ggplot(athletes, aes(Sport, fill=Sex)) +
  geom_bar(position = "dodge")

## Skip this question...
#Q15. Remake the barplot above...

## Skip this question...
# Q16. Notice that ....

# Q17. Finally to make the plot more informaation rich, try turning
# the x-axis labels on their side. To do this, find a parameter in theme()
# that allows you to set the angle.
# Lastly, add a title to the plot.

# Your ggplot commands
ggplot(athletes, aes(Sport, fill=Sex), title = "Relationship about Spoty and Sex") +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# This was the final version of the 4th plot.

########### PART 2
########### Apply statements
## In this homework we will do a few simple excercises with apply.  
## You will be using apply statements in many of the upcoming assignments.

# Load the rainfall data, you will get two lists, [rain] and [day]
load("rainfallCO.rda")

# Create a variable 
# max.rain : a vector of length 5 with the maximum rainfall at each station
max.rain <- lapply(rain, max)

# Create a variable 
# mean.rain : a vector of length 5 with the average rainfall at each station
mean.rain <- lapply(rain, mean)


# Create a variable 
# sd.rain : a vector of length 5 with the standard deviation of the rainfall at each station
sd.rain <- lapply(rain, sd)


# Create a variable 
# n1989.rain : a vector of length 5 with the number of measurements at each station in the year 1989 (use [day])
f <- function(x) {
  n <- 0
  for (i in 1:length(x)) {
    if (floor(x[i]) == 1989) {
      n <- n + 1
    }
  }
  return(n)
}
n1989.rain <- lapply(day, f)

