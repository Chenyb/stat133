# HW 2
# Graphics Skills
# Due Thursday February 12th by midnight 
# This .R file should contain your code and answers to questions

# The goals of this assignment are to: 
# 1. Become familiar with the variety of plotting functions available in R,
# 2. Learn which types of plots are appropriate for which types of data
# 3. Gain practice in making plots that make the data stand out,
#    facilitate comparison, and are information rich
# 4. Gain additional experience working with data frames and vectors

# The data are related to the 2012 Summer Olympics
# We have three data sets available to you:

# a. wr1500m - a data frame containing information about the 
# world record in the 1500 meter men's race

# b. SO2012Ctry which is a data frame with information about 
# each country that had an athlete participate in the olympics

# c. London2012ALL_ATHLETES.rda - a data frame which contains data
# on individual atheletes who participated in the 2012 Olympics


# We will be making 4 plots with these sources of data.
# You will make each plot by gradually adding more features to it.

# The help for plot.default describes many parameters that are  
# available for many of the plotting functions.
# Also, ?par provides help on many other plotting parameters.
# Some of these can be set in the plot function, while others
# are set in a call to par()

##############################
# PLOT 1. World Record in Men's 1500 meter run
# When watching the summer Olympics, we might be curious
# about how much faster today's runners are compared to
# runners 50 or 100 years ago. 

# A search on Wikipedia shows us tables of the times for the
# world record holder in 1500 meters dating back to 1892.
# We can use readHTMLTable() (from the package "XML" in R) and 
# this function goes off to the Web and downloads the tables we 
# need and reads them into R as data frames. After some text 
# manipulation, we have a data frame that we can use to examine 
# the world records graphically.
# Later in this semester, you will be able to do the same sort
# of thing (access data from the Web from within R and clean it
# for analysis.).  Right now, the data are available to you in
# the hw3 directory in the file WR1500MeterMen.rda.

# load the data
load('WR1500MeterMen.rda')
# The name of the object loaded is wr1500m
# The time (in the column "times") in these data are recorded in seconds, 
# and they are seconds over 3 minutes. 
# So a time of 70 is really 4 minutes and 10 seconds.

# Q1a. How many world records does this data frame contain?

n.wr <- nrow(wr1500m)

# Q1b. Use R commands to find out who currently holds the world
# record in the men's 1500 meter.
 
 wr.name <- subset(wr1500m,times == min(wr1500m$times))$athlete


# Let's look at the relationship between date and time.
# Q1c. What type of variable (numeric (continuous or discrete), nominal ordinal)
# are year and times? (no need to save the output, just look at it)
typeof(wr1500m$times)
discrete;

# When we are examining a variable to see how it changes in time,
# we typically make a line plot, with time on the x-axes and 
# the (x,y) values connected with line segments.

# Q2a. Begin by making a step plot of year by times for these data.
# (hint: use the type argument in plot)
# Don't bother to make the plot pretty yet; we will get to that later.
# But do add 180 to the times so that they are accurate measurements in seconds,
# store that in a new variable and add to the data frame.



times_sec <- 180 + wr1500m$times
wr1500m <- cbind(wr1500m, times_sec)
plot(wr1500m$year, wr1500m$times_sec, type = "s")


# Q2b. Redo the plot using a date that incorporates the month as 
# well as the year. For example, in Sep 1904 the world record 
# was broken by James Lightbody. Use a date of 1904.75 for this
# date. If any month is NA, use 0.5 for the fraction.
# Create a new variable, new_year, with the date in this format but
# first find and set all missing months to 0.5
# Add new_year to the dataframe.


new_month <- wr1500m$month
new_month[is.na(new_month)]<- 6
new_month <- new_month/12
new_year<- new_month + wr1500m$year
wr1500m <- cbind(wr1500m, new_year)
plot(wr1500m$new_year, wr1500m$times_sec, type = "s")


# Q3. The current world record was set in 1998. If we want to
# show that this record still stands in 2014, we could add a 
# horizontal line segment to the plot from 1998 to 2014 at the 
# 1998 record time.  
# To do this: remake the plot and set the xlim parameter 
# so that 2014 is included in the x-axis scale;
# then use the lines() function to add the additional segment.

wr_1998 <- wr1500m[wr1500m$year == 1998,]$times_sec
plot(wr1500m$new_year, wr1500m$times_sec, type = "s", xlim = c(1892, 2020))
lines(c(wr1500m[wr1500m$year == 1998,]$new_year, 2014.5), c(wr_1998, wr_1998))


# Q4. There are two times where the record stood for several
# years - in 1944 and 1998. Let's make it easier to see these
# dates and let's include the name of the athlete who set
# the record.  This additional reference information makes
# our plot richer.
# Add two grey vertical lines. One at 1944 and the other at 1998.
# Add the runner's name next to each vertical line.
# To do this, you will need the abline() function, the text() function,
# and you might want to consider the cex, col, pos, adj parameters.
# Also, do not type in the athlete's name. Instead, use subsetting
# of wr1500m$athlete to access it.

wr_1944 <- wr1500m[wr1500m$year == 1944,]$times_sec
lines(c(wr1500m[wr1500m$year == 1998,]$new_year, 2014.5), c(wr_1998, wr_1998))
abline(v = wr1500m[wr1500m$year == 1944,]$new_year, col = "grey")
abline(v = wr1500m[wr1500m$year == 1998,]$new_year, col = "grey")
text(wr1500m[wr1500m$year == 1944,]$new_year, wr_1944, levels(droplevels(wr1500m[wr1500m$year == 1944, ]$athlete)), pos = 4, cex = 0.6, col = "black")
text(wr1500m[wr1500m$year == 1998,]$new_year, wr_1998, levels(droplevels(wr1500m[wr1500m$year == 1998, ]$athlete)), pos = 4, cex = 0.6, col = "black")



# Q5. Now we are ready to add other contextual information.
# Remake the plot as before but now adding axis labels and a title.
# This is the FINAL version of the plot of world record times.

# put your final version of the plotting commands below.
plot(wr1500m$new_year, wr1500m$times_sec, type="s", xlim = c(wr1500m$new_year[wr1500m$year == 1892], 2020), xlab = 'Year', ylab = 'Record Time in Second', main = 'Man\'s 1500m Running World Record in Different Years')
abline(v = wr1500m[wr1500m$year == 1944,]$new_year, col = "grey")
abline(v = wr1500m[wr1500m$year == 1998,]$new_year, col = "grey")
text(wr1500m[wr1500m$year == 1944,]$new_year, wr_1944, levels(droplevels(wr1500m[wr1500m$year == 1944, ]$athlete)), pos = 4, cex = 0.6, col = "black")
text(wr1500m[wr1500m$year == 1998,]$new_year, wr_1998, levels(droplevels(wr1500m[wr1500m$year == 1998, ]$athlete)), pos = 4, cex = 0.6, col = "black")

## You have finised the first plot!!

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

# To begin, make a plot of GDP against population. 
# Consider which of the three principles of good graphics this
# plot violates and why.


plot(SO2012Ctry$GDP, SO2012Ctry$pop)

### Data stand out, Values are plotted on the top of each other  
### Facilitate comparison OR poor scale, We should zoom in on the bulk of the data
### Should fill data region, lots of empty space
### Information rich, for example identify outliers

#Q7. Let's examine GDP per person (create this new variable yourself)
# and population. Use a log scale for both axes. Use the symbols()
# function rather than plot(), and create circles for the plotting
# symbols() where the area of the circle is proportional to the 
# total number of medals.

GDP_per_person <- SO2012Ctry$GDP / SO2012Ctry$pop
SO2012Ctry <- cbind(SO2012Ctry, GDP_per_person)
symbols(SO2012Ctry$pop, SO2012Ctry$GDP_per_person, circles = SO2012Ctry$Total)


# Q8. It appears that the countries with no medals are circles too.
# Remake the plot, this time using *only the countries that won medals*. 
# If necessary adjust the size of the circles.
# Then add the non-medal countries to the plot using the "." 
# plotting character.

# your plotting code here
SO2012Ctry.non_medal <- SO2012Ctry[SO2012Ctry$Total == 0,]
SO2012Ctry.medal <- SO2012Ctry[SO2012Ctry$Total > 0,]
logpop_medal <- log(SO2012Ctry.medal$pop)
loggpp_medal <-  log(SO2012Ctry.medal$GDP_per_person)
logpop_non_medal <-log(SO2012Ctry.non_medal$pop)
loggpp_non_medal <- log(SO2012Ctry.non_medal$GDP_per_person)
symbols(logpop_medal, loggpp_medal, circles = sqrt(SO2012Ctry.medal$Total)/40, inch = F)
points(logpop_non_medal, loggpp_non_medal, pch = ".", cex = 0.5)

# Q9. Make the plot information rich by adding axis labels, 
# title, and label 5 of the more interesting points
# with the country name. Use text() to do this.

top5 <- order(SO2012Ctry$Total, decreasing = TRUE)[1:5]
# your plotting code here, including a new call to text()
symbols(logpop_medal, loggpp_medal, circles = sqrt(SO2012Ctry.medal$Total)/40, inches = F, xlab = "population in log", ylab = "GDP per person in log", main = "Number of Olympic Medals for Different Countries")
points(logpop_non_medal, loggpp_non_medal, pch = ".")
text(log(SO2012Ctry$pop[top5]), log(SO2012Ctry$GDP_per_person[top5]), SO2012Ctry$ISO[top5], adj = 0.5, col = "red", cex = 0.5)


######################################
# PLOT 3.
# Plotting points on maps can help us see geographic relationships
# 
#Q10. Install the maps library and load it into your R session.
# Make a map of the world, using the function map(),
# where the countries are filled with a light grey color.

## you only need to run these two lines once:
install.packages("maps")
library("maps")

world <- map("world", fill = TRUE, col = "light grey")

#Q11. Use the symbols() function to add circles to the map where
# the circles are proportional in area to the number of medals
# won by the country. You may find the add parameter useful.
# (Be sure to NOT plot circles for countries with 0 medals).
# adjust the size of the circles if necessary

# Check what variables you have in the dataframe using names().
names(SO2012Ctry)

# pull out the contries that won at least one medal (you will need at least
# the contries longitude, latitude and Total.)

wonMedal <- SO2012Ctry[SO2012Ctry$Total > 0,]
world <- map("world", fill = TRUE, col = "light grey")
symbols(wonMedal$longitude, wonMedal$latitude, circles = sqrt(wonMedal$Total)/4, add = TRUE, inch = F)


#Q12. Remake the plot and fill in the circles with a partially
# transparent gold color. To create this color: 
# install the RColorBrewer library and load it into R;
# call display.brewer.all() to examine the palettes;
# choose a palette and ask for the names of a few colors 
# using brewer.pal();
# pick one of the colors and create a new one that is transparent
# by adding two more digits to the end of the name, e.g.,
# if you want to use "#FEB24C" then make it transparent with
# e.g. myColor = "#FEB24CAA" or   "#FEB24C88"

# You only need to call these two lines once:
install.packages("RColorBrewer")
library("RColorBrewer")

display.brewer.all()
brewer.pal(3, "YlOrRd")


myGold <-"#FEB24C"

world <- map("world", fill = TRUE, col = "light grey")
symbols(wonMedal$longitude, wonMedal$latitude, circles = sqrt(wonMedal$Total)/4, add = TRUE, inch = F, bg = myGold)


## That was the FINAL version of this plot

##############################################
# PLOT 4
# During the news coverage of the Olympics it was noted that this
# Olympics had by far the greatest number of women competing and
# that some countries had female athletes competing for the first time.

# The data file is called London2012ALL_ATHLETES.rda
# and contains information about every athlete who competed 
# in the Olympics.

load("London2012ALL_ATHLETES.rda")

# There is one observation for each athlete. 
# (Actually, about 20 athletes have two records if they
# competed in different sporting events. Let's not worry about that.)

#Q13. We are interested in the relationship between Sport and Sex. 
# Examine the data frame and check which type of data each variable is.
names(athletes)
### Name : nominal
typeof(athletes$Name)
### Sex : nominal
typeof(athletes$Sex)
### Sport : nominal
typeof(athletes$Sport)
### Country : nominal
typeof(athletes$Country)
### MoreThan1Sport : ordinal or nominal is OK
typeof(athletes$MoreThan1Sport)

# The table() and sum() functions might be helpful for answering 
# some of the questions below. 

# How many athletes competed in the 2012 Olympics?
n.athletes <- nrow(athletes)

# How many women competed?
women.athletes <- nrow(athletes[athletes$Sex == "F",])

# What proportion of the participants were women?
frac.women <- women.athletes / n.athletes

# How many sports were there?
n.sports <- unique(athletes$Sport)


#Q14. Make a barplot of Sport and Sex that emphasizes the 
# important differences. To do this, first make a table of 
# Sex by Sport. This will be the input to barplot(). 
# Make the barplot with the parameter beside = TRUE and 
# and again with beside = FALSE. Determine which of these 
# barplots provides the easiest comparison. 

athTab <- table(athletes$Sport, athletes$Sex)
# make two barplots
barplot(athTab, beside = TRUE)
barplot(athTab, beside = FALSE)
# The barplot with beside = True is easier to compare.

# what should beside be set to, T/F?
set.beside <- TRUE

### Barplot with beside = TRUE provides the easiest comparison. 

#Q15. Remake the barplot above, but this time switch the order 
# of Sport and Sex in the call to table(). Use the value for
# the beside parameter that you decided was best for the 
# plot in Q 14. 

athTab2 <- table(athletes$Sex, athletes$Sport)
# make barplot
barplot(athTab2, beside = set.beside)

# Compare the barplot with (Sex, Sport) vs (Sport, Sex). 
# Which makes a more interesting visual comparison, plot 1 or 2?
# store your answer (1 or 2) in best.plot.

best.plot <- 2


# Q16. Notice that the bars are in alphabetical order by sport.
# To facilitate comparisons, we might want to arrange
# the bars in order of participation in a sport. To do this,
# call order() on the return value from making a table of Sport alone.
# Assign this vector to a variable, say orderSport.
# Then reorder your two-way table of Sport and Sex,
# using the orderSport vector and [ ] to subset the table and rearrange
# the rows/cols. The resulting barplot should show bars in 
# increasing height.

orderSport <- order(table(athletes$Sport))
barplot(athTab2[, orderSport], beside = set.beside)


# Q17. Finally to make the plot more informaation rich, try turning
# the x-axis labels on their side. To do this, find a parameter
# in par() that will rotate the x-axis tick mark labels. Even though
# you found the parameter in the par() function, this
# parameter can be added in the call to barplot().
# Also find and use a parameter to shrink the text for these labels. 
# Lastly, add a title to the plot.

barplot(athTab2[, orderSport], beside = set.beside, las = 2, cex.names = 0.8, main = "Relationship about Spoty and Sex")
# This was the final version of the 4th plot.

# You are DONE.
# Hope you had fun making increasingly complex and attractive plots with R.
