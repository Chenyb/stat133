# Please load in the dataset included in the midterm1 directory (SFHousing-2.rda). 
# It will be required to perform the following tasks. The dataset includes data for houses
# in the city of Berkeley.
load("SFHousing-2.rda")
# calculate the mean lot size (lsqft) of houses in Berkeley. Store it as the
# variables <mean.lsqft>.

mean.lsqft <- mean(housing$lsqft, na.rm=TRUE)

# How many unique area codes are there in the dataset?  Store them in the variable
# <n.zipcode>

n.zipcode <- nlevels(unique(housing$zip))

# For each house in the dataset, calculate how large the house is relative to
# the lot size, i.e. square foot of house over square foot of lot.
# Store it in the variable <rel.sqft>.

rel.sqft <- (housing$bsqft)/(housing$lsqft)


# Please create two new data frames with the following two subsets
# and store them with the indicated names:
# 1) houses whose bsqft is strictly greater than <mean.bsqft>:  <bsft.greater>
# 2) houses whose bsqft is less than or equal to  <mean.bsqft>: <bsqft.less>
mean.bsqft <- mean(housing$bsqft, na.rm=TRUE)
bsqft.greater <- subset(housing, bsqft > mean.bsqft)
bsqft.less <- subset(housing, bsqft <= mean.bsqft)


# For each of your subsets, create a vector giving the price of each house. Name
# these variables <rooms.greater.price> and <rooms.less.price>.

rooms.greater.price <- bsqft.greater$price
rooms.less.price <- bsqft.less$price



# Please implement the function priceByRooms. Your function should take the
# following arguments:
#
# <room.range>: a numeric vector of length 2 whose first and second observations
#   give the minimum and maximum number of rooms to consider
# <br>: a numeric vector giving the number of bedrooms for each observation
# <prices>: a numeric vector giving the price of each observation associated
#   with <br>
#
# Your function should return the average of <prices> for all observations with
# <br> in the range (inclusive) specified by <room.range>

priceByRooms <- function(room.range, br, prices) {
  df <- data.frame(br=br, prices=prices)
  return (mean(subset(df, br <= room.range[2] & br>= room.range[1])$prices))
}

# My test
# rr = c(1,7)
# priceByRooms(rr, housing$br, housing$price)

# Please create a plot of house price (y-axis) against br (x-axis). Your plot
# should include the following features:
# 1) a title "Housing price vs Number of Rooms"
# 2) axis labels: "Price" and "#rooms"
# 3) plotting character set to 19
plot(housing$br,housing$price, pch=19, 
     main = "Housing price vs Number of Rooms",
     xlab = "#rooms",
     ylab = "Price")
    

