# Tests for functions in homework 4

# Set your working directory to hw4 (i.e. ../src/stat133/assignments/hw4)

# 
# Copy and Paste the solution here.
# HW 4
# Writing functions
# Due Thursday February 26th by midnight 
# This .r file should contain your code

# Don't remove these lines:
#library(RUnit)
#errMsg = function(err) print(paste("ERROR:", err))
#load('hw4-tests.rda')
set.seed(1000)
#### Function #1
# Implement the function "listLengths". 

# Input variable:
# <data.list>: a list whose elements are vectors of varying length

# Output variable:
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {
  data.list <- sapply(data.list, length)
  return(data.list)
}

#### Andy's Tests
test = list()
sol = list()
a = seq(1, 5, by = 1)
b = seq(1, 10, by = 1)
c = seq(1, 15, by = 1)
d = seq(1, 20, by = 1)
e = seq(1, 25, by = 1)
test$listLengths = list(a,b,c,d,e)
sol$listLengths = listLengths(test$listLengths)
#####

#tryCatch(checkEquals(list.lengths.t, listLengths(func1.test)),
#         error=function(err) errMsg(err))

#### Function 2
#### Implement the function "powers"

# Input variable :
# <x> : a numeric vector of length n
# <k> : an integer

# Output variable
# <x.powers> : A matrix of size [n x k] where the first column is x, the second column x^2, the third column x^4, etc.
#              the column names should be : "x", "x^2", "x^3" etc.

powers <- function(x, k){
  if(!is.numeric(x)) warning("x should be a numeric vector")
  x.powers <- x
  for(i in 2:k){
    x.powers <- cbind(x.powers, x^i)
  }
  return(x.powers)
}

#### Andy's Tests
test$powers.x = seq(1, 3, by = 1)
test$powers.k = 3
sol$powers = powers(test$powers.x, test$powers.k)
#####

#### Function #3
#### Implement the function "recipeConversion"

# Input variable:
# <recipe> : A data frame with three columns named "amount", "unit" and "ingredient"

# Output variable:
# <recipe.metric> : A data frame with three columns where cups have been converted to ml and ounces to grams.
#                   the number in "amount" should be updated, and the entry in "unit" changed
#                   both ml and gr should be rounded to the nearest multiple of 5,
#                   e.g. a row that read : [2 cups flour] should now be [475 ml flour]
#                   Note, if the "unit" is neither "cup"/"cups" nor "oz" the row should not be changed

# The conversion constants are: 
# 1 cup = 236.6 ml and 1 oz = 28.3 gr
# Please use these exact numbers, do not add decimal places.

# "unit" can take any of a number of values but you need to find the rows where
# "unit" is : "cup", "cups" or "oz"

# Note: to find a match in "unit" you have a few different options, you can go row by row
# and check if the unit is equal to cup/cups/oz using the "==" operator, you can use the
# match() or %in% operators or finally you can look at the function grep(). 

# If the column names of the input data frame are not "amount", "unit" and "ingredient" the
# function should stop and print out an error message

# Put your code here
recipeConversion <- function(recipe){
  
  # check that we have the 3 required columns
  required.col <- c("amount", "unit", "ingredient")
  included.col <- colnames(recipe)
  if(! all(required.col %in% included.col) ) stop("Make sure to include the columns: amount, unit and ingredient.") 
  
  recipe.metric <- recipe
  
  # convert cups to ml
  cup.to.ml <- grep("cup", recipe$unit)
  recipe.metric[cup.to.ml, "amount"] <- recipe[cup.to.ml, "amount"] * 236.6
  recipe.metric[cup.to.ml, "unit"] <- "ml"
  
  # convert oz to gr
  oz.to.gr <- grep("oz", recipe$unit)
  recipe.metric[oz.to.gr, "amount"] <- recipe[oz.to.gr, "amount"] * 28.3
  recipe.metric[oz.to.gr, "unit"] <- "gr"
  
  return(recipe.metric)
}

#### Andy's Tests
test$recipe = read.table("choc_cake.txt",sep ="\t", header = TRUE, stringsAsFactors = FALSE)
sol$recipe = recipeConversion(test$recipe)
#####

#### Function #4a
# Implement the function "bootstrapVarEst"

# Input variable:
# <x> : data vector
# <B> : the number of boostrap iterations

# Output variable:
# <boot.sigma2.est> : Bootstrap estimate for the variance of the sample mean (see lecture notes)

bootstrapVarEst <- function(x, B){
  x.bar.boot <- NULL
  n <- length(x)
  for(i in 1:B){
    x.bar.boot[i] <- mean(sample(x, n, replace=TRUE))
  }
  return(boot.sigma2.est=var(x.bar.boot))
}

#### Andy' Test
test$bootstrapVarEst.x = rnorm(1000, sd = 100)
test$bootstrapVarEst.B = 1000
sol$bootstrapVarEst = bootstrapVarEst(test$bootstrapVarEst.x, test$bootstrapVarEst.B)
#### Function #4b
#### Implement the function "jackknifeVarEst"

# Input variable:
# <x> : data vector

# Output variable:
# <jack.sigma2.est> : Jackknife estimate for the variance of the sample mean (see lecture notes)

jackknifeVarEst <- function(x){
  x.bar.jack <- NULL
  for(i in 1:length(x)){
    x.bar.jack[i] <- mean(x[-i])
  }
  return(jack.sigma2.est=var(x.bar.jack))
}

#### Andy' Test
test$jackknifeVarEst.x = rnorm(1000, sd = 100)
sol$jackknifeVarEst = jackknifeVarEst(test$jackknifeVarEst.x)

#### Function #4c
#### Implement the function "samplingVarEst"

# Input variables:
# <x> : data vector
# <type> : string that takes the values "bootstrap" or "jackknife", the default should be bootstrap.

# Output variable:
# <sampling.sigma.est> : The bootstrap estimate if type="bootstrap" and the jackknife estimate if type="jackknife"

# Note: this function calls the previous two functions.

samplingVarEst <- function(x, type="bootstrap"){
  if(type=="bootstrap") return(bootstrapVarEst(x))
  else if(type=="jackknife") return(jackknifeVarEst(x))
  else warning("Type has to be bootstrap or jackknife.")
}

#### Andy' Test
test$samplingVarEst.x = rnorm(1000, sd = 100)
test$samplingVarEst.type = "jackknife"
sol$samplingVarEst = samplingVarEst(test$samplingVarEst.x, test$samplingVarEst.type)

# Save Andy's tests
save(test, sol, file="hw4-tests.rda")

