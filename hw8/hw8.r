xUnique = 1:5
trueCoeff = c(0, 1, 1)

getData = function(coefs = c(0, 1, 1), xs = 1:5, dupl = 10,
                   sd = 5, seed=2222){
  ### This function creates the artificial data
  set.seed(seed)
  x = rep(xs, each = dupl)
  y = coefs[1] + coefs[2]*x + coefs[3] * x^2 + 
      rnorm(length(x), 0, sd)
  return(data.frame(x, y))
}

### 
genBootY = function(x, y, rep = TRUE){
  ### For each unique x value, take a sample of the
  ### corresponding y values, with replacement.
  ### Return a vector of random y values the same length as y
  ### You can assume that the xs are sorted
  ### Hint use tapply here!
  return(unname(unlist(tapply(y, x, function(i) sample(i, length(i), replace=TRUE)))))
}

genBootR = function(fit, err, rep = TRUE){
  ### Sample the errors 
  ### Add the errors to the fit to create a y vector
  ### Return a vector of y values the same length as fit
  ### HINT: It can be easier to sample the indices than the values
  return(fit + sample(err, length(err), replace=FALSE)) 
}

fitModel = function(x, y, degree = 1){
  ### use the lm function to fit a line of a quadratic 
  ### e.g. y ~ x or y ~ x + I(x^2)
  ### y and x are numeric vectors of the same length
  ### Return the coefficients as a vector 
  ### HINT: Take a look at the repBoot function to see how to use lm()
  df <- data.frame(x=x, y=y)
  if (degree == 1) {
    model = lm(y ~ x - 1, df)
  } else if (degree == 2) {
    model = lm(y ~ x + I(x^2), df)
  } else {
    stop ("Wrong degree input")
  }
 coeff <- coef(model)
  return(coeff)
}

oneBoot = function(data, fit = NULL, degree = 1){
  ###  data are either your data (from call to getData)
  ###  OR fit and errors from fit of line to data
  ###  OR fit and errors from fit of quadratic to data  
  if (fit == Null){
    data[,2] <- genBootY(data[,1], data[,2])
  } else {
    data[,2] <- genBootR(fit[,1], fit[,2])
  }
 
  ### Use fitModel to fit a model to this bootstrap Y 
 return (fitModel(data[,1], data[,2], degree))
}

repBoot = function(data, B = 1000){
  
  ### Set up the inputs you need for oneBoot, i.e.,
  ### create errors and fits for line and quadratic

  ### replicate a call to oneBoot B times
  ### format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a data frame with B rows
  ### and one or two columns, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  ### Replicate a call to oneBoot B times for 
  ### each of the four conditions
  linear.resampling <- t(sapply(lapply(1:B, function(x) oneBoot(as.matrix(data), fit=NULL, degree=1)), unlist))
  quadratic.resampling <- t(sapply(lapply(1:B, function(x) oneBoot(as.matrix(data), fit=NULL, degree=2)), unlist))
  
  
  
  ### Format the return value so that you have a list of
  ### length 4, one for each set of coefficients
  ### each element will contain a matrix with B columns
  ### and two or three rows, depending on whether the 
  ### fit is for a line or a quadratic
  ### Return this list
  
  linear.model <- lm(y ~ x, data)
  linear.fits <- predict(linear.model, data)
  linear.residuals <- residuals(linear.model)
  linear.residual.resampling <- t(sapply(lapply(1:B, function(x) oneBoot(as.matrix(data), fit=cbind(linear.fits, linear.residuals), degree=1)), unlist))
  
  quadratic.model <- lm(y ~ x + I(x^2), data)
  quadratic.fits <- predict(quadratic.model, data)
  quadratic.residuals <- residuals(quadratic.model)
  quadratic.residual.resampling <- t(sapply(lapply(1:B, function(x) oneBoot(as.matrix(data), fit=cbind(quadratic.fits, quadratic.residuals), degree=2)), unlist))
  
  coeff <- list(linear.resampling, quadratic.resampling, linear.residual.resampling, quadratic.residual.resampling)
  
  return(coeff)
} 

bootPlot = function(x, y, coeff, trueCoeff){
  ### x and y are the original data
  ### coeff is a matrix from repBoot
  ### trueCoeff contains the true coefficients 
  ### that generated the data
  
  ### Make a scatter plot of data
  plot(x, y, pch=20)
  ### Add lines or curves for each row in coeff
  ### Use transparency
  ### You should use mapply to construct all 
  ### 1000 of the bootstrapped lines of best fit 
  ### Have a look at ?mapply for details.
  ### This can be done in ggplot2 or base graphics.
  color <- alpha('blue', 0.01)
  if (ncol(coeff) == 2) {
    mapply(function(a, b) abline(a, b, col=color), coeff[,1], coeff[,2])
  } else {
    mapply(function(a, b, c) curve(a + b*x + c*x^2, add=TRUE,  col=color), coeff[,1], coeff[,2], coeff[,3])
  }
  ### Use trueCoeff to add true line/curve - 
  ###  Make the true line/curve stand out
  color <- 'blue'
  width <- 5
  if (length(trueCoeff) == 2) {
    abline(trueCoeff[1], trueCoeff[2], col=color, lwd=width)
  } else {
    curve(trueCoeff[1] + trueCoeff[2]*x + trueCoeff[3]*x^2, add=TRUE, col=color, lwd=width)
  }
}


### Run your simulation by calling this function
### This function doesn't need any changing
runSim = function() {
  xUnique = 1:5
  trueCoeff = c(0, 1, 1)
  myData = getData(coefs = trueCoeff, xs = xUnique)
  expt = repBoot(data = myData)
  par(mfrow = c(2, 2))
  for (i in 1:4){
   bootPlot(myData$x, myData$y, 
            coeff = expt[[i]], trueCoeff) 
  }
  return(expt)
}

