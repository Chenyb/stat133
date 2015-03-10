#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r,c,p){
  m<-matrix(sample(c(0,1,2), r*c, prob = c(1-p, p/2, p/2), replace = T), nrow = r)
  print(m)
  return (m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.

bml.seteast<- function(m){
  r <- nrow(m)
 c <- ncol(m)
 if(ncol(m)==1) {
     return(m)
   }
 for (i in 1 : r){
   for (j in 1 : c) {
     if (j == c){
       if (m[i, j] == 1 & m[i, 1] == 0) {
         m[i, j] <- 0  
         m[i, 1] <- 1
       }
     } else if (m[i, j] == 1 & m[i, j + 1] == 0) {
       m[i, j] <- 0  
       m[i, j + 1] <- 1
     }
   }
 }
  return (m)
}
  
bml.setnorth<- function(m){
  if(nrow(m)==1) {
   return(m)
 }
 r <- nrow(m)
 c <- ncol(m)
 for (i in 1 : r){
   for (j in 1 : c) {
     if (i == 1){
       if (m[i, j ] == 2 && m[r, j] == 0) {
         m[i, j] <- 0 
         m[r, j] <- 2
       }
     } else if (m[i, j] == 2 && m[i - 1, j] == 0) {
       m[i, j] <- 0 
       m[i -1, j] <- 2
     }  
   }
 }
 return (m)
}

bml.step <- function(m){
  m.new <- bml.setnorth(bml.seteast(m))
  return(list(m.new, !all(m==m.new)))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  m = bml.init(r, c, p)
  image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
  for (i in 1:2000) {
    n = bml.step(m)
    if (n[[2]] == TRUE) {
      m = n[[1]]
      image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
    } else {
      image(t(apply(m,2,rev)), axes = FALSE, col = c("white", "red", "blue"))
      return (list(i, TRUE))
    }
  }
  return (list(i, FALSE))
}
