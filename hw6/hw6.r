# Homework 6
# Stat 133, Lec 2, Spring 2015
# Due : Friday March 20th by 5 pm

# Review the slides on simulations for this assignment.

# Consider the following model on use of a new drug:
# We have a population of doctors, population size : <n.doctors>
# Every doctor has either adopted the use of a new drug, or not (0/1 status)
# Now write a function that runs a simulation for a period of :
# <n.days> where
# - every day exactly two _random_ doctors meet
# - if one has adopted but the other one has not then the
#   holdout adopts the use of the drug with probability p
# Return a matrix that shows for each day which doctors have adopted
# the use of the drug.

# Input varibles are
# <n.days> : the number of days the simulation should be run for
# <n.doctors> : total number of doctors 
# <initial.doctors> : a 0/1 vector of length <n.doctors>, 1 for adopters
# <p> : the probability that the non-adopter adopts the drug.

# Ouput variable
# <has_adopted> : matrix with <n.doctors> rows and <n.days> columns
#                 i.e. one row for each doctor
#                 the entries are 0 for days where the doctor is a
#                 non-adopter, else 1 (so once a row turns to 1 it stays as 1).

sim.doctors <- function(initial.doctors, n.doctors, n.days, p){

  # Set up the output variable, define it as a matrix then use initial.doctors
  # to set the first column (day)
  has_adopted<- matrix(nrow = n.doctors, ncol = n.days)
  r <- n.doctors
  c <- n.days
  for (n in 1 : r){
    has_adopted[n, 1]<- initial.doctors[n]
  }
  for (i in 2: c) {
    doctor <- sample(1:r, 2, replace = F)
    doctor1 <- doctor[1]
    doctor2 <- doctor[2]
    for (j in 1 : r){
      has_adopted[j, i] <- has_adopted[j, (i - 1)]
      }
    if (has_adopted[doctor1, (i - 1)] != has_adopted[doctor2, (i - 1)]) {
      if (has_adopted[doctor1, (i - 1)] == 0) {
        has_adopted[doctor1, i] <- sample(c(0,1), 1, prob = c (1 - p, p))
      }
      else {
        has_adopted[doctor2, i] <- sample(c(0,1), 1, prob = c (1-p, p))
      }
    }
  }
  return (has_adopted)
}
  
  # Run a simulation for <n.days> (use a for loop).  In the loop:
  # 1) pick two random doctors
  # 2) check if one has adopted the other hasn't
  # 3) convert the non-adopter with probability p

  # return the output


# When you test your function you have to generate <initial.doctors> and
# pick values for the other input parameters.

#test1
days1 <- 2
doctor1 <- 4
initial.doctor1 <- c(1, 0, 1, 0)
p1 <- 0.4
sim.doctors(initial.doctor1, doctor1, days1, p1)

#test2
days2 <- 6
doctor2 <- 8
initial.doctor2 <- c(1, 0, 1, 0, 0, 0, 0, 1)
p2 <- 0.8
sim.doctors(initial.doctor2, doctor2, days2, p2)

#test3
days3 <- 20
doctor3 <- 8
initial.doctor3 <- c(1, 0, 1, 0, 0, 0, 0, 1)
p3 <- 0.8
sim.doctors(initial.doctor3, doctor3, days3, p3)

set.seed(42)
# Generate a value for <initial.doctors> that has 10% 1s and 90% 0s.
# Run your function for at least 5 different values of <p> and plot
# on x-axis: days,
# on y-axis : the number of doctors that have already adopted the drug, on that day
# Put all 5 lines in one figure (e.g. use first plot() then lines() for the subsequent lines)
initial.doctor <- sample(c(0,1), 30, replace = TRUE, prob = c(0.9, 0.1))
number_of_doctor <- function(days, result_matrix){
  result <- c()
  for (i in 1 : days){
    result <- append(result, sum(result_matrix[, i]), i)
  }
  return (result)
}
p1 <- 0.2
result1<- sim.doctors(initial.doctor, 30, 100, p1)
num_of_doc1 <- number_of_doctor(100, result1)
print(num_of_doc1)
plot(x <- 1: 100, y <- num_of_doc1, xlab = "days", ylim = c(1, 30), ylab = "num of doctors", type = "l")

p2 <- 0.4
result2 <- sim.doctors(initial.doctor, 30, 100, p2)
num_of_doc2 <- number_of_doctor(100, result2)
print(num_of_doc2)
lines(x <- 1: 100, y <- num_of_doc2,type = "l", col = "red")

p3 <- 0.6
result3 <- sim.doctors(initial.doctor, 30, 100, p3)
num_of_doc3 <- number_of_doctor(100, result3)
lines(x <- 1: 100, y <- num_of_doc3, type = "l", col = "blue")

p4 <- 0.8
result4 <- sim.doctors(initial.doctor, 30, 100, p4)
num_of_doc4 <- number_of_doctor(100, result4)
lines(x <- 1: 100, y <- num_of_doc4, type = "l", col = "green")

p5 <- 1
result5 <- sim.doctors(initial.doctor, 30, 100, p5)
num_of_doc5 <- number_of_doctor(100, result5)
lines(x <- 1: 100, y <- num_of_doc5, type = "l", col = "yellow")
