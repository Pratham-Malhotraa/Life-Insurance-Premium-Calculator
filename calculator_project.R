library(readxl)

# Loading and cleaning the data
mortality_data <- readRDS("mortality_data.rds")

colnames(mortality_data) <- c("age", "qx", "lx", "Ax")

# Remove all extra spaces and convert to numeric safely
clean_numeric <- function(x) as.numeric(gsub(" ", "", x))

mortality_data$age <- clean_numeric(mortality_data$age)
mortality_data$qx  <- clean_numeric(mortality_data$qx)
mortality_data$lx  <- clean_numeric(mortality_data$lx)
mortality_data$Ax  <- clean_numeric(mortality_data$Ax)

# Constants
interest <- 0.04
v <- 1 / (1 + interest)
D <- interest / (1 + interest)

# Functions
Ax <- function(age) {
  val <- mortality_data$Ax[mortality_data$age == age]
  if (length(val) == 0) return(NA)
  return(val)
}

npx <- function(age, n) {
  lx_current <- mortality_data$lx[mortality_data$age == age]
  lx_future  <- mortality_data$lx[mortality_data$age == age + n]
  if (length(lx_current) == 0 || length(lx_future) == 0 || lx_current == 0) return(NA)
  return(lx_future / lx_current)
}
expenseTA <-2000 #expense is assumed for the company
expenseWL <- 3000#expense is assumed for the company
expensePE <- 4000#expense is assumed for the company
expenseEA <- 6000#expense is assumed for the company

calculate_term_premium <- function(age, term, sum_assured) {
  Ax <- Ax(age)
  Ax_later <- Ax(age + term)
  npx <- npx(age, term)
  
  TAxn <- Ax - (v^term) * npx * Ax_later
  adue <- (1 - TAxn) / D
  premium <- (sum_assured * TAxn + expenseTA*adue) / adue
  

  return(round(premium,3))
}



calculate_wholelife_premium <- function(age, sum_assured) {
  Ax <- Ax(age)
  
  
  aduex <- (1 - Ax) / D
  premium <- (sum_assured * Ax + expenseWL*aduex) / aduex
  
  
  return(round(premium,3))
}


calculate_endowment_premium <- (function(age, term, sum_assured) {
  Ax <- Ax(age)
  Ax_later <- Ax(age + term)
  npx <- npx(age,term)
  
  EAxn <- (Ax - (v^term) * npx * Ax_later)+(v^term)*npx
  adue <- (1 - (Ax - (v^term) * npx * Ax_later)) / D
  premium <- (sum_assured * EAxn + expenseEA*adue) / adue
  
  
  return(round(premium,3))
})

calculate_pure_premium <- function(age, term, sum_assured) {
  Ax <- Ax(age)
  Ax_later <- Ax(age + term)
  npx <- npx(age, term)
  
  PAxn <- (v^term) * npx 
  adue <- (1 - (Ax - (v^term) * npx * Ax_later)) / D
  premium <- (sum_assured * PAxn + expensePE*adue) / adue
  
  
  return(round(premium,3))
}
#the code for the calculator is now complete
#you can calculate your premium here. please put your details in the below mentioned format

#put your details here -> calculate_wholelife_premium(your age,sum assured)
calculate_wholelife_premium(30,100000) 

#put your details here -> calculate_pure_premium(your age,polcy term ,sum assured)
calculate_pure_premium(30,20,1000000)

#put your details here ->calculate_endowment_premium(age,policy term,sum assured)
calculate_endowment_premium(30,20,1000000)

#calculate_term_premium(age,policy term,sum assured)
calculate_term_premium(30,20,1000000)   

#please note this calculated premium is a final quote, it is just
#to provide you with an estimate 