# Load plumber
library(plumber)
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

# Load your calculator code (must be in same folder)
source("calculator_project.R")
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "*")
  plumber::forward()
}

#* @apiTitle Life Insurance Premium API

#* Calculate Premium
#* @param age Age of the policyholder
#* @param term Policy term (years); optional for whole life
#* @param sum_assured Sum assured in rupees
#* @param product Type of product: Term Assurance, Endowment Assurance, Pure Endowment, Whole Life
#* @get /calculate
function(age, term = 0, sum_assured, product = "term") {
  age <- as.numeric(age)
  term <- as.numeric(term)
  sum_assured <- as.numeric(sum_assured)
  
  premium <- switch(
    product,
    term = calculate_term_premium(age, term, sum_assured),
    endowment = calculate_endowment_premium(age, term, sum_assured),
    pure_endowment = calculate_pure_premium(age, term, sum_assured),
    wholelife = calculate_wholelife_premium(age, sum_assured),
    NA
  )
  
  if (is.na(premium)) {
    return(list(error = "❌ Unsupported product type. Use: term, endowment, pure_endowment, or wholelife."))
  } else {
    return(list(
      age = age,
      term = ifelse(product == "wholelife", "Lifetime", term),
      sum_assured = sum_assured,
      product = product,
      premium = round(premium, 2)
    ))
  }
}

