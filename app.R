library(plumber)
library(readxl)

pr <- plumb("api.R")
port <- as.numeric(Sys.getenv("PORT", unset = 8000))
pr$run(host = "0.0.0.0", port = port)
