# Set working directory ----------------------------

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read csv -----------------------------------------

data.demo.raw <- read.csv("data.csv", stringsAsFactors = FALSE)

# Preview data -------------------------------------

head(data.demo.raw, 10)
