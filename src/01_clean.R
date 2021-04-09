library(jsonlite)

vectorize_fromJSON <- Vectorize(fromJSON)

# Initialize demo data frame for manipulation -----

data.demo.cleaned <- as.data.frame(matrix(nrow = nrow(data.demo.raw), ncol = 0))

# Append ID column --------------------------------

data.demo.cleaned$id <- NA

# Clean -------------------------------------------

# Identifier
data.demo.cleaned$pid <- as.factor(gsub('^.|.$', '', data.demo.raw$pid))

# Independent variable
data.demo.cleaned$mode <- as.factor(gsub('^.|.$', '', data.demo.raw$mode))

# Independent variable
data.demo.cleaned$n_ <- as.factor(ifelse(data.demo.raw$n == 0, 3, ifelse(data.demo.raw$n == 1, 5, ifelse(data.demo.raw$n == 2, 9, 25))))

data.demo.cleaned$r_ <- as.factor(data.demo.raw$r)

temp.values <- unname(vectorize_fromJSON(data.demo.raw$values))
temp.correctValue <- unlist(lapply(temp.values, median), use.names = FALSE)
temp.selectedValue <- data.demo.raw$selectedValue

# Dependent variable
data.demo.cleaned$error <- abs((temp.correctValue - temp.selectedValue) / (99 - 0))

# Dependent variable
data.demo.cleaned$time <- data.demo.raw$time

temp.cursor <- unname(vectorize_fromJSON(data.demo.raw$cursor))

# Sort data ---------------------------------------

data.demo.cleaned <- data.demo.cleaned[order(data.demo.cleaned$pid, data.demo.cleaned$mode), ]

# Append IDs --------------------------------------

data.demo.cleaned$id <- 1:nrow(data.demo.cleaned)

# Remove objects in environment -------------------

#remove(data.demo.raw)
remove(temp.cursor)
remove(temp.values)
remove(temp.correctValue)
remove(temp.selectedValue)
remove(vectorize_fromJSON)

# Preview data ------------------------------------

head(data.demo.cleaned, 10)
