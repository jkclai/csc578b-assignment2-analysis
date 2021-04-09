# Initialize demo data frame for manipulation -----

data.demo.transformed <- as.data.frame(matrix(nrow = nlevels(data.demo.cleaned$pid) * nlevels(data.demo.cleaned$mode), ncol = 0))

# Append columns ----------------------------------

data.demo.transformed[c("id", "pid", "mode", "error.n3", "time.n3", "error.n5", "time.n5", "error.n9", "time.n9", "error.n25", "time.n25")] <- NA

# Input independent variables ---------------------

data.demo.transformed$pid <- as.factor(sort(rep(levels(data.demo.cleaned$pid), nlevels(data.demo.cleaned$mode))))
data.demo.transformed$mode <- as.factor(rep(levels(data.demo.cleaned$mode), nlevels(data.demo.cleaned$pid)))

# Input dependent variables -----------------------

for(i in levels(data.demo.transformed$pid)) {
  for(j in levels(data.demo.transformed$mode)) {
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$error.n3 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 3, ]$error)
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$time.n3 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 3, ]$time)
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$error.n5 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 5, ]$error)
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$time.n5 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 5, ]$time)
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$error.n9 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 9, ]$error)
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$time.n9 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 9, ]$time)
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$error.n25 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 25, ]$error)
    data.demo.transformed[data.demo.transformed$pid == i & data.demo.transformed$mode == j, ]$time.n25 <- mean(data.demo.cleaned[data.demo.cleaned$pid == i & data.demo.cleaned$mode == j & data.demo.cleaned$n_ == 25, ]$time)
  }
}

# Sort data ---------------------------------------

data.demo.transformed <- data.demo.transformed[order(data.demo.transformed$pid, data.demo.transformed$mode), ]

# Append IDs --------------------------------------

data.demo.transformed$id <- 1:nrow(data.demo.transformed)

# Remove objects in environment -------------------

remove(data.demo.cleaned)
remove(i)
remove(j)

# Preview data ------------------------------------

head(data.demo.transformed, 10)
