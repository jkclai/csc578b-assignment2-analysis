# Outliers check ----------------------------------

data.demo.error %>%
  identify_outliers(error)
data.demo.time %>%
  identify_outliers(time)

#data.demo.error <- data.demo.error[!data.demo.error$id %in% identify_outliers(data.demo.error, error)$id, ]
#data.demo.time <- data.demo.time[!data.demo.time$id %in% identify_outliers(data.demo.time, time)$id, ]

# Normal distribution check -----------------------

data.demo.error %>%
  group_by(mode, n_) %>%
  shapiro_test(error)

data.demo.time %>%
  group_by(mode, n_) %>%
  shapiro_test(time)

### if shapiro test returns "Problem with `mutate()` input `data`." error, use this ###
data.demo.error[c(), c("error")] <- .Machine$double.xmin

data.demo.error %>%
  group_by(mode, n_) %>%
  shapiro_test(error)

data.demo.error[c(), c("error")] <- 0.000000000

# Homogeneity of variances check ------------------

data.demo.error %>%
  levene_test(error ~ n_ * mode)

data.demo.time %>%
  levene_test(time ~ n_ * mode)

# Additional checks -------------------------------

data.demo.error.means <- aggregate(data.demo.error$error, by = list(data.demo.error$mode, data.demo.error$n_), FUN = mean)
data.demo.time.means <- aggregate(data.demo.time$time, by = list(data.demo.time$mode, data.demo.time$n_), FUN = mean)

interaction.plot(x.factor = data.demo.error.means[, 2], trace.factor = data.demo.error.means[, 1], response = data.demo.error.means$x, trace.label = "mode", xlab = "n", ylab = "error")
interaction.plot(x.factor = data.demo.time.means[, 2], trace.factor = data.demo.time.means[, 1], response = data.demo.time.means$x, trace.label = "mode", xlab = "n", ylab = "time")

ggqqplot(data.demo.error, "error", ggtheme = theme_bw()) +
  facet_grid(n_ ~ mode, labeller = "label_both", scales = "free")

ggqqplot(data.demo.time, "time", ggtheme = theme_bw()) +
  facet_grid(n_ ~ mode, labeller = "label_both", scales = "free")

data.demo.error.lm.two <- lm(error ~ mode * n_, data = data.demo.error)
data.demo.time.lm.two <- lm(time ~ mode * n_, data = data.demo.time)

plot(data.demo.error.lm.two, 2)
plot(data.demo.time.lm.two, 2)

plot(data.demo.error.lm.two, 1)
plot(data.demo.time.lm.two, 1)

### DO NOT USE ###

isFirst <- TRUE
minVar <- 0
maxVar <- 0
for(i in levels(data.demo.error$mode)) {
  for(j in levels(data.demo.error$n_)) {
    tempVar <- var(data.demo.error[data.demo.error$mode == i & data.demo.error$n_ == j, ]$error)
    
    if(isFirst == TRUE) {
      isFirst <- FALSE
      minVar <- tempVar
      maxVar <- tempVar
    }
    else {
      if(tempVar < minVar) {
        minVar <- tempVar
      }
      if(tempVar > maxVar) {
        maxVar <- tempVar
      }
    }
  }
}
evROT.error <- maxVar/minVar

isFirst <- TRUE
minVar <- 0
maxVar <- 0
for(i in levels(data.demo.time$mode)) {
  for(j in levels(data.demo.time$n_)) {
    tempVar <- var(data.demo.time[data.demo.time$mode == i & data.demo.time$n_ == j, ]$time)
    
    if(isFirst == TRUE) {
      isFirst <- FALSE
      minVar <- tempVar
      maxVar <- tempVar
    }
    else {
      if(tempVar < minVar) {
        minVar <- tempVar
      }
      if(tempVar > maxVar) {
        maxVar <- tempVar
      }
    }
  }
}
evROT.time <- maxVar/minVar

cat("error evROT: ", evROT.error, "\n")
cat("time evROT: ", evROT.time, "\n")
