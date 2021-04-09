library(ggpubr)
library(rstatix)
library(tidyverse)

# Tidy data ---------------------------------------

data.demo.error <- data.demo.transformed[c("id", "pid", "mode", "error.n3", "error.n5", "error.n9", "error.n25")] %>%
  gather(key = "n_", value = "error", error.n3, error.n5, error.n9, error.n25)

data.demo.error$n_ <- as.numeric(gsub("error.n", "", data.demo.error$n_))

data.demo.error <- data.demo.error %>%
  convert_as_factor(id, n_)

data.demo.time <- data.demo.transformed[c("id", "pid", "mode", "time.n3", "time.n5", "time.n9", "time.n25")] %>%
  gather(key = "n_", value = "time", time.n3, time.n5, time.n9, time.n25)

data.demo.time$n_ <- as.numeric(gsub("time.n", "", data.demo.time$n_))

data.demo.time <- data.demo.time %>%
  convert_as_factor(id, n_)

# Plot time column --------------------------------

plot(data.demo.time$n_, data.demo.time$time)

### if exponentially growing trend is observed, log transform the time column ###
data.demo.time$time <- log(data.demo.time$time)

# Group data --------------------------------------

data.demo.error %>%
  group_by(mode, n_) %>%
  get_summary_stats(error, type = "mean_sd")

data.demo.time %>%
  group_by(mode, n_) %>%
  get_summary_stats(time, type = "mean_sd")

# Plot data ---------------------------------------

bxp.error <- ggboxplot(
  data.demo.error, x = "n_", y = "error",
  color = "mode", palette = "jco"
)
bxp.error

bxp.time <- ggboxplot(
  data.demo.time, x = "n_", y = "time",
  color = "mode", palette = "jco"
)
bxp.time

line.error <- ggline(
  data.demo.error, x = "n_", y = "error", color = "mode", add = c("mean_se", "dotplot"), palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600")
)
line.error

line.time <- ggline(
  data.demo.time, x = "n_", y = "time", color = "mode", add = c("mean_se", "dotplot"), palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600")
)
line.time

# Remove objects in environment -------------------

remove(data.demo.transformed)
