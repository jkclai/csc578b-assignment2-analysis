# Compute two way anova ---------------------------

res.aov.time <- anova_test(
  data = data.demo.time, dv = time, wid = pid,
  within = c(mode, n_)
)
get_anova_table(res.aov.time)

# Post-hoc test -----------------------------------

### if significant two-way interaction

oneway1.time <- data.demo.time %>%
  group_by(n_) %>%
  anova_test(dv = time, wid = pid, within = mode) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
oneway1.time

pwc1.time <- data.demo.time %>%
  group_by(n_) %>%
  pairwise_t_test(
    time ~ mode, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1.time

oneway2.time <- data.demo.time %>%
  group_by(mode) %>%
  anova_test(dv = time, wid = pid, within = n_) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
oneway2.time

pwc2.time <- data.demo.time %>%
  group_by(mode) %>%
  pairwise_t_test(
    time ~ n_, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2.time

### if no significant two-way interaction

data.demo.time %>%
  pairwise_t_test(
    time ~ mode, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

data.demo.time %>%
  pairwise_t_test(
    time ~ n_, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
