# Compute two way anova ---------------------------

res.aov.error <- anova_test(
  data = data.demo.error, dv = error, wid = pid,
  within = c(mode, n_)
)
get_anova_table(res.aov.error)

# Post-hoc test -----------------------------------

### if significant two-way interaction

oneway1.error <- data.demo.error %>%
  group_by(n_) %>%
  anova_test(dv = error, wid = pid, within = mode) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
oneway1.error

pwc1.error <- data.demo.error %>%
  group_by(n_) %>%
  pairwise_t_test(
    error ~ mode, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc1.error

oneway2.error <- data.demo.error %>%
  group_by(mode) %>%
  anova_test(dv = error, wid = pid, within = n_) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
oneway2.error

pwc2.error <- data.demo.error %>%
  group_by(mode) %>%
  pairwise_t_test(
    error ~ n_, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc2.error

### if no significant two-way interaction

data.demo.error %>%
  pairwise_t_test(
    error ~ mode, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )

data.demo.error %>%
  pairwise_t_test(
    error ~ n_, paired = TRUE, 
    p.adjust.method = "bonferroni"
  )
