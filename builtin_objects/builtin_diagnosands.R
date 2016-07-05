# built-in diagnosands ----------------------------------------------------

bias <- declare_diagnosand(diagnostic_statistic_text = "est - estimand", summary_function = mean, label = "bias")
rmse <- declare_diagnosand(diagnostic_statistic_text = "(est - estimand)^2", summary_function = function(x) sqrt(mean(x)), label = "RMSE")
power <- declare_diagnosand(diagnostic_statistic_text = "p < .05", summary_function = mean, label = "power")
coverage <- declare_diagnosand(diagnostic_statistic_text = "est <= ci_upper & est >= ci_lower", summary_function = mean, label = "coverage")
sd_estimate <- declare_diagnosand(diagnostic_statistic_text = "est", summary_function = sd, label = "sd(estimate)")
type_s_rate <- declare_diagnosand(diagnostic_statistic_text = "sign(est) != sign(estimand)", summary_function = mean, label = "type S rate")


save(bias,rmse,power,coverage,sd_estimate,type_s_rate,file = "data/diagnosands.rda")
