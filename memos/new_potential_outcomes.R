
pop <- declare_population(individuals = list(noise = declare_variable()),
                          villages = list(),
                          N_per_level = c(1000, 10), super_population = FALSE)

po <- declare_potential_outcomes(formula = Y ~ .01 + .1*Z, condition_names = c(0, 1),
                                 treatment_variable = "Z")

smp <- declare_sampling(prob = .1, strata_variable_name = "villages_ID")

assignment <- declare_assignment(condition_names = c(0, 1))

estimator <- declare_estimator(formula = Y ~ Z, treatment_variable = "Z",
                                estimates = difference_in_means)

pop_draw <- draw_population(population = pop, potential_outcomes = po)

smp_draw <- draw_sample(data = pop_draw, sampling = smp)

smp_draw <- assign_treatment(data = smp_draw, assignment = assignment)

smp_draw <- draw_outcome(data = smp_draw, potential_outcomes = po)


library(magrittr)
draw_population(population = pop, potential_outcomes = po) %>%
  draw_sample(sampling = smp) %>%
  assign_treatment(assignment = assignment) %>%
  draw_outcome(potential_outcomes = po) %>%
  head()

sims <- diagnose(potential_outcomes = po, 
                 sample =  smp, 
                 assignment = assignment, 
                 analysis = analysis_1, 
                 sims = 100)
summary(sims)
