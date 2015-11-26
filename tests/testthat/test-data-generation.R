
rm(list=ls())
library(testthat)
library(DeclareDesign)

context("generating data with declare_population")

test_that("test data generation functions", {
  
  # Simple 1-level cases
  
  one_lev1 <- declare_population(
    income = "rnorm(n_)",
    age = "rpois(10,30)",
    size = 10
  ) 
  one_lev2 <- declare_population(
    list(income = "rnorm(n_)",
         age = "rpois(10,30)"),
    size = 10
  )
  one_lev3 <- declare_population(
    level_A = list(income = "rnorm(n_)",
                   age = "rpois(10,30)"),
    size = 10
  )
  
  draw_population(one_lev1)
  draw_population(one_lev2)
  draw_population(one_lev3)
  
  # Changing size
  one_lev1$population(size = 20)
  one_lev2$population(size = 20)
  one_lev3$population(size = 20)
  
  # Simple multi-level cases
  multi_lev1 <- declare_population(
    indiv = list(
      income = "rnorm(n_)",
      age = "rpois(n_,30)"
    ),
    city = list(
      city_educ_mean = "rnorm(n = n_, mean = 100, sd = 10)",
      city_educ_sd = "rgamma(n = n_, shape = 2, rate = 2)"
    ),
    region = list(),
    make_unique_ID = T,
    size = c(10,5,2)
  ) 
  # With transformations within levels
  multi_lev2 <- declare_population(
    indiv = list(
      income = "rnorm(n_)",
      age = "rpois(n_,30)",
      income_times_age = "income*age"
    ),
    city = list(
      city_educ_mean = "rnorm(n = n_, mean = 100, sd = 10)",
      city_educ_sd = "rgamma(n = n_, shape = 2, rate = 2)",
      city_educ_zscore = "scale(city_educ_mean)"
    ),
    region = list(),
    make_unique_ID = T,
    size = c(10,5,2)
  ) 
  # With global transformations of all variables 
  multi_lev3 <- declare_population(
    indiv = list(
      income = "rnorm(n_)",
      age = "rpois(n_,30)",
      income_times_age = "income*age"
    ),
    city = list(
      city_educ_mean = "rnorm(n = n_, mean = 100, sd = 10)",
      city_educ_sd = "rgamma(n = n_, shape = 2, rate = 2)",
      city_educ_zscore = "scale(city_educ_mean)"
    ),
    region = list(),
    global_transformations = list(
      ml_educ = "rnorm(n_, mean = city_educ_mean, sd = city_educ_sd)"
    ),
    make_unique_ID = T,
    size = c(10,5,2)
  ) 
  multi_lev1$population()
  multi_lev2$population()
  multi_lev3$population()
  
  # Changing size
  head(multi_lev1$population(size = c(100,20,5)))
  # Heterogeneous city and region sizes
  het_sizes <- multi_lev2$population(
    size = list(
      # Note that 1's are no longer necessary
      cities = c(3,4,4,5,6,7,9),
      regions = c(3,4)
    ))
  table(het_sizes$city_ID,het_sizes$region_ID)
  # We can see that the individual-level draws are close to the higher level
  ml_vars <- multi_lev3$population(size = c(10^5,10^3,10))
  
  mean_of_individuals <- tapply(X = ml_vars$ml_educ,
                                INDEX = ml_vars$city_ID,
                                FUN = mean)
  mean_of_cities <- unique(ml_vars$city_educ_mean)
  
  plot(mean_of_individuals,
       mean_of_cities)
  abline(coef = c(0,1), col = "red")
  
  sd_of_individuals <- tapply(X = ml_vars$ml_educ,
                              INDEX = ml_vars$city_ID,
                              FUN = sd)
  
  sd_of_cities <- unique(ml_vars$city_educ_sd)
  
  plot(sd_of_individuals,
       sd_of_cities)
  abline(coef = c(0,1), col = "red")
  
  # Now incorporating arguments that vary
  original_args <- list(
    age_lambda = 30,
    city_educ_mu = 100,
    city_sigma = 10,
    city_educ_shape = 2,
    city_educ_rate = 2,
    party_vector = LETTERS[1:4]
  )
  
  multi_lev4 <- declare_population(
    indiv = list(
      income = "rnorm(n_)",
      age = "rpois(n_,age_lambda)",
      income_times_age = "income*age",
      party_ID = "sample(x = party_vector,size = n_,replace = TRUE)"
    ),
    city = list(
      city_educ_mean = "rnorm(n = n_, mean = city_educ_mu, sd = city_sigma)",
      city_educ_sd = "rgamma(n = n_, shape = city_educ_shape, rate = city_educ_rate)",
      city_educ_zscore = "scale(city_educ_mean)"
    ),
    region = list(),
    global_transformations = list(
      ml_educ = "rnorm(n_, mean = city_educ_mean, sd = city_educ_sd)"
    ),
    make_unique_ID = T,
    other_arguments = original_args,
    size = c(10,5,2)
  ) 
  # Data with original arguments
  multi_lev4$population()
  
  # Changing the original arguments
  new_args <- original_args
  new_args$party_vector <- c("Dem","Rep","Ind")
  new_args$age_lambda <- 20
  new_args$city_educ_shape <- 5
  
  multi_lev4$population(other_arguments = new_args)
  
  # Changing arguments and size together
  multi_lev4$population(
    size = c(12,6,1),
    other_arguments = new_args)
  
  # Bringing in data from other datasets with respect to levels
  original_args$user_data <- multi_lev4$population(size = c(100,30,10))
  
  names(original_args$user_data)
  
  grab_vars <- declare_population(
    individuals = list(
      income = "rnorm(n_)",
      happiness = "rpois(n_,3)"
    ),
    cities = list(
      # Here we just grab a variable that does not vary at city level
      city_educ_mean = get_variable(level_ID = "city_ID",
                                      variable_name = "city_educ_mean",
                                      data = user_data),
      city_educ_sd = get_variable(level_ID = "city_ID",
                                    variable_name = "city_educ_sd",
                                    data = user_data),
      # Here we make a new variable by aggregating individuals to cities
      city_income_mean = get_variable(level_ID = "city_ID",
                                           variable_name = "income",
                                           data = user_data,
                                           aggregate_function = "mean"
      ),
      city_income_sd = get_variable(level_ID = "city_ID",
                                         variable_name = "income",
                                         data = user_data,
                                         aggregate_function = "sd"
      )
    ),
    countries = list(),
    global_transformations = list(
      ml_income = "rnorm(n_,mean = city_income_mean,sd = city_income_sd)"
        ),
    # The sizes are the same as the user data, this is a constraint for now
    size = c(100,30,10),
    other_arguments = original_args
  )
  grab_vars$population()
  
  
  # Fixed weird bug
  
  population <- declare_population(
    individuals = list(noise = "rnorm(n_)"
    ),
    villages = list(elevation = "rnorm(n_)"
    ), 
    make_unique_ID = TRUE,
    size = c(1000, 100))
  
  table(population$population()$villages_ID)

})

















