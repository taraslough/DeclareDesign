
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
  
  # Using objects from the global environment
  
  my_function <- function(x) x + 10
  a <- 1000
  
  global_objects <- declare_population(
    income = "my_function(rnorm(n_))",
    age = "rpois(10,30)",
    a = "a",
    size = 10
  ) 
  
  global_objects$population()
  
  a <- 500
  
  global_objects$population()
  
  # testing user-provided data with transformations
  
  my_data <- declare_population(
    individual = list(
      income = "rnorm(n_)"
    ),
    village = list(
      development = "rnorm(n_)"
    ),
    region = list(
      reg_dev = "rnorm(n_)"
    ),
    size = c(1000,100,10)
  )$population()
  
  user_data_pop <- declare_population(
    individual = list(
      age = "rpois(n = n_,lambda = 30)"
    ),
    village = list(
      village_school = "rbinom(n = n_,size = 1,prob = .3)"
    ),
    region = list(),
    level_IDs = c("individual_ID","village_ID","region_ID"),
    global_transformations = list(
      income_Z = "scale(income)",
      dist_max_dev = "development - max(development)"
    ),
    data = my_data
  )
  
  head(user_data_pop$population())
  
  # Testing declare_variable()
  
  # See available variable types: 
  get_variable_types()
  
  # Continuous variables
  declare_population(
    stan_normal = declare_variable(),
    normal1 = declare_variable(location_scale = c(0,10)),
    normal2 = declare_variable(type = "continuous",
                               location_scale = c(0,10)),
    # Case insensitive
    normal3 = declare_variable(type = "CoNTInUOuS",
                               location_scale = c(0,10)),
    normal4 = declare_variable(type = "Normal",
                               # Doesn't require scale, defaults to scale = mean/2
                               location_scale = 50),
    stan_unif = declare_variable(type = "uniform"),
    uniform100 = declare_variable(type = "uniform",min_max = c(-100,100)),
    size = 20
  )$population()
  
  # Categorical variables
  declare_population(
    multinom1 = declare_variable("multinomial"),
    multinom2 = declare_variable("categorical"),
    multinom3 = declare_variable("categorical",probabilities = c(.25,0,0,.75)),
    multinom4 = declare_variable("categorical",outcome_categories = c("happy","sad")),
    multinom5 = declare_variable("categorical",
                                 outcome_categories = c("happy","sad"),
                                 probabilities = c(.2,.8)
                                 ),
    race = declare_variable("race"),
    US_party = declare_variable("US_party"),
    us_party = declare_variable("us_party",probabilities = c(.4,.4,.2)),
    size = 20
  )$population()
  
  # Binary variables
  declare_population(
    binomial = declare_variable("binomial"),
    binary1 = declare_variable("binary"),
    binary2 = declare_variable("binary",probabilities = .8),
    gender = declare_variable("gender"),
    hotcold = declare_variable("BINOMIAL",outcome_categories = c("hot","cold")),
    hotcold2 = declare_variable("binary",
                                outcome_categories = c("hot","cold"),
                                probabilities = .9),
    size = 20
  )$population()
  
  # Count variables
  declare_population(
   poisson = declare_variable("poisson"),
   poisson2 = declare_variable("poisson",location_scale = 500),
   count = declare_variable("count",location_scale = 20),
   gamma = declare_variable("gamma",location_scale = 20),
   gamma2 = declare_variable("gamma",location_scale = c(20,5)),
   age = declare_variable("age",location_scale = 30),
   events = declare_variable("events"),
   size = 20
  )$population()
  
  # Rate / proportion variables
  # uses reparameterization of beta
  declare_variable("rate",location_scale = c(.5,.01))
  declare_variable("rate",location_scale = c(.1,.01))
  # Informative error:
  declare_variable("rate",location_scale = c(.1,10))
  
  declare_population(
    beta = declare_variable("beta"),
    beta1 = declare_variable("beta",location_scale = .3),
    # These are just synonynms for beta:
    proportion = declare_variable("proportion"),
    ratio = declare_variable("ratio"),
    rate = declare_variable("rate"),
    elections = declare_variable("percentage"),
    comp_elections = declare_variable("percentage",location_scale = c(.5,.00001)),
    # You could use this for RD designs
    RD_forcing_var = "round(comp_elections - .5,2)",
    size = 20
  )$population()
  
})







