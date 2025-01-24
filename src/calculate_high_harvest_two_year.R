harv.prop.init <- seq(0.5, 0.99, 0.10)
harv.prop.maint <- seq(0.1, 0.5, 0.10)
# harvest rate 200-280
harv.prop.consist <- seq(0.2, 0.99, 0.05) # sequence harvest/culling quotas, e.g remove 0.5-.99 porportion of founding pop

# define our quasi-extinction probability storage vector
min.med.n <- min.lo.n <- min.up.n <- rep(0, length(harv.prop.consist))
library(comprehenr)
library("ggplot2")
library("latex2exp")
library(popdyn)
source("R/feral_cat.R")
source("src/parameters_of_fertility_and_survival.R")
####################################################
## iterations and quasi ext for each following model
####################################################
iter <- 10000 # final model run at 10 000
itdiv <- iter / 1000 # final model rate at iter/1000
################################################################################################################
## untreated population
###############################################################################################################
## stochatic projection with density feedback
## set storage matrices & vectors

initial_population <- 1629
capacity <- Carry_Capacity$new()
coefficients <- capacity$coefficients_model(half_capacity = initial_population)
yr_now <- 2020 # update if more data available post-2010
yr_end <- 2030 # set projection end date
interval_time <- Interval_Time$new(initial_year = yr_now, final_year = yr_end)
number_year <- yr_end - yr_now + 1
n_sums_mat <- matrix(data = 0, nrow = iter, ncol = number_year)


# storage
minn.med.mat <- minn.lo.mat <- minn.up.mat <- pmin.med.mat <- pmin.lo.mat <- pmin.up.mat <- matrix(data = NA, ncol = length(harv.prop.maint), nrow = length(harv.prop.init)) # storage matrices

for (m in 1:length(harv.prop.maint)) {
  for (n in 1:length(harv.prop.init)) {
    # storage
    harvest <- c(rep(harv.prop.init[n], 2), rep(harv.prop.maint[m], 9))

    for (simulation in seq(1, iter)) {
      survival <- Stochastic_Survival_Fertility$new(fertility, survival_probability)
      survival$set_standard_desviations(std_fertility, std_survival_probability)
      population <- Population$new(survival)
      two_harvest <- Many_Harvest$new(harvest)
      simulator2 <- Runner_Population_With_CC_harvest$new(population, coefficients, two_harvest)
      simulator2$run_generations(interval_time, initial_population = initial_population)
      n_sums_mat[simulation, ] <- colSums(simulator2$n_mat) / initial_population
    } # end e loop (stochastic iterations)

    min.ppop.vec <- apply(n_sums_mat, MARGIN = 1, min, na.rm = T)
    pmin.med.mat[n, m] <- median(min.ppop.vec, na.rm = T)
    pmin.lo.mat[n, m] <- quantile(min.ppop.vec, probs = 0.025, na.rm = T)
    pmin.up.mat[n, m] <- quantile(min.ppop.vec, probs = 0.975, na.rm = T)
    print("##############################")
    print(paste("init harvest proportion = ", harv.prop.init[n], sep = ""))
    print("##############################")
  } # end n loop (initial harvest rate)

  print("##############################")
  print(paste("maint harvest proportion = ", harv.prop.maint[m], sep = ""))
  print("##############################")
} # end m loop (maintenance harvest rate)
twophase.up <- data.frame(pmin.med.mat)
colnames(twophase.up) <- harv.prop.maint
rownames(twophase.up) <- harv.prop.init

datos <- expand_grid(x = harv.prop.init, y = harv.prop.maint)
nx <- length(harv.prop.init)
ny <- length(harv.prop.maint)
datos$z <- to_vec(for (x in seq(1, nx)) for (y in seq(1, ny)) twophase.up[x, y])
write_csv(datos, "reports/tables/final_population_remaining_combinations_culling_scenarios.csv")
