source("src/parameters_of_fertility_and_survival.R")
initial_population <- 1629
source("src/data_of_cost.R")
source("src/fit_model_efficiency.R")

####################################################
## iterations and quasi ext for each following model
####################################################
iter <- 10000 # final model run at 10 000
itdiv <- iter / 1000 # final model rate at iter/1000

## run choices
## make up shortfall in kill by ...
# shortfall.method <- "F" # adding Felixer units
shortfall.method <- "T" # adding traps
# shortfall.method <- "H" # increasing hunting pressure

# harvest rate
harv.prop.init <- seq(0.5, 0.9, 0.05)
harv.prop.maint <- seq(0.1, 0.5, 0.05)
q.ext <- 20

# storage
qext.mat <- minn.med.mat <- minn.lo.mat <- minn.up.mat <- pmin.med.mat <- pmin.lo.mat <- pmin.up.mat <- totcost.med <- totcost.lo <- totcost.up <- matrix(data = NA, ncol = length(harv.prop.maint), nrow = length(harv.prop.init))

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


      init.k.sums.mat[e, ] <- as.vector(colSums(init.k.mat))
      k.sums.mat[e, ] <- as.vector(colSums(simulator2$k_mat))
      n_sums_mat[simulation, ] <- colSums(simulator2$n_mat) / initial_population

      # cost of cats killed here
      eff.vec.iter <- a.eff / (1 + (b.eff * exp(-c.eff * n_sums_mat[simulation, ]))) # efficiency this iteration

      # calculate numbers killed per year using baiting and trapping first two years
      dispatched_cats_base <-
      bait.kill.base <- round(init.k.sums.mat[e, ] * (eff.vec.iter * pbait.killr), 0)
      trap.kill.base <- round(k.sums.mat[e, ] * (eff.vec.iter * ptrap.killr), 0)
      bt.kill.base <- trap.kill.base + bait.kill.base
      shortfall <- k.sums.mat[e, ] - bt.kill.base # how many cats not being killed by these methods?

      # base cost
      base.cost <- (cost.total.bait * 2) + (KI.trap.num * runif(1, min = trap.unit[1], max = trap.unit[2])) # at initial roll-out numbers

      # make up shortfall
      if (shortfall.method == "H") {
        makeup.iter <- shoot.ph * (shortfall / (cats.pph * eff.vec.iter)) # how many person-hours required to make up shortfall?
      }
      if (shortfall.method == "F") {
        makeup.iter <- felixer.unit * (shortfall / (pfelixer.killr * eff.vec.iter)) # how many person-hours required to make up shortfall?
      }
      if (shortfall.method == "T") {
        makeup.iter <- (runif(1, min = trap.unit[1], max = trap.unit[2])) * (shortfall / (ptrap.killr * eff.vec.iter)) # how many person-hours required to make up shortfall?
      }

      totalcost.mat[e, ] <- base.cost + makeup.iter

      if (e %% itdiv == 0) print(e)
    } # end e loop (stochastic iterations)

    min.ppop.vec <- apply(p.sums.mat, MARGIN = 1, min, na.rm = T)

    # median, lower & upper minimum proportional population sizes
    pmin.med.mat[n, m] <- median(min.ppop.vec, na.rm = T)
    pmin.lo.mat[n, m] <- quantile(min.ppop.vec, probs = 0.025, na.rm = T)
    pmin.up.mat[n, m] <- quantile(min.ppop.vec, probs = 0.975, na.rm = T)

    # quasi-extinction
    qext.mat[n, m] <- (sum(ifelse(round(min.pop.vec, 0) < q.ext, 1, 0)) / iter)

    ## costs
    totcost.vec <- apply(totalcost.mat, MARGIN = 1, sum, na.rm = T)
    totcost.med[n, m] <- median(totcost.vec, na.rm = T)
    colnames(totcost.med) <- harv.prop.maint
    rownames(totcost.med) <- harv.prop.init

    print("##############################")
    print(paste("init harvest proportion = ", harv.prop.init[n], sep = ""))
    print("##############################")
  } # end n loop (initial harvest rate)

  print("##############################")
  print(paste("maint harvest proportion = ", harv.prop.maint[m], sep = ""))
  print("##############################")
} # end m loop (maintenance harvest rate)
