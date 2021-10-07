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
    init.k.sums.mat <- k.sums.mat <- n.sums.mat <- p.sums.mat <- totalcost.mat <- matrix(data = NA, nrow = iter, ncol = (t + 1))

    for (e in 1:iter) {
      popmat <- popmat.orig

      init.k.mat <- n.mat <- k.mat <- matrix(0, nrow = age.max, ncol = (t + 1))
      n.mat[, 1] <- init.vec

      for (i in 1:t) {
        # stochastic survival values
        s.alpha <- estBetaParams(s.vec, s.sd.vec^2)$alpha
        s.beta <- estBetaParams(s.vec, s.sd.vec^2)$beta
        s.stoch <- rbeta(length(s.alpha), s.alpha, s.beta)

        # stochastic fertilty sampler (gaussian)
        fert.stch <- rnorm(length(popmat[, 1]), popmat[1, ], m.sd.vec)
        fert.stoch <- ifelse(fert.stch < 0, 0, fert.stch)

        totN.i <- sum(n.mat[, i])
        pred.red <- a.lp / (1 + (totN.i / b.lp)^c.lp)

        popmat[1, ] <- fert.stoch # add new stochastically resampled fertilities
        diag(popmat[2:age.max, ]) <- s.stoch * pred.red
        # popmat[age.max,age.max] <- s.stoch[age.max]*pred.red

        n.mat[, i + 1] <- popmat %*% n.mat[, i]

        ## harvest things here
        if (i < 3) {
          k.mat[, i + 1] <- round(stable.stage.dist(popmat) * round(sum(n.mat[, i + 1]) * harv.prop.init[n], 0), 0)
          n.mat[, i + 1] <- n.mat[, i + 1] - k.mat[, i + 1]
          init.k.mat[, i + 1] <- n.mat[, i + 1]
        } else {
          k.mat[, i + 1] <- round(stable.stage.dist(popmat) * round(sum(n.mat[, i + 1]) * harv.prop.maint[m], 0), 0)
          n.mat[, i + 1] <- n.mat[, i + 1] - k.mat[, i + 1]
        }

        if (length(which(n.mat[, i + 1] < 0)) > 0) {
          n.mat[which(n.mat[, i + 1] < 0), i + 1] <- 0
        }
        if (length(which(k.mat[, i + 1] < 0)) > 0) {
          k.mat[which(k.mat[, i + 1] < 0), i + 1] <- 0
        }
      } # end i loop

      init.k.sums.mat[e, ] <- as.vector(colSums(init.k.mat))
      k.sums.mat[e, ] <- as.vector(colSums(k.mat))
      n.sums.mat[e, ] <- as.vector(colSums(n.mat))
      p.sums.mat[e, ] <- n.sums.mat[e, ] / pop.found

      # cost of cats killed here
      eff.vec.iter <- a.eff / (1 + (b.eff * exp(-c.eff * p.sums.mat[e, ]))) # efficiency this iteration

      # calculate numbers killed per year using baiting and trapping first two years
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
