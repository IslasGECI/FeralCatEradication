#######################################################################################################################################################################################################
######################################################################################### COSTS #################################################################################################
########################################################################################################################################################################################################
## high harvest for initial 2 years, consistent harvest remaining years
############################################################################################################################################################################################################
## contributed by CJA Bradshaw
###########################################################################################

Dudley.area <- 375*100 #ha
KI.area <- 4405*100  #ha

## cost parameters
felixer.unit <- 13000 # AU$ #cost from felix vs felixer report 
trap.unit <- c(157,297) # AU$ # cost per trap from traps.com.au
shoot.ph <- 518.54/20 # ammo & labour (total AU$ over 20 hours) # Holmes et al 2015
only.bait.unit <- (2.07 + 0.2) # From Curiosity correspondence. $2.07 per bait + $0.20  + $250 administration fee per order, + freight fee. 500 baits per pack

# Felixers data from Moseby et al (2020)
# 20 felixers for "felixer paddock", Arid Recovery (n1 = 48), killed 31 cats (n2 = 17), over 41 days 
num.felixer <- round((20/48) * pop.found, 0)
pfelixer.killr <- (31/48 * (1/(41/365)))


felixer.area <- 26*100 #ha; density from Arid Recovery trial, "felixer paddock" = 26 km^2
felixer.dens <- 20/felixer.area #20 felixer traps over the area, average density 0.77 felixers/km^2
KI.felixer.num <- round(KI.area * felixer.dens, 0) # number of felixers needed if same density was applied throughout Kangaroo Island
KI.felixer.num # not neccessarily reflective of the use of felixers as they are used in targeted areas and spread sporadically, as opposed to systematicaly placed like traps or baits

# traps
# 40 traps killed 21 over 148 days Hodgens 
ptrap.killr <- (21/262 * (1/(148/365)))
trap.dens <- 40/Dudley.area
KI.trap.num <- round(KI.area * trap.dens, 0)
KI.trap.num

# shooting
# 14725 person-hours killed 872 (+ 172 from wounds) cats (Marion) Parkes et al. 2014 & Bloomer & Bester 1992
# assume cats not killed by Felixers & traps shot by hunters
cats.pph <- (872+172)/14725


# baiting 
# 943 baits killed 11 cats over 18.86 km^2. Pre-baiting dens = 1.18 cats/km^2, post-baiting = 0.58 cats/km^2. Ref, 'Dudley peninsula feral cat eradication operations plan: summary may 2020 - mid 2023"
# KI uses Curiosity (PAPP)
## Kangaroo Island area - 4,405 km^2  or 440 500 ha 
# can't bait built-up areas, need 500m buffer zone around towns, built up areas 362 ha, how many built up areas? 
## parndana (second largest town) approx area as circle - 2km^2 (??), + 500m buffer = area 10km^2. 5 'main towns' KI. 5*2 = 10km^2 or 1000 ha, :- approx 1000 ha can't bait urban
# can't bait beaches. KI 540 km coastline, arbitary 100m buffer around coastline = 594 km can't bait + buffer zone. 540 * 1.1 = area no bait
## Dirk Hartog Island, 15 cats collared, average density 0.701 cats/km^2 (average area = 10.515 km^2 (A = 15*0.701), 50 baits per km, baits = 50*10.515 = 525.75), 14 died following bait consumption ... 525.74/14 = 37.55 baits/cat
# Dirk Hartog Island, used eradicat (1080)
nobaitfarm <- (2303 - (2303*.94))*100 #ha; can't be baited 
nobaitcoast <- (540 * 1.1)*100 #ha; dist around costline, *1.1 for the 100m buffer around coast 
nobaittown <- 1000 #ha; can't bait town 
nobaitarea <- nobaitfarm + nobaitcoast + nobaittown # total area can't be baited
baitareaKI <- KI.area - nobaitarea #ha; area eligible for baiting 
baitdens <- 50/100 # 50 baits per km^2 converted to baits per ha
baitnum <- (baitareaKI * baitdens) #number of baits need for entire Island

baitadminfee <- 250 # administration fee, once off for baits, or twice off for two years 
baitdrop.time <- (30/60/60) * (baitareaKI/100) # 50 baits drops every 30 seconds or 1 km^2, with plane speed 240km/h - bait area/100 to convert back to km^2
trips <- baitnum/3500 #can only take 3500 baits per trip 
upandback <- seq(1,32,0.5) #ha; dist from airport to start of each bait transect
averagedist <- (sum(upandback))/(length(upandback)) #ha; average dist from airport to transect
baitreloadtime <- ((averagedist*trips)*2)/240 #52 trips needed to drop all baits, *2 for to and from airport, plan speed 240km
planehph <- 750  #cost per hour plane hire when dropping baits, inc wages of 2x pilots (1x loading and dropping baits)
planeferrycost <- (3*600)*2 # 3 hour flight William Creek to Kangaroo Island (*2 for return), $600 p/h for charter
baitreloadcost <- baitreloadtime*planehph #average extra cost for reloading baits
baitdropcost <- baitdrop.time*planehph
planecost <- planeferrycost + baitreloadcost +  baitdropcost
cost.total.bait <- planecost + baitadminfee + (only.bait.unit * baitnum) #cost of total baiting to cover entire island 

  
pbait.killr <- 14/525.74 # 14 cats killed by 525.74 baits



###########################################################################################

###########################################################################################
## Type III functional response (reduction in capture efficiency with decreasing density)
max.eff <- 1 # max efficiency 
min.eff <- 0 #min efficiency
max.pN <- 1 #max population proportion
min.pN <- 0 #min population proportion
infl.eff <- 0.5

pN.vec <- c(min.pN, 0.2, 0.4, 0.5, 0.7, 0.8, max.pN) 
eff.vec <- c(min.eff, 0.05, 0.3, infl.eff, 0.85, 0.95, max.eff)
plot(pN.vec, eff.vec, type="b", pch=19)
eff.dat <- data.frame(pN.vec, eff.vec)
colnames(eff.dat) <- c("pN", "eff")

# a/(1 + b*e^(-cx)) (logistic)
param.init <- c(1, 85, 8.9)
fit.eff <- nls(eff ~ a/(1+(b*exp(-c*pN))), 
               data = eff.dat,
               algorithm = "port",
               start = c(a = param.init[1], b = param.init[2], c = param.init[3]),
               trace = TRUE,      
               nls.control(maxiter = 1000, tol = 1e-05, minFactor = 1/1024))
fit.eff.summ <- summary(fit.eff)
plot(pN.vec,eff.vec,pch=19,xlab="pN",ylab="efficiency")
pN.vec.cont <- seq(0,1,0.01)
pred.eff.fx <- coef(fit.eff)[1]/(1+(coef(fit.eff)[2]*exp(-coef(fit.eff)[3]*pN.vec.cont)))
lines(pN.vec.cont,pred.eff.fx,lty=2,lwd=3,col="red")

a.eff <- coef(fit.eff)[1]
b.eff <- coef(fit.eff)[2]
c.eff <- coef(fit.eff)[3]

###########################################################################################


#################################################### 
## iterations and quasi ext for each following model
####################################################
iter <- 10000 #final model run at 10 000
itdiv <- iter/1000 #final model rate at iter/1000

## run choices
## make up shortfall in kill by ...
#shortfall.method <- "F" # adding Felixer units
shortfall.method <- "T" # adding traps
#shortfall.method <- "H" # increasing hunting pressure

# harvest rate
harv.prop.init <- seq(0.5,0.9,0.05)
harv.prop.maint <- seq(0.1,0.5,0.05)
q.ext <- 20

# storage
qext.mat <- minn.med.mat <- minn.lo.mat <- minn.up.mat <- pmin.med.mat <- pmin.lo.mat <- pmin.up.mat <- totcost.med <- totcost.lo <- totcost.up <- matrix(data=NA, ncol=length(harv.prop.maint), nrow=length(harv.prop.init))

for (m in 1:length(harv.prop.maint)) {
  
  for (n in 1:length(harv.prop.init)) {
    
    # storage
   init.k.sums.mat <- k.sums.mat <- n.sums.mat <- p.sums.mat <- totalcost.mat <- matrix(data=NA, nrow=iter, ncol=(t+1))
    
    for (e in 1:iter) {
      
      popmat <- popmat.orig
      
      init.k.mat <- n.mat <- k.mat <- matrix(0, nrow=age.max,ncol=(t+1))
      n.mat[,1] <- init.vec
      
      for (i in 1:t) {
        # stochastic survival values
        s.alpha <- estBetaParams(s.vec, s.sd.vec^2)$alpha
        s.beta <- estBetaParams(s.vec, s.sd.vec^2)$beta
        s.stoch <- rbeta(length(s.alpha), s.alpha, s.beta)
        
        # stochastic fertilty sampler (gaussian)
        fert.stch <- rnorm(length(popmat[,1]), popmat[1,], m.sd.vec)
        fert.stoch <- ifelse(fert.stch < 0, 0, fert.stch)
        
        totN.i <- sum(n.mat[,i])
        pred.red <- a.lp/(1+(totN.i/b.lp)^c.lp)
        
        popmat[1,] <- fert.stoch  # add new stochastically resampled fertilities
        diag(popmat[2:age.max,]) <- s.stoch*pred.red
        #popmat[age.max,age.max] <- s.stoch[age.max]*pred.red
        
        n.mat[,i+1] <- popmat %*% n.mat[,i]
        
        ## harvest things here
        if (i < 3) {
          k.mat[,i+1] <- round(stable.stage.dist(popmat) * round(sum(n.mat[,i+1])*harv.prop.init[n], 0), 0)
          n.mat[,i+1] <- n.mat[,i+1] - k.mat[,i+1]
          init.k.mat[,i+1] <- n.mat[,i+1]
        } else {
          k.mat[,i+1] <- round(stable.stage.dist(popmat) * round(sum(n.mat[,i+1])*harv.prop.maint[m], 0), 0)
          n.mat[,i+1] <- n.mat[,i+1] - k.mat[,i+1]
        }
        
        if (length(which(n.mat[,i+1] < 0)) > 0) {
          n.mat[which(n.mat[,i+1] < 0), i+1] <- 0
        }
        if (length(which(k.mat[,i+1] < 0)) > 0) {
          k.mat[which(k.mat[,i+1] < 0), i+1] <- 0
        }
        
      } # end i loop
      
      init.k.sums.mat[e,] <- as.vector(colSums(init.k.mat))
      k.sums.mat[e,] <- as.vector(colSums(k.mat))
      n.sums.mat[e,] <- as.vector(colSums(n.mat))
      p.sums.mat[e,] <- n.sums.mat[e,] / pop.found
      
      # cost of cats killed here
      eff.vec.iter <- a.eff/(1+(b.eff*exp(-c.eff*p.sums.mat[e,]))) # efficiency this iteration
      
      # calculate numbers killed per year using baiting and trapping first two years
      bait.kill.base <- round(init.k.sums.mat[e,] * (eff.vec.iter*pbait.killr), 0)
      trap.kill.base <- round(k.sums.mat[e,] * (eff.vec.iter*ptrap.killr), 0)
      bt.kill.base <- trap.kill.base + bait.kill.base
      shortfall <- k.sums.mat[e,] - bt.kill.base # how many cats not being killed by these methods?
      
      #base cost
      base.cost <- (cost.total.bait*2) + (KI.trap.num*runif(1,min=trap.unit[1],max=trap.unit[2])) # at initial roll-out numbers
      
      # make up shortfall
      if (shortfall.method == "H") {
        makeup.iter <- shoot.ph*(shortfall / (cats.pph*eff.vec.iter)) # how many person-hours required to make up shortfall?
      }
      if (shortfall.method == "F") {
        makeup.iter <- felixer.unit*(shortfall / (pfelixer.killr*eff.vec.iter)) # how many person-hours required to make up shortfall?
      }
      if (shortfall.method == "T") {
        makeup.iter <- (runif(1,min=trap.unit[1],max=trap.unit[2]))*(shortfall / (ptrap.killr*eff.vec.iter)) # how many person-hours required to make up shortfall?
      }
      
      totalcost.mat[e,] <- base.cost + makeup.iter 
      
      if (e %% itdiv==0) print(e) 
    } # end e loop (stochastic iterations)
    
    min.pop.vec <- apply(n.sums.mat, MARGIN=1, min, na.rm=T)
    min.ppop.vec <- apply(p.sums.mat, MARGIN=1, min, na.rm=T)
    
    # median, lower & upper minimum population sizes
    minn.med.mat[n, m] <- median(min.pop.vec, na.rm=T) 
    minn.lo.mat[n, m] <- quantile(min.pop.vec, probs=0.025, na.rm=T)
    minn.up.mat[n, m] <- quantile(min.pop.vec, probs=0.975, na.rm=T)
    
    # median, lower & upper minimum proportional population sizes
    pmin.med.mat[n, m] <- median(min.ppop.vec, na.rm=T)  
    pmin.lo.mat[n, m] <- quantile(min.ppop.vec, probs=0.025, na.rm=T) 
    pmin.up.mat[n, m] <- quantile(min.ppop.vec, probs=0.975, na.rm=T)
    
    # quasi-extinction
    qext.mat[n, m] <- (sum(ifelse(round(min.pop.vec, 0) < q.ext, 1, 0)) / iter)
    
    ## costs
    totcost.vec <- apply(totalcost.mat, MARGIN=1, sum, na.rm=T)
    totcost.med[n, m] <- median(totcost.vec, na.rm=T)
    colnames(totcost.med) <- harv.prop.maint
    rownames(totcost.med) <- harv.prop.init
    
    totcost.lo[n, m] <- quantile(totcost.vec, probs=0.025, na.rm=T)
    colnames(totcost.lo) <- harv.prop.maint
    rownames(totcost.lo) <- harv.prop.init
    
    totcost.up[n, m] <- quantile(totcost.vec, probs=0.975, na.rm=T)
    colnames(totcost.up) <- harv.prop.maint
    rownames(totcost.up) <- harv.prop.init
    
    print("##############################")
    print(paste("init harvest proportion = ", harv.prop.init[n], sep=""))
    print("##############################")
    
  } # end n loop (initial harvest rate)
  
  print("##############################")
  print(paste("maint harvest proportion = ", harv.prop.maint[m], sep=""))
  print("##############################")
  
} # end m loop (maintenance harvest rate)

## plot 3D surfaces
f1 <- list(
  family = "Avenir Light",
  size = 26,
  color = "black"
)
f2 <- list(
  family = "Avenir Light",
  size = 18,
  color = "black"
)
f3 <- list(
  family = "Avenir Light",
  size = 16,
  color = "black"
)

# total cost (median)
par(mar=c(5,5,2,8))
costcontmed3d <- plot_ly(z = ~totcost.med, autocontour=T, type="contour", line = list(smoothing = 0.90), contours = list(showlabels = TRUE, labelfont=list(
  size=18, family="Avenir Light", face="bold", color="white"))) %>%
  colorbar(title = "tot $", titlefont=f2, tickfont=f2) %>%
  layout(
    xaxis = list(title="maintenance cull", titlefont=f1, tickfont=f2, ticketmode='array', ticktext=as.character(seq(0.1,0.5,0.1)), tickvals=seq(0,8,2)),
    yaxis = list(title="initial cull", titlefont=f1, tickfont=f2, ticketmode='array', ticktext=as.character(seq(0.5,0.9,0.1)), tickvals=seq(0,8,2)))
costcontmed3d

cost3d <- plot_ly(showscale = FALSE) %>% 
  add_surface(z = ~totcost.med) %>%
  add_surface(z = ~totcost.lo, opacity = 0.55) %>%
  add_surface(z = ~totcost.up, opacity = 0.55) %>%
  layout(scene = list(
    xaxis = list(title="maintenance cull", titlefont=f1, tickfont=f2, ticketmode='array', ticktext=as.character(seq(0.1,0.5,0.1)), tickvals=seq(0,8,2)),
    yaxis = list(title="initial cull", titlefont=f1, tickfont=f2, ticketmode='array', ticktext=as.character(seq(0.5,0.9,0.1)), tickvals=seq(0,8,2)),
    zaxis = list(title="tot $", tickfont=f3, titlefont=f1)))
cost3d
