#######################################################################################################################################################################################################
######################################################################################### COSTS #################################################################################################
########################################################################################################################################################################################################
## high harvest for initial 2 years, consistent harvest remaining years
############################################################################################################################################################################################################
## contributed by CJA Bradshaw
###########################################################################################

Dudley.area <- 375 * 100 # ha
KI.area <- 4405 * 100 # ha

## cost parameters
felixer.unit <- 13000 # AU$ #cost from felix vs felixer report
trap.unit <- c(157, 297) # AU$ # cost per trap from traps.com.au
shoot.ph <- 518.54 / 20 # ammo & labour (total AU$ over 20 hours) # Holmes et al 2015
only.bait.unit <- (2.07 + 0.2) # From Curiosity correspondence. $2.07 per bait + $0.20  + $250 administration fee per order, + freight fee. 500 baits per pack

# Felixers data from Moseby et al (2020)
# 20 felixers for "felixer paddock", Arid Recovery (n1 = 48), killed 31 cats (n2 = 17), over 41 days
num.felixer <- round((20 / 48) * initial_population, 0)
pfelixer.killr <- (31 / 48 * (1 / (41 / 365)))


felixer.area <- 26 * 100 # ha; density from Arid Recovery trial, "felixer paddock" = 26 km^2
felixer.dens <- 20 / felixer.area # 20 felixer traps over the area, average density 0.77 felixers/km^2
KI.felixer.num <- round(KI.area * felixer.dens, 0) # number of felixers needed if same density was applied throughout Kangaroo Island
KI.felixer.num # not neccessarily reflective of the use of felixers as they are used in targeted areas and spread sporadically, as opposed to systematicaly placed like traps or baits

# traps
# 40 traps killed 21 over 148 days Hodgens
ptrap.killr <- (21 / 262 * (1 / (148 / 365)))
trap.dens <- 40 / Dudley.area
KI.trap.num <- round(KI.area * trap.dens, 0)
KI.trap.num

# shooting
# 14725 person-hours killed 872 (+ 172 from wounds) cats (Marion) Parkes et al. 2014 & Bloomer & Bester 1992
# assume cats not killed by Felixers & traps shot by hunters
cats.pph <- (872 + 172) / 14725


# baiting
# 943 baits killed 11 cats over 18.86 km^2. Pre-baiting dens = 1.18 cats/km^2, post-baiting = 0.58 cats/km^2. Ref, 'Dudley peninsula feral cat eradication operations plan: summary may 2020 - mid 2023"
# KI uses Curiosity (PAPP)
## Kangaroo Island area - 4,405 km^2  or 440 500 ha
# can't bait built-up areas, need 500m buffer zone around towns, built up areas 362 ha, how many built up areas?
## parndana (second largest town) approx area as circle - 2km^2 (??), + 500m buffer = area 10km^2. 5 'main towns' KI. 5*2 = 10km^2 or 1000 ha, :- approx 1000 ha can't bait urban
# can't bait beaches. KI 540 km coastline, arbitary 100m buffer around coastline = 594 km can't bait + buffer zone. 540 * 1.1 = area no bait
## Dirk Hartog Island, 15 cats collared, average density 0.701 cats/km^2 (average area = 10.515 km^2 (A = 15*0.701), 50 baits per km, baits = 50*10.515 = 525.75), 14 died following bait consumption ... 525.74/14 = 37.55 baits/cat
# Dirk Hartog Island, used eradicat (1080)
nobaitfarm <- (2303 - (2303 * .94)) * 100 # ha; can't be baited
nobaitcoast <- (540 * 1.1) * 100 # ha; dist around costline, *1.1 for the 100m buffer around coast
nobaittown <- 1000 # ha; can't bait town
nobaitarea <- nobaitfarm + nobaitcoast + nobaittown # total area can't be baited
baitareaKI <- KI.area - nobaitarea # ha; area eligible for baiting
baitdens <- 50 / 100 # 50 baits per km^2 converted to baits per ha
baitnum <- (baitareaKI * baitdens) # number of baits need for entire Island

baitadminfee <- 250 # administration fee, once off for baits, or twice off for two years
baitdrop.time <- (30 / 60 / 60) * (baitareaKI / 100) # 50 baits drops every 30 seconds or 1 km^2, with plane speed 240km/h - bait area/100 to convert back to km^2
trips <- baitnum / 3500 # can only take 3500 baits per trip
upandback <- seq(1, 32, 0.5) # ha; dist from airport to start of each bait transect
averagedist <- (sum(upandback)) / (length(upandback)) # ha; average dist from airport to transect
baitreloadtime <- ((averagedist * trips) * 2) / 240 # 52 trips needed to drop all baits, *2 for to and from airport, plan speed 240km
planehph <- 750 # cost per hour plane hire when dropping baits, inc wages of 2x pilots (1x loading and dropping baits)
planeferrycost <- (3 * 600) * 2 # 3 hour flight William Creek to Kangaroo Island (*2 for return), $600 p/h for charter
baitreloadcost <- baitreloadtime * planehph # average extra cost for reloading baits
baitdropcost <- baitdrop.time * planehph
planecost <- planeferrycost + baitreloadcost + baitdropcost
cost.total.bait <- planecost + baitadminfee + (only.bait.unit * baitnum) # cost of total baiting to cover entire island


pbait.killr <- 14 / 525.74 # 14 cats killed by 525.74 baits

###########################################################################################


matriz2dataframe <- function(pmin.med.mat, harv.prop.init, harv.prop.main) {
  twophase.up <- data.frame(pmin.med.mat)
  colnames(twophase.up) <- harv.prop.maint
  rownames(twophase.up) <- harv.prop.init
  datos <- expand_grid(x = harv.prop.init, y = harv.prop.maint)
  nx <- length(harv.prop.init)
  ny <- length(harv.prop.maint)
  datos$z <- to_vec(for (x in seq(1, nx)) for (y in seq(1, ny)) twophase.up[x, y])
  return(datos)
}