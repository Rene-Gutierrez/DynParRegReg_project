source('./fun/spiLas/dfpSpiLas.R')
source('./fun/spiLas/dfpSpiLasSte.R')
source('./fun/bayLasso/graPar.R')
source('./fun/bayLasso/par.R')
source('./fun/bayLasso/pre.sta.R')
library(doParallel)

### Creates a Data Directory (If necessary)
if(!dir.exists("./out")){
  dir.create("./out")
}

# Problem Type
datTyp    <- "low-high"
regMod    <- "spiLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 7
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
c      <- 0.0001
p      <- length(coe)

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")

# Runs DFP with Spike and Lasso
out <- dfpSpiLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hg = rep(0, p),
                                  hs = 1,
                                  hl = 1,
                                  hd = 0.5),
                 c         = c)

# Problem Type
datTyp    <- "low-high"
regMod    <- "spiLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 8
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
c      <- 0.0001
p      <- length(coe)

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")

# Runs DFP with Spike and Lasso
out <- dfpSpiLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hg = rep(0, p),
                                  hs = 1,
                                  hl = 1,
                                  hd = 0.5),
                 c         = c)

# Problem Type
datTyp    <- "low-high"
regMod    <- "spiLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 9
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
c      <- 0.0001
p      <- length(coe)

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", 1/c, ".RData")

# Runs DFP with Spike and Lasso
out <- dfpSpiLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hg = rep(0, p),
                                  hs = 1,
                                  hl = 1,
                                  hd = 0.5),
                 c         = c)