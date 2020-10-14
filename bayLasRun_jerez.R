source('./fun/bayLasso/dfpBayLas.R')
source('./fun/bayLasso/dfpBayLasSte.R')
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
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 3
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 10
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 3
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 50
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 3
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 100
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, "-", updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 3
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 500
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, "-", updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 6
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 10
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 6
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 50
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 6
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 100
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, "-", updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
met       <- "dfp"

XDatPat <- paste0("./dat/datX-", datTyp, "-")
yDatPat <- paste0("./dat/daty-", datTyp, "-")

# Set-Up
ite    <- 6
per    <- get(load(file = paste0('./dat/per-', ite,'.RData')))
coe    <- get(load(file = paste0('./dat/coe-',ite,'.RData')))
gr1    <- get(load(file = paste0('./dat/gr1-',ite,'.RData')))
gr2    <- get(load(file = paste0('./dat/gr2-',ite,'.RData')))
gr3    <- get(load(file = paste0('./dat/gr3-',ite,'.RData')))
rb     <- get(load(file = './dat/rb.RData'))
M      <- 50
updPar <- 500
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, "-", updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
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
M      <- 50
updPar <- 10
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
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
M      <- 50
updPar <- 50
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
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
M      <- 50
updPar <- 100
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, "-", updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))

# Problem Type
datTyp    <- "low-high"
regMod    <- "bayLas"
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
M      <- 50
updPar <- 500
# Initial Random Partition
p      <- length(coe)
P      <- list()
rem    <- p %% M
if(rem == 0){
  comNum <- p %/% M 
} else {
  comNum <- p %/% M + 1
}
for(i in 1:(comNum - 1)){
  P[[i]] = coe[((i - 1) * M + 1):(i * M)]
}
if(rem == 0){
  P[[comNum]] <- coe[((comNum - 1) * M + 1):(comNum * M)]
} else {
  P[[comNum]] <- coe[((comNum - 1) * M + 1):((comNum - 1) * M + rem)]
}

# Creates the Paths
curParPat <- paste0('./out/curPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, "-", updPar, ".RData")
savPat    <- paste0('./out/savPar-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")
staPat    <- paste0('./out/sta-', datTyp, "-", regMod, "-", met, "-", ite, "-", M, updPar, ".RData")

out <- dfpBayLas(staBat    = 1,
                 endBat    = 500,
                 savCoe    = c(gr1, gr2, gr3),
                 curParPat = curParPat,
                 savPat    = savPat,
                 staPat    = staPat,
                 XDatPat   = XDatPat,
                 yDatPat   = yDatPat,
                 datPer    = per,
                 M         = M,
                 updPar    = 10,
                 iniPar    = P,
                 staPar    = list(hb = rep(0, p),
                                  ht = rep(1, p),
                                  hs = 1,
                                  hl = 1))