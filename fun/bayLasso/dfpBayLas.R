##########################
### DFP Bayesian Lasso ###
##########################

dfpBayLas <- function(staBat    = 1,
                      endBat    = staBat,
                      XDatPat   = "./dat/datX-complete-",
                      yDatPat   = "./dat/daty-complete-",
                      staPat    = "./out/sta.RData",
                      curParPat = "./out/curPar.RData",
                      savPat    = "./out/savPar.RData",
                      suffix    = "complete-dfp-BayLas",
                      cores     = parallel::detectCores() / 2,
                      staFla    = TRUE,
                      nmcmc     = 100,
                      hlsh      = 1,
                      hlsc      = 1,
                      progress  = TRUE,
                      staPar,
                      savBol    = TRUE,
                      savCoe,
                      datPer    = seq(1:endBat),
                      M,
                      iniPar){
  
  # Parallel Set-Up
  print(paste0("Number of Cores = ", cores))
  doParallel::registerDoParallel(cores)
  
  # Progress Bar Initialization
  if(progress){
    pb <- txtProgressBar(min     = 0,
                         max     = 1,
                         initial = 0,
                         style   = 3,
                         width   = 72)
  }
  
  # Saved Parameters Initialization and Retrieve
  ## Checks if the Parameter Saving Is requested
  if(savBol){
    # Checks if a saved parameter Exists
    if(file.exists(savPat)){
      # Retrieves the Saved Parameters
      savPar <- get(load(file = savPat))
    } else {
      # Initializes a Saved Parameter List
      savPar <- list()
    }
  }
  
  # Stats Initialization
  ## Checks if it is the first Batch
  if(staBat == 1){
    # Initializes the Parameters
    sta <- list()
  } else {
    # Recalls the Stats if Available
    if(file.exists(staPat)){
      sta <- get(load(file = staPat))
    } else {
      sta <- list()
    }
  }
  
  # Sufficient Statistics Initialization
  ## Checks if it is the first Batch
  if(staBat == 1){
    # Initializes the Sufficient Statistics
    XX = matrix(0,p,p) 
    Xy = matrix(0,p,1)
    yy = 0
    sN = 0
  } else {
    # Gets the Sufficient Statistics
    curPar <- get(load(file = curParPat))
    # Sets the Current Sufficient Statitistics
    XX <- curPar$XX
    Xy <- curPar$Xy
    yy <- curPar$yy
    sN <- curPar$sN
  }
  
  # Variable Initialization
  ## Checks if it is the first Batch
  if(staBat == 1){
    # Initializes the Variables
    hs <- staPar$hs
    hl <- staPar$hl
    hb <- staPar$hb
    ht <- staPar$ht
  } else {
    # Gets the Current Point Estimates
    curPar <- get(load(file = curParPat))
    # Sets the Sufficient Statitistics
    hs <- curPar$hs
    hl <- curPar$hl
    hb <- curPar$hb
    ht <- curPar$ht
  }
  
  # Partition Initialization
  if(staBat == 1){
    # Initializes the Partition
    P <- iniPar
  } else {
    P <- curPar$P
  }
  
  # Performs Bayesian Lasso with DFP
  for(i in staBat:endBat){
    # Obtains the Data Batch
    ## Regression Matrix
    X <- get(load(file=paste0(XDatPat, per[i], '.RData')))
    ## Dependent Variable
    y <- get(load(file=paste0(yDatPat, per[i], '.RData')))
    
    # Time Start
    beg = Sys.time()
    
    # Updates the Sufficient Statistics
    XX <- XX + t(X) %*% X
    Xy <- Xy + t(X) %*% y
    yy <- yy + sum(y^2)
    sN <- sN + length(y)
    
    # Perfroms Bayesian Lasso with DFP
    samOut <- dfpBayLasSte(P     = P,
                           XX    = XX,
                           Xy    = Xy,
                           yy    = yy,
                           sN    = sN,
                           hb    = hb,
                           hs    = hs,
                           hl    = hl,
                           ht    = ht,
                           hlsh  = hlsh,
                           hlsc  = hlsc,
                           nmcmc = nmcmc)
    # Obtains the Samples
    sb <- samOut$sb
    ss <- samOut$ss
    st <- samOut$st
    sl <- samOut$sl
    # Updates the Point Estimates
    hb <- colMeans(sb)
    hs <- mean(ss)
    ht <- colMeans(st)
    hl <- mean(sl)
    
    # Partition Update
    ## Computes the Correlation
    if(i == 1){
      updCor <- cor(sb)
    } else {
      updCor <- (cor(sb) + updCor) / 2
    }
    ## Creates a New Partition Based on the Connected Components
    graParOut      <- graPar(updCor, M)
    P              <- graParOut$partition
    treshold       <- graParOut$treshold
    
    # Runtime
    tim <- Sys.time() - beg
    
    # Saves the Requested Coefficients and Associated Parameters
    ## Checks if saved is requested
    if(savBol){
      savPar[[i]] <- list(sb     = sb[, savCoe],
                          st     = st[, savCoe],
                          ss     = ss,
                          sl     = sl,
                          P      = P,
                          treshold = treshold,
                          savCoe = savCoe)
    }
    ## Saves to File
    save(savPar, file = savPat)
    
    # Saves the Current Point Estimates and Partition
    curPar <- list(hb = hb,
                   ht = ht,
                   hs = hs,
                   hl = hl,
                   P  = P,
                   XX = XX,
                   Xy = Xy,
                   yy = yy,
                   sN = sN)
    ## Saves to File
    save(curPar, file = curParPat)
    
    # Computes the Relevant Statistics for Evaluation
    ## Checks if the Statistics are required
    if(staFla){
      # Gets the Stats for period i
      perSta <- pre.sta(y, X, sb, ss)
      # Saves the stats
      sta[[i]] <- c(perSta, tim)
      # Saves to File
      save(sta, file = staPat)
    }
    
    ### Progress Bar Display
    if(progress){
      setTxtProgressBar(pb    = pb,
                        value = (i - staBat + 1) / (endBat - staBat + 1))
    }
  }
  ### Closes the Progress Bar
  if(progress){
    close(pb)
  }
  
  return(list(ss = ss, sb = sb, sl = sl, st = st, XX = XX, Xy = Xy, yy = yy, sN = sN, P = P, treshold = treshold))
}