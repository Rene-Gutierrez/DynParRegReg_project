###############################################
### Creates Data for a Low and High Setting ###
###############################################

### Libraries
library(doParallel)
library(fields)
library(distrEx)
library(mvtnorm)

### Parallel Set-Up
cl <- detectCores(all.tests = FALSE) / 2
print(cl)
registerDoParallel(cl)

### Parameter Settings
n           = 200       # Observations per Batch or Period
batNum      = 500       # Number of Batches or Periods
numCoe      = 50        # Number of Coefficients in Each Division
numCor      = numCoe    # Number of Correlated Coefficients in Each Division
numDiv      = 100       # Number of Divisions
corCoe      = 0.9       # Correlation Coefficient for the predictors
sigToNoiRat = 10        # Signal To Noise Ratio
sim         = 100       # Number of simulations
M           = numCoe    # Maximum number of elements

### Some Computations
N         = n*batNum                # Total number of observations
p         = numCoe*numDiv           # Total number of Coefficients
corMat    = corCoe^rdist(1:numCor)  # Correlation Matrix for the regressors
choCorMat = t(chol(corMat))         # Cholesky Decomposition of the Correlation Matrix

### ### Sparse 2 levels (High and Low)
prefix   = "low-high"
rb       = rep(0,p)                        # All coefficients
selh     = sample(seq(1,p), numCoe)        # Selects high levels
rb[selh] = rnorm(numCoe, 3, 1)             # Sets Coefficients with high level
sell     = sample(seq(1,p)[-selh], numCoe) # Selects low levels
rb[sell] = rnorm(numCoe, 1, 1)             # Sets Coefficients with low level
sigVar   = max(rb)                         # Signal Theoretical Variance (Case Dependent)

### Varance Magnitude
rs     = sigVar/sigToNoiRat # Noise Variance

### Creates a Data Directory (If necessary)
if(!dir.exists("./dat")){
  dir.create("./dat")
}

### Saves Variables Necesary
save(n,     file='./dat/n.RData')
save(p,     file='./dat/p.RData')
save(sim,   file='./dat/sim.RData')
save(batNum,file='./dat/batNum.RData')
save(numDiv,file='./dat/numDiv.RData')
save(numCoe,file='./dat/numCoe.RData')
save(prefix,file='./dat/prefix.RData')
save(rb,    file='./dat/rb.RData')
save(rs,    file='./dat/rs.RData')

### Generates the Data
print("Generating Data")
comy = list()        # Complete y
comX = list()        # Complete X
X    = matrix(0,n,p) # Batch X
### For every period or batch
tim = Sys.time()
### #### Generates X
corInd = ((seq(1,p) %% numCoe) <= (numCoe %/% 2)) & ((seq(1,p) %% numCoe) != 0)
notInd = !(corInd)
comX = foreach(i=1:batNum) %dopar%
  {
    for(j in 1:n)
    {
      Xcor        = as.vector(choCorMat %*% matrix(rnorm(numCor*numDiv, 0, 1),nrow = numCor, ncol = numDiv))    # Correlated observations
      X[j, ] = Xcor                                                                                        # Adds correlated observations
      #Xnot        = rnorm(p-numCor*numDiv,0,1)                                                                  # Uncorrelated observations
      #X[j,notInd] = Xnot                                                                                        # Adds the uncorrelated observations
    }
    X - matrix(rep(colMeans(X),n), n, p, byrow=T) # Zero Mean Regression Matrix
  }

### ### Generates y
comy = foreach(i=1:batNum) %dopar%
  {
    X = comX[[i]]
    X %*% rb + rnorm(n,0,sqrt(rs)) # Predictors Matrix
  }
### ### Saves the Data
for(i in 1:batNum)
{
  if(i < 10)
  {
    ### Save X
    filNam = paste0('./dat/datX-',prefix,'-00',i,'.RData')  # File Name
    datObj = comX[[i]]
    save(datObj,file=filNam)                                # Data Save
    ### Save y
    filNam = paste0('./dat/daty-',prefix,'-00',i,'.RData')  # File Name
    datObj = comy[[i]]
    save(datObj,file=filNam)                                # Data Save
  } else {
    if(i < 100)
    {
      ### Save X
      filNam = paste0('./dat/datX-',prefix,'-0',i,'.RData') # File Name
      datObj = comX[[i]]
      save(datObj,file=filNam)                              # Data Save
      ### Save y
      filNam = paste0('./dat/daty-',prefix,'-0',i,'.RData')   # File Name
      datObj = comy[[i]]
      save(datObj,file=filNam)                                # Data Save
    } else {
      ### Save X
      filNam = paste0('./dat/datX-',prefix,'-',i,'.RData')  # File Name
      datObj = comX[[i]]
      save(datObj,file=filNam)                              # Data Save
      ### Save y
      filNam = paste0('./dat/daty-',prefix,'-',i,'.RData')  # File Name
      datObj = comy[[i]]
      save(datObj,file=filNam)                              # Data Save
    }
  }
}

tim = Sys.time() - tim
print(tim)              # Prints Data Generating Time

### Generates the Permutations of the Data
for(i in 1:9)
{
  per = sample(batNum,batNum)                    # Permutation for Iteration i
  gr1 = sample(sell,1)                           # Selected to compute statstics
  gr2 = sample(selh,1)                           # Selected to compute statstics
  gr3 = sample(seq(1,p)[-c(sell,selh)],1)        # Selected to compute statstics
  coe = sample(seq(1:p),p)                       # Reorder of the indexes
  save(per,file=paste0('./dat/per-',i,'.RData')) # Saves the Permutation of the Iteration
  save(gr1,file=paste0('./dat/gr1-',i,'.RData')) # Saves the Index for Evaluation
  save(gr2,file=paste0('./dat/gr2-',i,'.RData')) # Saves the Index for Evaluation
  save(gr3,file=paste0('./dat/gr3-',i,'.RData')) # Saves the Index for Evaluation
  save(coe,file=paste0('./dat/coe-',i,'.RData')) # Saves the reordered coefficient orders
}
