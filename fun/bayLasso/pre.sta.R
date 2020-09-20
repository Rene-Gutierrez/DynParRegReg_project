### Prediction Performance Statistics

### Inputs:
### ### y: Test observation
### ### X: Test regressor
### ### b: Sample for beta
### ### s: Sample for sigma
### ### a: Interval Length
### Outputs:
### ### preCov:    Prediction 1-a)% Credible Interval Coverage
### ### preSquErr: Mean Prediction Square Error
### ### preLowQua: Lower Prediction Quantile
### ### preUppQua: Upper Prediction Quantile
### ### preCreLen: Prediction (1-a)% Credible Interval Length

pre.sta = function(y, X, b, s)
{
  #  Gets the Dimensions
  ## Number of Observations
  n     <- length(y)
  ## Number of Simulations
  nmcmc <- length(s) 
  # Computes Prediction Samples for the Test
  ## Computes the Prediction Mean
  preMea <- X %*% t(b)
  ## Starts the Prediction Samples
  pre    <- matrix(0, n, nmcmc) 
  for(i in 1:n)              
  {
    # For every observation we compute in the Prediction Set
    ## Samples predictions
    pre[i,] <- rnorm(nmcmc, preMea[i,], sqrt(s) ) 
  }
  
  # Prediction Statistics
  ## Prediction Mean
  preMea <- rowMeans(pre) 
  ## Credibility Interval
  # Lower Quantile
  preLowQua <- apply(t(pre), 2, quantile, 0.025 )
  # Upper Quantile
  preUppQua <- apply(t(pre), 2, quantile, 0.975 )
  ## Credibility Interval length
  preCreLen <- mean(preUppQua - preLowQua)/sqrt(var(y))
  ## Coverage of Credibility Interval
  lg <- as.numeric(preLowQua < y)
  ug <- as.numeric(y < preUppQua)
  preCreCov <- mean(lg * ug)
  ## Square Error
  preSquErr <- mean((y - preMea)^2)/var(y)
  ## Interval Score
  preIntSco <- (preUppQua - preLowQua) + (40) * (preLowQua - y) * (1-lg) + (40) * (y - preUppQua) * (1-ug)
  preIntSco <- mean(preIntSco)
  
  ### Returns Results
  return( c(preCreCov, preSquErr, preCreLen, preIntSco))
}