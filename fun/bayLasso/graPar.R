### Creates a partition from a graph maximizing the
### number of connected components in each subset

### Inputs:
### ### corMat: Correlation Matrix
### ### M:      Maximum Number of Components
### Outputs:
### ### P: Partition

graPar = function(corMat, M)
{
  
  ### Initialization and Auxilary Variables
  absCor = abs(corMat)             # Absolute Value of the Correlation Matrix
  P      = list()                  # Initializes the Partition
  corTre = seq(0.001,1,0.001)      # Possible Correlation Values
  curCor = floor(length(corTre)/2) # Initial Value for the Binary search
  maxCor = length(corTre)
  minCor = 1
  ### Looks for the maximum correlation on the grid that satisfies the restriction
  ### that the largest connected component is of size M
  for(i in 1:length(corTre))
  {
    corGra = corTre[curCor]
    g      = igraph::graph_from_adjacency_matrix(absCor>corGra)
    conCom = igraph::clusters(g)
    cluNum = conCom$csize
    maxCon = max(cluNum)
    if( minCor == curCor | maxCor == curCor )
    {
      break
    }
    if(maxCon <= M)
    {
      maxCor = curCor
      curCor = ceiling((curCor+minCor)/2)
    } else {
      minCor = curCor
      curCor = ceiling((curCor+maxCor)/2)
    }
  }
  
  ### Creates the Partition
  p = dim(corMat)[1]
  for(i in 1:conCom$no)
  {
    P[[i]] = (1:p)[conCom$membership == i]
  }
  P <- par(P, M)
  return(list(partition = P, treshold = corTre[curCor]))
}