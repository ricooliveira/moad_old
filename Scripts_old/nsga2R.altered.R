nsga2R.altered = function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
          upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
          generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
          MuDistIdx = 10, ubcf.index, td.index) 
{
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  cat("initializing the population")
  cat("\n")
  parent <- t(sapply(1:(popSize - 2), function(u) array(runif(length(lowerBounds), 
                                                        lowerBounds, upperBounds))))
  parent <- floor(parent)
  parent = rbind(ubcf.index, td.index, parent)
  parent <- cbind(parent, t(apply(parent, 1, fn)))
  cat("ranking the initial population")
  cat("\n")
  ranking <- fastNonDominatedSorting.altered(parent[, (varNo + 1):(varNo + 
                                                             objDim)])
  rnkIndex <- integer(popSize)
  i <- 1
  while (i <= length(ranking)) {
    rnkIndex[ranking[[i]]] <- i
    i <- i + 1
  }
  parent <- cbind(parent, rnkIndex)
  cat("crowding distance calculation")
  cat("\n")
  objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                    2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                    2, min)
  cd <- crowdingDist4frnt(parent, ranking, objRange)
  parent <- cbind(parent, apply(cd, 1, sum))
  for (iter in 1:generations) {
    cat("---------------generation---------------", iter, 
        "starts")
    cat("\n")
    cat("tournament selection")
    cat("\n")
    matingPool <- tournamentSelection(parent, popSize, tourSize)
    cat("crossover operator")
    cat("\n")
    childAfterX <- boundedSBXover.altered(matingPool[, 1:varNo], 
                                  lowerBounds, upperBounds, cprob, XoverDistIdx)
    cat("mutation operator")
    cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    cat("evaluate the objective fns of childAfterM")
    cat("\n")
    childAfterM <- cbind(childAfterM, t(apply(childAfterM, 
                                              1, fn)))
    cat("Rt = Pt + Qt")
    cat("\n")
    parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
    cat("ranking again")
    cat("\n")
    ranking <- fastNonDominatedSorting.altered(parentNext[, (varNo + 
                                                       1):(varNo + objDim)])
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    cat("crowded comparison again")
    cat("\n")
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    cd <- crowdingDist4frnt(parentNext, ranking, objRange)
    parentNext <- cbind(parentNext, apply(cd, 1, sum))
    parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                     objDim + 1], -parentNext[, varNo + objDim + 2]), 
                                  ]
    cat("environmental selection")
    cat("\n")
    parent <- parentNext.sort[1:popSize, ]
    cat("---------------generation---------------", iter, 
        "ends")
    cat("\n")
    if (iter != generations) {
      cat("\n")
      cat("********** new iteration *********")
      cat("\n")
    }
    else {
      cat("********** stop the evolution *********")
      cat("\n")
    }
  }
  result = list(functions = fn, parameterDim = varNo, objectiveDim = objDim, 
                lowerBounds = lowerBounds, upperBounds = upperBounds, 
                popSize = popSize, tournamentSize = tourSize, generations = generations, 
                XoverProb = cprob, XoverDistIndex = XoverDistIdx, mutationProb = mprob, 
                mutationDistIndex = MuDistIdx, parameters = parent[, 
                                                                   1:varNo], objectives = parent[, (varNo + 1):(varNo + 
                                                                                                                  objDim)], paretoFrontRank = parent[, varNo + objDim + 
                                                                                                                                                       1], crowdingDistance = parent[, varNo + objDim + 
                                                                                                                                                                                       2])
  class(result) = "nsga2R"
  return(result)
}

nsga2R.altered.random = function (fn, varNo, objDim, lowerBounds = rep(-Inf, varNo), 
                           upperBounds = rep(Inf, varNo), popSize = 100, tourSize = 2, 
                           generations = 20, cprob = 0.7, XoverDistIdx = 5, mprob = 0.2, 
                           MuDistIdx = 10) 
{
  cat("********** R based Nondominated Sorting Genetic Algorithm II *********")
  cat("\n")
  cat("initializing the population")
  cat("\n")
  parent <- t(sapply(1:(popSize), function(u) array(runif(length(lowerBounds), 
                                                              lowerBounds, upperBounds))))
  parent <- floor(parent)
  #parent = rbind(ubcf.index, td.index, parent)
  parent <- cbind(parent, t(apply(parent, 1, fn)))
  cat("ranking the initial population")
  cat("\n")
  ranking <- fastNonDominatedSorting.altered(parent[, (varNo + 1):(varNo + 
                                                                     objDim)])
  rnkIndex <- integer(popSize)
  i <- 1
  while (i <= length(ranking)) {
    rnkIndex[ranking[[i]]] <- i
    i <- i + 1
  }
  parent <- cbind(parent, rnkIndex)
  cat("crowding distance calculation")
  cat("\n")
  objRange <- apply(parent[, (varNo + 1):(varNo + objDim)], 
                    2, max) - apply(parent[, (varNo + 1):(varNo + objDim)], 
                                    2, min)
  cd <- crowdingDist4frnt(parent, ranking, objRange)
  parent <- cbind(parent, apply(cd, 1, sum))
  for (iter in 1:generations) {
    cat("---------------generation---------------", iter, 
        "starts")
    cat("\n")
    cat("tournament selection")
    cat("\n")
    matingPool <- tournamentSelection(parent, popSize, tourSize)
    cat("crossover operator")
    cat("\n")
    childAfterX <- boundedSBXover(matingPool[, 1:varNo], 
                                  lowerBounds, upperBounds, cprob, XoverDistIdx)
    cat("mutation operator")
    cat("\n")
    childAfterM <- boundedPolyMutation(childAfterX, lowerBounds, 
                                       upperBounds, mprob, MuDistIdx)
    cat("evaluate the objective fns of childAfterM")
    cat("\n")
    childAfterM <- cbind(childAfterM, t(apply(childAfterM, 
                                              1, fn)))
    cat("Rt = Pt + Qt")
    cat("\n")
    parentNext <- rbind(parent[, 1:(varNo + objDim)], childAfterM)
    cat("ranking again")
    cat("\n")
    ranking <- fastNonDominatedSorting.altered(parentNext[, (varNo + 
                                                               1):(varNo + objDim)])
    i <- 1
    while (i <= length(ranking)) {
      rnkIndex[ranking[[i]]] <- i
      i <- i + 1
    }
    parentNext <- cbind(parentNext, rnkIndex)
    cat("crowded comparison again")
    cat("\n")
    objRange <- apply(parentNext[, (varNo + 1):(varNo + 
                                                  objDim)], 2, max) - apply(parentNext[, (varNo + 
                                                                                            1):(varNo + objDim)], 2, min)
    cd <- crowdingDist4frnt(parentNext, ranking, objRange)
    parentNext <- cbind(parentNext, apply(cd, 1, sum))
    parentNext.sort <- parentNext[order(parentNext[, varNo + 
                                                     objDim + 1], -parentNext[, varNo + objDim + 2]), 
                                  ]
    cat("environmental selection")
    cat("\n")
    parent <- parentNext.sort[1:popSize, ]
    cat("---------------generation---------------", iter, 
        "ends")
    cat("\n")
    if (iter != generations) {
      cat("\n")
      cat("********** new iteration *********")
      cat("\n")
    }
    else {
      cat("********** stop the evolution *********")
      cat("\n")
    }
  }
  result = list(functions = fn, parameterDim = varNo, objectiveDim = objDim, 
                lowerBounds = lowerBounds, upperBounds = upperBounds, 
                popSize = popSize, tournamentSize = tourSize, generations = generations, 
                XoverProb = cprob, XoverDistIndex = XoverDistIdx, mutationProb = mprob, 
                mutationDistIndex = MuDistIdx, parameters = parent[, 
                                                                   1:varNo], objectives = parent[, (varNo + 1):(varNo + 
                                                                                                                  objDim)], paretoFrontRank = parent[, varNo + objDim + 
                                                                                                                                                       1], crowdingDistance = parent[, varNo + objDim + 
                                                                                                                                                                                       2])
  class(result) = "nsga2R"
  return(result)
}

fastNonDominatedSorting.altered = function (inputData) 
{
  popSize = nrow(inputData)
  idxDominators = vector("list", popSize)
  idxDominatees = vector("list", popSize)
  for (i in 1:(popSize - 1)) {
    for (j in i:popSize) {
      if (i != j) {
        xi = inputData[i, ]
        xj = inputData[j, ]
        if (all(xi >= xj) && any(xi > xj)) {
          idxDominators[[j]] = c(idxDominators[[j]], 
                                 i)
          idxDominatees[[i]] = c(idxDominatees[[i]], 
                                 j)
        }
        else if (all(xj >= xi) && any(xj > xi)) {
          idxDominators[[i]] = c(idxDominators[[i]], 
                                 j)
          idxDominatees[[j]] = c(idxDominatees[[j]], 
                                 i)
        }
      }
    }
  }
  noDominators <- lapply(idxDominators, length)
  rnkList <- list()
  rnkList <- c(rnkList, list(which(noDominators == 0)))
  solAssigned <- c()
  solAssigned <- c(solAssigned, length(which(noDominators == 
                                               0)))
  while (sum(solAssigned) < popSize) {
    Q <- c()
    noSolInCurrFrnt <- solAssigned[length(solAssigned)]
    for (i in 1:noSolInCurrFrnt) {
      solIdx <- rnkList[[length(rnkList)]][i]
      hisDominatees <- idxDominatees[[solIdx]]
      for (i in hisDominatees) {
        noDominators[[i]] <- noDominators[[i]] - 1
        if (noDominators[[i]] == 0) {
          Q <- c(Q, i)
        }
      }
    }
    rnkList <- c(rnkList, list(sort(Q)))
    solAssigned <- c(solAssigned, length(Q))
  }
  return(rnkList)
}

boundedSBXover.altered =function (parent_chromosome, lowerBounds, upperBounds, cprob, 
          mu) 
{
  popSize = nrow(parent_chromosome)
  varNo = ncol(parent_chromosome)
  child <- parent_chromosome
  p <- 1
  for (i in 1:(popSize/2)) {
    if (runif(1) < cprob) {
      for (j in 1:varNo) {
        parent1 <- child[p, j]
        parent2 <- child[p + 1, j]
        yl <- lowerBounds[j]
        yu <- upperBounds[j]
        rnd = runif(1)
        if (rnd <= 0.5) {
          if (abs(parent1 - parent2) > 1e-06) {
            if (parent2 > parent1) {
              y2 <- parent2
              y1 <- parent1
            }
            else {
              y2 <- parent1
              y1 <- parent2
            }
            if ((y1 - yl) > (yu - y2)) {
              beta = 1 + (2 * (yu - y2)/(y2 - y1))
            }
            else {
              beta = 1 + (2 * (y1 - yl)/(y2 - y1))
            }
            alpha = 2 - (beta^(-(1 + mu)))
            rnd = runif(1)
            if (rnd <= 1/alpha) {
              alpha = alpha * rnd
              betaq = sign(alpha)*abs(alpha)^(1/(1 + mu))
            }
            else {
              alpha = alpha * rnd
              alpha = 1/(2 - alpha)
              
              betaq = sign(alpha)*abs(alpha)^(1/(1 + mu))
            }
            child1 = 0.5 * ((y1 + y2) - betaq * (y2 - 
                                                   y1))
            child2 = 0.5 * ((y1 + y2) + betaq * (y2 - 
                                                   y1))
          }
          else {
            betaq = 1
            y1 = parent1
            y2 = parent2
            child1 = 0.5 * ((y1 + y2) - betaq * (y2 - 
                                                   y1))
            child2 = 0.5 * ((y1 + y2) + betaq * (y2 - 
                                                   y1))
          }
          teste <- tryCatch({
            if (child1 > yu) {
              child1 = yu
            }
          },
          error = function(err) {
            print(paste("i",i))
            print(paste("j",j))
            print(paste("p",p))
            print(paste("y1",y1))
            print(paste("y2",y2))
            print(paste("rnd",rnd))
            print(paste("alpha",alpha))
            print(paste("betaq",betaq))
            print(parent_chromosome)
            print(child)
            #fwrite(parent_chromosome,"~/Documentos/experimento_doutorado/teste.txt",row.names = F,col.names = F)
            }
          )
          # if (is.na(child1)){
          #   print("breakpoint")
          # }
          if (child1 > yu) {
            child1 = yu
          }
          else if (child1 < yl) {
            child1 = yl
          }
          if (child2 > yu) {
            child2 = yu
          }
          else if (child2 < yl) {
            child2 = yl
          }
        }
        else {
          child1 = parent1
          child2 = parent2
        }
        child[p, j] <- child1
        child[p + 1, j] <- child2
      }
    }
    p <- p + 2
  }
  return(child)
}

