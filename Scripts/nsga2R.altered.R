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
