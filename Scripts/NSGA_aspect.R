library(dplyr)
library(data.table)
library(lsa)
library(nsga2R)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50
address <- "~/Documentos/experimento_doutorado/"

# aspects setup
# aspects: 1 = "Contemporaneity", 2 = "Gender", 3 = "Locality", 4 = "Genre")
aspects.to.diversify = c(1)
aspects.not.to.diversify = c(2,3,4)

########################################## Metric Functions ##########################################

# Intra List Diversity Metric (ILD)

similarity.function.genre = function(data){
  return(cosine(t(as.matrix(data))))
}

similarity.function.gender = function(data){
  n = nrow(data)
  m = matrix(0L,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(data[i,"gender"] == data[j,"gender"]){
        v.gender = 1
      } else {  
        v.gender = 1 / (1 + log10(as.numeric(freq.gender[which(data[i,"gender"] == freq.gender$gender),"total"])) 
                        * log10(as.numeric(freq.gender[which(data[j,"gender"] == freq.gender$gender),"total"])))
      }
      if(data[i,"type"] == data[j,"type"]){
        v.type = 1
      } else {  
        v.type = 1 / (1 + log10(as.numeric(freq.type[which(data[i,"type"] == freq.type$type),"total"])) 
                      * log10(as.numeric(freq.type[which(data[j,"type"] == freq.type$type),"total"])))
      }
      m[i,j] = (v.gender + v.type) / 2
    }
  }
  return(m)
}

similarity.function.locality = function(data){
  n = nrow(data)
  m = matrix(0L,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(data[i,"area"] == data[j,"area"]){
        m[i,j] = 1
      } else {  
        m[i,j] = 1 / (1 + log10(as.numeric(freq.area[which(data[i,"area"] == freq.area$area),"total"])) 
                        * log10(as.numeric(freq.area[which(data[j,"area"] == freq.area$area),"total"])))
      }
    }
  }
  return(m)
}

similarity.function.contemporaneity = function(data){
  n = nrow(data)
  m = matrix(0L,n,n)
  for(i in 1:n){
    for(j in 1:n){
      m[i,j] = 1 - (abs(data[i,"begin_date_year"] - data[j, "begin_date_year"]))
    }
  }
  return(m)
}

ILD = function(data, similarity.function){
  k=nrow(data)
  if(k > 1){
    sum.dissimilarity = 0
    similarity.matrix = similarity.function(data)
    dissimilarity.matrix = 1 - similarity.matrix
    sum.dissimilarity = sum(colSums(dissimilarity.matrix))
    return(sum.dissimilarity/(k*(k-1)))
  }else{
    return(0)
  }
}

# Distance List History Metric (DLH)

distance.function.genre = function(data, history){
  centroid = genre.centroids[which(genre.centroids$`user-id` == u),]
  centroid$`user-id` = NULL
  distance = 0
  for(i in 1:nrow(data)){
    distance = distance + cosine(as.vector(t(data[1,])),as.vector(t(centroid)))
  }
  return(distance/nrow(data))
}

distance.function.gender = function(data, history){
  rec <- data[, 2] 
  h <- history[, 2]
  
  rec.g <- sapply(rec, function(x) {freq.gender[which(x == freq.gender$gender), ]$total})
  h.g <- sapply(h, function(x) {freq.gender[which(x == freq.gender$gender), ]$total})
  
  rec <- data[, 1] 
  h <- history[, 1]
  
  rec.t <- sapply(rec, function(x) {freq.type[which(x == freq.type$type), ]$total})
  h.t <- sapply(h, function(x) {freq.type[which(x == freq.type$type), ]$total})
  
  f <- function(i, j) {
    ifelse(i == j, 1, 1 / (1 + log10(i) * log10(j)))
  }
  
  r1 <- outer(rec.g, h.g, f)
  r2 <- outer(rec.t, h.t, f)
  return(mean((r1 + r2) / 2))
}


distance.function.locality = function(data, history){
  rec <- as.vector(t(data))
  h <- as.vector(t(history))
  
  rec.g <- sapply(rec, function(x) {freq.area[which(x == freq.area$area), ]$total})
  h.g <- sapply(h, function(x) {freq.area[which(x == freq.area$area), ]$total})
  
  # rec <- data[, 1] 
  # h <- history[, 1]
  # 
  # rec.t <- sapply(rec, function(x) {freq.type[which(x == freq.type$type), ]$total})
  # h.t <- sapply(h, function(x) {freq.type[which(x == freq.type$type), ]$total})
  
  f <- function(i, j) {
    ifelse(i == j, 1, 1 / (1 + log10(i) * log10(j)))
  }
  
  r <- outer(rec.g, h.g, f)
  return(mean(r))
}

distance.function.contemporaneity = function(data, history){
  n = nrow(data)
  m = nrow(history)
  sum.contemporaneity = 0
  for(i in 1:n){
    for(j in 1:m){
      sum.contemporaneity = sum.contemporaneity + abs(data[i,"begin_date_year"] - history[j, "begin_date_year"])
    }
  }
  return(sum.contemporaneity / (m * n))
}

DLH = function(data, history, distance.function){
  return(distance.function(data, history))
}

# Pearson correlation

pearson.correlation = function(candidate,list.ubcf){
  soma = 0
  for(i in 1:nrow(list.ubcf)){
    soma = soma + cor.test(as.vector(list.ubcf[i,],mode = "numeric"),as.vector(candidate,mode = "numeric"),method = "pearson")$estimate
  }
  return(soma/nrow(list.ubcf))
}

########################################## NSGA2R Altered Functions ##########################################

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
            print("aqui")
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


########################################## DATA LOAD ##########################################

artist.data <- fread(paste0(address,"bases de dados/experimento/artist.data.txt"), 
                     sep = ";", 
                     verbose = TRUE,
                     header = TRUE)
artist.data = as.data.frame(artist.data)
artist.data$ended = NULL
artist.data$area.name = NULL
artist.data$area_type.name = NULL

# Input 0 in NA values

col = c(3:7)
for(i in col){
  artist.data[which(is.na(artist.data[,i])),i] = 0
}

# boundings for Contemporaneity
earliest.year = min(artist.data$begin_date_year)
latest.year = 2016L # the year the database was collected
year.gap = latest.year = earliest.year

# normalize begin_year
for(i in 1:nrow(artist.data)){
  artist.data[i, "begin_date_year"] = (artist.data[i, "begin_date_year"] - earliest.year) / year.gap
  
}

# Recommendations load

ubcf.top10 <- fread(paste0(address,"bases de dados/experimento/sample1000.ubcf.top50.csv"), 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top10 = as.data.frame(ubcf.top10)

td.top10 <- fread(paste0(address,"bases de dados/experimento/sample1000.topic-diversification.top10.txt"),
                    sep = "\t",
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
td.top10 = as.data.frame(td.top10)


data.train <- fread(paste0(address,"bases de dados/experimento/LFM_train.txt"), 
                   sep = "\t", 
                   verbose = TRUE,
                   header = TRUE)
data.train = as.data.frame(data.train)

data.test <- fread(paste0(address,"bases de dados/experimento/LFM_test.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.test = as.data.frame(data.test)

genre.centroids <- fread(paste0(address,"bases de dados/experimento/user.history.centroids.txt"), 
                         sep = ",", 
                         verbose = TRUE,
                         header = TRUE)
genre.centroids = as.data.frame(genre.centroids)

#################### Calculate frequencies for IOF for Gender and Locality  ####################

by.type = group_by(artist.data,type)
by.gender = group_by(artist.data,gender)
by.area = group_by(artist.data,area)

freq.type = summarise(by.type, total = n())
freq.gender = summarise(by.gender, total = n())
freq.area = summarise(by.area, total = n())

################################ OBJECTIVE FUNCTION ################################

# This function will be executed for a user, with specific data.test, specified aspects to diversify and a determined 
# list size (TOPN)

multi.objective = function(list.elements.index){
   size = length(list.elements.index)
   list.elements = as.data.frame(artist.data.new[list.elements.index,"Artist"])
   names(list.elements) = "artist"
   n = length(aspects.to.diversify)
   df = artist.data.new[artist.data.new$Artist %in% list.elements$artist,]
   sum.ild = 0
   
   start = proc.time()
   
   for(i in aspects.to.diversify){
     df.aspect = df[(aspects[[i]])]
     sum.ild = sum.ild + ILD(data = df.aspect, similarity.function = similarity.functions[[i]])
   }
   y1 = (sum.ild / n)
   
   print(proc.time() - start)
   
   start = proc.time()
   
   sum.dlh = 0
   for(i in aspects.not.to.diversify){
     df.aspect = df[(aspects[[i]])]
     sum.dlh = sum.dlh + DLH(data = df.aspect, history = data.train.user[(aspects[[i]])], distance.function = distance.functions[[i]])
   }
   y2 = (sum.dlh / length(aspects.not.to.diversify))
   
   print(proc.time() - start)
   
   return(c(y1, y2))
}

########################################## ASPECTS ##########################################

aspects <- vector(mode="list", length=4)
names(aspects) <- c("Contemporaneity", "Gender", "Locality", "Genre")
aspects[[1]] <- 3:4; aspects[[2]] <- c(5,7); aspects[[3]] <- 6; aspects[[4]] <- 8:ncol(artist.data)
similarity.functions <- vector(mode="list", length=4)
names(similarity.functions) <- c("Contemporaneity", "Gender", "Locality", "Genre")
similarity.functions[[1]] <- similarity.function.contemporaneity; similarity.functions[[2]] <- similarity.function.gender
similarity.functions[[3]] <- similarity.function.locality; similarity.functions[[4]] <- similarity.function.genre

distance.functions <- vector(mode="list", length=4)
names(distance.functions) <- c("Contemporaneity", "Gender", "Locality", "Genre")
distance.functions[[1]] <- distance.function.contemporaneity; distance.functions[[2]] <- distance.function.gender
distance.functions[[3]] <- distance.function.locality; distance.functions[[4]] <- distance.function.genre

################################ NSGA-II ################################

start.time <- Sys.time()
users = unique(ubcf.top10$user)
user = 0
for(u in users){
  user = user + 1; print(user)
  artist.data.listenned = unique(data.train[which(data.train$`user-id` == u),]$`artist-name`)
  artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
  ubcf.index = which(artist.data$Artist %in% ubcf.top10[which(ubcf.top10$user == u),"artist"])
  td.index = which(artist.data$Artist %in% as.data.frame(td.top10[which(td.top10$user == u),"artist"])[1:10,]) #adapted for eliminate duplicated recommendation

  data.train.user = artist.data[artist.data$Artist %in% artist.data.listenned,]
  
  results <- nsga2R.altered(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
              upperBounds=rep(nrow(artist.data.new),TOPN), popSize=10, tourSize=2,
              generations=20, cprob=0.9, XoverDistIdx=20, mprob=0.1, MuDistIdx=10,
              ubcf.index, td.index)

  nondominated = (fastNonDominatedSorting(results$objectives))[[1]]
  best.solution = which.max(results$objectives[nondominated,1]+results$objectives[nondominated,2])
  
  if(length(nondominated) == 1){
    df = bind_cols(as.data.frame(rep(u,TOPN)),
                 as.data.frame(artist.data.new[as.vector(t(as.data.frame(t(results$parameters[nondominated,]))[best.solution,])),"Artist"]))
  }else{
    df = bind_cols(as.data.frame(rep(u,TOPN)),
                   as.data.frame(artist.data.new[results$parameters[nondominated,][best.solution,],"Artist"]))
  }
  fwrite(df,
         paste0(address,"bases de dados/experimento/resultados/sample1000.nsga.top10.div1.pop10.gen20.txt"),
         col.names = FALSE, row.names = FALSE, quote = TRUE, append = TRUE)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

