users = unique(data.train$`user-id`)

subsample.index = runif(30,1,length(users))
subsample = users[subsample.index]

aspects.to.diversify = c(4)
aspects.not.to.diversify = c(1,2,3)
user = 0
popSize = 10
generations = 10
cprob = 0.9
mprob = 0.1



for(i in c(10,20,30,50)){
  user = 0
  for(u in subsample){
    user = user + 1; print(paste0("Generations - user ",user))
    artist.data.listenned = unique(data.train[which(data.train$`user-id` == u),]$`artist-name`)
    artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
    ubcf.index = which(artist.data$Artist %in% ubcf.top10[which(ubcf.top10$user == u),"artist"])
    td.index = which(artist.data$Artist %in% as.data.frame(td.top10[which(td.top10$user == u),"artist"])[1:10,]) #adapted for eliminate duplicated recommendation
    results <- nsga2R.altered.random(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                                      upperBounds=rep(nrow(artist.data.new),TOPN), popSize, tourSize=2,
                                      generations = i, cprob, XoverDistIdx=20, mprob,MuDistIdx=3)
    results2 <- nsga2R.altered(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                               upperBounds=rep(nrow(artist.data.new),TOPN), popSize, tourSize=2,
                               generations = i, cprob, XoverDistIdx=20, mprob, MuDistIdx=3,
                               ubcf.index, td.index)
    best1 = evaluate(results)
    best2 = evaluate(results2)
    fwrite(as.data.frame(best1),
           paste0(address,"bases de dados/experimento/parameters/generations-",i,".random.txt"),
           row.names = F,
           col.names = F,
           append = T
    )
    fwrite(as.data.frame(best2),
           paste0(address,"bases de dados/experimento/parameters/generations-",i,".lists.txt"),
           row.names = F,
           col.names = F,
           append = T
    )
  }
}

for(i in c(5,10,20,30)){
  user = 0
  for(u in subsample){
    user = user + 1; print(paste0("PopSize - user ",user))
    artist.data.listenned = unique(data.train[which(data.train$`user-id` == u),]$`artist-name`)
    artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
    ubcf.index = which(artist.data$Artist %in% ubcf.top10[which(ubcf.top10$user == u),"artist"])
    td.index = which(artist.data$Artist %in% as.data.frame(td.top10[which(td.top10$user == u),"artist"])[1:10,]) #adapted for eliminate duplicated recommendation
    results <- nsga2R.altered.random(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                                     upperBounds=rep(nrow(artist.data.new),TOPN), popSize = i, tourSize=2,
                                     generations, cprob, XoverDistIdx=20, mprob,MuDistIdx=3)
    results2 <- nsga2R.altered(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                               upperBounds=rep(nrow(artist.data.new),TOPN), popSize = i, tourSize=2,
                               generations, cprob, XoverDistIdx=20, mprob, MuDistIdx=3,
                               ubcf.index, td.index)
    best1 = evaluate(results)
    best2 = evaluate(results2)
    fwrite(as.data.frame(best1),
           paste0(address,"bases de dados/experimento/parameters/popSize-",i,".random.txt"),
           row.names = F,
           col.names = F,
           append = T
    )
    fwrite(as.data.frame(best2),
           paste0(address,"bases de dados/experimento/parameters/popSize-",i,".lists.txt"),
           row.names = F,
           col.names = F,
           append = T
    )
  }
}

for(i in c(0.1,0.5,0.9)){
  user=0
  for(u in subsample){
    user = user + 1; print(paste0("mprob - user ",user))
    artist.data.listenned = unique(data.train[which(data.train$`user-id` == u),]$`artist-name`)
    artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
    ubcf.index = which(artist.data$Artist %in% ubcf.top10[which(ubcf.top10$user == u),"artist"])
    td.index = which(artist.data$Artist %in% as.data.frame(td.top10[which(td.top10$user == u),"artist"])[1:10,]) #adapted for eliminate duplicated recommendation
    results <- nsga2R.altered.random(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                                     upperBounds=rep(nrow(artist.data.new),TOPN), popSize, tourSize=2,
                                     generations, cprob, XoverDistIdx=20, mprob = i, MuDistIdx=3)
    results2 <- nsga2R.altered(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                               upperBounds=rep(nrow(artist.data.new),TOPN), popSize, tourSize=2,
                               generations, cprob, XoverDistIdx=20, mprob = i, MuDistIdx=3,
                               ubcf.index, td.index)
    best1 = evaluate(results)
    best2 = evaluate(results2)
    fwrite(as.data.frame(best1),
           paste0(address,"bases de dados/experimento/parameters/mprob-",i,".random.txt"),
           row.names = F,
           col.names = F,
           append = T
    )
    fwrite(as.data.frame(best2),
           paste0(address,"bases de dados/experimento/parameters/mprob-",i,".lists.txt"),
           row.names = F,
           col.names = F,
           append = T
    )
  }
}

evaluate = function(results){
  nondominated = (fastNonDominatedSorting(results$objectives))[[1]]
  best.solution = which.max(results$objectives[nondominated,1]+results$objectives[nondominated,2])
  row = c(length(nondominated),results$objectives[nondominated,][best.solution,])
  return(t(row))
}

