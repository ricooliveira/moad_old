users = unique(data.train$`user-id`)

subsample.index = runif(30,1,length(users))
subsample = users[subsample.index]

aspects.to.diversify = c(1,2,3)
aspects.not.to.diversify = c(4)

start.time <- Sys.time()
user = 0
for(u in subsample){
  user = user + 1; print(user)
  artist.data.listenned = unique(data.train[which(data.train$`user-id` == u),]$`artist-name`)
  artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
  # ubcf.index = which(artist.data$Artist %in% ubcf.top10[which(ubcf.top10$user == u),"artist"])
  # td.index = which(artist.data$Artist %in% as.data.frame(td.top10[which(td.top10$user == u),"artist"])[1:10,]) #adapted for eliminate duplicated recommendation
  popSize = 10
  generations = 10
  cprob = 0.9
  mprob = 0.1
  results <- nsga2R.altered.random(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
                                    upperBounds=rep(nrow(artist.data.new),TOPN), popSize, tourSize=2,
                                    generations, cprob, XoverDistIdx=20, mprob,MuDistIdx=3)
  # results2 <- nsga2R.altered(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
  #                            upperBounds=rep(nrow(artist.data.new),TOPN), popSize=10, tourSize=2,
  #                            generations=10, cprob=0.9, XoverDistIdx=20, mprob=0.1,MuDistIdx=3,
  #                            ubcf.index, td.index)
  nondominated = (fastNonDominatedSorting(results$objectives))[[1]]
  best.solution = which.max(results$objectives[nondominated,1]+results$objectives[nondominated,2])
  row = c(length(nondominated),results$objectives[nondominated,][best.solution,])
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
