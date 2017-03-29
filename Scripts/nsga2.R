library(data.table)
library(nsga2R)

################################ Constants ################################

TOPN = 10
address <- "~/Documentos/experimento_doutorado/"

################################ Data Load ################################

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

ubcf.top10 <- fread(paste0(address,"bases de dados/experimento/sample1000.ubcf.top10.csv"), 
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
   start.time <- Sys.time()
   for(i in aspects.to.diversify){
     df.aspect = df[(aspects[[i]])]
     sum.ild = sum.ild + ILD(data = df.aspect, similarity.function = similarity.functions[[i]])
   }
   end.time <- Sys.time()
   time.taken <- end.time - start.time
   time.taken
   y1 = (sum.ild / n)
   
   sum.dlh = 0
   start.time <- Sys.time()
   for(i in aspects.not.to.diversify){
     df.aspect = df[(aspects[[i]])]
     start.time <- Sys.time()
     sum.dlh = sum.dlh + DLH(data = df.aspect, history = data.train.user[(aspects[[i]])], distance.function = distance.functions[[i]])
   }
   end.time <- Sys.time()
   time.taken <- end.time - start.time
   time.taken
   y2 = (sum.dlh / length(aspects.not.to.diversify))
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

#setup
# aspects: 1 = "Contemporaneity", 2 = "Gender", 3 = "Locality", 4 = "Genre")
aspects.to.diversify = c(1)
aspects.not.to.diversify = c(2,3,4)

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
  
  start.time <- Sys.time()
  # results <- nsga2R.altered.random(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
  #                   upperBounds=rep(nrow(artist.data.new),TOPN), popSize=10, tourSize=2,
  #                   generations=20, cprob=0.9, XoverDistIdx=20, mprob=0.1,MuDistIdx=3)
  # 
  
  results <- nsga2R.altered(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN),
              upperBounds=rep(nrow(artist.data.new),TOPN), popSize=10, tourSize=2,
              generations=20, cprob=0.9, XoverDistIdx=20, mprob=0.1, MuDistIdx=3,
              ubcf.index, td.index)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  time.taken
  
  df = bind_cols(as.data.frame(rep(u,TOPN)),
                 as.data.frame(artist.data.new[results$parameters[10,],"Artist"]))
  fwrite(df,
         paste0(address,"bases de dados/experimento/sample1000.nsga.top10.div1.pop10.gen20.txt"),
         col.names = FALSE, row.names = FALSE, quote = TRUE, append = TRUE)
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(results$objectives)
resultados = unique(results$parameters)
objetivos = unique(results$objectives)
#recommendation.list = artist.data
################################ Example ################################
# 
# results <- nsga2R(fn=zdt3, varNo=30, objDim=2, lowerBounds=rep(0,30), upperBounds=rep(1,30),
#                   popSize=50, tourSize=2, generations=50, cprob=0.9, XoverDistIdx=20, mprob=0.1,MuDistIdx=3)
# 
# plot(results$objectives)

################################ Garbage ################################

# user = ubcf.top10$user[1]
# data.user = data.test[which(data.test$`user-id` == user),]
# precision(list.elements = ubcf.top10[which(ubcf.top10$user == user),],data.user, TOPN)
# precision(list.elements = td.top10[which(td.top10$user == user),],data.user, TOPN)
# ILD(list.ubcf = ubcf.top10[which(ubcf.top10$user == user),],artist.data)
# ILD(list.ubcf = ubcf.top10[which(td.top10$user == user),],artist.data)
