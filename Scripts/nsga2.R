library(data.table)
library(nsga2R)

################################ Constants ################################

TOPN = 10
address <- "~/Documentos/Experimento Doutorado/"

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

col = c(1,4:7)
for(i in col){
  artist.data[which(is.na(artist.data[,i])),i] = 0
}

# boundings for Contemporaneity
earliest.year = min(artist.data$begin_date_year)
latest.year = 2016L # the year the database was collected
year.gap = latest.year = earliest.year

# Recommendations load

ubcf.top10 <- fread(paste0(address,"bases de dados/experimento/sample1000.ubcf.top10.csv"), 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top10 = as.data.frame(ubcf.top10)

# td.top10 <- fread(paste0(address,"bases de dados/experimento/sample1000.topic-diversification.pearson.top10.txt"), 
#                     sep = "\t", 
#                     verbose = TRUE,
#                     header = FALSE,
#                     col.names = c("user", "artist"))
# td.top10 = as.data.frame(td.top10)

data.test <- fread(paste0(address,"bases de dados/experimento/LFM_test.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.test = as.data.frame(data.test)

# IOF load
iof.gender <- fread(paste0(address,"bases de dados/experimento/sample1000.ubcf.top10.csv"), 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top10 = as.data.frame(ubcf.top10)


################################ OBJECTIVE FUNCTION ################################

# This function will be executed for a user, with specific data.test, specified aspects to diversify and a determined 
# list size (TOPN)

multi.objective = function(list.elements.index){
   size = length(list.elements.index)
   list.elements = as.data.frame(artist.data[list.elements.index,"Artist"])
   names(list.elements) = "artist"
   n = length(aspects.to.diversify)
   df = artist.data[artist.data$Artist %in% list.elements$artist,]
   sum.ild = 0
   for(i in aspects.to.diversify){
     df.aspect = df[(aspects[[i]])]
     sum.ild = sum.ild + ILD(data = df.aspect, similarity.function = functions[[i]])
   }
   y1 = sum.ild / n
   sum.ild = 0
   for(i in aspects.not.to.diversify){
     df.aspect = df[(aspects[[i]])]
     sum.ild = sum.ild + ILD(data = df.aspect, similarity.function = functions[[i]])
   }
   y2 = 1 - (sum.ild / (length(aspects) - n))
   return(c(y1, y2))
}

########################################## ASPECTS ##########################################

aspects <- vector(mode="list", length=4)
names(aspects) <- c("Contemporaneity", "Gender", "Locality", "Genre")
aspects[[1]] <- 3:4; aspects[[2]] <- c(5,7); aspects[[3]] <- 6; aspects[[4]] <- 8:ncol(artist.data)
functions <- vector(mode="list", length=4)
names(functions) <- c("Contemporaneity", "Gender", "Locality", "Genre")
functions[[1]] <- similarity.function.contemporaneity; functions[[2]] <- similarity.function.gender.locality
functions[[3]] <- similarity.function.gender.locality; functions[[4]] <- similarity.function.genre

################################ NSGA-II ################################

#setup
user = ubcf.top10$user[21] # test user
data.user = data.test[which(data.test$`user-id` == user),] # test data for specified user
# aspects: 1 = "Contemporaneity", 2 = "Gender", 3 = "Locality", 4 = "Genre")
aspects.to.diversify = c(1,2,3)
aspects.not.to.diversify = c(4)
#artist.data.aspects.to.diversify = artist.data[,c(1,2,aspects.to.diversify)]
#artist.data.aspects.not.to.diversify = artist.data[,c(1,2,aspects.not.to.diversify)]
TOPN = 10

start.time <- Sys.time()
results2 <- nsga2R(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN), upperBounds=rep(nrow(artist.data),TOPN),
                  popSize=10, tourSize=2, generations=20, cprob=0.9, XoverDistIdx=20, mprob=0.1,MuDistIdx=3)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

plot(results$objectives)
results
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
