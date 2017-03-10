library(nsga2R)

################################ Constants ################################

TOPN = 10

################################ Data Load ################################

artist.data <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/artist.data.txt", 
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

# Recommendations load

ubcf.top10 <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/sample1000.ubcf.top10.csv", 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top10 = as.data.frame(ubcf.top10)

td.top10 <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/sample1000.topic-diversification,pearson.top10.txt", 
                    sep = "\t", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
td.top10 = as.data.frame(td.top10)

data.test <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_test.txt", 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.test = as.data.frame(data.test)

########################################## ASPECTS ##########################################

aspects <- vector(mode="list", length=4)
names(aspects) <- c("Contemporaneity", "Gender", "Locality", "Genre")
aspects[[1]] <- 4:5; aspects[[2]] <- 6:7; aspects[[3]] <- 1; aspects[[4]] <- 11:ncol(artist.data)

################################ OBJECTIVE FUNCTION ################################

# This function will be executed for a user, with specific data.test, specified aspects to diversify and a determined 
# list size (TOPN)

multi.objective = function(list.elements.index){
   size = length(list.elements.index)
   list.elements = as.data.frame(artist.data[list.elements.index,3])
   names(list.elements) = "artist"
   
   y1 = precision(list.elements, data.user, size)
   y2 = ILD(list.ubcf = list.elements, artist.data.aspects)
   return(c(y1, y2))
}

################################ NSGA-II ################################

#setup
user = ubcf.top10$user[21] # test user
data.user = data.test[which(data.test$`user-id` == user),] # test data for specified user
aspects.to.diversify = c(aspects[["Contemporaneity"]], aspects[["Gender"]], aspects[["Locality"]])
artist.data.aspects = artist.data[,c(2,3,aspects.to.diversify)]
TOPN = 10

results <- nsga2R(fn=multi.objective, varNo=TOPN, objDim=2, lowerBounds=rep(1,TOPN), upperBounds=rep(nrow(artist.data),TOPN),
                   popSize=5, tourSize=2, generations=50, cprob=0.9, XoverDistIdx=20, mprob=0.1,MuDistIdx=3)

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
