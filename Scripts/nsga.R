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

data.test <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_test.txt", 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.test = as.data.frame(data.test)


################################ Example ################################

results <- nsga2R(fn=zdt3, varNo=30, objDim=2, lowerBounds=rep(0,30), upperBounds=rep(1,30),
                  popSize=50, tourSize=2, generations=50, cprob=0.9, XoverDistIdx=20, mprob=0.1,MuDistIdx=3)

plot(results$objectives)
