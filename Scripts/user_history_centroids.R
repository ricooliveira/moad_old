library(data.table)
library(dplyr)

address <- "~/Documentos/experimento_doutorado/"

data.train <- fread(paste0(address,"bases de dados/experimento/LFM_train.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    header = TRUE)
data.train = as.data.frame(data.train)
names(data.train) = c("album-id","artist-id","user-id","timestamp","country","age","gender","playcount",
                      "registered timestamp","Artist","album-name")

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

history = group_by(data.train,`user-id`,Artist)
history = summarise(history,total = n())

history = inner_join(history, artist.data, by = "Artist")
history$id = NULL
history$begin_date_year = NULL
history$end_date_year = NULL
history$type = NULL
history$area = NULL
history$gender = NULL
history$Artist = NULL

# history.centroid = data$total %*% as.matrix(data[,3:ncol(data)])

users = unique(history$`user-id`)
history.prov = history
history.prov$total = NULL
fwrite(as.data.frame(t(names(history.prov))),
       paste0(address,"bases de dados/experimento/user.history.centroids.txt"),
       row.names = F, col.names = F)
i = 1
for(u in users){
  print(i);i = i + 1
  data = history[which(history$`user-id` == u),]
  centroid = data$total %*% as.matrix(data[,3:ncol(data)])
  centroid = centroid / sum(data$total)
  fwrite(cbind(u,as.data.frame(centroid)),
         paste0(address,"bases de dados/experimento/user.history.centroids.txt"),
         row.names = F, col.names = F, append = T)
}

# calculate.centroid = function(data){
#   return(as.vector(data$total) %*% as.matrix(data[,3:ncol(data)]))
# }
# 
# by.user = group_by(history,`user-id`)
# history.centroids = summarise(history,calculate.centroid)
