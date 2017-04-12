library(dplyr)
library(data.table)
library(recommenderlab)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50
address <- "~/Documentos/experimento_doutorado/"

########################## LOAD ##########################

# Last.fm load
# LFM_treated <- fread("~/Documentos/Experimento Doutorado/bases de dados/LFM/LFM-treated.txt", sep=";")
LFM.artists.available <- fread(paste0(address,"bases de dados/experimento/LFM.artists.available.txt"), 
                               sep="\t",
                               verbose = TRUE)

######################################################

# Spliting LFM data in train and test data (80% / 20%)

# Trying to decrease user's history eliminating artists listenned less than 10 times
teste = group_by(LFM.artists.available,`user-id`,`artist-name`)
teste = summarise(teste,total = n())
teste = teste[which(teste$total >= 10),]
LFM.artists.available = inner_join(LFM.artists.available, teste, by = c("user-id", "artist-name"))

LFM.artists.available = LFM.artists.available[order(LFM.artists.available$`user-id`, LFM.artists.available$timestamp),]

percent.train = 0.8

LFM.train = data.frame()
LFM.test = data.frame()

quant.users = length(unique(LFM.artists.available$`user-id`))
for (u in unique(LFM.artists.available$`user-id`)){
  print(quant.users)
  quant.users = quant.users - 1
  LFM.prov = LFM.artists.available[which(LFM.artists.available$`user-id` == u), ]
  size = nrow(LFM.prov)
  size.train = round(size * percent.train)
  LFM.train = bind_rows(LFM.train,LFM.prov[1:size.train,])
  LFM.test = bind_rows(LFM.test,LFM.prov[(size.train+1):size,])
}

fwrite(LFM.train, 
       paste0(address,"bases de dados/experimento/LFM_train.txt"), 
       row.names = FALSE, col.names = TRUE, sep = "\t", na = "")

fwrite(LFM.test,
       paste0(address,"bases de dados/experimento/LFM_test.txt"), 
       row.names = FALSE, col.names = TRUE, sep = "\t", na = "")

######################################################

LFM.train <- fread(paste0(address,"bases de dados/experimento/LFM_train.txt"),
                   sep="\t", na.strings = "", verbose = TRUE)
                        
# Transforming LFM.artists.available in a rating matrix - Artists rating

users = unique(LFM.train$`user-id`)
quant.users = length(unique(LFM.train$`user-id`))
artists.listenned = unique(LFM.train$`artist-name`)
# LFM.affinity = as.data.frame(matrix(0L, quant.users, length(artists.listenned)))
# colnames(LFM.affinity) = c(artists.listenned)

byUserArtist = group_by(LFM.train, `user-id`, `artist-name`)
sumArtistsUser = summarise(byUserArtist,total = n())

# sumArtistsUser = sumArtistsUser[-which(is.na(sumArtistsUser$`artist-name`)),]

# for(i in 1:nrow(sumArtistsUser)){
#   print(i)
#   LFM.affinity[which(users == as.integer(sumArtistsUser[i,1])), as.character(sumArtistsUser[i,2])] = sumArtistsUser[i,3]
# }
# 
# write.table(LFM.affinity, "~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_affinity.txt", 
#             row.names = FALSE, col.names = TRUE, sep = "\t")
# 
# affinity.matrix = as(LFM.affinity,"realRatingMatrix")

colnames(sumArtistsUser) = c("user", "item", "rating")
sumArtistsUser = as.data.frame(sumArtistsUser)
affinity.matrix = as(sumArtistsUser,"realRatingMatrix")

Rec.model = Recommender(affinity.matrix,
                        method="UBCF", 
                        param=list(normalize = "Z-score",
                                   method="Cosine",
                                   nn=5))

ubcf.top.rerank = vector("list",length(users))

# Generate UBCF Top 50 Recommendations
for(i in 1:length(users)){ # refazer pra tirar o laço. Usar stack. Data frame transforma elementos em fatores, atrapalha o stack
  print(i)
  recommended.items <- predict(Rec.model, affinity.matrix[as.character(users[i]),], n = TOPN_RERANK)
  ubcf.top.rerank[[i]] = as.data.frame(as(recommended.items, "list"))
}

# Write UBCF Top 50 Recommendations
for(i in 1:length(users)){
  print(i)
  df.user = as.data.frame(matrix(as.character(users[i]),TOPN_RERANK,1))
  df.user = bind_cols(df.user, ubcf.top.rerank[[i]])
  fwrite(df.user, 
         paste0(address,"bases de dados/experimento/sample1000.ubcf.top50.csv"),
         row.names = FALSE, col.names = FALSE, sep = ";", append = TRUE, quote = TRUE)
}

# Write UBCF Top 10 Recommendations
for(i in 1:length(users)){
  print(i)
  df.user = as.data.frame(matrix(as.character(users[i]),TOPN,1))
  df.user = bind_cols(df.user, as.data.frame(ubcf.top.rerank[[i]][1:10,]))
  fwrite(df.user, 
         paste0(address,"bases de dados/experimento/sample1000.ubcf.top10.csv"),
         row.names = FALSE, col.names = FALSE, sep = ";", append = TRUE, quote = TRUE)
}

########################################## Topic Diversification ##########################################

ubcf.top50 <- fread(paste0(address,"bases de dados/experimento/sample1000.ubcf.top50.csv"), 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top50 = as.data.frame(ubcf.top50)

users = unique(ubcf.top50$user) # REFACTOR ME, PLEASE
attributes.to.compare = c(1,4:ncol(artist.data))
x=1
for(u in users){
  print(x); x = x + 1
  df.rec = ubcf.top50[which(ubcf.top50$user == u),]
  df.metadata = artist.data[artist.data$Artist %in% df.rec$artist,]
  topic.diversification = df.metadata[which(df.rec[1,"artist"] == df.metadata$Artist),]
  for(z in 2:TOPN){
    df.metadata = df.metadata[!df.metadata$Artist %in% topic.diversification$Artist,]
    for(i in 1:nrow(df.metadata)){ # replace this with a apply code
      df.metadata[i,"dist"] = pearson.correlation(df.metadata[i,attributes.to.compare],
                                                  topic.diversification[,attributes.to.compare])
    }
    df.metadata = df.metadata[order(df.metadata$dist,decreasing = TRUE),]
    df.metadata$dist = NULL
    topic.diversification = bind_rows(topic.diversification,df.metadata[1,])
  }
  df.rec = df.rec[df.rec$artist %in% topic.diversification$Artist,]
  fwrite(df.rec,
         paste0(address,"bases de dados/experimento/sample1000.topic-diversification.top10.txt"),
         row.names = FALSE, 
         col.names = FALSE, 
         sep = "\t", 
         append = TRUE,
         quote = TRUE)
}

