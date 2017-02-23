library(readr)
library(dplyr)
library(data.table)
library(recommenderlab)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50

########################## LOAD ##########################

# Last.fm load
# LFM_treated <- fread("~/Documentos/Experimento Doutorado/bases de dados/LFM/LFM-treated.txt", sep=";")
LFM.artists.available <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM.artists.available.txt", 
                               sep="\t",
                               verbose = TRUE)

######################################################

# Spliting LFM data in train and test data (80% / 20%)

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

fwrite(LFM.train, "~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_train.txt", 
       row.names = FALSE, col.names = TRUE, sep = "\t", na = "")

fwrite(LFM.test, "~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_test.txt", 
       row.names = FALSE, col.names = TRUE, sep = "\t", na = "")

######################################################

LFM.train <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_train.txt", sep="\t", na.strings = "", verbose = TRUE)
                        
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
for(i in 1:length(users)){
  print(i)
  recommended.items <- predict(Rec.model, affinity.matrix[as.character(users[i]),], n = TOPN_RERANK)
  ubcf.top.rerank[[i]] = as.data.frame(as(recommended.items, "list"))
}

# Write UBCF Top 50 Recommendations
for(i in 1:length(users)){
  print(i)
  df.user = as.data.frame(matrix(as.character(users[i]),TOPN_RERANK,1))
  df.user = bind_cols(df.user, ubcf.top.rerank[[i]])
  fwrite(df.user, "~/Documentos/Experimento Doutorado/bases de dados/experimento/sample1000.ubcf.top50.csv",row.names = FALSE, col.names = FALSE, sep = ";", append = TRUE, quote = TRUE)
}

# Write UBCF Top 10 Recommendations
for(i in 1:length(users)){
  print(i)
  df.user = as.data.frame(matrix(as.character(users[i]),TOPN,1))
  df.user = bind_cols(df.user, as.data.frame(ubcf.top.rerank[[i]][1:10,]))
  fwrite(df.user, "~/Documentos/Experimento Doutorado/bases de dados/experimento/sample1000.ubcf.top10.csv",row.names = FALSE, col.names = FALSE, sep = ";", append = TRUE, quote = TRUE)
}

############################################ G A R B A G E ######################################################
# 
# #Teste menos dados
# for(x in 1:9480){
#   LFM.artists.available.part = LFM.artists.available[((x-1)*1000+1):(1000*x),]
#   print(x)
#   LEs.lj.partial = left_join(LFM.artists.available.part, mb_albums, by = c("artist-name" = "artist.name"))
#   if (x == 1){
#     LEs.lj = LEs.lj.partial
#   }else{
#     LEs.lj = bind_rows(LEs.lj,LEs.lj.partial)
#   }
# }
# 
# ######################################################
# # Merge LFM and DBpedia
# 
# #LEs = merge(DBpedia_Artist_genres, LFM.artists.available, by.x = "Artist", by.y = "artist-name")
# #LEs.inner = inner_join(LFM.artists.available, DBpedia_Artist_genres, by = c("artist-name" = "Artist"))
# #LEs.inner = inner_join(LFM.artists.available, mb_albums, by = c("artist-name" == "artist.name"))
# 
# # Calculate the number of cores
# no_cores <- detectCores() - 1
# # Initiate cluster
# cl <- makeCluster(no_cores)
# 
# parLapply(cl, LFM.artists.available, FUN = inner_join, mb_albums, by = c("artist-name" == "artist.name"))

# # Exemplos de obtenção de recomendação
# 
# # recommended top 5 items for a user 
# recommended.items <- predict(Rec.model, affinity.matrix["8686821",], n=10)
# # to display them
# as(recommended.items, "list")
# # to obtain the top 3
# recommended.items.top3 <- bestN(recommended.items, n = 3)
# # to display them
# as(recommended.items.top3, "list")
