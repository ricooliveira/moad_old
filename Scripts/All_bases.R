library(readr)
library(dplyr)
library(recommenderlab)

# Music Brainz load
# mb_albums <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/experimento/mb_albums.csv", 
#                         "\t", escape_double = FALSE, trim_ws = TRUE)
 
# Last.fm load
LFM_treated <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/LFM/LFM-treated.txt", 
                          "\t", escape_double = FALSE, trim_ws = TRUE)

# DBpedia load
# DBpedia_Artist_genres <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/DBpedia/DBpedia Artist_genres.txt", 
#                                     "\t", escape_double = FALSE, trim_ws = TRUE)
 
# Exclude genres that doesn't contain at least 5 artists
# min.artists = 5
# quant = c(min.artists)
# for (i in 2:ncol(DBpedia_Artist_genres)){
#   quant = c(quant,sum(DBpedia_Artist_genres[,i]))
# }
# DBpedia_Artist_genres = DBpedia_Artist_genres[,-which(quant<min.artists)]

######################################################

# Spliting LFM data in train and test data (80% / 20%)

LFM_treated = LFM_treated[order(LFM_treated$`user-id`, LFM_treated$timestamp),]
percent.train = 0.8

LFM.train = data.frame()
LFM.test = data.frame()

quant.users = length(unique(LFM_treated$`user-id`))
for (u in unique(LFM_treated$`user-id`)){
  # print(quant.users)
  # quant.users = quant.users - 1
  LFM.prov = LFM_treated[which(LFM_treated$`user-id` == u), ]
  size = nrow(LFM.prov)
  size.train = round(size * percent.train)
  LFM.train = bind_rows(LFM.train,LFM.prov[1:size.train,])
  LFM.test = bind_rows(LFM.test,LFM.prov[(size.train+1):size,])
}

write.table(LFM.train, "~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_train.txt", 
            row.names = FALSE, col.names = TRUE, sep = "\t")

write.table(LFM.test, "~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_test.txt", 
            row.names = FALSE, col.names = TRUE, sep = "\t")

######################################################

LFM.train <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_train.txt", 
                        "\t", escape_double = FALSE, trim_ws = TRUE)

# Transforming LFM_treated in a rating matrix - Artists rating

quant.users = length(unique(LFM.train$`user-id`))
artists.listenned = unique(LFM.train$`artist-name`)
LFM.affinity = as.data.frame(matrix(0L, quant.users, length(artists.listenned)))
# row.names(LFM.affinity) = unique(LFM_treated$`user-id`)
LFM.affinity = cbind(unique(LFM.train$`user-id`),LFM.affinity)
# user.ids = unique(LFM_treated$`user-id`)
colnames(LFM.affinity) = c("user", artists.listenned)

byUserArtist = group_by(LFM.train, `user-id`, `artist-name`)
sumArtistsUser = summarise(byUserArtist,total = n())

sumArtistsUser = sumArtistsUser[-which(is.na(sumArtistsUser$`artist-name`)),]

for(i in 1:nrow(sumArtistsUser)){
  print(i)
  LFM.affinity[which(LFM.affinity$user == as.integer(sumArtistsUser[i,1])), as.character(sumArtistsUser[i,2])] = sumArtistsUser[i,3]
}

write.table(LFM.affinity, "~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM_affinity.txt", 
            row.names = FALSE, col.names = TRUE, sep = "\t")

affinity.matrix<- as(LFM.affinity,"realRatingMatrix")
Rec.model = Recommender(affinity.matrix,
                        method="UBCF", 
                        param=list(normalize = "Z-score",
                                   method="Cosine",
                                   nn=5))

# ######################################################
# 
# #Teste menos dados
# for(x in 1:9480){
#   LFM_treated.part = LFM_treated[((x-1)*1000+1):(1000*x),]
#   print(x)
#   LEs.lj.partial = left_join(LFM_treated.part, mb_albums, by = c("artist-name" = "artist.name"))
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
# #LEs = merge(DBpedia_Artist_genres, LFM_treated, by.x = "Artist", by.y = "artist-name")
# #LEs.inner = inner_join(LFM_treated, DBpedia_Artist_genres, by = c("artist-name" = "Artist"))
# #LEs.inner = inner_join(LFM_treated, mb_albums, by = c("artist-name" == "artist.name"))
# 
# # Calculate the number of cores
# no_cores <- detectCores() - 1
# # Initiate cluster
# cl <- makeCluster(no_cores)
# 
# parLapply(cl, LFM_treated, FUN = inner_join, mb_albums, by = c("artist-name" == "artist.name"))
