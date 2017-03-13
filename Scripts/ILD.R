library(dplyr)
library(data.table)
library(lsa)
library(nomclust)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50
address <- "~/Documents/experimento_doutorado/"

########################################## Functions ##########################################

# Intra List Diversity Metric (ILD)

ILD = function(list.ubcf,artist.data){ 
  k=nrow(list.ubcf)
  if(k > 1){
    df = artist.data[artist.data$Artist %in% list.ubcf$artist,]
    df$id = NULL
    df$Artist = NULL
    #similarity.matrix = eskin(df)
    sum.dissimilarity = 0
    for(i in 1:(nrow(list.ubcf)-1)){
      for(j in (i+1):nrow(list.ubcf)){
        sum.dissimilarity = sum.dissimilarity + (1 - as.numeric(pearson.correlation(df[i,],df[j,])))
      }
    }
    #dissimilarity.matrix = 1 - similarity.matrix
    #sum.dissimilarity = sum(colSums(dissimilarity.matrix)) - k
    return(sum.dissimilarity/(k*(k-1)))
  }else{
    return(0)
  }
}

# Pearson correlation

pearson.correlation = function(candidate,list.ubcf){
  soma = 0
  for(i in 1:nrow(list.ubcf)){
    soma = soma + cor.test(as.vector(list.ubcf[i,],mode = "numeric"),as.vector(candidate,mode = "numeric"),method = "pearson")$estimate
  }
  return(soma/nrow(list.ubcf))
}

########################################## DATA LOAD ##########################################

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

# Recommendations load

ubcf.top50 <- fread(paste0(address,"bases de dados/experimento/sample1000.ubcf.top50.csv"), 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top50 = as.data.frame(ubcf.top50)

########################################## ASPECTS ##########################################

aspects <- vector(mode="list", length=4)
names(aspects) <- c("Contemporaneity", "Gender", "Locality", "Genre")
aspects[[1]] <- 3:4; aspects[[2]] <- 5:7; aspects[[3]] <- 6; aspects[[4]] <- 7:ncol(artist.data)

########################################## Topic Diversification ##########################################

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
  fwrite(df.rec,"~/Documentos/Experimento Doutorado/bases de dados/experimento/sample1000.topic-diversification.top10.txt",
         row.names = FALSE, 
         col.names = FALSE, 
         sep = "\t", 
         append = TRUE,
         quote = TRUE)
}

#############################################################################################

# Tests

# list.ubcf = as.data.frame(ubcf.top50[1:50,])
# cosine(as.vector(artist.data[1,aspects[["Genre"]]], mode = "numeric"), as.vector(artist.data[2,aspects[["Genre"]]], mode = "numeric"))
# eskin(artist.data[1:2,aspects[["Genre"]]])



########################################## GARBAGE ##########################################

# # Join 
# 
# Join = function (artist, MB_albums, DBpedia_Artist_genres){
#   artist = data.frame(artist)
#   names(artist) = c("artist.name")
#   music.brainz = MB_artists[which(MB_artists$name == artist$artist.name),]
#   artist = inner_join(music.brainz, DBpedia_Artist_genres, by = "Artist")
#   return(artist)
# }
