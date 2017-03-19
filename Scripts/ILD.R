library(dplyr)
library(data.table)
library(lsa)
#library(nomclust)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50
address <- "~/Documents/experimento_doutorado/"

########################################## Functions ##########################################

# Intra List Diversity Metric (ILD)

similarity.function.genre = function(data){
  return(cosine(t(as.matrix(data))))
}

similarity.function.gender = function(data){
  n = nrow(data)
  m = matrix(0L,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(data[i,"gender"] == data[j,"gender"]){
        v.gender = 1
      } else {  
        v.gender = 1 / (1 + log10(as.numeric(freq.gender[which(data[i,"gender"] == freq.gender$gender),"total"])) 
                        * log10(as.numeric(freq.gender[which(data[j,"gender"] == freq.gender$gender),"total"])))
      }
      if(data[i,"type"] == data[j,"type"]){
        v.type = 1
      } else {  
        v.type = 1 / (1 + log10(as.numeric(freq.type[which(data[i,"type"] == freq.type$type),"total"])) 
                      * log10(as.numeric(freq.type[which(data[j,"type"] == freq.type$type),"total"])))
      }
      m[i,j] = (v.gender + v.type) / 2
    }
  }
  return(m)
}

similarity.function.locality = function(data){
  n = nrow(data)
  m = matrix(0L,n,n)
  for(i in 1:n){
    for(j in 1:n){
      if(data[i,"area"] == data[j,"area"]){
        m[i,j] = 1
      } else {  
        m[i,j] = 1 / (1 + log10(as.numeric(freq.area[which(data[i,"area"] == freq.area$area),"total"])) 
                        * log10(as.numeric(freq.area[which(data[j,"area"] == freq.area$area),"total"])))
      }
    }
  }
  return(m)
}

similarity.function.contemporaneity = function(data){
  n = nrow(data)
  m = matrix(0L,n,n)
  for(i in 1:n){
    for(j in 1:n){
      m[i,j] = (abs(data[i,"begin_date_year"] - data[j, "begin_date_year"])) / year.gap
    }
  }
  return(m)
}

ILD = function(data, similarity.function){
  k=nrow(data)
  if(k > 1){
    #df$id = NULL
    #df$Artist = NULL
    sum.dissimilarity = 0
    similarity.matrix = similarity.function(data)
    dissimilarity.matrix = 1 - similarity.matrix
    sum.dissimilarity = sum(colSums(dissimilarity.matrix)) - k
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

# boundings for Contemporaneity
earliest.year = min(artist.data$begin_date_year)
latest.year = 2016L # the year the database was collected
year.gap = latest.year = earliest.year

# Input 0 in NA values

col = c(3:7)
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

#################### Calculate frequencies for IOF for Gender and Locality  ####################

by.type = group_by(artist.data,type)
by.gender = group_by(artist.data,gender)
by.area = group_by(artist.data,area)

freq.type = summarise(by.type, total = n())
freq.gender = summarise(by.gender, total = n())
freq.area = summarise(by.area, total = n())


############################## Write IOF for Gender and Locality  ##############################

# This information must be pre-computed for the use of frequencies in all the elements, not only the frequencies
# on the recommended list

# Very high processing time

# start.time <- Sys.time()
# df.iof.gender = artist.data[,aspects[[2]]]
# iof.gender = iof(df.iof.gender)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# 
# start.time <- Sys.time()
# df.iof.locality = artist.data[,aspects[[3]]]
# iof.locality = iof(df.iof.locality)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# fwrite(iof.gender,
#        paste0(address,"bases de dados/experimento/iof.gender.txt"),
#        row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)
# fwrite(iof.locality,
#        paste0(address,"bases de dados/experimento/iof.locality.txt"),
#        row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE)

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

# code snipet for pearson correlation
# sum.dissimilarity = 0
# for(i in 1:(nrow(list.ubcf)-1)){ # this code snipet is used for pearson correlation
#   for(j in (i+1):nrow(list.ubcf)){
#     sum.dissimilarity = sum.dissimilarity + (1 - as.numeric(pearson.correlation(df[i,],df[j,])))
#   }
# }
