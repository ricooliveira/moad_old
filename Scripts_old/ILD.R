library(dplyr)
library(data.table)
library(lsa)
#library(nomclust)

########################## CONSTANTS ##########################
TOPN = 10
TOPN_RERANK = 50
address <- "~/Documentos/experimento_doutorado/"

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
      m[i,j] = 1 - (abs(data[i,"begin_date_year"] - data[j, "begin_date_year"]))
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
    sum.dissimilarity = sum(colSums(dissimilarity.matrix))
    return(sum.dissimilarity/(k*(k-1)))
  }else{
    return(0)
  }
}

# Distance List History Metric (DLH)

distance.function.genre = function(data, history){
  centroid = genre.centroids[which(genre.centroids$`user-id` == u),]
  centroid$`user-id` = NULL
  distance = 0
  for(i in 1:nrow(data)){
    distance = distance + cosine(as.vector(t(data[1,])),as.vector(t(centroid)))
  }
  return(distance/nrow(data))
}

distance.function.gender = function(data, history){
  rec <- data[, 2] 
  h <- history[, 2]
  
  rec.g <- sapply(rec, function(x) {freq.gender[which(x == freq.gender$gender), ]$total})
  h.g <- sapply(h, function(x) {freq.gender[which(x == freq.gender$gender), ]$total})
  
  rec <- data[, 1] 
  h <- history[, 1]
  
  rec.t <- sapply(rec, function(x) {freq.type[which(x == freq.type$type), ]$total})
  h.t <- sapply(h, function(x) {freq.type[which(x == freq.type$type), ]$total})
  
  f <- function(i, j) {
    ifelse(i == j, 1, 1 / (1 + log10(i) * log10(j)))
  }
  
  r1 <- outer(rec.g, h.g, f)
  r2 <- outer(rec.t, h.t, f)
  return(mean((r1 + r2) / 2))
}

# distance.function.gender = function(data, history){
#   data.t <- as.data.frame(t(data))
#   history.t <- as.data.frame(t(history))
# 
#   my_correlation_2 <- function(h.col, d.col) {
#     # get aspects from artist
#     h.type <- h.col[1]
#     h.gender <- h.col[2]
# 
#     d.type <- d.col[1]
#     d.gender <- d.col[2]
# 
#     #init
#     v.gender = 1
#     v.type = 1
# 
#     # get global freq
#     h.gender.freq <- subset(freq.gender, freq.gender$gender == h.gender)$total
#     h.type.freq <- subset(freq.type, freq.type$type == h.type)$total
# 
#     d.gender.freq <- subset(freq.gender, freq.gender$gender == d.gender)$total
#     d.type.freq <- subset(freq.type, freq.type$type == d.type)$total
# 
#     # compute
#     if(h.gender != d.gender){
#       v.gender = 1 / (1 + log10(as.numeric(d.gender.freq)) * log10(as.numeric(h.gender.freq)))
#     }
#     if(h.type != d.type){
#       v.type = 1 / (1 + log10(as.numeric(d.type.freq)) * log10(as.numeric(h.type.freq)))
#     }
#     ((v.gender + v.type) / 2)
#   }
# 
# 
#   bl <- lapply(data.t, function(u){
#     lapply(history.t, function(v){
#       my_correlation_2(u,v) # Function with column from x and column from y as inputs
#     })
#   })
# 
#   return(mean(unlist(bl)))
# 
#   # n = nrow(data)
#   # m = nrow(history)
#   # sum.iof = 0
#   # for(i in 1:n){
#   #   for(j in 1:m){
#   #     if(data[i,"gender"] == history[j,"gender"]){
#   #       v.gender = 1
#   #     } else {
#   #       v.gender = 1 / (1 + log10(as.numeric(freq.gender[which(data[i,"gender"] == freq.gender$gender),"total"]))
#   #                       * log10(as.numeric(freq.gender[which(history[j,"gender"] == freq.gender$gender),"total"])))
#   #     }
#   #     if(data[i,"type"] == history[j,"type"]){
#   #       v.type = 1
#   #     } else {
#   #       v.type = 1 / (1 + log10(as.numeric(freq.type[which(data[i,"type"] == freq.type$type),"total"]))
#   #                     * log10(as.numeric(freq.type[which(history[j,"type"] == freq.type$type),"total"])))
#   #     }
#   #     sum.iof = sum.iof + ((v.gender + v.type) / 2)
#   #   }
#   # }
#   # return(sum.iof / (m * n))
# }

# distance.function.locality = function(data, history){
#   data.t <- as.data.frame(t(data))
#   history.t <- as.data.frame(t(history))
# 
#   my_correlation.area <- function(h.col, d.col) {
#     # get aspects from artist
#     h.area <- h.col[1]
#     #h.gender <- h.col[2]
# 
#     d.area <- d.col[1]
#     #d.gender <- d.col[2]
# 
#     #init
#     #v.gender = 1
#     v.area = 1
# 
#     # get global freq
#     # h.gender.freq <- subset(freq.gender, freq.gender$gender == h.gender)$total
#     h.area.freq <- subset(freq.area, freq.area$area == h.area)$total
# 
#     # d.gender.freq <- subset(freq.gender, freq.gender$gender == d.gender)$total
#     d.area.freq <- subset(freq.area, freq.area$area == d.area)$total
# 
#     # compute
#     # if(h.gender != d.gender){
#     #   v.gender = 1 / (1 + log10(as.numeric(d.gender.freq)) * log10(as.numeric(h.gender.freq)))
#     # }
#     if(h.area != d.area){
#       v.area = 1 / (1 + log10(as.numeric(d.area.freq)) * log10(as.numeric(h.area.freq)))
#     }
#     return(v.area)
#   }
# 
# 
#   bl <- lapply(data.t, function(u){
#     lapply(history.t, function(v){
#       my_correlation.area(u,v) # Function with column from x and column from y as inputs
#     })
#   })
# 
#   return(mean(unlist(bl)))
#   
#   # n = nrow(data)
#   # m = nrow(history)
#   # sum.iof = 0
#   # for(i in 1:n){
#   #   for(j in 1:m){
#   #     if(data[i,"area"] == history[j,"area"]){
#   #       v.area = 1
#   #     } else {
#   #       v.area = 1 / (1 + log10(as.numeric(freq.area[which(data[i,"area"] == freq.area$area),"total"]))
#   #                     * log10(as.numeric(freq.area[which(history[j,"area"] == freq.area$area),"total"])))
#   #     }
#   #     sum.iof = sum.iof + v.area
#   #   }
#   # }
#   # return(sum.iof / (m * n))
# }

distance.function.locality = function(data, history){
  rec <- as.vector(t(data))
  h <- as.vector(t(history))
  
  rec.g <- sapply(rec, function(x) {freq.area[which(x == freq.area$area), ]$total})
  h.g <- sapply(h, function(x) {freq.area[which(x == freq.area$area), ]$total})
  
  # rec <- data[, 1] 
  # h <- history[, 1]
  # 
  # rec.t <- sapply(rec, function(x) {freq.type[which(x == freq.type$type), ]$total})
  # h.t <- sapply(h, function(x) {freq.type[which(x == freq.type$type), ]$total})
  
  f <- function(i, j) {
    ifelse(i == j, 1, 1 / (1 + log10(i) * log10(j)))
  }
  
  r <- outer(rec.g, h.g, f)
  return(mean(r))
}

distance.function.contemporaneity = function(data, history){
  n = nrow(data)
  m = nrow(history)
  sum.contemporaneity = 0
  for(i in 1:n){
    for(j in 1:m){
      sum.contemporaneity = sum.contemporaneity + abs(data[i,"begin_date_year"] - history[j, "begin_date_year"])
    }
  }
  return(sum.contemporaneity / (m * n))
}

DLH = function(data, history, distance.function){
  return(distance.function(data, history))
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
  fwrite(df.rec,
         paste0(address,"bases de dados/experimento/sample1000.topic-diversification.top10.txt"),
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
