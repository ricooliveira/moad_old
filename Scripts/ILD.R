library(dplyr)
library(data.table)
library(lsa)
library(nomclust)

########################################## Functions ##########################################

# Intra List Diversity Metric (ILD)

similarity = function(artist1, artist2){
  return(cosine(as.vector(artist1),as.vector(artist2)))
}

ILD = function(list.ubcf,artist.data){ 
  if(nrow(list.ubcf) > 1){
    soma = 0
    for(i in 1:(nrow(list.ubcf)-1)){
      for(j in (i+1):nrow(list.ubcf)){
        soma = soma + (1 - similarity(artist.data[which(artist.data$Artist == list.ubcf[i,"artist"]),],
                                      artist.data[which(artist.data$Artist == list.ubcf[j,"artist"]),]))
      }
    }
    k=nrow(list.ubcf)
    return(soma/(k*k-k))
  }else{
    return(0)
  }
}

# Artists data load

artist.data <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/artist.data.txt", 
                    sep = ";", 
                    verbose = TRUE,
                    header = TRUE)
artist.data = as.data.frame(artist.data)


# Recommendations load

ubcf.top50 <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/sample1000.ubcf.top50.csv", 
                    sep = ";", 
                    verbose = TRUE,
                    header = FALSE,
                    col.names = c("user", "artist"))
ubcf.top50 = as.data.frame(ubcf.top50)

# Tests

list.ubcf = as.data.frame(ubcf.top50[1:50,])


########################################## ILD ##########################################


artists.attribs = data.frame()

if

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
