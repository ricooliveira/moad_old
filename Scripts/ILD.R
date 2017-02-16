library(dplyr)
library(data.table)

########################################## Functions ##########################################

# Intra List Diversity Metric (ILD)

ILD = function(lista,){
  if(nrow(lista) > 1){
    soma = 0
    for(i in 1:(nrow(lista)-1)){
      for(j in (i+1):nrow(lista)){
        soma = soma + (1 - similaridade())
      }
    }
    k=nrow(lista)
    return(soma/(k*k-k))
  }else{
    return(0)
  }
}

# Join

Join = function (artist){
  artist = data.frame(artist)
  names(artist) = c("artist.name")
  albums = MB_albums[which(MB_albums$artist.name == artist),]
  
  artist = inner_join(artist, MB_albums, by = "artist.name")
}

########################################## Load files ##########################################

# Music Brainz load
MB_albums <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/mb_albums.csv", 
                   sep = "\t", 
                   verbose = TRUE)
MB_albums = as.data.frame(MB_albums)

# DBpedia load
DBpedia_Artist_genres <- fread("~/Documentos/Experimento Doutorado/bases de dados/DBpedia/DBpedia Artist_genres.txt",
                               sep="\t", 
                               verbose = TRUE)
DBpedia_Artist_genres = as.data.frame(DBpedia_Artist_genres)

# Drop albums (release) info

MB_albums$release.id = NULL
MB_albums$release.name = NULL
MB_albums$release_type.name = NULL
MB_albums$release.date_year = NULL
MB_albums = MB_albums[-which(duplicated(MB_albums)),]
MB_albums = MB_albums[order(MB_albums$artist.name),]

# Exclude genres that doesn't contain at least 5 artists
min.artists = 5
quant = c(min.artists)
for (i in 2:ncol(DBpedia_Artist_genres)){
  quant = c(quant,sum(DBpedia_Artist_genres[,i]))
}
DBpedia_Artist_genres = DBpedia_Artist_genres[,-which(quant<min.artists)]

########################################## ILD ##########################################

