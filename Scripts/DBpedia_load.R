library(readr)
library(dplyr)

######################### FUNCTION ########################

# Function for generating a data.frame containing multiple items grouped in one cell, in DBpedia database, in the format {x|x|...}
dismemberObjects = function(str) {
  if(is.na(str))
    return(data.frame(NA))
  lista = strsplit(str,"|", fixed = T)
  if (length(lista[[1]]) > 1) {
    lista[[1]][1] = substr(lista[[1]][1],2,nchar(lista[[1]][1]))
    lista[[1]][length(lista[[1]])] = substr(lista[[1]][length(lista[[1]])],1,nchar(lista[[1]][length(lista[[1]])])-1)
  }
  lista = data.frame(lista)
  names(lista) = "Items"
  return(lista)
}

################################### MusicalArtist ###################################

MusicalArtist = read.csv("~/Documentos/Experimento Doutorado/bases de dados/DBpedia/MusicalArtist.csv", header=TRUE, na.strings="NULL", stringsAsFactors=FALSE, sep = ",")
MusicalArtist.Origin = MusicalArtist[,c(2,26)]
names(MusicalArtist.Origin) = c("Artist","Origin")
MusicalArtist = MusicalArtist[,c(2,48)]
MusicalArtist = MusicalArtist[-(1:3),]

################################### Band ###################################

Band = read.csv("~/Documentos/Experimento Doutorado/bases de dados/DBpedia/Band.csv", header=TRUE, na.strings="NULL", stringsAsFactors=FALSE, sep = ",")
Band.Origin1 = Band[,c(2,23)]
names(Band.Origin1) = c("Artist","Origin")
Band.Origin2 = Band[,c(2,37)]
names(Band.Origin2) = c("Artist","Origin")
Band.Origin = rbind(Band.Origin1,Band.Origin2)
Band = Band[,c(2,34)]
Band = Band[-(1:3),]

################################### Artist = MusicalArtist + Band ###################################

Artist = rbind(MusicalArtist,Band)
Artist = subset(Artist,!is.na(Artist$genre_label))
Artist = subset(Artist,!is.na(Artist$rdf.schema.label))
Artist = subset(Artist,!duplicated(Artist))
names(Artist) = c("Artist","Genre")
# Artist$Artist = tolower(Artist$Artist)

# genres = list of data.frames containing all the genres already splited
genres <- vector("list", nrow(Artist))
for (i in 1:nrow(Artist)) {
  genres[[i]] = dismemberObjects(Artist[i,"Genre"])
}

# transform genres in columns of Artist data frame, filled with zeros
genres.all = bind_rows(genres)
genres.all = distinct(genres.all)
genres.total = nrow(genres.all)
genres.all.columns = data.frame(matrix(0,ncol = genres.total, nrow = nrow(Artist)))
names(genres.all.columns) = genres.all$Items

# fill the genre columns of each artist
Artist.genres = bind_cols(Artist,genres.all.columns)
for (i in 1:nrow(Artist)) {
  for(j in 1:nrow(genres[[i]])) {
    Artist.genres[i,as.character(genres[[i]][j,1])] = 1
  }
}
Artist.genres[,2] = NULL

# write text file with artists and its genres
write.table(Artist.genres, "~/Documentos/Experimento Doutorado/bases de dados/DBpedia/DBpedia Artist_genres.txt",row.names = FALSE, col.names = TRUE, sep = "\t")


# lods <- vector("list", nrow(Artist))
# for (i in 1:nrow(Artist)){
#   lods[[i]] = read.delim(paste0("~/Documentos/LOD/LOD_graph5/",i,".LOD.genre.csv"))
# }

# 
# Artist.Origin = rbind(MusicalArtist.Origin,Band.Origin)
# Artist.Origin = subset(Artist.Origin,!is.na(Artist.Origin$Artist))
# Artist.Origin = subset(Artist.Origin,!is.na(Artist.Origin$Origin))
# Artist.Origin = subset(Artist.Origin,!duplicated(Artist.Origin))
# Artist.Origin$Artist = tolower(Artist.Origin$Artist)
# Artist.Origin = Artist.Origin[-(1:3),]
