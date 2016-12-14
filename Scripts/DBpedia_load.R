library(readr)

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
Artist$Artist = tolower(Artist$Artist)

Artist.Origin = rbind(MusicalArtist.Origin,Band.Origin)
Artist.Origin = subset(Artist.Origin,!is.na(Artist.Origin$Artist))
Artist.Origin = subset(Artist.Origin,!is.na(Artist.Origin$Origin))
Artist.Origin = subset(Artist.Origin,!duplicated(Artist.Origin))
Artist.Origin$Artist = tolower(Artist.Origin$Artist)
Artist.Origin = Artist.Origin[-(1:3),]
