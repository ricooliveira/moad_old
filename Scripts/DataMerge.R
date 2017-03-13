library(data.table)
library(dplyr)

########################## CONSTANTS ##########################
address <- "~/Documents/experimento_doutorado/"

########################################## Load files ##########################################

# Music brainz albums load
# 
# MB_albums <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/mb_albums.csv", 
#                    sep = "\t", 
#                    verbose = TRUE)
# MB_albums = as.data.frame(MB_albums)

# DBpedia load
DBpedia_Artist_genres <- fread(paste0(address,"bases de dados/DBpedia/DBpedia Artist_genres.txt"),
                               sep="\t", 
                               verbose = TRUE)
DBpedia_Artist_genres = as.data.frame(DBpedia_Artist_genres)

# Exclude genres that doesn't contain at least 5 artists
min.artists = 5
quant = c(min.artists)
for (i in 2:ncol(DBpedia_Artist_genres)){
  quant = c(quant,sum(DBpedia_Artist_genres[,i]))
}
DBpedia_Artist_genres = DBpedia_Artist_genres[,-which(quant<min.artists)]

# Music Brainz artists load

MB_artists <- fread(paste0(address,"bases de dados/experimento/mb_artists.txt"), 
                    sep = "\t", 
                    verbose = TRUE,
                    na.strings = "")
MB_artists = as.data.frame(MB_artists)
colnames(MB_artists) = c("id", "Artist", "begin_date_year", "end_date_year", "type", "area", "gender", "ended")

# Merge Music Brainz and DBpedia

artist.data = inner_join(MB_artists, DBpedia_Artist_genres, by = "Artist")

# Remove duplicated artists from Music Brainz - pick the one with more attributes

artist.data.duplicated = artist.data[which(duplicated(artist.data$Artist)),]
artist.data.duplicated.names = unique(artist.data.duplicated$Artist)
nrow(artist.data) - nrow(artist.data.duplicated)

x = 0
print(length(artist.data.duplicated.names))
for(i in artist.data.duplicated.names){ #replace this terrible code snippet
  x = x + 1
  print(x)
  df = artist.data[which(artist.data$Artist == i),]
  attrib = 0
  for(j in 1:nrow(df)){
    attrib.line = sum(!is.na(df[j,]))
    if(attrib.line > attrib){
      attrib = attrib.line
      id = df[j,"id"]
    }
  }
  ids.out = setdiff(c(df$id),id)
  artist.data = artist.data[-which(artist.data$id %in% ids.out),]
}

# Filter artists with no metadata in Music Brainz

artist.data.noNA = artist.data[which(!is.na(artist.data$begin_date_year)),]
artist.data.noNA = artist.data.noNA[which(!is.na(artist.data.noNA$area)),]
artist.data.noNA = artist.data.noNA[which(!is.na(artist.data.noNA$type)),]

# Write data

fwrite(artist.data.noNA, paste0(address,"bases de dados/experimento/artist.data.txt"),
       row.names = FALSE, 
       col.names = TRUE, 
       sep = ";")

# LFM load and filter - ignore artists with no data available

LFM <- fread(paste0(address,"bases de dados/experimento/LFM-treated.txt"), 
                    sep = "\t", 
                    verbose = TRUE)
artists.available = unique(artist.data.noNA$Artist)
LFM.artists.available = LFM[which(LFM$`artist-name` %in% artists.available),]

fwrite(LFM.artists.available, 
       paste0(address,"bases de dados/experimento/LFM.artists.available.txt"),
       row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)


###############
# Only once in notebook

# artist.data = fread(paste0(address,"bases de dados/experimento/artist.data.txt"),
#                     sep = ";",
#                     verbose = TRUE)
# LFM = fread(paste0(address,"bases de dados/experimento/LFM.artists.available.txt"),
#                     sep = "\t",
#                     verbose = TRUE)
# artists.available = unique(artist.data$Artist)
# LFM.artists.available = LFM[which(LFM$`artist-name` %in% artists.available),]
# fwrite(LFM.artists.available, 
#        paste0(address,"bases de dados/experimento/LFM.artists.available.txt"),
#        row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)
