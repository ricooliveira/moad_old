library(data.table)

########################################## Load files ##########################################

# Music brainz albums load
# 
# MB_albums <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/mb_albums.csv", 
#                    sep = "\t", 
#                    verbose = TRUE)
# MB_albums = as.data.frame(MB_albums)

# DBpedia load
DBpedia_Artist_genres <- fread("~/Documentos/Experimento Doutorado/bases de dados/DBpedia/DBpedia Artist_genres.txt",
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

MB_artists <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/mb_artists.txt", 
                    sep = "\t", 
                    verbose = TRUE,
                    na.strings = "")
MB_artists = as.data.frame(MB_artists)
colnames(MB_artists) = c("area", "id", "Artist", "begin_date_year", "end_date_year", "type", "gender", 
                         "ended", "area.name", "area_type.name")

# Remove duplicated artists from Music Brainz - pick the one with more attributes

mb.artists.duplicated = MB_artists[which(duplicated(MB_artists$Artist)),]
mb.duplicated.names = unique(mb.artists.duplicated$Artist)
nrow(MB_artists) - nrow(mb.artists.duplicated)

x = 0
print(length(mb.duplicated.names))
for(i in mb.duplicated.names){ #replace this terrible code snippet
  x = x + 1
  print(x)
  df = MB_artists[which(MB_artists$Artist == i),]
  attrib = 0
  for(j in 1:nrow(df)){
    attrib.line = sum(!is.na(df[j,]))
    if(attrib.line > attrib){
      attrib = attrib.line
      id = df[j,"id"]
    }
  }
  ids.out = setdiff(c(df$id),id)
  MB_artists = MB_artists[-which(MB_artists$id %in% ids.out),]
}

# Merge Music Brainz and DBpedia

artist.data = inner_join(MB_artists, DBpedia_Artist_genres, by = "Artist")

# Write data

fwrite(artist.data, "~/Documentos/Experimento Doutorado/bases de dados/experimento/artist.data.txt",row.names = FALSE, col.names = TRUE, sep = ";")

# LFM load and filter - ignore artists with no data available

LFM <- fread("~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM-treated.txt", 
                    sep = "\t", 
                    verbose = TRUE)
artists.available = unique(artist.data$Artist)
LFM.artists.available = LFM[which(LFM$`artist-name` %in% artists.available),]

fwrite(LFM.artists.available, "~/Documentos/Experimento Doutorado/bases de dados/experimento/LFM.artists.available.txt",row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)

