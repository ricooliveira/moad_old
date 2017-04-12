library(readr)
library(data.table)
library(dplyr)

########################## CONSTANTS ##########################

address <- "/experimento/datasets/"
size_sample = 1000

###############################################################################
############################ Music Brainz Load  ###############################
###############################################################################

################################### RELEASE ###################################

release <- read_delim(paste0(address,"MusicBrainz/mbdump/release"),
                      "\t", 
                      escape_double = FALSE, 
                      col_names = FALSE, 
                      trim_ws = TRUE,
                      na = "\\N")
names(release) = c("id", "gid", "name", "artist_credit", "release_group", "status", "packaging", "language", "script", "barcode", 
                   "comment", "edits_pending", "quality", "last_updated")

################################### RELEASE GROUP ###################################

release_group <- read_delim(paste0(address,"MusicBrainz/mbdump/release_group"), 
                            "\t", 
                            escape_double = FALSE, 
                            col_names = FALSE, 
                            trim_ws = TRUE,
                            na = "\\N")
names(release_group) = c("id", "gid", "name", "artist_credit", "type", "comment", "edits_pending", "last_updated")

################################### RELEASE X RELEASE GROUP ###################################

release = merge(release,release_group,by.x = "release_group", by.y = "id")
release = release[,c(2,8,16,17,18)]
names(release) = c("release.id", "release.language", "release.name", "release.artist_credit", "release.type")

################################### ARTIST ###################################

artist <- read_delim(paste0(address,"MusicBrainz/mbdump/artist"), 
                     "\t", escape_double = FALSE, 
                     col_names = FALSE, 
                     trim_ws = TRUE,
                     na = "\\N")
names(artist) = c("id", "gid", "name", "sort_name", "begin_date_year", "begin_date_month", "begin_date_day", "end_date_year",
                  "end_date_month", "end_date_day", "type", "area", "gender", "comment", "edits_pending", "last_updated",
                  "ended_char", "begin_area", "end_area")
artist$ended = FALSE
artist[artist[,"ended_char"] == "t","ended"] = TRUE
artist = artist[,c(1, 3, 5, 8, 11, 12, 13, 20)]

################################### ARTIST CREDIT NAME ###################################

artist_credit_name <- read_delim(paste0(address,"MusicBrainz/mbdump/artist_credit_name"), 
                                 "\t", 
                                 escape_double = FALSE, 
                                 col_names = FALSE, 
                                 trim_ws = TRUE,
                                 na = "\\N")
names(artist_credit_name) = c("artist_credit", "position", "artist", "name", "join_phrase")

################################### ARTIST X ARTIST CREDIT NAME ###################################

artist = merge(artist, artist_credit_name, by.x = "id", by.y = "artist")
artist = artist[,c(1:9)]
names(artist) = c("artist.id", "artist.name", "artist.begin_date_year", "artist.end_date_year", "artist.type","artist.area", "artist.gender", "artist.ended", "artist.artist_credit")

################################### ALBUMS = ARTIST X RELEASE ###################################

albums = merge(artist, release, by.x = "artist.artist_credit", by.y = "release.artist_credit")
albums$artist.artist_credit = NULL

# Language
language <- read_delim(paste0(address,"MusicBrainz/mbdump/language"), 
                       "\t", 
                       escape_double = FALSE, 
                       col_names = FALSE, 
                       trim_ws = TRUE,
                       na = "\\N")
language = language[,c(1,5)]
names(language) = c("language.id", "language.name")
albums = merge(albums, language, by.x = "release.language", by.y = "language.id", all.x = TRUE)
albums$release.language = NULL

# Gender
gender <- read_delim(paste0(address,"MusicBrainz/mbdump/gender"), 
                     "\t", 
                     escape_double = FALSE, 
                     col_names = FALSE, 
                     trim_ws = TRUE,
                     na = "\\N")
gender = gender[,c(1,2)]
names(gender) = c("gender.id", "gender.name")
albums = merge(albums, gender, by.x = "artist.gender", by.y = "gender.id", all.x = TRUE)
albums$artist.gender = NULL

# Release Type
release_group_primary_type <- read_delim(paste0(address,"MusicBrainz/mbdump/release_group_primary_type"), 
                                         "\t", 
                                         escape_double = FALSE, 
                                         col_names = FALSE, 
                                         trim_ws = TRUE,
                                         na = "\\N")
release_group_primary_type = release_group_primary_type[,c(1,2)]
names(release_group_primary_type) = c("release_type.id", "release_type.name")
albums = merge(albums, release_group_primary_type, by.x = "release.type", by.y = "release_type.id")
albums$release.type = NULL

# Artist Type
artist_type <- read_delim(paste0(address,"MusicBrainz/mbdump/artist_type"), 
                          "\t", 
                          escape_double = FALSE, 
                          col_names = FALSE, 
                          trim_ws = TRUE,
                          na = "\\N")
artist_type = artist_type[,c(1,2)]
names(artist_type) = c("artist_type.id", "artist_type.name")
albums = merge(albums, artist_type, by.x = "artist.type", by.y = "artist_type.id", all.x = TRUE)
albums$artist.type = NULL

################################### RELEASE COUNTRY AND DATE ###################################

release_country <- read_delim(paste0(address,"MusicBrainz/mbdump/release_country"), 
                              "\t", 
                              escape_double = FALSE, 
                              col_names = FALSE, 
                              trim_ws = TRUE,
                              na = "\\N")
names(release_country) = c("release.id", "country.id", "release.date_year", "release.date_month", "release.date_day")
albums = merge(albums, release_country, by = "release.id", all.x = TRUE)
albums$release.date_month = NULL
albums$release.date_day = NULL

################################### AREA ###################################

area <- read_delim(paste0(address,"MusicBrainz/mbdump/area"), 
                   "\t", 
                   escape_double = FALSE, 
                   col_names = FALSE, 
                   trim_ws = TRUE,
                   na = "\\N")
names(area) = c("area.id", "area.gid", "area.name", "area.type", "area.edits_pending", "area.last_updated", "area.begin_date_year", 
                "area.begin_date_month", "area.begin_date_day", "area.end_date_year", "area.end_date_month", 
                "area.end_date_day", "area.ended", "area.comment")
area = area[,c(1, 3, 4)]
albums = merge(albums, area, by.x = "country.id", by.y = "area.id", all.x = TRUE)
albums$country.id = NULL

area_type <- read_delim(paste0(address,"MusicBrainz/mbdump/area_type"), 
                        "\t", 
                        escape_double = FALSE, 
                        col_names = FALSE, 
                        trim_ws = TRUE,
                        na = "\\N")
names(area_type) = c("area_type.id", "area_type.name", "area_type.parent", "area_type.child_order", "area_type.description",
                     "area_type.gid")
area_type = area_type[,c(1,2)]
albums = merge(albums, area_type, by.x = "area.type", by.y = "area_type.id", all.x = TRUE)
albums$area.type = NULL
albums$artist.area = NULL

################################### WRITING ###################################

write.table(albums, paste0(address,"experimento/mb_albums.txt"), 
            row.names = FALSE, col.names = TRUE, "\t")


###############################################################################
########################## Artist Load (no album) #############################
###############################################################################

artist = fread(paste0(address,"MusicBrainz/mbdump/artist"),
               sep="\t", 
               verbose = TRUE,
               na.strings = "\\N")

names(artist) = c("id", "gid", "name", "sort_name", "begin_date_year", "begin_date_month", "begin_date_day", "end_date_year",
                  "end_date_month", "end_date_day", "type", "area", "gender", "comment", "edits_pending", "last_updated",
                  "ended_char", "begin_area", "end_area")

artist$ended = artist[,"ended_char"] == "t"
artist = artist[,c(1, 3, 5, 8, 11, 12, 13, 20)]


################################### WRITING ###################################

fwrite(artist,
       paste0(address,"experimento/mb_artists.txt"), 
       row.names = FALSE, col.names = TRUE, sep = "\t")

###############################################################################
############################### DBpedia Load ##################################
###############################################################################

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

MusicalArtist = read.csv(paste0(address,"DBpedia/MusicalArtist.csv"), header=TRUE, na.strings="NULL", stringsAsFactors=FALSE, sep = ",")
MusicalArtist.Origin = MusicalArtist[,c(2,26)]
names(MusicalArtist.Origin) = c("Artist","Origin")
MusicalArtist = MusicalArtist[,c(2,48)]
MusicalArtist = MusicalArtist[-(1:3),]

################################### Band ###################################

Band = read.csv(paste0(address,"DBpedia/Band.csv"), header=TRUE, na.strings="NULL", stringsAsFactors=FALSE, sep = ",")
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
Artist.genres = bind_cols(Artist,genres.all.columns) # Substituir pela operação tidy, pacote reshape2
for (i in 1:nrow(Artist)) {
  for(j in 1:nrow(genres[[i]])) {
    Artist.genres[i,as.character(genres[[i]][j,1])] = 1
  }
}
Artist.genres[,2] = NULL

# write text file with artists and its genres
write.table(Artist.genres, paste0(address,"DBpedia/DBpedia Artist_genres.txt"),row.names = FALSE, col.names = TRUE, sep = "\t")

# Exclude genres that contain only one artist

quant = c(2)
for (i in 2:ncol(Artist.genres)){
  quant = c(quant,sum(Artist.genres[,i]))
}
Artist.genres = Artist.genres[,-which(quant<2)]

###############################################################################
################################# LFM Load ####################################
###############################################################################

######################### USERS #########################

LFM_1b_users <- read_delim(paste0(address,"LFM/LFM-1b_users.txt"), 
                           "\t", escape_double = FALSE, col_names = FALSE, 
                           trim_ws = TRUE)

names(LFM_1b_users) = c("user-id", "country", "age", "gender", "playcount", "registered timestamp")

nUsers = length(unique(LFM_1b_users$`user-id`))

sample_indexes = floor(runif(size_sample, min=1, max=nUsers+1))

sample = LFM_1b_users[sample_indexes,"user-id"]

sample = sort(sample$`user-id`)

write.table(sample, paste0(address,"LFM/sample_users.txt"), sep = "\t", col.names = FALSE, row.names = FALSE)

######################### LEs #########################

# Geração do arquivo LEs_sample.txt com as interações dos usuários contidos no sample realizado em linha de comando linux
# awk -F'\t' 'NR==FNR{check[$0];next} $1 in check' sample_users.txt LFM-1b_LEs.txt > LEs_sample.txt

LEs_sample <- read_delim(paste0(address,"LFM/LEs_sample.txt"), 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

names(LEs_sample) = c("user-id", "artist-id", "album-id", "track-id", "timestamp")

LFM = merge(LEs_sample, LFM_1b_users, by = "user-id")

######################### ARTISTS #########################

LFM_1b_artists <- read_delim(paste0(address,"LFM/LFM-1b_artists.txt"), 
                             "\t", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
names(LFM_1b_artists) = c("artist-id", "artist-name")

LFM = merge(LFM, LFM_1b_artists, by = "artist-id")

######################### ALBUMS #########################

LFM_1b_albums <- read_delim(paste0(address,"LFM/LFM-1b_albums.txt"), 
                            "\t", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
names(LFM_1b_albums) = c("album-id", "album-name", "artist-id")
LFM_1b_albums$`artist-id` = NULL

LFM = merge(LFM, LFM_1b_albums, by = "album-id")

######################### CLEANING & WRITING #########################

LFM$`track-id` = NULL

fwrite(LFM, paste0(address,"LFM/LFM-treated.txt"),row.names = FALSE, col.names = TRUE, sep = ";")

###############################################################################
################################ Data Merge ###################################
###############################################################################

# DBpedia load
DBpedia_Artist_genres <- fread(paste0(address,"DBpedia/DBpedia Artist_genres.txt"),
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
DBpedia_Artist_genres = DBpedia_Artist_genres[-which(apply(DBpedia_Artist_genres[2:ncol(DBpedia_Artist_genres)],1,sum) == 0),]

# Music Brainz artists load

MB_artists <- fread(paste0(address,"experimento/mb_artists.txt"), 
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

fwrite(artist.data.noNA, paste0(address,"experimento/artist.data.txt"),
       row.names = FALSE, 
       col.names = TRUE, 
       sep = ";")

# LFM load and filter - ignore artists with no data available

LFM <- fread(paste0(address,"experimento/LFM-treated.txt"), 
                    sep = "\t", 
                    verbose = TRUE)
artists.available = unique(artist.data.noNA$Artist)
LFM.artists.available = LFM[which(LFM$`artist-name` %in% artists.available),]

fwrite(LFM.artists.available, 
       paste0(address,"experimento/LFM.artists.available.txt"),
       row.names = FALSE, col.names = TRUE, sep = "\t", quote = TRUE)


