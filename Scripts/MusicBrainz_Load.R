library(readr)

################################### RELEASE ###################################

release <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/release", 
                      "\t", escape_double = FALSE, col_names = FALSE, 
                      trim_ws = TRUE)
names(release) = c("id", "gid", "name", "artist_credit", "release_group", "status", "packaging", "language", "script", "barcode", 
                   "comment", "edits_pending", "quality", "last_updated")

################################### RELEASE GROUP ###################################

release_group <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/release_group", 
                            "\t", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
names(release_group) = c("id", "gid", "name", "artist_credit", "type", "comment", "edits_pending", "last_updated")

################################### RELEASE X RELEASE GROUP ###################################

release = merge(release,release_group,by.x = "release_group", by.y = "id")
release = release[,c(2,8,16,17,18)]
names(release) = c("release.id", "release.language", "release.name", "release.artist_credit", "release.type")

################################### ARTIST ###################################

artist <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/artist", 
                     "\t", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
names(artist) = c("id", "gid", "name", "sort_name", "begin_date_year", "begin_date_month", "begin_date_day", "end_date_year",
                  "end_date_month", "end_date_day", "type", "area", "gender", "comment", "edits_pending", "last_updated",
                  "ended_char", "begin_area", "end_area")
artist$ended = FALSE
artist[artist[,"ended_char"] == "t","ended"] = TRUE
artist = artist[,c(1, 3, 5, 8, 11, 12, 13, 20)]

################################### ARTIST CREDIT NAME ###################################

artist_credit_name <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/artist_credit_name", 
                                 "\t", escape_double = FALSE, col_names = FALSE, 
                                 trim_ws = TRUE)
names(artist_credit_name) = c("artist_credit", "position", "artist", "name", "join_phrase")

################################### ARTIST X ARTIST CREDIT NAME ###################################

artist = merge(artist, artist_credit_name, by.x = "id", by.y = "artist")
artist = artist[,c(1:9)]
names(artist) = c("artist.id", "artist.name", "artist.begin_date_year", "artist.end_date_year", "artist.type","artist.area", "artist.gender", "artist.ended", "artist.artist_credit")

################################### ALBUMS = ARTIST X RELEASE ###################################

albums = merge(artist, release, by.x = "artist.artist_credit", by.y = "release.artist_credit")
albums$artist.artist_credit = NULL

# Language
language <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/language", 
                       "\t", escape_double = FALSE, col_names = FALSE, 
                       trim_ws = TRUE)
language = language[,c(1,5)]
names(language) = c("language.id", "language.name")
albums = merge(albums, language, by.x = "release.language", by.y = "language.id", all.x = TRUE)
albums$release.language = NULL

# Gender
gender <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/gender", 
                     "\t", escape_double = FALSE, col_names = FALSE, 
                     trim_ws = TRUE)
gender = gender[,c(1,2)]
names(gender) = c("gender.id", "gender.name")
albums = merge(albums, gender, by.x = "artist.gender", by.y = "gender.id", all.x = TRUE)
albums$artist.gender = NULL

# Release Type
release_group_primary_type <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/release_group_primary_type", 
                                         "\t", escape_double = FALSE, col_names = FALSE, 
                                         trim_ws = TRUE)
release_group_primary_type = release_group_primary_type[,c(1,2)]
names(release_group_primary_type) = c("release_type.id", "release_type.name")
albums = merge(albums, release_group_primary_type, by.x = "release.type", by.y = "release_type.id")
albums$release.type = NULL

# Artist Type
artist_type <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/artist_type", 
                          "\t", escape_double = FALSE, col_names = FALSE, 
                          trim_ws = TRUE)
artist_type = artist_type[,c(1,2)]
names(artist_type) = c("artist_type.id", "artist_type.name")
albums = merge(albums, artist_type, by.x = "artist.type", by.y = "artist_type.id", all.x = TRUE)
albums$artist.type = NULL

################################### RELEASE COUNTRY AND DATE ###################################

release_country <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/release_country", 
                              "\t", escape_double = FALSE, col_names = FALSE, 
                              trim_ws = TRUE)
names(release_country) = c("release.id", "country.id", "release.date_year", "release.date_month", "release.date_day")
albums = merge(albums, release_country, by = "release.id", all.x = TRUE)
albums$release.date_month = NULL
albums$release.date_day = NULL

################################### AREA ###################################

area <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/area", 
                   "\t", escape_double = FALSE, col_names = FALSE, 
                   trim_ws = TRUE)
names(area) = c("area.id", "area.gid", "area.name", "area.type", "area.edits_pending", "area.last_updated", "area.begin_date_year", 
                "area.begin_date_month", "area.begin_date_day", "area.end_date_year", "area.end_date_month", 
                "area.end_date_day", "area.ended", "area.comment")
area = area[,c(1, 3, 4)]
albums = merge(albums, area, by.x = "country.id", by.y = "area.id", all.x = TRUE)
albums$country.id = NULL

area_type <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/MusicBrainz/mbdump/area_type", 
                        "\t", escape_double = FALSE, col_names = FALSE, 
                        trim_ws = TRUE)
names(area_type) = c("area_type.id", "area_type.name", "area_type.parent", "area_type.child_order", "area_type.description",
                     "area_type.gid")
area_type = area_type[,c(1,2)]
albums = merge(albums, area_type, by.x = "area.type", by.y = "area_type.id", all.x = TRUE)
albums$area.type = NULL
albums$artist.area = NULL

################################### WRITING ###################################

write.table(albums, "~/Documentos/Experimento Doutorado/bases de dados/experimento/mb_albums.txt", 
            row.names = FALSE, col.names = TRUE, sep = "\t")
