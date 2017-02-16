library(readr)
library(data.table)

######################### CONSTANTS #########################

size_sample = 1000

######################### USERS #########################

LFM_1b_users <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/LFM/LFM-1b_users.txt", 
                           "\t", escape_double = FALSE, col_names = FALSE, 
                           trim_ws = TRUE)

names(LFM_1b_users) = c("user-id", "country", "age", "gender", "playcount", "registered timestamp")

nUsers = length(unique(LFM_1b_users$`user-id`))

sample_indexes = floor(runif(size_sample, min=1, max=nUsers+1))

sample = LFM_1b_users[sample_indexes,"user-id"]

sample = sort(sample$`user-id`)

write.table(sample, "~/Documentos/Experimento Doutorado/bases de dados/LFM/sample_users.txt", sep = "\t", col.names = FALSE, row.names = FALSE)

######################### LEs #########################

# Geração do arquivo LEs_sample.txt com as interações dos usuários contidos no sample realizado em linha de comando linux
# awk -F'\t' 'NR==FNR{check[$0];next} $1 in check' sample_users.txt LFM-1b_LEs.txt > LEs_sample.txt

LEs_sample <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/LFM/LEs_sample.txt", 
                         "\t", escape_double = FALSE, col_names = FALSE, 
                         trim_ws = TRUE)

names(LEs_sample) = c("user-id", "artist-id", "album-id", "track-id", "timestamp")

LFM = merge(LEs_sample, LFM_1b_users, by = "user-id")

######################### ARTISTS #########################

LFM_1b_artists <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/LFM/LFM-1b_artists.txt", 
                             "\t", escape_double = FALSE, col_names = FALSE, 
                             trim_ws = TRUE)
names(LFM_1b_artists) = c("artist-id", "artist-name")

LFM = merge(LFM, LFM_1b_artists, by = "artist-id")

######################### ALBUMS #########################

LFM_1b_albums <- read_delim("~/Documentos/Experimento Doutorado/bases de dados/LFM/LFM-1b_albums.txt", 
                            "\t", escape_double = FALSE, col_names = FALSE, 
                            trim_ws = TRUE)
names(LFM_1b_albums) = c("album-id", "album-name", "artist-id")
LFM_1b_albums$`artist-id` = NULL

LFM = merge(LFM, LFM_1b_albums, by = "album-id")

######################### CLEANING & WRITING #########################

LFM$`track-id` = NULL

data.table::fwrite(LFM, "~/Documentos/Experimento Doutorado/bases de dados/LFM/LFM-treated.txt",row.names = FALSE, col.names = TRUE, sep = ";")
