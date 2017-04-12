library(data.table)
library(dplyr)

################################ Constants ################################

TOPN = 10
address <- "~/Documentos/experimento_doutorado/"

################################ Data Load ################################
archive.name = "sample1000.nsga.top10.div1.pop10.gen20.txt"
results <- fread(paste0(address,"bases de dados/experimento/resultados/",archive.name), 
                     sep = ",", 
                     verbose = TRUE,
                     header = FALSE)
results = as.data.frame(results)
names(results) = c("user","artist")

results = results[-which(results$artist == ""),]

position = unlist(lapply(results$artist, function(x) which(artist.data$Artist == x)))
#teste = c(do.call("cbind",position))
results = cbind(results,position)

# results.grouped = group_by(results,user)
# results.summary = summarise(results.grouped, multi.objective(results.grouped$position))

users = unique(results$user)
user = 0
df.objectives = data.frame(matrix(0L,997,3))
for(u in users){
  user = user + 1; print(user)
  artist.data.listenned = unique(data.train[which(data.train$`user-id` == u),]$`artist-name`)
  artist.data.new = artist.data[-which(artist.data$Artist %in% artist.data.listenned),]
  data.train.user = artist.data[artist.data$Artist %in% artist.data.listenned,]
  df = multi.objective(results[results$user == u,]$position)
  df.objectives[user,1] = u; df.objectives[user,2] = df[1]; df.objectives[user,3] = df[2]
}
fwrite(df.objectives,
       paste0(address,"bases de dados/experimento/resultados/objectives.",archive.name),
       col.names = FALSE, row.names = FALSE, quote = FALSE)

