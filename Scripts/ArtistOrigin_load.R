Artist <- read.delim("~/Documentos/Origin/lastfm.filtered.top1000.train.origins.csv", header=FALSE)
names(Artist) = c("Artist","Origin","Value")
Origin <- read.delim("~/Documentos/Origin/lastfm.filtered.top1000.train.origins-Expanded.csv", na.strings="null",header=FALSE)
Artist.Origin = Artist

kNN <- read.delim("~/Documentos/RecSys/Old/lastfm.filtered.top1000.train-Prediction_ub-50.csv", header=FALSE, quote="")
names(kNN) = c("User","Artist","Value")

