library(dplyr)
library(data.table)
library(lsa)
library(nomclust)

########################################## Metrics ##########################################

# Precision

precision = function(list.elements, data.test, TOPN){
  return(length(list.elements[which(list.elements$artist %in% data.test$`artist-name`),]))
}

# Intra List Diversity Metric (ILD)

ILD = function(list.ubcf,artist.data){ 
  k=nrow(list.ubcf)
  if(k > 1){
    df = artist.data[artist.data$Artist %in% list.ubcf$artist,]
    df$id = NULL
    df$Artist = NULL
    #similarity.matrix = eskin(df)
    sum.dissimilarity = 0
    for(i in 1:(nrow(list.ubcf)-1)){
      for(j in (i+1):nrow(list.ubcf)){
        sum.dissimilarity = sum.dissimilarity + (1 - as.numeric(pearson.correlation(df[i,],df[j,])))
      }
    }
    #dissimilarity.matrix = 1 - similarity.matrix
    #sum.dissimilarity = sum(colSums(dissimilarity.matrix)) - k
    return(sum.dissimilarity/(k*(k-1)))
  }else{
    return(0)
  }
}

# Pearson correlation

pearson.correlation = function(candidate,list.ubcf){
  soma = 0
  for(i in 1:nrow(list.ubcf)){
    soma = soma + cor.test(as.vector(list.ubcf[i,],mode = "numeric"),as.vector(candidate,mode = "numeric"),method = "pearson")$estimate
  }
  return(soma/nrow(list.ubcf))
}
