my_correlation <- function(x) { 
  
  if(data[x["row_rec"], "gender"] == history[x["row_history"], "gender"]){
    v.gender = 1
  } else {  
    v.gender = 1 / (1 + log10(as.numeric(freq.gender[which(data[x["row_rec"], "gender"] == freq.gender$gender),"total"])) 
                    * log10(as.numeric(freq.gender[which(history[x["row_history"], "gender"] == freq.gender$gender),"total"])))
  }
  if(data[x["row_rec"],"type"] == history[x["row_rec"],"type"]){
    v.type = 1
  } else {  
    v.type = 1 / (1 + log10(as.numeric(freq.type[which(data[x["row_rec"], "type"] == freq.type$type),"total"])) 
                  * log10(as.numeric(freq.type[which(history[x["row_history"], "type"] == freq.type$type),"total"])))
  }
  return ((v.gender + v.type) / 2)
}

# calculate correlation
start.time <- Sys.time()
counter <- expand.grid(row_rec = 1:nrow(data), row_history = 1:nrow(history))
correlationMatrix <- data.frame(rec = counter$row_rec, history = counter$row_history, correlation = apply(counter, 1, my_correlation))
sum(correlationMatrix$correlation) / nrow(correlationMatrix)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken