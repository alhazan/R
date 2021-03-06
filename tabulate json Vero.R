tabulateJSON <- function (json.un, start.obj, columns, colnames) 
{
  if (length(columns) != length(colnames)) {
    stop("'columns' and 'colnames' must be the same length")
  }
  start.ind <- grep(start.obj, names(json.un))
  
  col.indexes <- lapply(columns, grep, names(json.un))
  col.position <- lapply(1:length(columns), function(x) findInterval(col.indexes[[x]], start.ind))
  
  
    temp.frames <- lapply(1:length(columns), function(x) data.frame(pos = col.position[[x]], ind = json.un[col.indexes[[x]]], stringsAsFactors = F))
    
    collapse.cols <- which(sapply(temp.frames, nrow) > length(start.ind))
  
  if(length(collapse.cols) > 0){
    temp.frames[collapse.cols] <- lapply(temp.frames[collapse.cols], function(x) 
      ddply(.data = x, .(pos), summarise, value = paste0(ind, collapse = ";")))
    
  }
  
  #### agregado Vero para evitar los warnings del merge (porque se duplican los nombres de las columnas)
  for (i in seq_along(temp.frames)){
    colnames(temp.frames[[i]]) <- c("pos",colnames[i])
  }
  #######
  
  matr <- Reduce(function(...) merge(...,all=T,by="pos"),temp.frames)
  matr$pos <- NULL
  #names(matr) <- colnames
  #matr <- as.matrix(matr)
  #colnames(matr) <- colnames
  return(matr)
}