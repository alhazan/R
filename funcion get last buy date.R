# funcion para obtener el mail y last buy date a partir de un tracker id

library("RJSONIO")

# userid tiene que venir en formato character

#FUNCION GET EMAIL####
getLastBuyDate <- function(uid){
  url <- paste0("10.2.7.6/euler-service/user/",uid,"/record/")
  #Le pego por curl al servicio
  json <- try(fromJSON(getURL(url, httpheader = c("X-Cluster : euler-web-beta-h2"))), 
              silent = T)
  if (class(json) == "try-error") {
    return(NA)
  }
  
  json.un <- unlist(json$data)
  lastbuy <- grep("^lastBuysDates\\.",names(json.un),value=T)
  lastbuydate <- ifelse(nchar(lastbuy)==0, NA, json.un[lastbuy][[1]])
  lastbuy <- as.Date(as.POSIXct(lastbuydate, tz="GMT", format= "%Y-%m-%dT%H:%M:%SZ"))
  
  if(length(lastbuy) > 0){
    #email.frame <- data.frame(id=uid, email=emails, lastbuydate=lastbuy, check.rows=T, stringsAsFactors=F)
    lastbuy.frame <- data.frame(id=uid, lastbuydate=lastbuy)
  } else {lastbuy.frame <- NA}
  
  return(lastbuy.frame)
  
}

##########


# como llamar a la funcion en parallel
# clus <- makeCluster(10)
# clusterExport(clus, list=c("getLastBuyDate", "fromJSON", "getURL"))
# uid.info <- parLapply(clus,userids$trackerid, getLastBuyDate)
# stopCluster(clus)

# uid.info <- lapply(userids$trackerid, getLastBuyDate)

#lala <- do.call(rbind.data.frame, uid.info)