# se le puede pegar por email o por userid AS.CHARACTER

# variables posibles:
# "userid",
# "datetime", (timestamp)
# "pr", (product)
# "fl", (flow)
# "txid", (transaction id) 
# "ci", (check in date)
# "co" (check out date)
# "ab" (ab tests)
# "cur" (currency)
# "dn" (domain name. ej DESPEGAR)
# "cc" (country)
# "dc" (ciudad de destino)
# "ro" (ruta de vuelo DOMESTIC o INTERNATIONAL)
# "dtype" (device type)
# "ft" (ONEWAY; ROUNDTRIP)
# activar el paquete de Despegar

#otras funciones requeridas:
source("/home/despegar/rstudio_data/vseminario/Funciones/pruebatabulatejson.R")
#source("/home/despegar/rstudio_data/vseminario/Funciones/tabulate json Vero.R")

library("RJSONIO")

tabulateUserHistory <- function(user,amount=1000, from=0, to="",
                                flow="", prod="", variables, 
                                var.names, brand="ALL", country="") {
  Sys.sleep(0.2) # CALCULO PARA CLUS=10 PEGANDO AL SERVICIO 1000 VECES POR MINUTO
  
  if(grepl("^[a-z0-9]{8}(\\-[a-z0-9]{4}){3}\\-[a-z0-9]{12}$",user)) {
    type <- "user"
  } 
  
  else {type <- "email" }
  

    flow.param <- ifelse(flow=="", "", "&flows=") # esto porque ya no acepta "NULL" entonces directamente debo sacarlo
    prod.param <- ifelse(prod=="", "", "&products=")
    country.param <- ifelse(country=="", "", "&countries=")
    to.timestamp.param <- ifelse(to=="", "", "&to_ts=")
  
    url <- paste0("http://10.2.7.6/euler-service/v3/", type, "/", user, "/history?amount=", amount,
                  "&from_ts=", from, to.timestamp.param, to, flow.param, flow, prod.param, prod, "&brands=", brand, country.param, country)

  json <- try(fromJSON(getURL(url, httpheader= c('X-Client' = "RM-analytics"))),silent = T)
    
  if(class(json) == "try-error" | class(json) == "AsIs" | length(json) == 0 ) {
    
    return(NA)  
  } 
    
  else{
    # AAAAAHH EL ERROR ESTABA ACA!! CUANDO NO HAY U HIST; VIENE VACIO!
      json.unl <- unlist(json)
      
      var.names <- variables
      variables <- gsub("\\[|\\]",".",variables)
      variables <- paste0("^actionData\\.", variables, "$")
      outp <- tabulateJSON(json.unl, "^userId$", variables, var.names)
      outp <- cbind(user,outp) 
      
      return(outp)
      
      }  
}


### Parametro obligatorio que hay que declarar
#variables1 <- c("datetime", "pr", "fl", "txid", "ci", "co", "ft", "oc", "dc")

### Parametros optativos
#flow1 <- "SEARCH,THANKS,DETAIL"   # flows separados por ,
#prod1 <- "FLIGHTS,HOTELS"    # productos separados por ,
#actions <- 100
#country <- "BR"

### llamar a la funcion con lapply
#ejemplo <- lapply(as.character(vector.de.userids.o.emails),function(x) tabulateUserHistory(x, variables=variables1, prod=prod1, flow=flow1))
#ejemplo <- do.call(rbind.data.frame, ejemplo)

### llamar a la funcion con parLapply
#clus <- makeCluster(10)
#clusterExport(clus, list("tabulateJSON", "fromJSON", "getURL", "tabulateUserHistory", "variables1", "prod1", "flow1"), envir=environment())
#ejemplo <- parLapply(clus, vector.de.userids.o.emails, function(x) tabulateUserHistory(x, variables=variables1, prod=prod1, flow=flow1))
#ejemplo <- do.call(rbind.data.frame, ejemplo)
#stopCluster(clus)

