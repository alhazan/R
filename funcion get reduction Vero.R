##### BAJAR y PREPARAR DATOS #####

getReduction2 <- function(type, sub.type, date){
  if(!type %in% c("FLOW","PRODUCT","SOURCE","VISITAS_PRODUCT")) stop("'type' must be FLOW, PRODUCT, SOURCE or VISITAS_PRODUCT")
    
  if (type == "VISITAS_PRODUCT") {
    url <- paste0("ftp://10.1.5.8/GENERICAS/",type,"/",date,"-VISITAS","_",sub.type,".zip")
  }
  
  else {
    url <- paste0("ftp://10.1.5.8/GENERICAS/",type,"/",date,"-DATA_UPA_",type,"_",sub.type,".zip")
    }
    
  userpwd <- "analytics:analytics2015"
  
  aaa <- tempfile()
  file.raw <- getBinaryURL(url, userpwd = userpwd,
                           ftp.use.epsv = FALSE)
  
  writeBin(file.raw,aaa)
  unzip(aaa)
  
}


# Descargamos la data
#getReduction2("VISITAS_PRODUCT","CARS",20160426)

# Levantamos la data
#cars <- read.delim("/home/despegar/rstudio_data/vseminario/Leo/20160426-VISITAS_CARS.txt", sep="\t", header =T)

