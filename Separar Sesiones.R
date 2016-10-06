# uso data de MX del test de elasticidades
data <- septooct

data <- as.data.frame(data[ , c("userid","datetime","fl","pr","dc","ci","co","hid")])

# me quedo solo con la gente que le toco el brazo 0
tag0 <- subset(ab.tag, abtest==0)
data <- subset(data, userid %in% tag0$userid)

rm(septooct, ab.tag, tag0)

data <- as.data.table(data)
# Generamos variable timestamp a partir de FECHA y HORA
#Timestamp esta en segundos
#flights7days$timestamp <- with(flights7days, paste(FECHA,HORA))
#flights7days$timestamp <- gsub("\\.[0-9]{3}","",flights7days$timestamp)
#flights7days$timestamp <- as.POSIXct(flights7days$timestamp, tz="GMT", format= "%Y-%m-%d %H:%M:%S")
# flights7days$timestamp <- as.numeric(flights7days$timestamp)

#Ordenamos los datos por usuario, timestamp
#flights7days <- flights7days[with(flights7days, order(userid,timestamp))]

data <- data[order(userid,datetime)] 

# Agregamos nro. de sesion a la tabla user.hist.final 
# 1. Ponemos un nro. que indica el nro. de sesion por userid (con cada nuevo userid reinicia en 0)
data[,action.id:=rank(datetime),list(userid)]

# 2. Aca se hacen dos pasos en uno. Es como si creara un data.auxiliar que tiene userid,
# ...una variable igual a datetime llamada datetime2, y un action.id que es igual al de la tabla
# ...data pero corrido un lugar. Entonces hago un merge de esa tabla con data 
# ...pidiendo que coincidan los userid y action.id con la variable que esta action.id corrida una fila. 
data = merge(data,data[,list(userid,datetime2=datetime,action.id=action.id+1)],by=c("userid","action.id"),all.x=T)
data[,time.dif:=datetime-datetime2]
data[,new.session:=ifelse((datetime-datetime2>1800000)|(is.na(datetime2)),1,0)]
data[,session.id:=cumsum(new.session),by=list(userid)]

data$datetime2 <- NULL
data$time.dif <- NULL
data$new.session <- NULL

# Antes del pivot table deberia separar sesiones. Lo hago con una variable userid.session que mergee userid"-"new.session
data$user.session <- paste(data$userid, data$session.id, sep = " _ ")

# Pivot Table para contar cuantas acciones por sesion
count.flows <- dcast(data = data, formula = user.session ~ fl, fun.aggregate = length, value.var = "pr")

# count.filters <- dcast(data = user.hist.final, formula = user.session ~ eventtype, fun.aggregate = length, value.var = "cc")


# deberia poner una columna 
# Filtro solo las que llegaron a comprar
flowsAR.tx <- subset(flowsAR, THANKS > 0)

# CHECKOUT, DETAIL Y SEARCH divididas por THANKS 
flowsAR.tx$CHECKOUT.per.tx <- flowsAR.tx$CHECKOUT / flowsAR.tx$THANKS 
flowsAR.tx$SEARCH.per.tx <- flowsAR.tx$SEARCH / flowsAR.tx$THANKS 

flowsAR.tx$num.session <- gsub("^.*_", "", flowsAR.tx$user.session) 

# Creo tablas de frecuencia
CO.freqAR <- table(flowsAR.tx$CHECKOUT.per.tx)
SE.freqAR <- table(flowsAR.tx$SEARCH.per.tx)

