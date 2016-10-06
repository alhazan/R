#-----------------------------------------Análisis de comportamiento hoteles-filtros---------------
#Bajo 3 dias de datos
getReduction2("PRODUCT","HOTELS",20160925)
veinticinconueve <- fread("//home/despegar/rstudio_data/ahazan/Analisis de comportamiento/Sorting Hoteles - filtros/20160925-DATA_UPA_PRODUCT_HOTELS.txt", sep="\t", header =T)

getReduction2("PRODUCT","HOTELS",20160924)
veinticuatronueve <- fread("/home/despegar/rstudio_data/ahazan/Analisis de comportamiento/Sorting Hoteles - filtros/20160924-DATA_UPA_PRODUCT_HOTELS.txt", sep="\t", header =T)

getReduction2("PRODUCT","HOTELS",20160923)
veintitresnueve <- fread("/home/despegar/rstudio_data/ahazan/Analisis de comportamiento/Sorting Hoteles - filtros/20160923-DATA_UPA_PRODUCT_HOTELS.txt", sep="\t", header =T)

names (veinticinconueve) = names(veinticuatronueve)
names (veintitresnueve) = names(veintitresnueve)

#Armo unica tabla con todos los datos
datos = rbind(veinticuatronueve,veintitresnueve)
datos = rbind(datos, veinticinconueve)
datos = data.table(datos)

setnames(datos,names(datos)[1],"FECHA") #Hay caracteres escondidos en algun lado que no los hacen iguales, los reseteo
setnames(datos,names(datos)[16],"hotel_Id")
datos$fecha_ci <- as.Date(strptime(datos$ci, "%Y-%m-%d"))
datos$fecha_co <- as.Date(strptime(datos$co, "%Y-%m-%d"))

datos$ci <- NULL
datos$co <- NULL

# saco a los userid vacios
datos <- subset(datos,userid != "")

#---------------------------------------------------------
#Me quedo con las secciones del flow que usan filtros
datos = subset(datos, !grepl(pattern = "HOME",x = flow) & !grepl(pattern = "LANDING",x = flow) & !grepl(pattern = "OFFERS",x = flow))

#-------------------------------------Buyers------------------------------------------
buyers = datos[flow =="THANKS",list(destino_IATA, hotel_Id, hotel_estrellas, country, fecha_t = FECHA, checkin = fecha_ci, checkout = fecha_co) ,by=userid]
buyers = data.table(buyers)

#------------------------------Non Buyers--------------- 
non_buyers = datos[flow !="THANKS",list(destino_IATA, eventtype, hotel_Id, flow, hotel_estrellas, country, FECHA) ,by=userid]
non_buyers = data.table(non_buyers)

#-------------------------------------------Eventos por usuario ------------------------
event_types = datos[flow =="SEARCH-E", list(cantidad_usuarios = length(unique(userid))), by = eventtype] #solo search-e tiene tipos de filtros
event_types = data.table(event_types)

event_types = event_types[order(-cantidad_usuarios)]
event_types$porcentaje_total = (event_types$cantidad_usuarios/length(unique(datos$userid)))*100
event_types$porcentaje_eventos = (event_types$cantidad_usuarios/sum(event_types$cantidad_usuarios))*100

ggplot(event_types, aes(eventtype, porcentaje_eventos, fill = eventtype))+
  geom_bar(stat="identity") + xlab("Eventos") + ylab("Porcentaje Aparición") + ggtitle("Eventos registrados")+
  theme(text = element_text(size=20))

#Filtros especificos
filtros = datos[flow =="SEARCH-E" & event =="selection" & grepl(pattern = "filter",x = eventtype), list(cantidad_usuarios = length(unique(userid))),by=eventtype]
filtros = data.table(filtros)
filtros = filtros[order(-cantidad_usuarios)]
filtros$porcentaje_total = (filtros$cantidad_usuarios/length(unique(datos$userid)))*100
filtros$porcentaje_filtros = (filtros$cantidad_usuarios/sum(filtros$cantidad_usuarios))*100  


#-------------------------------------Acciones buyers----------------
#Hospedaje
datos$tiempohospedaje = buyers$checkout - buyers$checkin

acciones_buyers = datos[userid %in% buyers$userid,]

events_buyers = acciones_buyers[flow =="SEARCH-E",list(cantidad_usuarios = length(unique(userid))), by = eventtype ]
events_buyers$porcentaje_total = (events_buyers$cantidad_usuarios/length(unique(datos$userid)))*100
events_buyers$porcentaje_buyers = (events_buyers$cantidad_usuarios/length(unique(buyers$userid)))*100

ggplot(events_buyers, aes(eventtype, porcentaje_buyers, fill = eventtype))+
  geom_bar(stat="identity") + xlab("Eventos") + ylab("Porcentaje Aparición") + ggtitle("Eventos registrados")+
  theme(text = element_text(size=20))


filtros_buyers = acciones_buyers[flow =="SEARCH-E" & event =="selection" & grepl(pattern = "filter:",x = eventtype),list(cantidad_usuarios = length(unique(userid))), by = "eventtype"]
filtros_buyers$porcentaje_total = (filtros_buyers$cantidad_usuarios/length(unique(datos$userid)))*100
filtros_buyers$porcentaje_buyers = (filtros_buyers$cantidad_usuarios/length(unique(buyers$userid)))*100


#Armo tabla de contingencias

total = datos[,list(userid = unique(userid))]
filtros = datos[flow =="SEARCH-E" & event =="selection" & grepl(pattern= "filter:", x =eventtype), list(filter=1),by=userid]
compras = datos[flow=="THANKS",list(thanks=1),by=userid]
contingency = merge(merge(total,filtros,by = "userid",all.x=T),compras,by = "userid",all.x=T) #Left join
contingency[is.na(filter),filter:=0]
contingency[is.na(thanks),thanks:=0]
contingency = as.data.frame(table(contingency[,list(filter,thanks)])) #Tabla de contingencias
contingency$porcentaje = (contingency$Freq/sum(contingency$Freq))*100



#Veo las acciones de los compradores por destinos principales
acciones_destino_comprador = acciones_buyers[grepl(pattern= "filter:", x =eventtype),list(cantidad_usuarios = length(unique(userid)), eventtype), by = destino_IATA]
tabla_acciones = table(eventtype = acciones_destino_comprador$eventtype, destino = acciones_destino_comprador$destino_IATA)
tabla_acciones = as.data.frame(tabla_acciones)
tabla_acciones$porcentaje = (tabla_acciones$Freq/sum(tabla_acciones$Freq))*100
tabla_acciones = subset(tabla_acciones, destino != "")
tabla_acciones=data.table(tabla_acciones)
tabla_acciones = tabla_acciones[order(-Freq)]

destinos_principales = as.data.frame(table(destino = buyers$destino_IATA))
destinos_principales = data.table (destinos_principales)
destinos_principales = head(destinos_principales[order(-Freq)],25) 

tabla_acciones_destinos_ppales = subset(tabla_acciones, destino %in% destinos_principales$destino)

#Grafico

ggplot(tabla_acciones_destinos_ppales, aes(destino, porcentaje, fill = eventtype))+
  geom_bar(stat="identity") + xlab("Destino") + ylab("Porcentaje") + ggtitle("Eventos registrados")+
  theme(text = element_text(size=15))

#por tiempo de viaje - No se si es muy relevante!

acciones_tiempo_comprador = acciones_buyers[grepl(pattern= "filter:", x =eventtype),list(cantidad_usuarios = length(unique(userid)), eventtype), by = tiempohospedaje]
acciones_tiempo_comprador$porcentaje = (acciones_tiempo_comprador$cantidad_usuarios/sum(acciones_tiempo_comprador$cantidad_usuarios))*100
acciones_tiempo_comprador = data.table(acciones_tiempo_comprador)
acciones_tiempo_comprador = acciones_tiempo_comprador[order(-porcentaje)]

#Grafico
ggplot(acciones_tiempo_comprador, aes(tiempohospedaje, porcentaje, fill = eventtype))+
  geom_bar(stat="identity") + xlab("tiempo hospedaje") + ylab("Porcentaje") + ggtitle("Eventos registrados")+
  theme(text = element_text(size=15))


#Non buyers
non_buyers = subset(non_buyers, !grepl(pattern = "HOME",x = flow) & !grepl(pattern = "LANDING",x = flow) & !grepl(pattern = "OFFERS",x = flow))

#Destinos mas buscados por no compradores
destinos_non_buyers = non_buyers[destino_IATA !="",list(cant_usuarios = length(unique(userid))), by = destino_IATA]
destinos_non_buyers = data.table(destinos_non_buyers)
destinos_non_buyers$porcentaje = (destinos_non_buyers$cant_usuarios/sum(destinos_non_buyers$cant_usuarios))*100
destinos_non_buyers = head(destinos_non_buyers[order(-porcentaje)],30)

ggplot(destinos_non_buyers, aes(destino_IATA, porcentaje, fill = destino_IATA))+
  geom_bar(stat="identity") + xlab("Destino") + ylab("Porcentaje") + ggtitle("Destinos Buscados por no compradores")+
  theme(text = element_text(size=15))

#por sites?
destinos_non_buyers_site = non_buyers[destino_IATA !="",list(cant_usuarios = length(unique(userid))), by = c("destino_IATA","country")]
destinos_non_buyers_site = data.table(destinos_non_buyers_site)
destinos_non_buyers_site$porcentaje = (destinos_non_buyers_site$cant_usuarios/sum(destinos_non_buyers_site$cant_usuarios))*100
destinos_non_buyers_site = head(destinos_non_buyers_site[order(-porcentaje)],30)

ggplot(destinos_non_buyers_site, aes(destino_IATA, porcentaje, fill = country))+
  geom_bar(stat="identity") + xlab("Destino") + ylab("Porcentaje") + ggtitle("Destinos Buscados por no compradores")+
  theme(text = element_text(size=15))

#Profundidad de busqueda de no compradores?
#debería numerar y quedarme con el máximo del flujo???? y ver que busco?

flujo_num = switch(as.character(non_buyers$flow), "EVENT-SELECTION" = 1, "SEARCH" = 2, CHECKOUT = 3, "SEARCH-E" = 4,
                   "EVENT-SUSCRIPTION"= 5,"THANKS-E" = 6,"DETAIL" = 7)#????????????????




