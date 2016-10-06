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

#Guardo columnas de valor
datos = datos[, list(FECHA, HORA, origen_IATA, destino_IATA, ci, co, flow, country, source, GB, hotel_Id, hotel_estrellas, event, eventtype), by = userid]
#Veo si el user id usario uso filtros
datos$filtrado = as.numeric(ifelse(datos$event == "filter", 1, 0)) #OK



pais = as.data.frame(table(datos$country))
setnames(pais, names(pais)[1], "Pais")
setnames(pais, names(pais)[2], "Frecuencia")



#Genero matriz de los que tuvieron eventos filter
filtrados = datos[, list(FECHA, HORA, origen_IATA, userid, destino_IATA, ci, co, flow, country, source, GB, hotel_Id, hotel_estrellas, event, eventtype, filtrado = max(filtrado)), by = userid]
filtrados = subset(filtrados, filtrado == 1) #OK


#Nivel de flujo mas profundo - VER!!

filtrados$nivel_flujo = ifelse(filtrados$flow == "THANKS",5, ifelse(filtrados$flow == "DETAIL",4,ifelse(filtrados$flow =="CHECKOUT", 3,
                                                                                                        ifelse(filtrados$flow=="SEARCH", 2,
                                                                                                               ifelse(filtrados$flow == "HOME",1, 0)))))

filtrados = datos[, list(FECHA, HORA, origen_IATA, userid, destino_IATA, ci, co, flow, country, source, GB, hotel_Id, hotel_estrellas, event, eventtype, filtrado = max(filtrado)), by = userid]

View(head(filtrados,40))  

#calculo frecuencia aparicion de eventos, entre ellos filtros
eventos = table(datos$event)
eventos = as.data.frame(eventos)
setnames(eventos, names(eventos)[1], "Evento")
setnames(eventos, names(eventos)[2], "Frecuencia")
eventos = subset(eventos, Evento !="")
eventos$porcentaje_aparicion = (eventos$Frecuencia/sum(eventos$Frecuencia))*100
eventos = data.table(eventos)
eventos = eventos[order(-porcentaje_aparicion)]
eventos_pais = datos[,porcentaje_aparicion, by = country]

ggplot(eventos, aes(Evento,porcentaje_aparicion, fill = Evento))+
  geom_bar(stat="identity") + xlab("Evento") + ylab("Porcentaje Aparición") + ggtitle("Eventos registrados")+
  theme(text = element_text(size=20))
                                                                                                                   axis.text.x = element_text(angle=90, vjust=1)) 

#Me quedo con los usuarios que aplicaron filtros en algun momento
datos_filter = subset(datos, event == "filter")

#Hago division por pais - Site
filtrado_pais = table(datos_filter$country)
filtrado_pais = as.data.frame(filtrado_pais)

#Cambio de nombres
setnames(filtrado_pais, names(filtrado_pais)[1], "Pais")
setnames(filtrado_pais, names(filtrado_pais)[2], "Frecuencia")

filtrado_pais$porcentaje_aparicion = (filtrado_pais$Frecuencia/sum(filtrado_pais$Frecuencia))*100
filtrado_pais = data.table(filtrado_pais)
filtrado_pais = filtrado_pais[order(-porcentaje_aparicion)]

ggplot(head(filtrado_pais, 10), aes(Pais, porcentaje_aparicion, fill = Pais))+
  geom_bar(stat="identity") + xlab("País") + ylab("Porcentaje Aparición") + ggtitle("Usos de filtros por país")+
  theme(text = element_text(size=20))

# Tipos de filtros
tipos_evento = table(datos$eventtype)
tipos_evento = as.data.frame(tipos_evento)
tipos_evento = subset(tipos_evento, Var1 !="") #Saco los vacios

setnames(tipos_evento, names(tipos_evento)[1], "Tipo filtro") #Esta vacio, no trackeado
setnames(tipos_evento, names(tipos_evento)[2], "Frecuencia")

tipos_evento = data.table (tipos_evento)
tipos_evento = tipos_filtros[order(-Frecuencia)]

tipos_evento$porcentaje_aparicion = (tipos_evento$Frecuencia/sum(tipos_evento$Frecuencia))*100


#destinos por pais
destinos_frecuentes = table(datos_filter$destino_IATA)
destinos_frecuentes = as.data.frame(destinos_frecuentes)

#Cambio de nombres
setnames(flow_filtrado, names(flow_filtrado)[1], "Flow") #Solo un flow
setnames(flow_filtrado, names(flow_filtrado)[2], "Frecuencia")

#tiempo_hospedaje = datos_n$ci - datos_n$co En los filtrados estan todos vacios
#
 
#Analisis de compradores

buyers = subset(datos, flow == "THANKS") #Me quedo con los que llegaron a thanks
buyers = data.table(buyers)
buyers = buyers[, list (userid = unique(userid))] #Lista de usuarios que compraron

acciones_buyers = merge(datos, buyers, by = "userid") #Solo me quedo con los usuarios que compraron

event_buyer = table(acciones_buyers$event) #Veo los eventos de los compradores
eventtype_buyer = table (acciones_buyers$eventtype)

setnames(eventtype_buyer, names(eventtype_buyer)[1], "Tipo filtro") #Esta vacio, no trackeado
setnames(eventtype_buyer, names(eventtype_buyer)[2], "Frecuencia")
eventtype_buyer = data.table (eventtype_buyer)

eventtype_buyer = tipos_filtros[order(-Frecuencia)]

eventtype_buyer$porcentaje_aparicion = (eventtype_buyer$Frecuencia/sum(eventtype_buyer$Frecuencia))*100
