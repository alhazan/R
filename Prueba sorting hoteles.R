#-----------------------------------------Análisis de comportamiento hoteles-filtros y sorting---------------

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

#cambio las fechas de todo
datos$fecha_ci <- as.Date(strptime(datos$ci, "%Y-%m-%d"))
datos$fecha_co <- as.Date(strptime(datos$co, "%Y-%m-%d"))

datos$ci <- NULL
datos$co <- NULL

#paso userid a character para que no queden como factors
datos$userid <- as.character(datos$userid) 
# saco a los userid vacios
datos <- subset(datos,userid != "")
#Corto las url y cosas que no sirven para el analisis
datos = datos[, list(FECHA, HORA, origen_IATA, userid, destino_IATA, fecha_ci, fecha_co, flow, country, source, GB, hotel_Id, hotel_estrellas, event, eventtype), by = userid]

#### Vars grales ####
# Separo compradores de no compradores #

#### BUYERS ####
# (los que compraron)
# Solo quiero las acciones de cada usuario cuyo timestamp sea menor al del thanks _ ESTO NO ESTA EN MI BASE! PORQUE?
# Pasos a seguir:
# 0. ordenar de mas reciente a mas antiguo asi me quedo con el thanks mas reciente
# 1. buscar el timestamp del thanks
# 2. poner ese timestamp en una columna aparte matcheando con ese user, con un merge
# 3. subsetear data quedandome solo con acciones donde el timestamp de la accion sea menor 
#    o igual al de la columna del timestamp fijo del thanks
# 4. contar los detail y search 
# me quiero quedar con las acciones mas recientes. por eso ordeno y luego saco userid duplicados

data.merge = datos[order(FECHA, decreasing = TRUE ),]

data.th <- subset(data.merge, flow =="THANKS") #Datos de thanks - compradores

data.th <- data.th[!duplicated(data.th[,userid]),] #Elimina duplicados

data.co <- subset(data.merge, flow == "CHECKOUT")
data.co
data.de <- subset(data.merge, flow =="DETAIL")
data.se <- subset(data.merge, flow == "SEARCH")


# este merge es para agregar la fecha de compra mas reciente a cada userid????????????
buyers <- merge(data.merge, data.th, by = "userid")
# si le saco el all.x=T al merge anterior esto es innecesario buyers <- subset(buyers, !is.na(buyers$fecha_thanks))






#Si fue filtrado pongo 1 sino 0
datos$filtrado = ifelse(datos$event == "filter", 1, 0) #OK

#Esto esta ok??
filtrados$nivel_flujo = ifelse(filtrados$flow == "THANKS",5, ifelse(filtrados$flow == "DETAIL",4,ifelse(filtrados$flow =="CHECKOUT", 3,
                                                                                                        ifelse(filtrados$flow=="SEARCH", 2,
                                                                                                               ifelse(filtrados$flow == "HOME",1, 0)))))

#Preparo los que fueron filtrados para su analisis
filtrados = datos[, list(FECHA, HORA, origen_IATA, userid, destino_IATA, fecha_ci, fecha_co, flow, country, source, GB, hotel_Id, hotel_estrellas, event, eventtype, filtrado = max(filtrado), flow = max(nivel_flujo)), by = userid]

filtrados = subset(filtrados, filtrado == 1) #OK


# ahora me quedo con la tabla buyers que tiene las acciones de los compradores. 
# tengo que contar los check outs details, searches previos a las compras
buyers.co <- subset(buyers, fl =="checkout")
buyers.co$userid <- as.character(buyers.co$userid) 
cant.co <- table(buyers.co$userid)
cant.co <-as.data.frame(cant.co)
colnames(cant.co)<-c("userid","checkout.count")
buyers <- merge(buyers, cant.co[,c("userid","checkout.count")], by.x="userid", by="userid", all.x=T)

buyers.de <- subset(buyers, fl =="detail")
cant.de <- table(buyers.de$userid) 
cant.de <-as.data.frame(cant.de)
colnames(cant.de) <- c("userid","detail.count")
buyers <- merge(buyers, cant.de[,c("userid","detail.count")], by.x="userid", by="userid", all.x=T)

buyers.se <- subset(buyers, fl =="search")
buyers.se$userid <- as.character(buyers.se$userid) 
cant.se <- table(buyers.se$userid)
cant.se <-as.data.frame(cant.se)
colnames(cant.se)<-c("userid","search.count")
buyers <- merge(buyers, cant.se[,c("userid","search.count")], by.x="userid", by="userid", all.x=T)

buyers.tx <- subset(buyers, fl =="thanks")
buyers.tx$userid <- as.character(buyers.tx$userid) 
cant.tx <- table(buyers.tx$userid)
cant.tx <-as.data.frame(cant.tx)
colnames(cant.tx)<-c("userid","thanks.count")
buyers <- merge(buyers, cant.tx[,c("userid","thanks.count")], by.x="userid", by="userid", all.x=T)

rm(cant.tx, cant.se, cant.de, cant.co, buyers.se, buyers.de, buyers.co, buyers.tx)

# algunos buyers tienen mas de una compra. voy a dividir (y redondear a nro entero) 
# la cant. de se, co y de por la cantidad de tx. 

buyers$checkout.count <- round((buyers$checkout.count/buyers$thanks.count), digits=0)
buyers$detail.count <- round((buyers$detail.count/buyers$thanks.count), digits=0)
buyers$search.count <- round((buyers$search.count/buyers$thanks.count), digits=0)

# finalmente, me quedo solo con la fila del thanks de cada usuario en la tabla buyers
# ya que es la fila que tiene la info del hotel comprado
buyers <- subset(buyers, fl =="thanks")

# algunos usuarios tenian mas de una compra. me quedo con la mas reciente
buyers <- buyers[order(buyers$fecha_reserva, decreasing = TRUE ),]
buyers <- subset(buyers, !duplicated(buyers$userid)) 

# por ultimo le agrego una columna que los etiquete como compradores con 1
buyers$compro <- 1


##### CHECKERS ####
# (los que abandonaron la busqueda antes de comprar)
# Como no compraron, tomo todas las acciones del usuario en estos dias
# (podrian haber comprado mas adelante y no lo se)
# Me voy a quedar con la info del detail mas reciente que vieron. 
# Si no hay detail, quiero el search mas reciente
# Pasos a seguir:
# 1. armo una tabla de checkers con los que no registraron una compra
# 2. contar los detail y searches de cada userid
# 3. dejar el search, checkout y detail mas reciente de cada usuario
# 4. hacer un rbind de checkout, detail y search EN ESE ORDEN 
# 5. quedarme con los uniques asi tengo el flow mas profundo mas reciente de cada usuario

checkers <- subset(data.merge, !data.merge$userid %in% buyers$userid)

checkers.co <- subset(checkers, fl =="checkout")
checkers.co$userid <- as.character(checkers.co$userid) 
cant.co <- table(checkers.co$userid)
cant.co <-as.data.frame(cant.co)
colnames(cant.co)<-c("userid","checkout.count")
checkers <- merge(checkers, cant.co[,c("userid","checkout.count")], by.x="userid", by="userid", all.x=T)

checkers.de <- subset(checkers, fl =="detail")
checkers.de$userid <- as.character(checkers.de$userid)
cant.de <- table(checkers.de$userid) 
cant.de <-as.data.frame(cant.de)
colnames(cant.de) <- c("userid","detail.count")
checkers <- merge(checkers, cant.de[,c("userid","detail.count")], by.x="userid", by="userid", all.x=T)

checkers.se <- subset(checkers, fl =="search")
checkers.se$userid <- as.character(checkers.se$userid) 
cant.se <- table(checkers.se$userid)
cant.se <-as.data.frame(cant.se)
colnames(cant.se)<-c("userid","search.count")
checkers <- merge(checkers, cant.se[,c("userid","search.count")], by.x="userid", by="userid", all.x=T)

rm(checkers.se, checkers.de, checkers.co, cant.se, cant.de, cant.co)

# voy a ordenar por timestamp y sacar las filas donde los ci y co date estan NA
# esto porque me voy a quedar con la fila mas reciente de cada uno,
# y si justo es una NA me quedo con pocos datos para estimar despues
# de 7,301,692 obs de checkers, 537,206 tienen "NA" en fecha de ci y de co
checkers <- checkers[order(checkers$fecha_reserva, decreasing = TRUE ),]

checkers <- subset(checkers, !is.na(checkers$fecha_ci))

checkers.co <- subset(checkers, fl =="checkout")
checkers.co <- subset(checkers.co, !duplicated(checkers.co$userid)) 
checkers.de <- subset(checkers, fl =="detail")
checkers.de <- subset(checkers.de, !duplicated(checkers.de$userid)) 
checkers.se <- subset(checkers, fl =="search")
checkers.se <- subset(checkers.se, !duplicated(checkers.se$userid)) 

#hago rbind en ese orden (1ero co, 2do de, 3ero se) y despues saco duplicados asi 1ero queda el flow mas profundo
checkers <- rbind(checkers.co, checkers.de, checkers.se)
checkers <- subset(checkers, !duplicated(checkers$userid)) 

rm(checkers.se, checkers.co, checkers.de)

checkers$thanks.count <- 0 # para que tenga los mismos campos que la tabla buyers

# ahora me quedó una tabla con la accion de flujo mas profundo y mas reciente de cada user
# los flujos son check out o detail. No hay gente cuyo ultima accion haya sido thanks
# tiene sentido porque el brazo del test se asigna en detail. 
# quienes no llegaron a detail no son parte del experimento

# por ultimo le agrego una columna que los etiquete como no-compradores con 0
checkers$compro <- 0

#### UNO BUY Y CHECK ####

all <- rbind(buyers, checkers)

all$detail.count <- ifelse(is.na(all$detail.count),0,all$detail.count)
all$search.count <- ifelse(is.na(all$search.count),0,all$search.count)
all$checkout.count <- ifelse(is.na(all$checkout.count),0,all$checkout.count)

#### agrego campo abtest #### 
# Hago un merge para llenar el campo abtest en todas las filas,
# sin aclarar all.x=T, es decir, no quiero quedarme con los usuarios 
# que no tienen identificable el brazo del test

### pero podria llenar ese campo mas adelante en la tabla all
# En PE y CL de 985.000 obs, me quedo solo con 247.000, que son los que llegaron a detail y recibieron tag y no son NOT APPLY
all <- merge(all, ab.tag, by="userid")
all$ab <- NULL
all$datetime <- NULL

# la diferencia de 11 filas entre ab.tag y all son las personas para las que no habia fecha de ci

# mobile. el campo mobile murio porque si navego desde mobile no se le asigno brazo para el test
# entonces quedan todos los que usaron desktop cuando hago merge con la tabla que asigna brazo. 

#### CARACT. DEL VIAJE ####

# duracion del viaje
all$duracion <- round(as.numeric(all$fecha_co - all$fecha_ci), digits=0)
all$anticipacion <- round(as.numeric(all$fecha_ci - as.Date(all$fecha_reserva)), digits=0)

# haciendo un table de anticipacion me da algunos valores negativos (unos 2,400). los filtro.
all <- subset(all, anticipacion >= 0)

# Ratio dias de semana y fines de semana

#esto me dice el nombre del dia de la semana
all$dayweek <- weekdays(as.Date(all$fecha_ci))

# esto me dice el numero de dia de semana, del 0 al 6, empezando por el domingo = 0 y terminando por sabado = 6
all$ndayweek <- as.POSIXlt(as.Date(all$fecha_ci))$wday

# quiero que el domingo sea 7 en vez de 0
all$ndayweek <- as.numeric(gsub(0, 7, all$ndayweek))

# cuantas semanas completas de 7 dias (con dos dias de finde) hay en la duracion del viaje?
# trunc(duracion/7)
all$semanascomp <- trunc(all$duracion/7)

# resto de dias fuera de semanas completas:
all$resto <- trunc(all$duracion) - all$semanascomp * 7

# cuento dias de fin de semana extras a la semana completa 
all$diaextra <- ifelse(all$ndayweek == 7, 1,
                       ifelse((all$resto + all$ndayweek) == 7, 1, 
                              ifelse((all$resto + all$ndayweek) > 7, 2, 0)))

# Saco el numero de dias de finde (dos por semana completa, mas los dias extra)
all$diasfinde <- all$semanascomp * 2 + all$diaextra 

# saco el ratio "dias de finde" / "duracion" del viaje
all$ratiofinde <- ( all$diasfinde ) / all$duracion

# Borro estas variables
all$dayweek <- NULL
all$diasfinde <- NULL
all$ndayweek <- NULL
all$diaextra <- NULL
all$semanascomp  <- NULL
all$resto  <- NULL

# distancia
load("/home/despegar/rstudio_data/vseminario/datos utiles/info iatas.rda")
iatas.destino <- subset(iatas.frame[,c(2,7,8,12,14)])
setnames(iatas.destino, 1:5, c("iataCode","lat.destino","lon.destino","country.destino","continente")) # aca cambie el nombre de la columna para no confundirme en el merge

all <- merge(all, iatas.destino, by.x="dc", by.y="iataCode", all.x=T)

# asumo que la latitud y longitud del origen son los de mexico df (19.4326077,-99.13320799999997)
all$lat.origen <- ifelse(all$cc=="MX",19.4326077, ifelse(all$cc=="PE", -12.046374, -33.4691199))
all$lon.origen <- ifelse(all$cc=="MX",-99.13320799999997, ifelse(all$cc=="PE", -77.0427934,-70.641997))  

source("/home/despegar/rstudio_data/vseminario/Funciones/funcion distancia km.R")

all$lat.origen <- as.numeric(as.character(all$lat.origen))
all$lon.origen <- as.numeric(as.character(all$lon.origen))
all$lat.destino <- as.numeric(as.character(all$lat.destino))
all$lon.destino <- as.numeric(as.character(all$lon.destino))

all$distancia <- earth.dist ( all$lat.origen, all$lon.origen, all$lat.destino, all$lon.destino)
all$distancia <- mapply(earth.dist, all$lat.origen, all$lon.origen, all$lat.destino, all$lon.destino)

all$lat.origen <- NULL
all$lon.origen <- NULL
all$lat.destino <- NULL
all$lon.destino <- NULL

# limpio el workspace
rm(earth.dist, iatas.destino, iatas.frame)

# Vuelo domestico o internacional?
all$cc <- as.character(all$cc)
all$country.destino <- as.character(all$country.destino)

all$domestico <- ifelse(all$cc == all$country.destino, 1, 0)

all$dest.type <- ifelse(all$cc == all$country.destino, "Domestico", 
                        ifelse(all$continente == 'SA' | all$continente == 'AMC', "Latam", "RestoDelMundo"))

# hotel rooms
# hay 10 que tienen mas de 4
all <- subset(all, hr <5)

####COMISIONES####
comisiones <- read.table("/home/despegar/rstudio_data/vseminario/Elasticidad Hoteles/PE y CL/comisionesPEyCL.csv", sep=",", header =T, quote = "")
# para MX: all <- merge(all, comisiones, by.x="hid", by.y="id", all.x=T)
# para CL y PE: 
all <- merge(all, comisiones, by.x=c("hid","cc"), by.y=c("id","country"), all.x=T)

#para los hoteles que no tengo la comision, usar 12,61% para MX, 13.59% para CL y 14.68% para PE
all$comision <- ifelse(is.na(all$comision)==T & all$cc=="CL", 0.1359,
                       ifelse(is.na(all$comision)==T & all$cc=="PE",0.1468,all$comision)) 

# precio por habitacion por noche
#mpri viene siempre vacio asi que lo completo dividiendo (pri/hr)/noches
all$mpri <- ifelse(all$fl=="detail", all$pri/all$hr, 
                   (all$pri/all$hr)/all$duracion) 
all$pritax.y.fee <- ifelse(all$fl=="detail", NA, 
                           (all$pritax/all$hr)/all$duracion) #es tax y fee por habitacion por noche

# Precios y tax
# hay 2500 obs donde el tipo de cambio es menor a 10 en MX
# all <- subset(all, exch >10)

all$mpri <- as.numeric(as.character(all$mpri)) #es precio final por habitacion por noche
all$exch <- as.numeric(as.character(all$exch))
all$pritax.y.fee <- as.numeric(as.character(all$pritax.y.fee))

all$pritax.y.fee.usd <- all$pritax.y.fee / all$exch # mas de la mitad es NA porque no se muestra en DETAIL
all$pri.usd <- all$mpri / all$exch

# haciendo un table de exch cuando la moneda es PEN, muchos son 0,31...
# El exch deberia ser 3,1.. asi que multiplico por 10 y me quedo con lo menor a 4
all$exch <- ifelse(all$cur=="PEN" & all$exch <3, all$exch *10,all$exch )
all <- subset(all, !(cur=="PEN" & exch>=4))

# el exch de CLP deberia rondar los 630. Saco todo lo menor a 600.
all <- subset(all, !(cur=="CLP" & exch<600))

#saco los 104 hoteles donde el precio es mayor a 3000 por noche en usd (hay uno de 9000 usd por noche)
allhigh <- subset(all, pri.usd >= 2500)

# vuelo hoteles de mas de US$2500, que son el 0,1% de la muestra
all <- subset(all, pri.usd <2500)

# saco una compra en pesos arg
all <- subset(all, cur!="ARS")

all$tax.y.fee.percent <-  all$pritax.y.fee.usd / all$pri.usd * 100
all$costo.y.comision.excl.tax <- all$pri.usd - all$pritax.y.fee.usd 
# all$tax.comision <- 16 # promedio IVA en MX
all$tax.comision <- ifelse(all$cc=="CL", 0.1596, 
                           ifelse(all$cc=="PE", 0.1525, 16))
all$pri.antes.de.fee.usd <- all$pri.usd/(1+(all$abtest/100)*(1+all$tax.comision/100))
all$fee.excl.tax.usd <- all$abtest/100*all$pri.antes.de.fee.usd
all$fee.incl.tax.usd <- all$fee.excl.tax.usd*(1+all$tax.comision/100)

all$tax.sin.fee.usd <- all$pritax.y.fee.usd - all$fee.incl.tax.usd 
all$tax.percent <- all$tax.sin.fee.usd/ all$pri.antes.de.fee.usd*100
all$costo.excl.tax <- all$costo.y.comision.excl.tax - all$pri.antes.de.fee.usd*all$comision

# ERROR!!! hay 4145 para MX y 6601 para PEyCL en "allerror" casos en que el fee que supuestamente se debio computar por el abtest es
# mayor a pritax.y.fee.usd, que es el campo que contiene fee e impuestos
# excluyo los casos en que eso pasa
allerror <- subset(all, pritax.y.fee.usd < fee.incl.tax.usd)

all <- subset(all, pritax.y.fee.usd>=fee.incl.tax.usd | is.na(pritax.y.fee.usd))

# Traveler Type
#    Single: "1", "1|0|0", "1|0"
#    Couple: "2", "2|0|0", "2|0"
#    Family: all others
all$traveler <- ifelse(all$di == "1" | all$di == "1|0|0" | all$di == "1|0", "single",
                       ifelse(all$di == "2" | all$di == "2|0|0" | all$di == "2|0", "couple","family"))

# Viaje de negocios
all$dayco <- weekdays(as.Date(all$fecha_co))
all$dayci <- weekdays(as.Date(all$fecha_ci))

all$work <- ifelse(all$duracion < 7 & all$anticipacion < 7,
                   ifelse(all$dayco != "Saturday" & all$dayco != "Sunday" & all$dayco != "Monday", 
                          ifelse(all$dayci != "Friday" & all$dayci != "Saturday", 1,0),0),0)

# channel no tiene sentido porque elegi una de varias acciones. 
# algunos usuarios tuvieron acciones tanto en desktop como en mobile

# Dummies duracion
all$dumdur <- ifelse(all$duracion < 3, "1o2", 
                     ifelse(all$duracion < 7 & all$duracion > 2, "3a6",
                            ifelse(all$duracion < 10 & all$duracion > 6, "7a9",
                                   ifelse(all$duracion < 15 & all$duracion > 9, "10a14",
                                          ifelse(all$duracion < 22 & all$duracion > 14, "15a21", "21mas")))))

# saco algunas variables que no sirven para el logit
#all$destino_IATA <- NULL
#all$fl <- NULL
all$pr <- NULL
#all$country <- NULL
# all$hotel_ID <- NULL
# all$email <- NULL

# Estrellas
# hay 25,376 filas de 552,931 que tienen 0 estrellas. las saco porque no tiene sentido
all$pricestar <- ifelse(all$hc==0, NA, all$pri.usd/all$hc)
all$pricestar.excl.fee <- ifelse(all$hc==0, NA, all$pri.antes.de.fee.usd / all$hc) 

#### TRANSFORMACIONES #####
all$duracion2 <- all$duracion^2
all$duracion3 <- all$duracion^3
all$anticipacion2 <- all$anticipacion^2 
all$anticipacion3 <- all$anticipacion^3 
all$ratiofinde2 <- all$ratiofinde^2 
all$ratiofinde3 <- all$ratiofinde^3
all$abtest2 <- all$abtest^2
all$abtest3 <- all$abtest^3
all$tax.percent2 <- all$tax.percent^2
all$tax.percent3 <- all$tax.percent^3
all$comi.mas.fee <- all$comision*100 + all$abtest
all$pricestar.excl.fee2 <- all$pri.antes.de.fee.usd / all$hc^2
all$pricestar.excl.fee12 <- all$pri.antes.de.fee.usd / all$hc^(1/2)
all$hc2 <- all$hc^2

#separo un subset donde las estrellas sean mayores a cero
allstarszero <- subset(all, hc == 0)
allstars <- subset(all, hc != 0)

#### CORRELACIONES ####
# veo las correlaciones entre las variables. solo puedo hacerlo con las numericas. 
all.num <- subset(all, select=c(compro,abtest,pri.usd,hc,tax.percent,checkout.count,detail.count,
                                search.count,thanks.count,duracion,anticipacion,ratiofinde,
                                distancia,domestico,hr))

correlaciones <- round((cor(all.num, use="pairwise")), digits=2)
write.csv(correlaciones, file = "correlaciones.csv")

#### TABLAS DE FRECUENCIAS #####
freqdur <- table(all$duracion)
write.csv(freqdur, file = "freq duracion.csv")

freqanticip <- table(all$duracion)
write.csv(freqanticip, file = "freq anticipacion.csv")

#### ME QUEDO SOLO CON CO Y TX ####
# y saco las obs donde hay NA en las variables de la regresion, asi despues hago 
# un cbind con el predict, ya que matchear por row.names no esta resultando
allsub <- subset(all, fl != "detail"&
                   !is.na(all$abtest)&!is.na(all$anticipacion)&
                   !is.na(all$duracion)&!is.na(all$traveler)&
                   !is.na(all$domestico)&
                   !is.na(all$pri.usd)&
                   !is.na(all$comision)&all$tax.percent>=0)
#&!is.na(all$pricestar.excl.fee)

#### separo data en train y test ####
## 90% of the sample size
smp.size <- floor(0.9 * nrow(allsub))

## set the seed to make your partition reproductible
set.seed(1)
train.ind <- sample(seq_len(nrow(allsub)), size = smp.size)

alltrain <- allsub[train.ind, ]
alltest <- allsub[-train.ind, ]

rm(smp.size,train.ind )
#### REGRESION GENERAL ####
# (resultante de muchas otras regresiones poniendo y sacando variables) tax.orig2 + 
glmt.0 <- glm(compro ~ abtest ,
              data = allsub, family = binomial, subset = c(cc=="CL"))
summary(glmt.0)


#### Predict y ROC de train ####
glmt0.fitted <- as.data.frame(predict(glmt.0, type = "response"))
# chequeo que los row.names sean identicos asi se que cada fitted value corresponde a esa fila
identical(row.names(alltrain), row.names(glmt0.fitted)) 
pred.t0 <- cbind(alltrain[,c("compro")], glmt0.fitted)

# curva ROC
pred <- prediction(pred.t0$predict, pred.t0$alltrain)
perf <- performance(pred,"tpr","fpr")
plot(perf)
AUC <- performance(pred, 'auc')

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(perf, pred))




#### Predict y ROC de test ####
glmt0.fitted.test <- as.data.frame(predict(glmt.0, newdata = alltest, type = "response"))
# chequeo que los row.names sean identicos asi se que cada fitted value corresponde a esa fila
identical(row.names(alltest), row.names(glmt0.fitted.test)) 
pred.t0.test <- cbind(alltest[,c("compro")], glmt0.fitted.test)

# curva ROC
pred.test <- prediction(pred.t0.test$predict, pred.t0.test$alltest)
perf.test <- performance(pred,"tpr","fpr")
par(bg = 'white')
plot(perf.test, print.AUC=T )
abline(a=0, b= 1)
AUC.test <- performance(pred, 'auc')





## precision/recall curve (x-axis: recall, y-axis: precision)
prec.rec <- performance(pred, "prec", "rec")
plot(prec.rec)

## sensitivity/specificity curve (x-axis: specificity,
## y-axis: sensitivity)
sens.spec <- performance(pred, "sens", "spec")
plot(sens.spec)


#### Regresiones POR SEGMENTO ####

glmt.1 <- glm(compro ~ abtest,
              data = allsub, family = binomial, 
              subset = c(cc=="CL"))
summary(glmt.1)

# mpri.baseusd es el precio por noche por habitacion en usd. tiene 332051 NA





hist(alltrain$duracion, plot=T, breaks=20)



#### VER COSAS ####

# costo sin tax promedio por estrellas
aggregate(alltrain$costo.excl.tax, by = list(stars = alltrain$hc), FUN="mean", na.rm = T)

# comision/precio (antes de fee) promedio por estrellas
aggregate(alltrain$comision, by = list(stars = alltrain$hc), FUN="mean", na.rm = T)

# precio (antes de fee) promedio por estrellas
aggregate(alltrain$pri.antes.de.fee.usd, by = list(stars = alltrain$hc), FUN="mean", na.rm = T)

# costo.y.comision.excl.tax promedio por estrellas
aggregate(alltrain$costo.y.comision.excl.tax, by = list(stars = alltrain$hc), FUN="mean", na.rm = T)

# tax.percent (sin fee) promedio por estrellas
aggregate(alltrain$tax.percent, by = list(stars = alltrain$hc), FUN="mean", na.rm = T)

# comision promedio por dest.type
aggregate(all$comision, by = list(stars = all$domestico), FUN="mean", na.rm = T)


#### CONTAR gente en cada segmento ####
nrow(subset(all, traveler=="couple"
            &dest.type=="Latam"
            &anticipacion>0&anticipacion>4
            &duracion>3&duracion<11))






#Guardo columnas de valor
datos = datos[, list(FECHA, HORA, origen_IATA, userid, destino_IATA, ci, co, flow, country, source, GB, hotel_Id, hotel_estrellas, event, eventtype), by = userid]
#Veo si el user id usario uso filtros
datos$filtrado = ifelse(datos$event == "filter", 1, 0) #OK

filtrados = datos[, list(FECHA, HORA, origen_IATA, userid, destino_IATA, ci, co, flow, country, source, GB, hotel_Id, hotel_estrellas, event, eventtype, filtrado = max(filtrado)), by = userid]
filtrados = subset(filtrados, filtrado == 1) #OK
