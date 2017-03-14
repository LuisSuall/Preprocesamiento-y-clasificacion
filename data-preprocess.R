# Libraries needed to run the script
library(readr)

#########################################
##### Load train and test databases #####
#########################################

accidentes_kaggle_raw <- read.csv("./data/accidentes-kaggle.csv")
accidentes_kaggle_test_raw <- read.csv("./data/accidentes-kaggle-test.csv")

# Set "HORA" attribute to numeric
accidentes_kaggle <- accidentes_kaggle_raw
accidentes_kaggle$HORA <- as.numeric(sub(",",".",as.character(accidentes_kaggle$HORA)))

accidentes_kaggle_test <- accidentes_kaggle_test_raw
accidentes_kaggle_test$HORA <- as.numeric(sub(",",".",as.character(accidentes_kaggle_test$HORA)))

# Order "MES" and "DIASEMANA" attributes
levels(accidentes_kaggle$MES) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
levels(accidentes_kaggle$DIASEMANA) <- c("LUNES","MARTES","MIERCOLES","JUEVES","VIERNES","SABADO","DOMINGO")

levels(accidentes_kaggle_test$MES) <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
levels(accidentes_kaggle_test$DIASEMANA) <- c("LUNES","MARTES","MIERCOLES","JUEVES","VIERNES","SABADO","DOMINGO")

###############################
##### Feature Engineering #####
###############################

# Rates

accidentes_kaggle$RAT_MUERTOS_VICTIMAS <- accidentes_kaggle$TOT_MUERTOS/accidentes_kaggle$TOT_VICTIMAS
accidentes_kaggle$RAT_HERIDOSGRAVES_VICTIMAS <- accidentes_kaggle$TOT_HERIDOS_GRAVES/accidentes_kaggle$TOT_VICTIMAS
accidentes_kaggle$RAT_HERIDOSLEVES_VICTIMAS <- accidentes_kaggle$TOT_HERIDOS_LEVES/accidentes_kaggle$TOT_VICTIMAS
accidentes_kaggle$RAT_VEHICULOS_VICTIMAS <- accidentes_kaggle$TOT_VEHICULOS_IMPLICADOS/accidentes_kaggle$TOT_VICTIMAS

accidentes_kaggle_test$RAT_MUERTOS_VICTIMAS <- accidentes_kaggle_test$TOT_MUERTOS/accidentes_kaggle_test$TOT_VICTIMAS
accidentes_kaggle_test$RAT_HERIDOSGRAVES_VICTIMAS <- accidentes_kaggle_test$TOT_HERIDOS_GRAVES/accidentes_kaggle_test$TOT_VICTIMAS
accidentes_kaggle_test$RAT_HERIDOSLEVES_VICTIMAS <- accidentes_kaggle_test$TOT_HERIDOS_LEVES/accidentes_kaggle_test$TOT_VICTIMAS
accidentes_kaggle_test$RAT_VEHICULOS_VICTIMAS <- accidentes_kaggle_test$TOT_VEHICULOS_IMPLICADOS/accidentes_kaggle_test$TOT_VICTIMAS


#################################
##### Deal with NA's values #####
#################################

# List attributes with NA values

colnames(accidentes_kaggle)[apply(accidentes_kaggle,2, function(x) any(is.na(x)))]
colnames(accidentes_kaggle_test)[apply(accidentes_kaggle_test,2, function(x) any(is.na(x)))]

# CARRETERA
# The NA values should correspond to urban streets.

levels(accidentes_kaggle$CARRETERA) <- c(levels(accidentes_kaggle$CARRETERA),"CALLE")
accidentes_kaggle$CARRETERA[is.na(accidentes_kaggle$CARRETERA)] <- "CALLE"

levels(accidentes_kaggle_test$CARRETERA) <- c(levels(accidentes_kaggle_test$CARRETERA),"CALLE")
accidentes_kaggle_test$CARRETERA[is.na(accidentes_kaggle_test$CARRETERA)] <- "CALLE"

# ACOND_CALZADA
# No clues about NA values here, but the other values can be useful. Turning NA values to "Desconocido"

levels(accidentes_kaggle$ACOND_CALZADA) <- c(levels(accidentes_kaggle$ACOND_CALZADA),"DESCONOCIDO")
accidentes_kaggle$ACOND_CALZADA[is.na(accidentes_kaggle$ACOND_CALZADA)] <- "DESCONOCIDO"

levels(accidentes_kaggle_test$ACOND_CALZADA) <- c(levels(accidentes_kaggle_test$ACOND_CALZADA),"DESCONOCIDO")
accidentes_kaggle_test$ACOND_CALZADA[is.na(accidentes_kaggle_test$ACOND_CALZADA)] <- "DESCONOCIDO"

# PRIORIDAD   
# Turn NA to "NINGUNA (SOLO NORMA)". Save a flag.    
accidentes_kaggle$NA_PRIORIDAD <- is.na(accidentes_kaggle$PRIORIDAD)
accidentes_kaggle$PRIORIDAD[is.na(accidentes_kaggle$PRIORIDAD)] <- "NINGUNA (SOLO NORMA)"

accidentes_kaggle_test$NA_PRIORIDAD <- is.na(accidentes_kaggle_test$PRIORIDAD)
accidentes_kaggle_test$PRIORIDAD[is.na(accidentes_kaggle_test$PRIORIDAD)] <- "NINGUNA (SOLO NORMA)"

# VISIBILIDAD_RESTRINGIDA
# Turn NA to "SIN RESTRICCION". Save a flag.
accidentes_kaggle$NA_VISIBILIDAD_RESTRINGIDA <- is.na(accidentes_kaggle$VISIBILIDAD_RESTRINGIDA)
accidentes_kaggle$VISIBILIDAD_RESTRINGIDA[is.na(accidentes_kaggle$VISIBILIDAD_RESTRINGIDA)] <- "SIN RESTRICCION"

accidentes_kaggle_test$NA_VISIBILIDAD_RESTRINGIDA <- is.na(accidentes_kaggle_test$VISIBILIDAD_RESTRINGIDA)
accidentes_kaggle_test$VISIBILIDAD_RESTRINGIDA[is.na(accidentes_kaggle_test$VISIBILIDAD_RESTRINGIDA)] <- "SIN RESTRICCION"

# OTRA_CIRCUNSTANCIA 
# Turn NA to "NINGUNA". Save a flag.
accidentes_kaggle$NA_OTRA_CIRCUNSTANCIA <- is.na(accidentes_kaggle$OTRA_CIRCUNSTANCIA)
accidentes_kaggle$OTRA_CIRCUNSTANCIA[is.na(accidentes_kaggle$OTRA_CIRCUNSTANCIA)] <- "NINGUNA"

accidentes_kaggle_test$NA_OTRA_CIRCUNSTANCIA <- is.na(accidentes_kaggle_test$OTRA_CIRCUNSTANCIA)
accidentes_kaggle_test$OTRA_CIRCUNSTANCIA[is.na(accidentes_kaggle_test$OTRA_CIRCUNSTANCIA)] <- "NINGUNA"

# ACERAS
accidentes_kaggle$NA_ACERAS <- is.na(accidentes_kaggle$ACERAS)
accidentes_kaggle$ACERAS[is.na(accidentes_kaggle$ACERAS)&accidentes_kaggle$ZONA_AGRUPADA=="VIAS URBANAS"] <- "SI HAY ACERA"
accidentes_kaggle$ACERAS[is.na(accidentes_kaggle$ACERAS)&accidentes_kaggle$ZONA_AGRUPADA!="VIAS URBANAS"] <- "NO HAY ACERA"

accidentes_kaggle_test$NA_ACERAS <- is.na(accidentes_kaggle_test$ACERAS)
accidentes_kaggle_test$ACERAS[is.na(accidentes_kaggle_test$ACERAS)&accidentes_kaggle_test$ZONA_AGRUPADA=="VIAS URBANAS"] <- "SI HAY ACERA"
accidentes_kaggle_test$ACERAS[is.na(accidentes_kaggle_test$ACERAS)&accidentes_kaggle_test$ZONA_AGRUPADA!="VIAS URBANAS"] <- "NO HAY ACERA"

# DENSIDAD_CIRCULACION
# Turn NA to "FLUIDA". Save a flag.
accidentes_kaggle$NA_DENSIDAD_CIRCULACION <- is.na(accidentes_kaggle$DENSIDAD_CIRCULACION)
accidentes_kaggle$DENSIDAD_CIRCULACION[is.na(accidentes_kaggle$DENSIDAD_CIRCULACION)] <- "FLUIDA"

accidentes_kaggle_test$NA_DENSIDAD_CIRCULACION <- is.na(accidentes_kaggle_test$DENSIDAD_CIRCULACION)
accidentes_kaggle_test$DENSIDAD_CIRCULACION[is.na(accidentes_kaggle_test$DENSIDAD_CIRCULACION)] <- "FLUIDA"

# MEDIDAS_ESPECIALES
# Turn NA to "NINGUNA MEDIDA". Save a flag.
accidentes_kaggle$NA_MEDIDAS_ESPECIALES <- is.na(accidentes_kaggle$MEDIDAS_ESPECIALES)
accidentes_kaggle$MEDIDAS_ESPECIALES[is.na(accidentes_kaggle$MEDIDAS_ESPECIALES)] <- "NINGUNA MEDIDA"

accidentes_kaggle_test$NA_MEDIDAS_ESPECIALES <- is.na(accidentes_kaggle_test$MEDIDAS_ESPECIALES)
accidentes_kaggle_test$MEDIDAS_ESPECIALES[is.na(accidentes_kaggle_test$MEDIDAS_ESPECIALES)] <- "NINGUNA MEDIDA"

# Check again the list of attributes with NA values
colnames(accidentes_kaggle)[apply(accidentes_kaggle,2, function(x) any(is.na(x)))]
colnames(accidentes_kaggle_test)[apply(accidentes_kaggle_test,2, function(x) any(is.na(x)))]

#####################################
##### Remove outliers and noise #####
#####################################

#TODO

##################################
##### Save preprocessed data #####
##################################

write.csv(accidentes_kaggle,"./data/preprocessed_accidentes_kaggle.csv", row.names = FALSE)
write.csv(accidentes_kaggle_test,"./data/preprocessed_accidentes_kaggle_test.csv", row.names = FALSE)