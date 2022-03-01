
library(readxl)
require(fields)
require(geoR)
require(akima)
require(scatterplot3d)
library(gstat)
library(sf)
library(sp)
library(rgdal)
library(ggplot2)  # Gráficas
library(plotly)



#cargamos la base de datos y damos frmato apropiado 
c1=read.csv("Datos1.csv", head = TRUE, dec = ".")

geo=as.geodata(c1)

c=c1

c=( cbind(as.numeric(c$este),as.numeric(c$norte),(as.numeric(c$kwtmc))) )
c=as.data.frame(c)
#formato adecuado para gstat variogram


#################################################
#cargamos el mapa formato shp
######################################################

shp_map <- read_sf("departamentos.shp")


#########################################################
#royectemos las observaciones
############################################################

map_radiacion <- function(datos, variable, map) {
  
  plot1 <- ggplot() +
    geom_sf(data = map, size = 0.3) +
    geom_point(data = datos, aes_string(x = "V1", y = "V2",
                                        color = variable)) +
    scale_color_viridis_c(direction = -1) +
    labs(
      x = "",
      y = "",
      title = paste(variable, "map Colombia")
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 14,
                                face = "bold",
                                colour = "black"),
      plot.margin = unit(c(1, 1, 1.5, 1.2), "cm"))
  
  return(plot1)
  
}

pl1 <- map_radiacion(c,
                           geo$data,
                           shp_map)

ggplotly(pl1)



#################################################################
#estimación semivariograma
#################################################################
coordinates(c) = ~V1 + V2
#objeto ajusta residuales
g_obj1 <- gstat(id = "V3",
               formula = V3 ~ V2 ,nmin = 60,
               data = c)


#variograma residuales obtenidos por la función
#valores obtenidos paquete geoR
var_s1 <- variogram(g_obj1,1376294)

#plot(var_s1)

#varigrama que sacamos a ojo eyefit
vgm_model2 <- vgm("Wav",
                  psill = 0.065,
                  range = 250000,nugget = 0.36 )



#ajuste teorico variograma
fitted_vgm1 <- fit.variogram(object = var_s1,
                             model = vgm_model2,fit.sills = FALSE, fit.ranges =FALSE)



#grafico
#plot(var_s1, fitted_vgm1, pl = T)


##################################################################


##############################################################
#predicción
#############################################################

###########
#dandole el formato correcto a las observaciones,coordenadas,proyección
#nombre
############

colnames(c@coords) <- c("X", "Y") #nombres adecuados para la tendencia
#proyección adecuada para usar predict
proj4string(c)= CRS("+proj=tmerc +lat_0=4 +lon_0=-73 +k=0.9992 +x_0=5000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

###################################################
#Función prediccion
###################################################

prediction_plot <- function(g_object, variable, map_path) {
  
  map <- readOGR(map_path)
  new <- sp::spsample(map, n = 10000, type = "regular")
       new=spTransform(new, CRS=CRS("+proj=tmerc +lat_0=4 +lon_0=-73 +k=0.9992 +x_0=5000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
            coordinates(new)~ x1 +x2
             colnames(new@coords) <- c("X", "Y")
  
  predic <- predict(g_object, newdata = new)
           prediction <- data.frame(predic)
  
  pred <- paste(variable, ".pred", sep = "")
  
  
  plot1 <- plot_ly(
    x = prediction[["X"]],
    y = prediction[["Y"]],
    z = (prediction[[pred]]),
    type = "heatmap",
    colorbar = list(title = "Prediction"),
    reversescale = T
    
  ) %>%
    layout(
      title = paste("prediction kwh/m2"),
      xaxis = list(showticklabels = FALSE),
      yaxis = list(showticklabels = FALSE),
      scene = list(aspectration = list(x = 1, y = 1))
    )
  return(plot1)
  }

g_obj1 <- gstat(id = "V3",
               formula = V3~ Y,
               model = fitted_vgm1,nmin = 60,
              
               data = c)

#mapa usando krigging universal

prediction_plot(g_obj1, "V3", "departamentos.shp")



######################################################
#mapa varianza
########################################
variance_plot <- function(g_object, variable, map_path) {
  
  map <- readOGR(map_path)
  new <- sp::spsample(map, n = 10000, type = "regular")
  new=spTransform(new, CRS=CRS("+proj=tmerc +lat_0=4 +lon_0=-73 +k=0.9992 +x_0=5000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  coordinates(new) ~ x1 + x2
  colnames(new@coords) <- c("X", "Y")
  
  predic <- predict(g_object, newdata = new)
  
  prediction <- data.frame(predic)
  
  var <- paste(variable, ".var", sep = "")
  
  plot2 <- plot_ly(
    x = prediction[["X"]],
    y = prediction[["Y"]],
    z = prediction[[var]],
    type = "heatmap",
    colorbar = list(title = "variance"),
    colorscale = "Hot",
    reversescale = T
  )  %>%
    layout(
      title = paste( "Variance kmh/m2"),
      xaxis = list(showticklabels = FALSE),
      yaxis = list(showticklabels = FALSE),
      scene = list(aspectration = list(x = 1, y = 1))
    )
  return(plot2)
  
}

variance_plot(g_obj1, "V3", "departamentos.shp")


#####################################################
#mapa cpeficien te de variación 
####################################################
cv_plot <- function(g_object, variable, map_path) {
  
  map <- readOGR(map_path)
  new <- sp::spsample(map, n = 10000, type = "regular")
  new=spTransform(new, CRS=CRS("+proj=tmerc +lat_0=4 +lon_0=-73 +k=0.9992 +x_0=5000000 +y_0=2000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  coordinates(new) ~ x1 + x2
  colnames(new@coords) <- c("X", "Y")
  
  predic <- predict(g_object, newdata = new)
  
  prediction <- data.frame(predic)
  pred <- paste(variable, ".pred", sep = "")
  var <- paste(variable, ".var", sep = "")
  aux <- abs(sqrt(prediction[var]) / abs(prediction[pred]))
  aux[aux > 1] <- 1
  prediction["cv"] <- aux
  
  plot3 <- plot_ly(
    x = prediction[["X"]],
    y = prediction[["Y"]],
    z = prediction[["cv"]],
    type = "heatmap",
    colorbar = list(title = "cv"),
    colorscale = "Electric",
    reversescale = T,
    zmid = 0.5
  )  %>%
    layout(
      title = paste(variable, "CV"),
      xaxis = list(showticklabels = FALSE),
      yaxis = list(showticklabels = FALSE),
      scene = list(aspectration = list(x = 1, y = 1)))
  
  
  return(plot3)
  
}

cv_plot(g_obj1, "V3", "departamentos.shp")




##################################################################
#¿hay tendencia ?
###############################################################


#analisis descriptivo
scatterplot3d(geo$coords[,1], geo$coords[,2],
              geo$data,pch=1,xlab="este",ylab="norte", zlab="Kw/m^2",color="red", main="Gráfico de Dispersión de la temperatura 3d", zlim=c(2,8))

plot(geo, qt.col = c("purple","pink","green",
                          "yellow"))


#### modelamos la tendencia de la media
#observamos el modelo que sospechamos
A=lm(c$V3~c$V2) # modelo seleccionado por parsimonia, aunque hay otros modelos con parametros significativos, esto no aportun mucho al ajuste del modelo
summary(A) #beta_0=1.274(valor esperado cuando la cordenada norte es cero) beta_1=0.0000001487 por cada
#1000000 de metros que nos desplacemos en las coordenadas norte la radiación solar promedio diaria  por metro cuadrado del mes de julio aumenta en un 0.1487 kilovatios 
plot(geo$coords[,2],geo$data, main="Modelo tendencia de la media", ylab="radiación solar media diaria en julio ", xlab="coordenadas norte")
abline(A, col=2)
legend(2200000,2.5,
       "^RS=1.274+0.0000001487CN")
#modelos desechados
#modelos desechado absolutamente(se observaba graficamente)
a=lm(c$V3~c$V1)

A2=lm(c$V3~c$V2+c$V1) #el parametro asociado a la coordenada este no aportu mucho a el ajuste de la tendecia
summary(A2)

Ai=lm(c$V3~c$V1+c$V2 +c$V1*c$V2)
summary(Ai) #similar al de arriba, incumple el criterio de parsimonia comparado con A
#modelo cuadratico 
Ac=lm(c$V3~c$V1+c$V2+I((c$V2^2))+c$V1*c$V2)
summary(Ac) #lo mismo que el anterior

#hagamos nuevamente el grafico considerando el la tendencia que encontramos

plot(geo, trend = ~geo$coords[,2]) 
plot(geo, qt.col = c("purple","pink","green",
                     "yellow"),trend = ~geo$coords[,2])
############################################################################################
#isotropico o no isotropico?
#########################################################################################

#explorando isotropia, forma y altura similar


vari_0 <- variog(geo,trend=~geo$coords[,2], max.dist = 1445108.27,
                 dir = 0)

vari_45 <- variog(geo,trend=~geo$coords[,2], max.dist = 1445108.27,
                  dir = pi / 4)
vari_90 <- variog(geo,trend=~geo$coords[,2], max.dist = 1445108.27,
                  dir = pi / 2)
vari_135 <- variog(geo,trend=~geo$coords[,2], max.dist = 1445108.27,
                   dir = 3 * pi / 4)
par(mfrow = c(2, 2),
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0))

plot(vari_0, main = "vari 0")
plot(vari_45, main = "vari 45")
plot(vari_90, main = "vari 90")
plot(vari_135, main = "vari 195")

##################################################################################
#clasico  cressie ?
##########################################################################################

#estimador clasico o estimador o estimador de cressie
par(mfrow = c(2, 1),
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0))
box=vgram(loc = geo$coords, y = geo$data, type = "variogram")
plot(box)
boxplotVGram(box,N=10,breaks = pretty(box$d, 10, eps.correct = 1), main="Boxplot semivarigrama clásico")
#notamos la presencia de datos que se podrian considerar atipicos 

vari2Cloud <- variog(geo, op = "cloud", trend = ~geo$coords[, 2],estimator.type = "modulus")
vari2Cloud <- variog(geo, op = "cloud", trend = ~geo$coords[, 2])

plot(vari2Cloud)

bplot.xy(vari2Cloud$u,vari2Cloud$v,N=10,breaks = pretty(box$d, 10, eps.correct = 1),main="Boxplot semivarigrama resistente a datos atipicos")
#notamos como mejora el comportamiento de los posibles datos atipicos 
#Asi podria ser recomendable usar el ajuste empirico resistente a datos atipicos, ya eremos que pasa.

########################################################################
#log verosimilitud
??variog
##################################################################3
#Cargamos los datos
c1=read.csv("Datos1.csv", head = TRUE, dec = ",")
c=c1
c=cbind(as.numeric(c$este),as.numeric(c$norte),(as.numeric(c$kwtmc)))
c=as.data.frame(c)
# convertimos al tipo geo data
geo=as.geodata(c)

Z=as.matrix(geo$data) #observaciones variabe respuesta la asumimos normal
#matriz de diseño para la tendencia de la media
X=as.matrix(cbind(c(rep(1,119)),c(geo$coords[,2]))) 
#valores estimados para el modelo de la tendencia
beta_estimado=as.matrix(c(1.274,0.0000001487),ncol=1) 
#valores ajustados
XB=X%*%beta_estimado


#matriz de distancias 
distancias=as.matrix(dist(geo$coords)) #matrix de distancia
#matriz de covarianza estimación teorica usando por minimos cuadrados ordinarios
#usando paquete varcov.spatial de geoR
SIGMAestimado=varcov.spatial(distancias,cov.model="wave",
                      nugget=0.36,cov.pars= c(0.059,67900)) 
?cov.spatial
SIGMAestimado=as.matrix(SIGMAestimado$varcov) #matriz de covarianza 


# inversa de la matriz de covarianza de estimación  teorica usando 
#varcov.spatial de geoR
#
invSIGMAestimado=(varcov.spatial(distancias,cov.model="wave",
                              nugget=0.36,cov.pars= c(0.059,67900),inv = TRUE))
invSIGMAestimado=as.matrix(invSIGMAestimado$inverse)

?variog

#determinante matriz de covarianza estimación a teorica usando geo R
#la siguiente  funcion devuelve el logaritmo de la raiz cuadrada del determinante
detSIGMAestimado=(varcov.spatial(distancias,cov.model="wave",
                              nugget=0.36,cov.pars= c(0.059,67900),det  = TRUE))
#despejamos el determinante de la expresión obtenida
log_sqrt_detSIGMAestimado=detSIGMAestimado$log.det.to.half
sqrtdetSIGMAestimado=exp(log_sqrt_detSIGMAestimado)
detSIGMAestimado=sqrtdetSIGMAestimado^2

##btenemos el logaritmo del determinante
log_det_SIGMA_estimado=log(detSIGMAestimado)



#función log-verosimilitud modelo teorico
logver_beta_tetha_T=((-119/2)*log(2*pi) -1/2*abs(log_det_SIGMA_estimado)-
                       (1/2)*(t(Z-XB)%*%invSIGMAestimado%*%(Z-XB)))


#########################################################
#logaritmo de la verosimilitud, estimación a ojo 

#########################################################

#matriz de covarianza estimación empirica usando paquete geoR
SIGMAOjo=varcov.spatial(distancias,cov.model="wave",
                    nugget=0.36,cov.pars= c(0.065,80000)) #matriz de covarianza 
SIGMAOjo=as.matrix(SIGMAOjo$varcov)

#matriz de covarianza estimación empirica usando geo R
invSIGMAOjo=(varcov.spatial(distancias,cov.model="wave",
                          nugget=0.36,cov.pars= c(0.065,80000),inv = TRUE))
invSIGMAOjo=as.matrix(invSIGMAOjo$inverse)


#determinante matriz de covarianza estimación a empirica usando geo R
#la siguiente  funcion devuelve el logaritmo de la raiz cuadrada del determinant
detSIGMAOjo=(varcov.spatial(distancias,cov.model="wave",
                          nugget=0.36,cov.pars= c(0.065,80000),det  = TRUE))

#despejamos el determinante de la expresión obtenida
log_sqrt_detSIGMAOjo=detSIGMAOjo$log.det.to.half
#hacemos exponencia
sqrtdetSIGMAOjo=exp(log_sqrt_detSIGMAOjo)
#elevamos al cuadrado
detSIGMAOjo=sqrtdetSIGMAOjo^2


##obtenemos el logaritmo del determinante
log_det_SIGMA_Ojo=log(detSIGMAOjo)
###############################

logver_beta_tetha_Ojo=((-119/2)*log(2*pi) -1/2*abs(log_det_SIGMA_Ojo)-
                         (1/2)*(t(Z-XB)%*%invSIGMAOjo%*%(Z-XB)))






######################################
#ajustamos el semivariograma tomando en cuenta la tendencia

vari1=variog(geo,trend = ~ geo$coords[,2])

vari2=variog(geo,trend=~geo$coords[,2],pairs.min = 60)
plot(vari2)
#compararemos los semivariogramas empiricos para las dos estimaciones clasicas

#notamos que el ultimo punto se sale de la tendencia

#anotación para Fernanda(En la expo mostraremos los variograma empiricos, vari1, vari2) y elegiremos vari2
#y adicional comparearemos este ultimo vari2 con los variR, variR30, variR166,vari268 y concluiremos que no vale la pena asumir normalidad para hacer la estimación
#resistente a datos atipicos

par(mfrow = c(2, 1),
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0))

plot(vari1,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "semivariograma empirico clásico lleno ",
     col.main = 4, cex.main =1.3)

plot(vari2,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "semivariograma empirico clásico-30",
     col.main = 4, cex.main =1.3)
#la estacionariedad se logra con la remoción de los grupos con menos de 30 pares de datos
#asi que entre los clasico elegiremos vari2 e igual entre todos

### ahora los resistentes a datos atipicos 

variR=variog(geo,trend = ~ geo$coords[,2], estimator.type = c( "modulus"))
variR30=variog(geo,trend = ~ geo$coords[,2], estimator.type = c( "modulus"),pairs.min=30)
variR166=variog(geo,trend = ~ geo$coords[,2], estimator.type = c( "modulus"),pairs.min=166)
variR268=variog(geo,trend = ~ geo$coords[,2], estimator.type = c( "modulus"),pairs.min=268)

par(mfrow = c(2, 2),
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0))
plot(variR,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "semivariograma empirico cressie",
     col.main = 4, cex.main =1.3)

plot(variR30,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "semivariograma empirico cressie-30",
     col.main = 4, cex.main =1.3)

plot(variR166,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "semivariograma empirico cressie-166",
     col.main = 4, cex.main =1.3)


plot(variR268,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "semivariograma empirico cressie-268",
     col.main = 4, cex.main =1.3)

#### vemos que el cressie268, toma un comportamiento similar al del clasico, pero requiere quitar mucha información, por lo cual preferiremos las estimaciones clasicas. 


####estimación teorica 
#iniciales variograma sin remover punts 
#ini11=c(0.075,104752)

#fitvari1 <- variofit(vari1,
 #                   cov.model = "wave",
  #                  ini11,
   #                 fix.nugget = TRUE,
    #                nugget = 0.35,
     #               wei = "equal")



#fitvari2 <- variofit(vari1,
 #                    cov.model = "wave",
  #                   ini11,
   #                  fix.nugget = TRUE,
    #                 nugget = 0.35,
     #                wei = "npairs")

#fitvari3 <- variofit(vari1,
 #                    cov.model = "wave",
  #                   ini11,
   #                  fix.nugget = TRUE,
    #                 nugget = 0.35,
     #                wei = "cressie")



#fitvari4 <- likfit(geo,
 #                 coords = geo$coords,
  #                data = geo$data,
   #               trend = ~geo$coords[,2],
    #              ini.cov.pars = ini11,
     #             fix.nugget = T,
      #            nugget = 0.35,
       #           cov.model = "wave",
        #          lik.method = "ML")


#fitvari5 <- likfit(geo,
 #                 coords = geo$coords,
  #                data = geo$data,
   #               trend = ~geo$coords[,2],
    #              ini.cov.pars = ini11,
     #             fix.nugget = T,
      #            nugget = 0.35,
       #           cov.model = "wave",
        #          lik.method = "REML")


########################################################


#plot(vari1,
 #    xlab = "h",
  #   ylab = "semivarianza", ylim=c(0,0.5),
   #  cex.lab = 1.3,
    # cex.axis = 1.2,
     #main = "Estimación teórica del modelo de semivariograma",
    # col.main = 4, cex.main =1.3)


###############################################
#########################################

#lines(fitvari1, col = 1)
#lines(fitvari2, col = 2)
#lines(fitvari3, col = 3)
#lines(fitvari4, col = 4)
#lines(fitvari5, col = 5)


#legend(1000000,0.17,
     #  c("MCO","MCPnpairs",  "MCPcressie", "ML", "REML"),
      # lwd = 2,
       #lty = 1:6,
#       col = 1:6,
 #      box.col = 9,
  #     text.col = 1:6)


################################################
#estimación teorica removiendo 30 puntos del final. 

#############################3

### notamos que al remover el grupo con menos de 3 pares de puntos se logra estabilizar el comportamiento del variograma empirico


#estimación empirica #removiendo los grupos con menos de 30 pares


#iniciales removiendo grupos con enos de 30 pares de datos
ini11=c(0.065,250000)#parametros, sigma^2 y phi
ini12=c(0.065,80000)
######################################################
#estimación teorica wave







#ajustes modelos, MCO, wls, cressiemaxima verosimilitud y maxima verocimilitud restringida
fitvari21 <- variofit(vari2,
                      cov.model = "wave",
                      ini11,
                      fix.nugget = TRUE,
                      nugget = 0.36,
                      wei = "equal")

fitvari212 <- variofit(vari2,
                      cov.model = "wave",
                      ini12,
                      fix.nugget = TRUE,
                      nugget = 0.36,
                      wei = "equal")
summary(fitvari21
        )

fitvari22 <- variofit(vari2,
                      cov.model = "wave",
                      ini11,
                      fix.nugget = TRUE,
                      nugget = 0.36,
                      wei = "npairs")

fitvari23 <- variofit(vari2,
                      cov.model = "wave",
                      ini11,
                      fix.nugget = TRUE,
                      nugget = 0.36,
                      wei = "cressie")
FV=likfit(geo,
            coords = geo$coords,
            data = geo$data,
            trend = ~geo$coords[,2],
            ini.cov.pars = ini11,
            fix.nugget = T,
            nugget = 0.36,
            cov.model = "wave",
            lik.method = "ML",limits = pars.limits())



FV2 <- likfit(geo,
                   coords = geo$coords,
                   data = geo$data,
                   trend = ~geo$coords[,2],
                   ini.cov.pars = ini11,
                   fix.nugget = T,
                   nugget = 0.36,
                   cov.model = "wave",
                   lik.method = "REML")

####estimación teorica 
par(mfrow = c(1, 1),
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0))

plot(vari2,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "Estimación teórica del modelo de semivariograma",
     col.main = 4, cex.main =1.3)


#proyectarlas sobre el grafico anterior cerca del final

lines(fitvari212, col = 1)
lines(fitvari21, col = 2)
lines(fitvari23, col = 3)
lines(FV, col = 4)
lines(FV2, col = 5 )

legend(600000,0.17,
       c("MCO", "phi=67990","sigma^2=0.0597"),
       lwd = 2,
       lty = 1:4,
       col = 1:4,
       box.col = 9,
       text.col = 1:4)

### ya que todas las estimaciones son tan similares, y que los distintas sumas de cuadrados no son comparable 
#construimos el cuadrado medio del error para poder decidir entre los distintos modelos 


F1<-function(i){ result <- 0.36 +0.0592*(1-(((67990.3327 /i)*sin(i/67990.3327 ))))}
F2<-function(i){ result <- 0.36 +0.0625*(1-(((67104.0061 /i)*sin(i/67104.0061 ))))}
F3<-function(i){ result <- 0.36 +0.0643*(1-((( 80000/i)*sin(i/ 80000))))}
F4<-function(i){ result <- 0.36 +0.0601*(1-(((80000/i)*sin(i/80000))))}
F5<-function(i){ result <- 0.36 +0.0758*(1-(((80000/i)*sin(i/80000))))}




###calculamos los cuadrados medios

CMRF1=sum((t((vari2$v-F1(vari2$u)))*(vari2$v-F1(vari2$u))))
CMRF2=sum((t((vari2$v-F2(vari2$u)))*(vari2$v-F2(vari2$u))))
CMRF3=sum((t((vari2$v-F3(vari2$u)))*(vari2$v-F3(vari2$u))))
CMRF4=sum((t((vari2$v-F4(vari2$u)))*(vari2$v-F4(vari2$u))))
CMRF5=sum((t((vari2$v-F5(vari2$u)))*(vari2$v-F5(vari2$u))))
CMRs=c(list("REML",CMRF5),list("ML",CMRF4),list("cresiie",CMRF3),list("npairs",CMRF2),list("MCO",CMRF1))
CMRs

 
###############################
#Ajuste variograma resistente datos atipico 268
##########################################3
iniR2=c(0.075,55000)







fitvariR1 <- variofit(variR268,
                      cov.model = "wave",
                      iniR2,
                      fix.nugget = TRUE,
                      nugget = 0.36,
                      wei = "equal")


fitvariR2 <- variofit(variR268,
                      cov.model = "wave",
                      iniR2,
                      fix.nugget = TRUE,
                      nugget = 0.36,
                      wei = "npairs")

fitvariR3 <- variofit(variR268,
                      cov.model = "wave",
                      iniR2,
                      fix.nugget = TRUE,
                      nugget = 0.36,
                      wei = "cressie")
FVR=likfit(geo,
          coords = geo$coords,
          data = geo$data,
          trend = ~geo$coords[,2],
          ini.cov.pars = iniR2,
          fix.nugget = T,
          nugget = 0.36,
          cov.model = "wave",
          lik.method = "ML")
?likfit 



FV2R <- likfit(geo,
              coords = geo$coords,
              data = geo$data,
              trend = ~geo$coords[,2],
              ini.cov.pars = iniR2,
              fix.nugget = T,
              nugget = 0.36,
              cov.model = "wave",
              lik.method = "REML")

plot(variR2,
     xlab = "h",
     ylab = "semivarianza", ylim=c(0,0.5),
     cex.lab = 1.3,
     cex.axis = 1.2,
     main = "Estimació teorica semivariogra R-268",
     col.main = 4, cex.main =1.3)


lines(fitvariR1, col = 1)
lines(fitvariR2, col = 2)
lines(fitvariR3, col = 3)
lines(FVR, col = 4)
lines(FV2R, col = 5)


FR1<-function(i){ result <- 0.36 + 0.0509*(1-(((49052.2033 /i)*sin(i/49052.2033 ))))}
FR2<-function(i){ result <- 0.36 +0.0498*(1-(((46348.5550/i)*sin(i/46348.5550))))}
FR3<-function(i){ result <- 0.36 +0.0499*(1-(((55000/i)*sin(i/55000))))}
FR4<-function(i){ result <- 0.36 +0.0804*(1-(((55000/i)*sin(i/55000))))}
FR5<-function(i){ result <- 0.36 +0.937*(1-(((55000/i)*sin(i/55000))))}


CMRFR1=sum((t((variR268$v-FR1(variR268$u)))*(variR268$v-FR1(variR268$u))))
CMRFR2=sum((t((variR268$v-FR2(variR268$u)))*(variR268$v-FR2(variR268$u))))
CMRFR3=sum((t((variR268$v-FR3(variR268$u)))*(variR268$v-FR3(variR268$u))))
CMRFR4=sum((t((variR268$v-FR4(variR268$u)))*(variR268$v-FR4(variR268$u))))
CMRFR5=sum((t((variR268$v-FR5(variR268$u)))*(variR268$v-FR5(variR268$u))))

CMRRs=c(list("REML",CMRFR5),list("ML",CMRFR4),list("cresiie",CMRFR3),list("npairs",CMRFR2),list("MCO",CMRFR1))



x(11)
 #nuget 0.3, sigma 0.1, d=1445108.27, range = 41702.76
ini1=eyefit(vari1)
ini2=eyefit(vari1)
summary(ini1)
?eyefit

?plot.variogram
 ?variofit
plot(variog(geo))

puntos <- interp(c[,1],c[,2],c[,3])


persp(puntos$x,
      puntos$y,
      puntos$z,
      xlab = "Este",
      ylab = "Norte",
      zlab = "Nivel freatico",
      phi = 30,
      theta = 20,
      col = "lightblue",
      expand = .5,
      ticktype = "detailed")


drape.plot(puntos$x,
           puntos$y,
           puntos$z,
           xlab = "Este",
           ylab = "Norte",
           zlab = "z",
           theta = -10,
           col = topo.colors(64),
           expand = .2,
           ticktype = "detailed")
?drape.plot

drape.plot(puntos$x,
           puntos$y,
           puntos$z,
           xlab = "Este",
           ylab = "Norte",
           zlab = "z",
           theta = 45,
           col = topo.colors(64),
           expand = .5,
           ticktype = "detailed",horizontal = FALSE)

drape.plot(puntos$x,
           puntos$y,
           puntos$z,
           xlab = "Este",
           ylab = "Norte",
           zlab = "z",
           theta = 60,
           col = topo.colors(64),
           expand = .5,
           ticktype = "detailed")



par(mfrow = c(2, 1),
    mar = c(1,1,1,1))

contour(puntos, nlevels = 10, main = "Contorno")
image(puntos$z, main =  "Grilla")







#semivariograma



curve(cov.spatial(x,cov.model = "exponential",cov.pars =c(25,30)),0,200 )
curve(25-cov.spatial(x,cov.model = "exponential",cov.pars =c(25,25)),0,200)
      
curve(cov.spatial(x,cov.model = "exponential",cov.pars =c(25,30)) )     









######
?cov.spatial
?optim
