library("tidyverse")

# 1. Carga en memoria el fichero CSV como tibble, asegurándote de que las variables
#    cualitativas sean le?das como factores

df <- read_csv("C:/data/13846.csv",
               col_types =cols(
                 sexo = col_factor(),
                 dietaEsp = col_factor(),
                 nivEstPad = col_factor(),
                 nivEstudios = col_factor(),
                 nivIngresos = col_factor()
               ))


# 2. Construye una nueva columna llamada IMC que sea igual al peso dividido por la altura
#    al cuadrado. La variable explicada será IMC, las variables explicatorias serán el resto
#    de 12 variables exceptuando peso y altura

df <- mutate(df, IMC = peso / (altura^2))

# 3. Elimina completamente las filas que tengan algún valor NA en una de sus columnas.

df <- na.omit(df)

# 4. Calcula las medias y desviaciones típicas de todas las variables numéricas.

# medias <- summarise(df, peso=mean(df$peso), altura=mean(df$altura), edad=mean(df$edad),
#                     tabaco=mean(df$tabaco), ubes=mean(df$ubes), carneRoja=mean(df$carneRoja), 
#                     verduras=mean(df$verduras), deporte=mean(df$deporte), drogas=mean(df$drogas))

# Primero guardamos solo los valores numericos
numericos <- df %>% keep(is.numeric)
# Y con la funcion map hacemos la media a todas las columnas
medias <- numericos %>% map(mean)

# Funcion para calcular desviacion tipica
fdt <- function(vector){
  sqrt(mean(vector^2) - mean(vector)^2)
}

desTipicas <- summarise(df, peso=fdt(df$peso), altura=fdt(df$altura), edad=fdt(df$edad),
                        tabaco=fdt(df$tabaco), ubes=fdt(df$ubes), carneRoja=fdt(df$carneRoja), 
                        verduras=fdt(df$verduras), deporte=fdt(df$deporte), drogas=fdt(df$drogas))

# Lo mismo que antes pero con la nueva funcion creada
desTipicas <- numericos %>% map(fdt)

# 5. Calcula los coeficientes de regresión y el coeficiente de determinación para las 11
#    regresiones lineales unidimensionales

# Funcion para hacer un ajuste y guardar informacion sobre este.
ajuste <- function (x,y,df){
  mod <- lm(str_c(y,"~",x ), df)
  list(y=y,x=x,modelo=mod,R2=summary(mod)$r.squared)
}

# Guardamos aparte los nombres de las columnas a las que hacerle el ajuste
explicatorias <- names(df)[-1:-3]
# Aplicamos el ajuste a todas estas columnas
modelos <- explicatorias %>% map(~ajuste(.,"IMC",df))

# coeficientes <- list(sexo=fcoeficientes(df$IMC, df$sexo), edad=fcoeficientes(df$IMC, df$edad),
#                      tabaco=fcoeficientes(df$IMC, df$tabaco), ubes=fcoeficientes(df$IMC, df$ubes),
#                      carneRoja=fcoeficientes(df$IMC, df$carneRoja), verduras=fcoeficientes(df$IMC, df$verduras),
#                      deporte=fcoeficientes(df$IMC, df$deporte), drogas=fcoeficientes(df$IMC, df$drogas),
#                      dietaEsp=fcoeficientes(df$IMC, df$dietaEsp), nivEstPad=fcoeficientes(df$IMC, df$nivEstPad),
#                      nivEstudios=fcoeficientes(df$IMC, df$nivEstudios), nivIngresos=fcoeficientes(df$IMC, df$nivIngresos))

# 6. Representa los gráficos de dispersión en el caso de variables numéricas y los boxplots en el caso de variables cualitativas. En el caso de las variables numéricas, el
#    gráfico debe tener sobreimpresa la recta de regresión simple correspondiente.

# Funcion para crear graficos y guardarlos en un jpeg
plotRegresion <- function(mod, df){
  jpeg(str_c("C:/data/graficos/Grafico_",mod$x,".jpeg"))
  plot(df[[mod$x]], df[[mod$y]])
  if (is.numeric(df[[mod$x]])){
    abline(mod$modelo, col="red")
  }
  dev.off()
}

# Recorremos las columnas con la funcion plotRegresion
modelos %>% walk(~plotRegresion(.,df))

# 7. Separa el conjunto original de datos en tres conjuntos de entrenamiento, test y
#    validaci?n en las proporciones 60%, 20% y 20%.


# Funcion que separa lineas del dataframe en entrenamiento, test y validacion
separarSets <- function(df, p1, p2) {
  rDf    <- 1:nrow(df)
  rTrain <- sample(rDf, p1 * length(rDf))
  rAux    <- setdiff(rDf, rTrain)
  rTest  <- sample(rAux, p2 * length(rDf))
  rVal   <- setdiff(rAux, rTest)
  
  list(train=df[rTrain,], test=df[rTest,], val=df[rVal,])  
}

# 60% Entrenamiento, 20% Test y 20% validacion
dat <- separarSets(df, .6, .2)

# 8. Selecciona cu?l de las 11 variables ser?a la que mejor explica la variable IMC de
#    manera individual, entrenando con el conjunto de entrenamiento y testeando con el
#    conjunto de test.

# explicatorias[which.max(map_dbl(modelos, 'R2'))]

# Ajuste linear que acepta varias columnas
linearAdjust <- function(df, y, x) {
  lm(str_c(y, "~", str_c(x, collapse="+")), df)
}

# Calcula R2
calcR2 <- function(df, mod, y) {
  MSE  <- mean((df[[y]] - predict.lm(mod, df)) ^ 2)
  varY <- mean(df[[y]] ^ 2) - mean(df[[y]]) ^ 2
  R2   <- 1 - MSE / varY
  aR2  <- 1 - (1- R2) * (nrow(df) - 1) / (nrow(df) - mod$rank)
  
  tibble(MSE=MSE, varY=varY, R2=R2, aR2=aR2)
}

# Calcula R2 en test con el modelo de entrenamiento
calcModR2 <- function(dfTrain, dfTest, y, x) {
  mod <- linearAdjust(dfTrain, y, x)
  calcR2(dfTest, mod, y)$aR2
}

# Y ahora calculamos los coeficientes de determinaci?n de los modelos lineales unidimensionales
# Esto nos permitir?a escoger el mejor modelo lineal unidimensional eliminando fluctuaciones estad?sticas
ar2 <- explicatorias %>% map_dbl(calcModR2, dfTrain=dat$train, dfTest=dat$test, y="IMC")

# Vemos cu?l es la mejor variable para una predicci?n unidimensional
bestVar <- explicatorias[which.max(ar2)]
# Este es el mejor modelo y su R2 sobre el conjunto de test
bestMod <- linearAdjust(dat$train, "IMC", "nivIngresos")
calcR2(dat$test, bestMod, "IMC")

# Calculamos su R2 sobre el conjunto de validaci?n
calcR2(dat$val, bestMod, "IMC")

# 9. Selecciona un modelo ?ptimo lineal de regresi?n, entrenando en el conjunto de
#    entrenamiento, testeando en el conjunto de test el coeficiente de determinaci?n
#    ajustado y utilizando una t?cnica progresiva de ir a?adiendo la mejor variable.

encontrarMejorAjuste <- function(dfTrain, dfTest, varPos) {
  bestVars <- character(0)
  aR2      <- 0
  
  repeat {
    aR2v <- map_dbl(varPos, ~calcModR2(dfTrain, dfTest, "IMC", c(bestVars, .)))
    i    <- which.max(aR2v)
    aR2M <- aR2v[i]
    if (aR2M <= aR2) break
    
    cat(sprintf("%1.4f %s\n", aR2M, varPos[i]))
    aR2 <- aR2M
    bestVars <- c(bestVars, varPos[i])
    varPos   <- varPos[-i]
  }
  
  mod <- linearAdjust(dfTrain, "IMC", bestVars)
  
  list(vars=bestVars, mod=mod)
}

# Calculamos el mejor ajuste para variables simples
bestMod <- encontrarMejorAjuste(dat$train, dat$test, explicatorias)
calcR2(dat$val, bestMod$mod, "IMC")

