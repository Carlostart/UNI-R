a <- 1103515245  # Valor a de la ecuaci???n de congruencias.
b <- 12345 # Valor b/c de la ecuaci???n de congruencias.
m <- 2^31 # Valor m de la ecuaci???n de congruencias.
x <- 341 # Valor inicial de la serie de x.

N <- 25
n <- 10
p <- 0.3
reps <- N*n; # Este es el n???mero de n???meros que vamos a generar en U(0,1). 

cat("Normal B(",n,",",p,")\n")

cat(N, " números aleatorios\n")

um <- replicate(reps, 0)
xm <- replicate(reps, 0)

# Generador de numeros aleatorios entre 0 y 1
for (i in 1:reps){
  x <- (a*x+b) %% m
  xm[i] <- x
  u <- x/m
  um[i] <- u

}

# Numeros aleatorios generados siguiendo la Binomial(n,p)
X <- replicate(N,0)
for (i in 1:reps/n){
  cont <- 0
  for (j in 1:n){
    if(um[i+j] <= p){
      cont <- cont + 1
    }
  }
  X[i] <- cont
}

X

hist(x = X, main = "Histograma de frecuencias", ylab = "Frecuencias", xlab = "Valores")



probInterval <- function(a,b){
  cont <- 0
  for(i in X){
    if (i >= a && i <= b){
      cont <- cont + 1
    }
  }
  r <- cont / length(X)
  cat("Probabilidad en muestra -> ", r, "\n")
  return (r)
}

errorInterval <- function(a,b){
  pb <- pbinom(b,n,p)-pbinom(a,n,p)
  cat("Probabilidad real -> ", pb, "\n")
  return (probInterval(a,b)-pb)
}

errorInterval(1,6) # Ejemplo de intervalo de probabilidad P(1 <= X <= 5)

media <- sum(X)/N
cat("Media X: ",media,"\n")

varianza <- var(X)

cat("Varianza X: ",varianza,"\n")

# Aplicamos la formula
Y <- replicate(N,0)
for (i in 1:N){
  Y[i] = (X[i] - media) / sd(X)
}

cat("Media Y: ",mean(Y),"  (Prácticamente 0)\n")
cat("Varianza Y: ",var(Y),"\n")


hist(Y,freq = F ,xlim = c(-3,3))
curve(dnorm(x, mean(Y), sd(Y)), lwd=2, add = TRUE)

