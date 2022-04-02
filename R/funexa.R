# FUNCIONES - PRACTICA

# Función: cuadrados de una sequencia de números
n.f <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
 
n.f(13) 

# Función: crear sin argumentos.
n.f1 <- function() {
  for(i in 1:5) {
    print(i^2)
  }
}

n.f1()

# Función: crear con argumentos.
n.f2 <- function(a,b,c) {
  result <- a*b+c
  print(result)
}

n.f2(2,4,3)

# Función: crear con argumentos - cont.
n.f3 <- function(a = 3,b =6) {
  result <- a*b
  print(result)
}

n.f3()
n.f3(4, 5)

# Función: ROC creada para medir "sensibilidad" y "especifidad"
# de una variable continua 

# Env para ROC
x<-rnorm(100,mean=0)
y<-rnorm(100, mean=1)
isx<-rep(c(TRUE,FALSE),each=100)

# Función ROC
ROC <- function(test, disease){
  cutpoints <- c(-Inf, sort(unique(test)), Inf)
  sensitivity<-sapply(cutpoints,
                      function(result) mean(test>result & disease)/mean(disease))
  specificity<-sapply(cutpoints,
                      function(result) mean(test<=result & !disease)/mean(!disease))
  plot(sensitivity, 1-specificity, type="l")
  abline(0,1,lty=2)
  return(list(sens=sensitivity, spec=specificity))
}

ROC(c(x, y), isx)


# OTRO EJEMPLO
df <- data.frame("pri" = 1:3, "seg" = 4:6)

#f <- function(df, ocol, ncol) {
#  dfn <- df
#  dfn[, ncol] <- dfn[, ocol] * 3
#  return(dfn)
#}
#ocol <- "seg"
#ncol <- "ter"
#f(df, ocol, ncol)

#f.tidy <- function(df, oldc, newc) {
#  df %>%
#    mutate_at(vars(oldc),  funs(new = .* 2)) %>%
#    rename_at(vars(matches("new")), ~ newc)
#  }
#oldc <- "seg"
#newc <- "trr"
#f.tidy(df, oldc, newc)

f.tidy <- function(df, ocl, ncl) {
  df %>%
    mutate(!! (ncl) := !!rlang::sym(ocl) - 1)
}
ocl <- "pri"
ncl <- "cua"
f.tidy(df, ocl, ncl)
  