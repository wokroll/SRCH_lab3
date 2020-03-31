
n_GARM <- 8
w_GRAN <- 1200
N <- 32

generate_signal <- function(n_GARM = 8, w_GRAN = 1200, N = 32){
  
  #амплітуда і зсув
  A <- runif(n_GARM,0,10)
  PHI <- runif(n_GARM,0,6.28)
  #список зі змінними омега
  w <- seq(w_GRAN/n_GARM,w_GRAN,w_GRAN/n_GARM)
  my_x <- seq(1,n_GARM)
  my_y = as.data.frame(matrix(0, nrow = N, ncol = my_x))
  
  for (i in my_x) for (j in seq(1,N)){
    my_y[j,i] <- A[i]*sin(w[i]*j+PHI[i])
  }
  return(data.frame(my_x,my_y))
}  
xx <- generate_signal()

get_DFT <- function(x){
  data <- xx[,-1]
  Freal <- numeric(N)
  Fim <- numeric(N)
  for (p in 1:N) for (k in 1:N){
    Freal[p] <- sum(data[k,] * cos(2*pi/N*p*k))
    Fim[p] <- sum(data[k,] * sin(2*pi/N*p*k))
  }
  return (data.frame(Freal, Fim))
}

get_tables_w <- function(p, k){
  w_table_real <- matrix(0, p,k)
  w_table_image <- matrix(0, p ,k)
  w_table <- matrix(0, p ,k)
  
  for (i in 1:p) for(j in 1:k){
    w_table_real[i,j] <- cos(2*pi/N*i*j)
    w_table_image[i,j] <- sin(2*pi/N*i*j)
  }
  
  # for (i in 1:p) for(j in 1:k){
  #   w_table[i,j] <- sqrt(w_table_real[i,j]**2 + w_table_image[i,j]**2)
  # }
  return(list(w_table_real, w_table_image))
}

x <- get_DFT(generate_signal())

# real <- get_DFT()[[1]]
# image <- get_DFT()[[2]]

Fp <- apply(x, 1, function(x) sqrt(x[[1]]**2+x[[2]]**2))

w_table <- get_tables_w(N, N)





