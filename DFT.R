library(ggplot2)

n_GARM <- 8
w_GRAN <- 1200
N <- 1024

generate_signal <- function(n_GARM = 8, w_GRAN = 1200, N){
  
  #амплітуда і зсув
  A <- runif(n_GARM,0,1)
  PHI <- runif(n_GARM,0,6.28)
  #список зі змінними омега
  w <- seq(w_GRAN/n_GARM,w_GRAN,w_GRAN/n_GARM)
  my_x <- seq(1,n_GARM)
  my_y = as.data.frame(matrix(0, nrow = N, ncol = my_x))
  
  for (i in my_x) for (j in seq(1,N)){
    my_y[j,i] <- A[i]*sin(w[i]*j+PHI[i])
  }
  return(rowSums(my_y))
}  


get_DFT <- function(x){
  data <- x
  Freal <- numeric(N)
  Fim <- numeric(N)
  for (p in 1:N) for (k in 1:N){
    Freal[p] <- Freal[p] + data[k] * cos(2*pi/N*p*k)
    Fim[p] <-  Fim[p] + data[k] * sin(2*pi/N*p*k)
  }
  return (data.frame(Freal, Fim))
}

get_tables_w <- function(p, k){
  w_table_real <- matrix(0, p, k)
  w_table_image <- matrix(0, p ,k)
  w_table <- matrix(0, p ,k)
  
  for (i in 1:p) for(j in 1:k){
    w_table_real[i,j] <- cos(2*pi/N*i*j)
    w_table_image[i,j] <- sin(2*pi/N*i*j)
  }
  
  return(list(w_table_real, w_table_image))
}

sig <- generate_signal(N = N)

x <- get_DFT(generate_signal(N = N))

Fp <- apply(x, 1, function(x) sqrt(x[[1]]**2+x[[2]]**2))

w_table <- get_tables_w(N, N)

Fp2_real <- numeric(N)
Fp2_image <- numeric(N)

for (i in 1:nrow(w_table[[1]])) for (j in  1:ncol(w_table[[1]])){
  Fp2_real[i] <- Fp2_real[i] + sig[j] * w_table[[1]][i,j]
  Fp2_image[i] <- Fp2_image[i] + sig[j] * w_table[[2]][i,j]
}
data <- data.frame(Fp2_real, Fp2_image)

Fp2 <- apply(data, 1, function(x) sqrt(x[[1]]**2+x[[2]]**2))

plot_data <- data.frame(seq(1,N), Fp)
ggplot(plot_data, aes(plot_data[[1]], plot_data[[2]]))+
  geom_line()

plot_data2 <- data.frame(seq(1,N), Fp2)
ggplot(plot_data2, aes(plot_data2[[1]], plot_data2[[2]]))+
  geom_line()
