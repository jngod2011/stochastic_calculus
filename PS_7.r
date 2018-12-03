s_0 <- 100
paths <- 1000
sigma <- 0.2
mu <- 0.05 
r <- 0 
dt <- 0.01
T <- 1/dt
k <-100

rands <- matrix(rnorm(100000,0,1),paths,T)
S <- matrix(rep(0, T*paths), paths, T)
#W <- matrix(rep(0,T*paths), paths, T)

S[,1] <- s_0

for(i in 2: T){
  S[,i] <- S[,i-1] + r * dt * S[,i-1] + sigma * S[,i-1] * rands[,i] * sqrt(dt)
  #W[,i] <- W[, i-1] + rands[,i] * sqrt(delta)
}

plot(S[1,], ylim = c(20,180),type = "l", ylab="")
for(i in 2:paths){
  lines(S[i,], type = "l")
}

mean(max(S[,100]-100,0))

max_s <- sapply(S[,T] - 100, function(x){
  max(x,0)
})
# pay off using simulation

mean(max_s)

# (a)
# B-S model
#find d1 and d2
T_bs <- 1
d1 <- (log(k/s_0) - (r - sigma^2/2) * T_bs)/ sigma * T_bs
d2 <- d1 - sigma * sqrt(T_bs)

nd1 <- pnorm(d1, mean=0, sd=1) 
nd2 <- pnorm(d2, mean=0, sd=1) 

# pay off using B-S model
call_pay_off <- s_0 * nd1 - k* exp(-r * T_bs) * nd2

# (b)
S_asian <- rowSums(S * dt)

max_s_asian <- sapply(S_asian - 100, function(x){
  max(x,0)
})
# pay off of Asian option
mean(max_s_asian)


