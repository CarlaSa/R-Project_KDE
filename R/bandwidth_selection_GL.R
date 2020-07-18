#TODO: vectorize the code

####
# Simulating the model
####
n <- 1000

# Parameters for gamma or beta distributions:
alpha <-  3 # In case we want to use gamma
beta <- 3  #  or beta distributions

# Generating data from the chosen distribution (normal, mu=0, sigma=1):
Z <-  rnorm(n) # Can use normal, gamma or beta


# Intervals on which to plot the data etc.:
K <- 50
x_points <- seq(min(Z),max(Z), length.out = K)


# The true density function (for the normal distribution):
f_real <- exp(-0.5 * x_points^2)/sqrt(2*pi)

# The number of estimates:
M <- 40

f_estimate <- matrix(0, nrow = M, ncol = K)
f_hat_prime <- array(0, c(M, M, K)) # an intermediate term
norm <- matrix(0, nrow=M, ncol=M)
# TODO: What does A do?
A <- rep(0, M)#matrix(0, nrow=M, ncol=1)

#####
# Computation of V(h) for h= 1/M,...,M/M
#####
kappa <- 1.2
V <- kappa * (M/(1:M))/2/sqrt(pi)/n

for(k in 1:M){
  bandwidth <- k/M
  # Update 18/7: t(x_points) needed to make dimensions match
  # (Matlab's linspace and R's seq output objects of different dimensions)
  MKdens <- dnorm((kronecker(matrix(1,1,K), Z) - kronecker(matrix(1, n, 1), t(x_points)))/bandwidth)
  f_estimate[k,] <- apply(MKdens,2,mean)/bandwidth
  for (j in 1:M){
    hprime <- j/M
    #####
    # Convolution of gaussian kernels
    #####
    another <- dnorm(kronecker(matrix(1,1,K), Z) - kronecker(matrix(1, n, 1), t(x_points)), 0, sqrt(bandwidth^2 + hprime^2))
    #####
    # Computation of the estimator f_{h,h'}
    #####
    f_hat_prime[k,j,] <- apply(another,2,mean) #mean(another,1)
  }
  }
# Carla and Leon have implemented an lnorm function for this part:
for (k in 1:M){
  for (l in 1:M){
    for (j in 1:K){
      norm[k,l] <- norm[k,l] + (f_hat_prime[k,l,j] - f_estimate[l,j])^2 * (b-a)/K
    }
  }
  }

for (k in 1:M){
  A[[k]] <- max(max(norm[k,]-2*V, 0))
  }

########
# Bandwidth selection
########

# We don't need to transpose V because it's already in the right dimensions.
# Why choose this minimum value for our index? It doesn't seem to give an optimum estimate.
kc <- which.min(A+V) # this index gives k where bandwidth h = k/M
final_estimate <- f_estimate[kc,]

# Graphical representation

lines(x_points, f_real, col="blue")
lines(x_points, f_estimate[kc,], col = "red")
