# simulate data
N = 1000
X = rnorm(N)

a = min(X)
b = max(X)

Xplot = seq(a,b, length.out = 100)
f_real = exp(-0.5 * Xplot^2)/sqrt(2*pi)

h = 1

fest = rep(0, 100)
fest2 = rep(0,100)
Mat = matrix(0, nrow =100, ncol = N)
EPA = matrix(0, nrow =100, ncol = N)

for (i in 1:100){
  for (j in 1:N){
    U = (Xplot[i]- X[j]) / h
    Mat[i,j] = exp(-0.5 * U^2) / sqrt(2*pi)/ h
    TT = abs((Xplot[i]- X[j])/h) <1
    EPA[i,j] = 0.75 * (1-U^2) * TT/h
  }
}

fest = apply(Mat, 1, mean)
fest2 = apply(EPA, 1, mean)

plot(Xplot, f_real)
lines(Xplot, fest, col = "red")
lines(Xplot, fest2, col = "green")
