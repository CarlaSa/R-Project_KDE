devtools::install("KDE_package")


# comparison of different kernels

plot(x = NA, xlim = c(-2,2), ylim = c(-0.2, 1.3) , xlab = "x", ylab = "Kernel(x)")
dots = seq(-3, 3, length.out = 1000)
for (ker in KDE::kernels){
  lines(dots, ker(dots), col = clr)
  #lines(ker)
}
grid(col="lightgray")

data = rnorm(100)

