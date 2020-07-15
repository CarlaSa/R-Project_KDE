kernels <- c(
             gaussian = function(u) {
                 1 / (sqrt(2*pi)) * exp(-u^2 / 2)
             },
             rectangular = function(u) {
                 1/2 * (abs(u) <= 1)
             },
             triangular = function(u) {
                 (1 - abs(u)) * (abs(u) <= 1)
             },
             epanechnikov = function(u) {
                 3/4 * (1 - u^2) * (abs(u) <= 1)
             },
             biweight = function(u) {
                 15/16 * (1 - u^2)^2 * (abs(u) <= 1)
             },
             silverman = function(u) {
                 1/2 * exp(-abs(u)/sqrt(2)) *
                     sin(abs(u)/sqrt(2) + pi/4)
             }
)
kernels$parabolic <- kernels$epanechnikov

