# Podaci preuzeti sa profesorkinog sajta i obradjeni u Python-u za prosli cas.
data <- c(0,0,68,95,31,9,0,0,19,40,97,13,7,15,
  11,0,7,0,63,26,56,97,83,74,100,0,22,20,
  96,30,0,10,13,8,0,8,9,7,1,40,5,21,
  64,98,63,7,0,0,75,64,84,15,26,0,0,0,
  11,3,18,64,87,0,15,71,12,77,56,0,13,38,
  68,7,0,15,13,77,4,0,20,0,84,0,2,30,
  13,13,0,0,20,69,30,16,35,23,32,0,14,13,
  51,22,24,7,7,95,76,0,4,22,0,0,23,100,
  8,0,71,15,0,22,9,5,100,19,67,0,7,23,
  96,0,67,0,0,95,21,0,0,54,57,16,75,21,
  25,9,0,40,0,0,25,27,4,20,0,94,0,17,
  0,3,22,0,16,7,13,3,27,28,5,40,7,0,
  13,61,74,35,91,0,3,10,18,25,0,24,31,2,
  40,66,30,0,81,14,98,24,83,10,0,1,18,11,
  84,17,15,14,69,40,35,21,0,2,95,88,37,7,
  9,22,0)

# Konstrukcija histograma.
data <- sort(data)
n <- length(data)
k <- floor(log(n, 2)) + 1
d <- diff(range(data)) / k
podela <- sort(data)[1] + 0:k * d
hist(data, breaks = podela, main = "Exp(0.03616913)",
     xlab="Ukupan broj poena", probability = TRUE)

# Ovo lici na eksponencijalnu raspodelu. Ocena parametra je ista za obe metode.
lambda.hat = 1/mean(data)
curve(dexp(x, rate =lambda.hat), lwd =2, col ='blue', add = T)
print(lambda.hat)

