# Podaci preuzeti sa profesorkinog sajta i obradjeni u Python-u za prosli cas.
# Za ovaj cas, pronadjen pdf sa bodovanjem do 60 poena, isparsiran, i 
# sam niz iskopiran ovde.
data <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
  ,0,0,0,0,0,0,0,0,0,0,0,1,1,2,2,2,2,3,3,3,3,4,4,4
  ,5,5,5,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,9,9,9,9,9,9
  ,9,9,10,10,10,11,11,11,11,12,12,13,13,13,13,13,13,13,13,13,13,13,13,14
  ,14,14,14,14,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,17,17,18,18,18
  ,19,19,19,19,20,20,20,20,20,20,20,21,21,21,21,21,21,22,22,22,22,22,22,22
  ,23,23,23,23,24,24,24,24,24,24,24,24,24,25,25,25,25,25,26,26,26,26,27,27
  ,27,27,28,28,28,29,30,30,30,30,30,30,30,31,31,31,32,32,32,33,33,33,34,34
  ,34,34,34,35,35,35,35,35,35,36,37,38,39,39,39,39,39,40,40,40,40,41,42,42
  ,42,43,45,47,48,48,49,54,55,55,55,56,57,60)

# Konstrukcija histograma
n <- length(data)
k <- floor(log(n, 2)) + 1
d <- diff(range(data)) / k
podela <- sort(data)[1] + 0:k * d
hist(data, breaks = podela, main = "Exp(0.05893271)", probability = TRUE, xlab="Broj predispitnih poena")

# Ovo lici na eksponencijalnu raspodelu. Ocena parametra je za eksponencijalnu raspodelu
# ista, bez obzira na metodu koju koristimo.
lambda.hat = 1/mean(data)
curve(dexp(x, rate =lambda.hat), lwd =2, col ='coral1', add = T)
print(lambda.hat)

