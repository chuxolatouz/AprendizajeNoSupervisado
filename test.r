install.packages("plot3D")
library(plot3D)

a <- read.csv("a.csv", header = FALSE)
a <- as.data.frame(a)
plot(x = a$V1, y = a$V2)

a_big <- read.csv("a_big.csv", header = FALSE)
a_big <- as.data.frame(a_big)
plot(x = a_big$V1, y = a_big$V2)

jambu <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

guess <- read.csv("guess.csv", header = FALSE)
guess <- as.data.frame(guess)
#Como se observa en la grafica la mejor opcion es talfi
plot(guess)
jambu(guess)
guess.kmeans <- kmeans(guess, centers = 4)
plot(guess, col = guess.kmeans$cluster)


h <- read.csv("h.csv", header = FALSE)
h <- as.data.frame(h)
plot(h)

points3D(x = h$V1, y = h$V2, z = h$V3)

moon <- read.csv("moon.csv", header = FALSE)
moon <- as.data.frame(moon)
scatter3D(moon$V1, moon$V3, moon$V2)

gl <- read.csv("good_luck.csv", header = FALSE)
gl <- as.data.frame(gl)
plot(gl)
