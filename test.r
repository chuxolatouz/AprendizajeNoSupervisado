###############################################
#                                             #
#               Libreria                      #
#                                             #
###############################################
install.packages("plot3D")
library(plot3D)
install.packages("caret")
library("caret")
###############################################
#                                             #
#               Jambu                         #
#                                             #
###############################################

jambu <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Numero de Clusters",
       ylab="Dentro de los grupos")}

###############################################
#                                             #
#               Data set A                    #
#                                             #
###############################################

a <- read.csv("a.csv", header = FALSE)
a <- as.data.frame(a)
plot(x = a$V1, y = a$V2)
jambu(a)
a.plot <- subset(a, select = -c(V3))

a.kmeans <- kmeans(a.plot, centers = 3)
plot(a.plot, col = a.kmeans$cluster)

MCk <- table(true = a$V3, pred = a.kmeans$cluster)

a.single <- hclust(dist(as.matrix(a.plot)), method = "single")
cortes.single <- cutree(a.single, k = 3)

MCs <- table(true = a$V3, pred = cortes.single)

a.complete <- hclust(dist(as.matrix(a.plot)), method = "complete")
cortes.complete <- cutree(a.complete, k = 3)

MCc <- table(true = a$V3, pred = cortes.complete)

a.average <- hclust(dist(as.matrix(a.plot)), method = "average")
cortes.average <- cutree(a.average, k = 3)

MCa <- table(true = a$V3, pred = cortes.average)

###############################################
#                                             #
#               Data set A_big                #
#                                             #
###############################################

a_big <- read.csv("a_big.csv", header = FALSE)
a_big <- as.data.frame(a_big)

samp_size <- floor(0.05 * nrow(a_big))
train_ind <- sample(seq_len(nrow(a_big)), size = samp_size)
a_subseted<- a_big[train_ind,]

jambu(a_subseted)

# kmeans con centers = 3 
a_subseted.kmeans <- kmeans(a_subseted, centers = 3)
#agarras los centroides y pasarlos a a_big
a_big.kmeans <- kmeans(a_big, centers = a_subseted.kmeans$centers )

#hacer tabla de chiruli para comparar

MCk <- table(true = a_big$V3, pred = a_big.kmeans$cluster)

a_big.single <- hclust(dist(as.matrix(a_subseted)), method = "single")
cortes.a_big.single <- cutree(a_big.single, k = 3)

MCs <- table(true = a_subseted$V3, pred = cortes.a_big.single)

a_big.complete <- hclust(dist(as.matrix(a_subseted)), method = "complete")
cortes.a_big.complete <- cutree(a.complete, k = 3)

MCc <- table(true = a_subseted$V3, pred = cortes.a_big.complete)

a_big.average <- hclust(dist(as.matrix(a_subseted)), method = "average")
cortes.a_big.average <- cutree(a.average, k = 3)

MCa <- table(true = a_subseted$V3, pred = cortes.a_big.average)



###############################################
#                                             #
#               Data set guess                #
#                                             #
###############################################

guess <- read.csv("guess.csv", header = FALSE)
guess <- as.data.frame(guess)
plot(guess)
jambu(guess)

guess.kmeans <- kmeans(guess, centers = 4)
plot(guess, col = guess.kmeans$cluster)

guess.single <- hclust(dist(as.matrix(guess)), method = "single")
cortes.single <- cutree(guess.single, k = 4)

guess.complete <- hclust(dist(as.matrix(guess)), method = "complete")
cortes.complete <- cutree(guess.complete, k = 4)

guess.average <- hclust(dist(as.matrix(guess)), method = "average")
cortes.complete <- cutree(guess.complete, k = 4)


###############################################
#                                             #
#               Data set H                    #
#                                             #
###############################################


h <- read.csv("h.csv", header = FALSE)
h <- as.data.frame(h)
hclass <- as.data.frame(h$V4)
jambu(h)

summary(hclass)

#Hacemos regla para las clases
hclass[ hclass < 7.046] = 1
hclass[ hclass < 9.386 & hclass >= 7.046 ] = 2
hclass[ hclass < 11.661 & hclass >= 9.386 ] = 3
hclass[ hclass >= 11.661] = 4

points3D(x = h$V1, y = h$V2, z = h$V3)

h.kmeans <- kmeans(h, centers = 4)

MCk <- table(true = h$V4, predict = h.kmeans$cluster)

h.single <- hclust(dist(as.matrix(h)), method = "single")
corte.h.single <- cutree(h.single, k = 4)

MCs <- table( true = h$V4, predict = corte.h.single)

h.complete <- hclust(dist(as.matrix(h)), method = "complete")
corte.h.complete <- cutree(h.complete, k = 4)

MCs <- table( true = h$V4, predict = corte.h.complete)

h.average <- hclust(dist(as.matrix(h)), method = "average")
corte.h.average <- cutree(h.average, k = 4)

MCs <- table( true = h$V4, predict = corte.h.average)


###############################################
#                                             #
#               Data set Moon                 #
#                                             #
###############################################

moon <- read.csv("moon.csv", header = FALSE)
moon <- as.data.frame(moon)
moon$V3 = moon$V3 + 1

points3D(moon$V1, moon$V3, moon$V2)

jambu(moon)

moon.kmeans <- kmeans(moon, centers = 4)

MCk <- table(true = moon$V3, pred = moon.kmeans$cluster)

moon.single <- hclust(dist(as.matrix(moon)), method = "single")
corte.moon.single <- cutree(moon.single, k = 4)

MCs <- table(true = moon$V3, pred = corte.moon.single)

moon.complete <- hclust(dist(as.matrix(moon)), method = "complete")
corte.moon.complete <- cutree(moon.complete, k = 4)

MCc <- table(true = moon$V3, pred = corte.moon.complete)

moon.average <- hclust(dist(as.matrix(moon)), method = "average")
corte.moon.average <- cutree(moon.average, k = 4)

MCa <- table( true = moon$V3, pred = corte.moon.average)


###############################################
#                                             #
#               Data set Good Luck            #
#                                             #
###############################################

gl <- read.csv("good_luck.csv", header = FALSE)
gl <- as.data.frame(gl)

jambu(gl)

###############################################
#                                             #
#               Data set Help                 #
#                                             #
###############################################

help <- read.csv("help.csv", header = FALSE)
help <- as.data.frame(help)
points3D(help$V1, help$V2, help$V3)

