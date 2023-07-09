library(jpeg)

image1 <- readJPEG("C:/Users/Vadym/Desktop/iceland.jpg")
image2 <- readJPEG("C:/Users/Vadym/Desktop/cyprus.jpg")


dm1 <- dim(image1);dm1[1:2]



dm2 <-  dim(image2);dm2[1:2]

rgbImage1 <- data.frame(
  x=rep(1:dm1[2], each=dm1[1]),
  y=rep(dm1[1]:1, dm1[2]),
  r.value=as.vector(image1[,,1]),
  g.value=as.vector(image1[,,2]),
  b.value=as.vector(image1[,,3]))

rgbImage2 <- data.frame(
  x=rep(1:dm2[2], each=dm2[1]),
  y=rep(dm2[1]:1, dm2[2]),
  r.value=as.vector(image2[,,1]),
  g.value=as.vector(image2[,,2]),
  b.value=as.vector(image2[,,3]))




plot(y ~ x, data=rgbImage1, main="Iceland",
     col = rgb(rgbImage1[c("r.value", "g.value", "b.value")]),
     asp = 1, pch = ".")


plot(y ~ x, data=rgbImage2, main="Cyprus",
     col = rgb(rgbImage2[c("r.value", "g.value", "b.value")]),
     asp = 1, pch = ".")




library(cluster)

n1 <- c()
for (i in 1:10) {
  cl <- clara(rgbImage1[, c("r.value", "g.value", "b.value")], i)
  n1[i] <- cl$silinfo$avg.width
}

plot(n1, type = 'l',
     main = "Optimal number of clusters for Iceland",
     xlab = "Number of clusters",
     ylab = "Average silhouette",
     col = "blue")



n2 <- c()
for (i in 1:10) {
  cl <- clara(rgbImage2[, c("r.value", "g.value", "b.value")], i)
  n2[i] <- cl$silinfo$avg.width
}

plot(n2, type = 'l',
     main = "Optimal number of clusters for Cyprus",
     xlab = "Number of cluster",
     ylab = "Average silhouette",
     col = "blue")





iceland = rgbImage1[, c("r.value", "g.value", "b.value")]
clara <- clara(iceland, 3)
plot(silhouette(clara))



colours <- rgb(clara$medoids[clara$clustering, ])
plot(y ~ x, data=rgbImage1, main="Iceland",
     col = colours, 
     asp = 1, pch = ".")


cyprus = rgbImage2[, c("r.value", "g.value", "b.value")]
clara2 <- clara(cyprus, 4)
plot(silhouette(clara2))


colours2 <- rgb(clara2$medoids[clara2$clustering, ])
plot(y ~ x, data=rgbImage2, main="Cyprus",
     col = colours2, 
     asp = 1, pch = ".")



dominantColours <- as.data.frame(table(colours))

max_col  <- max(dominantColours$Freq)/sum(dominantColours$Freq)
min_col  <- min(dominantColours$Freq)/sum(dominantColours$Freq)
medium_col <- 1-max_col - min_col

dominantColours$distribution <- round((c(min_col, medium_col, max_col) * 100), 2)
dominantColours


dominantColours$colours <- as.character(dominantColours$colours)
pie(dominantColours$Freq, labels = dominantColours$distribution,
    col = dominantColours$colours,
    xlab = "Colours",
    ylab = "Frequency")



dominantColours2 <- as.data.frame(table(colours2))

max_col2  <- (dominantColours2$Freq[1])/sum(dominantColours2$Freq)
min_col2  <- (dominantColours2$Freq[3])/sum(dominantColours2$Freq)
medium_col2 <- (dominantColours2$Freq[2])/sum(dominantColours2$Freq)
bg <- (dominantColours2$Freq[4])/sum(dominantColours2$Freq)
dominantColours2$distribution <- round((c(max_col2, medium_col2, min_col2, bg) * 100), 2)

dominantColours2




dominantColours2$colours2 <- as.character(dominantColours2$colours2)
pie(dominantColours2$Freq, labels = dominantColours2$distribution,
    col = dominantColours2$colours2,
    xlab = "Colours",
    ylab = "Frequency")
