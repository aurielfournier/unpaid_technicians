require(ggplot2)
require(ggtern)
require(MASS) 
require(scales)
require(plyr)

palette <- c( "#FF9933", "#002C54", "#3375B2", "#CCDDEC", "#BFBFBF", "#000000")

inc = 0.1
# Example data
sig <- matrix(c(3,0,0,2),2,2)
data <- data.frame(mvrnorm(n=10000, rep(2, 2), sig))
data$X1 <- data$X1/max(data$X1)
data$X2 <- data$X2/max(data$X2)
data$X1[which(data$X1<0)] <- runif(length(data$X1[which(data$X1<0)]))
data$X2[which(data$X2<0)] <- runif(length(data$X2[which(data$X2<0)]))


# Example data
data$X3 <- with(data, 1-X1-X2)
data <- data[data$X3 >= 0,]

# Auxiliary function for heatmap3d
count_bin <- function(data, minT, maxT, minR, maxR, minL, maxL) {
  ret <- data
  ret <- with(ret, ret[minT <= X1 & X1 < maxT,])
  ret <- with(ret, ret[minL <= X2 & X2 < maxL,])
  ret <- with(ret, ret[minR <= X3 & X3 < maxR,])
  if(is.na(nrow(ret))) {
    ret <- 0
  } else {
    ret <- nrow(ret)
  }
  ret
}


  count <- 1
  points <- data.frame()
  for (z in seq(0,1,inc)) {
    x <- 1- z
    y <- 0
    while (x>0) {
      points <- rbind(points, c(count, x, y, z))
      x <- round(x - inc, digits=2)
      y <- round(y + inc, digits=2)
      count <- count + 1
    }
    points <- rbind(points, c(count, x, y, z))
    count <- count + 1
  }
  colnames(points) = c("IDPoint","T","L","R")
  
  
  polygons <- data.frame()
  c <- 1
  #   Normal triangles
  for (p in points$IDPoint) {
    if (is.element(p, points$IDPoint[points$T==0])) {
      next
    } else {
      pL <- points$L[points$IDPoint==p]
      pT <- points$T[points$IDPoint==p]
      pR <- points$R[points$IDPoint==p]
      polygons <- rbind(polygons, 
                        c(c,p),
                        c(c,points$IDPoint[abs(points$L-pL) < inc/2 & abs(points$R-pR-inc) < inc/2]),
                        c(c,points$IDPoint[abs(points$L-pL-inc) < inc/2 & abs(points$R-pR) < inc/2]))    
      c <- c + 1
    }
  }
  
  # Upside down triangles
  for (p in points$IDPoint) {
    if (!is.element(p, points$IDPoint[points$T==0])) {
      if (!is.element(p, points$IDPoint[points$L==0])) {
        pL <- points$L[points$IDPoint==p]
        pT <- points$T[points$IDPoint==p]
        pR <- points$R[points$IDPoint==p]
        polygons <- rbind(polygons, 
                          c(c,p),
                          c(c,points$IDPoint[abs(points$T-pT) < inc/2 & abs(points$R-pR-inc) < inc/2]),
                          c(c,points$IDPoint[abs(points$L-pL) < inc/2 & abs(points$R-pR-inc) < inc/2])) 
        c <- c + 1
      }
    }
  }
  
  #   IMPORTANT FOR CORRECT ORDERING.
  polygons$PointOrder <- 1:nrow(polygons)
  colnames(polygons) = c("IDLabel","IDPoint","PointOrder")
  
  df.tr <- merge(polygons,points)
  

  
  C = c(1,2,56,11,3,57,12,65,20,4,58,13,66,21,73,28, 5,59,14,67,22,74,29,80,35,6,60,15,68,23,75,30,81,36,86,41,82,31,76)
  CT = c(50,95,46,91,87,42,92,47,96,37)
  D = c(10,19,64,9,8,63,18,72,27,71)
  DC = c(7,61,62,17,16,70)
  TT = c(55,53,100,54,98,51,99)
  TD = c()
  all = c()
  
  dftr <- df.tr %>%
              mutate(fac = "other",
                     fac = ifelse(IDLabel %in% C, "C",fac),
                     fac = ifelse(IDLabel %in% TT,"TT",fac),
                     fac = ifelse(IDLabel %in% D, "D",fac),
                     fac = ifelse(IDLabel %in% CT ,"CT",fac),
                     fac = ifelse(IDLabel %in% DC, "DC",fac))
    
#df.tr$fac <- factor(c(rep("C",40*3),rep("T",8*3),rep("D",10*3),rep("CT",30*3),rep("TD",5*3),rep("DC",5*3),rep("all",2*3)))
  
colnames(dftr) <- c("IDPoint","IDLabel","PointOrder","C","D","TT","fac")
  
Labs = ddply(dftr,"IDLabel",function(x){c(c(mean(x$TT),mean(x$C),mean(x$D)))})
colnames(Labs) = c("Label","TT","C","D")

  
  ggtern(data=dftr,aes(C,TT,D))+
    geom_polygon(aes(group=IDLabel, fill=fac),col="black",alpha=0.25) +
    geom_text(data=Labs,aes(label=Label),size=4,color="black")+
    theme_bw()
  