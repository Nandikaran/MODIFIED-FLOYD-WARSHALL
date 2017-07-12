# Hierarchial Clustering 

# Task-1 : Verification Example Matrix 
# mydata <- matrix(c(0,1,7,8,2,0,1,7,9,3),nrow=5,ncol=2)

# Operating on iris data-set
 mydata <- iris  # Loading iris dataframe into mydata
 mydata$Species <- NULL # Remove the column representing classes
 mydata <- as.matrix(mydata)

n <- ncol(mydata)  # Dimension for clustering
m <- nrow(mydata)  # Number of points for clustering

# Generating the dissimilairty distance matrix from the given points
dissimilarity <- matrix(nrow=nrow(mydata),ncol=nrow(mydata))
rownames(dissimilarity) <- colnames(dissimilarity) <- LETTERS[1:nrow(mydata)]


# Generating the 2nd dissimilairty distance matrix from the given points
dissimilarity2 <- matrix(nrow=nrow(mydata),ncol=nrow(mydata))
rownames(dissimilarity2) <- colnames(dissimilarity2) <- LETTERS[1:nrow(mydata)]


# Generating the 3rd dissimilairty distance matrix from the given points
dissimilarity3 <- matrix(nrow=nrow(mydata),ncol=nrow(mydata))
rownames(dissimilarity3) <- colnames(dissimilarity3) <- LETTERS[1:nrow(mydata)]



for (i in 1:m)
{
  for (j in 1:m)
  {
    # Hamming Distance
     dissimilarity3[i,j] <- sum(abs(mydata[i,] - mydata[j,]))

    # Euclidean Distance
     dissimilarity[i,j] <- sqrt(sum((mydata[i,]-mydata[j,])^2))
	
	# standardized Euclidean Distance with a digaonal variance matrix 
     dissimilarity2[i,j] <- 0
      for (z in 1:n)
       {
       dissimilarity2[i,j] <- dissimilarity2[i,j] + ((mydata[i,z] - mydata[j,z]))^2/sd(mydata[,z])
      }
      dissimilarity2[i,j] <- sqrt(dissimilarity2[i,j])
}
}

B=dissimilarity
D=dissimilarity2
E=dissimilarity3
mincon=.Machine$integer.max # Maximum value of an integer
#C = matrix(c(1, 3, 4, 1, 3, 5),nrow = 2,ncol =3,byrow = TRUE) #for Sample dataset
# for iris dataset ->
C = matrix(c(1, 13, 64, 14, 18, 54, 56, 58, 112, 15, 17, 71, 23, 25, 142, 16, 21, 122, 56, 89, 133, 33, 34, 66, 90, 91, 125, 2, 50, 149),nrow = 10,ncol =3,byrow = TRUE)
h<-nrow(C)
for(i in 1:m){
   for(j in 1:m){
	for(k in 1:m){
	  for(l in 1:h){
	     if(C[l,1]==i & C[l,2]==j){
		 mincon = min(mincon,dissimilarity[i,C[l,3]])
		}
	  }
        B[i,j] = min(B[i,j],(dissimilarity[i,k]+dissimilarity[k,j]),mincon)
	  D[i,j] = min(D[i,j],(dissimilarity2[i,k]+dissimilarity2[k,j]),mincon)
 	  E[i,j] = min(E[i,j],(dissimilarity3[i,k]+dissimilarity3[k,j]),mincon)

	}
   }
}
for(i in 1:m){
	for(j in 1:m){
			B[i,j]=min(B[i,j],D[i,j],E[i,j])
	}
}
dissimilarity = B
mydata.complete.link <- hclust(as.dist(dissimilarity), method='complete')
plot(mydata.complete.link, mydata.complete.link$V1, ylab="Distance")
#h2c<- cutree(mydata.complete.link,k=6)
#plot(h2c)



