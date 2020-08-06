
#Step 1 - Initializing params
requireNamespace("stats")
x = cbind(iris[,1], iris[,2], iris[,3], iris[,4])

no_p = nrow(iris)
no_i = 5
no_d = ncol(iris) - 1
w = 0.7
c1 = 2
c2 = 2
no_clusters = length(unique(iris[,5]))

#Step 2 - Creating a swarm
n = no_p
c = no_clusters
X = matrix(0, nrow = n, ncol = c)
V = matrix(0, nrow = n, ncol = c)
pBest = matrix(0, nrow = n, ncol = c)
gBest = c(1:c)*0

#Step 3 - Initialize random X and V
for(i in 1:n) {
  X[i,] = runif(no_clusters)
  t_sum = 0
  for(k in 1:c) 
    t_sum = t_sum + X[i,k]
  for(j in 1:c)
    X[i,j] = X[i,j]/t_sum
}

for(i in 1:n) 
  V[i,] = runif(1,min = -1,max = 1)

centroids = matrix(0, nrow = c, ncol = no_d)
pBest_fitness = c(1:n)*0

sx<-split(iris[,1:4], sample(1:5, nrow(iris), replace=T))
y<-list()
y[[1]]<-sx$'1'
y[[2]]<-sx$'2'
y[[3]]<-sx$'3'
y[[4]]<-sx$'4'
y[[5]]<-sx$'5'
sx<-list()

for(big_iter in 1:5) {
  
  sx <- rbind(sx,y[[big_iter]])
  x <- matrix(unlist(sx), ncol = 4)
  n = no_p = length(x)/4
  
  for(iter in 1:no_i) {
    
    #Step 4.1 - Calculate cluster centers
    for(j in 1:c) {
      sum1 = c(1:no_d)*0
      sum2 = 0
      for(i in 1:n) {
        sum1 = sum1 + X[i,j]*X[i,j]*x[i,]
        sum2 = sum2 + X[i,j]*X[i,j]
      }
      centroids[j,] = sum1/sum2
    }
    
    #Step 4.2 - Calculate the fitness value
    d = matrix(0, nrow = n, ncol = c)
    dist<-c()
    k = 0
    for(i in 1:n) {
      dist = c()
      for(j in 1:c) {
        dist <- rbind(dist, distance(rbind(x[i,], centroids[j,]), method = "euclidean"))
        colnames(dist)<-c()
        d[i,j] = dist[j,]
      }
    }
    
    f = c(1:n)*0
    k = 1
    J = c(1:n)*0
    for(j in 1:c)
      for(i in 1:n)
        J[i] = J[i] + X[i,j]*X[i,j]*d[i,j]
    
    for(i in 1:n) {
      temp = k/J[i]
      f[i] = temp
    }
    
    #Step 4.3 - Calculate pBest
    for(i in 1:n) {
      if(f[i] > pBest_fitness[i]) {
        pBest_fitness[i] = f[i]
        pBest[i,] = X[i,]
      }
    }
    
    ind = which.max(pBest_fitness)
    gBest = pBest[ind,]
    
    #Step 4.4 - Calculate V and X
    for(t in 1:n) {
      r1 = runif(1)
      r2 = runif(1)
      for(j in 1:c) {
        V[t,j] = w*V[t,j] + c1*r1*(pBest[t,j]-X[t,j]) + c2*r2*(gBest[j]-X[t,j])
        X[t,j] = X[t,j] + V[t,j]
      }
    }
    
    #Step 4.5 - Normalizing
    for(i in 1:n) {
      for(j in 1:c) {
        if(X[i,j] < 0)
          X[i,j] = 0
      }
    }
    
    Y = c(1:c)*0
    for(i in 1:n) {
      if(identical(Y,X[i,]))
        X[i,] = runif(no_clusters)
    }
    
    for(i in 1:n) {
      t_sum = 0
      for(k in 1:c) 
        t_sum = t_sum + X[i,k]
      for(j in 1:c)
        X[i,j] = X[i,j]/t_sum
    }
    
    mem = c(1:n)*0
    for(i in 1:n)
      mem[i] = which.max(X[i,])
    
    #print(mem)
    
    co1 = 0
    co2 = 0
    co3 = 0
    for(i in 1:length(mem)){
      if(mem[i] == 1)
        co1 = co1 + 1
      if(mem[i] == 2)
        co2 = co2 + 1
      if(mem[i] == 3)
        co3 = co3 + 1
    }
    
    #clusplot(x, mem, color=TRUE, shade=TRUE, labels = 0, lines=0)
    
  }
  
  for(iter in 1:no_i) {
    
    #Step 5.1 - Calculate cluster centers
    for(j in 1:c) {
      sum1 = c(1:no_d)*0
      sum2 = 0
      for(i in 1:n) {
        sum1 = sum1 + X[i,j]*X[i,j]*x[i,]
        sum2 = sum2 + X[i,j]*X[i,j]
      }
      centroids[j,] = sum1/sum2
    }
    
    #Step 5.2 - Compute distance
    d = matrix(0, nrow = n, ncol = c)
    dist<-c()
    k = 0
    for(i in 1:n) {
      dist = c()
      for(j in 1:c) {
        dist <- rbind(dist, distance(rbind(x[i,], centroids[j,]), method = "euclidean"))
        colnames(dist)<-c()
        d[i,j] = dist[j,]
      }
    }
    
    #Step 5.3 - Update membership function
    for(i in 1:n) {
      for(j in 1:c) {
        t_sum = 0
        for(k in 1:c) {
          t_sum = t_sum + (d[i,j]/d[i,k])*(d[i,j]/d[i,k])
        }
        X[i,j] = 1/t_sum
      }
    }
    
    #Step 5.4 - Calc pBest
    f = c(1:n)*0
    k = 1
    J = c(1:n)*0
    for(j in 1:c)
      for(i in 1:n)
        J[i] = J[i] + X[i,j]*X[i,j]*d[i,j]
    
    for(i in 1:n) {
      temp = k/J[i]
      f[i] = temp
    }
    
    for(i in 1:n) {
      if(f[i] > pBest_fitness[i]) {
        pBest_fitness[i] = f[i]
        pBest[i,] = X[i,]
      }
    }
    
    #Step 5.5 - Calculate gBest
    ind = which.max(pBest_fitness)
    gBest = pBest[ind,]
    
    mem = c(1:n)*0
    for(i in 1:n)
      mem[i] = which.max(X[i,])
    
    #print(mem)
    
    co1 = 0
    co2 = 0
    co3 = 0
    for(i in 1:length(mem)){
      if(mem[i] == 1)
        co1 = co1 + 1
      if(mem[i] == 2)
        co2 = co2 + 1
      if(mem[i] == 3)
        co3 = co3 + 1
    }
    
    clusplot(x, mem, color=TRUE, shade=TRUE, labels = 0, lines=0)
  }
}
