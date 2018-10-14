library(tidyverse)
library(blockmodeling)
library(tnet)

setwd('~/Dropbox/Dev/burritoooz')

data <- read_csv("data/ingredients.csv") %>%
  select(-c(X1, Count))

n <- 8 #if larger, the number of partitions increases dramaticaly,
# as does if we increase the number of clusters
net <- matrix(NA, ncol = n, nrow = n)
clu <- rep(1:2, times = c(3, 5))
tclu <- table(clu)
net[clu == 1, clu == 1] <- rnorm(n = tclu[1] * tclu[1], mean = 0, sd = 1)
net[clu == 1, clu == 2] <- rnorm(n = tclu[1] * tclu[2], mean = 4, sd = 1)
net[clu == 2, clu == 1] <- rnorm(n = tclu[2] * tclu[1], mean = 0, sd = 1)
net[clu == 2, clu == 2] <- rnorm(n = tclu[2] * tclu[2], mean = 0, sd = 1)

#we select a random parition and then optimise it

all.tpar <- nkpartitions(n=n, k=length(tclu))
#forming the partitions
all.par <- lapply(apply(all.tpar, 1, list), function(x) x[[1]])
# to make a list out of the matrix

#optimizing one partition
res <- optParC(M = net,
               clu = all.par[[sample(1:length(all.par), size = 1)]],
               approaches = "hom", homFun = "ss" , blocks = "com")
plot(res) #Hopefully we get the original partition

#optimizing 10 random partitions with opt.random.par
res <- optRandomParC(M = net, k = 2, rep = 10,
                     approaches = "hom", homFun = "ss", blocks = "com")
plot(res) #Hopefully we get the original partition

#using indidect approach - structural equivalence
D <- sedist(M = net)
plot.mat(net, clu = cutree(hclust(d = D, method = "ward.D"), k = 2))
