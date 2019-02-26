library(coRanking)
# DR
set.seed(123)
index2 <- sample.split(class, 4/5)
class.train <- class[index2]
class.test <- class[!index2]
# PCA
pca.class <- function(ncp){
  
  pca <- PCA(x.sampling, ncp = ncp, graph = F)
  x.train <- get_pca_ind(pca)$coord[index2,]
  x.test <- get_pca_ind(pca)$coord[!index2,]
  model.pca <- svm(x.train, class.train)
  pred <- predict(model.pca, x.test)
  pred.table <- table(pred, class.test)
  acc.pca <- sum(diag(pred.table))/length(class.test)
  
}
# ncp = 10
acc.pca.1 <- sapply(c(1:3, seq(4, 20, 2)), pca.class)
acc.pca.2 <- sapply(seq(25, 100, 5), pca.class)
acc.pca <- c(acc.pca.1, acc.pca.2)
plot(c(1:3, seq(4, 20, 2), seq(25, 100, 5)), acc.pca, type = "b", pch = "*", col = "navy",
     xlab = "ndim", ylab = "accuracy", main = "PCA") 
axis(1, at = 10)
abline(h = 0.6, col = "red", lty = 5)
abline(v = 10, col = "black", lty = 2)
# mds
# maxdim = 6
mds.class <- function(maxdim){
  
  mds <- cmdscale(dist(x.sampling), k = maxdim)
  x.train <- as.matrix(mds[index2,])
  x.test<- as.matrix(mds[!index2,])
  model.mds <- svm(x.train, class.train)
  pred <- predict(model.mds, x.test)
  pred.table <- table(pred, class.test)
  acc <- sum(diag(pred.table))/length(class.test)
  
} 
acc.mds.1 <- sapply(c(1:3, seq(4, 10, 2)), mds.class)
acc.mds.2 <- sapply(seq(15, 100, 5), mds.class)
acc.mds <- c(acc.mds.1, acc.mds.2)
plot(c(1:3, seq(4, 10, 2), seq(15, 100, 5)), acc.mds, type = "b", col = "navy",
     xlab = "ndim", ylab = "accuracy", main = "MDS")  
axis(1, at = c(6, 90))
abline(h = 0.7, col = "red", lty = 5)
abline(v = c(6, 90), col = "black", lty = 2)
# isomap
# 不同K
isolate.isomap.8 <- isomap(dist(x.sampling), ndim=2, k=8)
isolate.isomap.7 <- isomap(dist(x.sampling), ndim=2, k=7)
isolate.isomap.6 <- isomap(dist(x.sampling), ndim=2, k=6)
isolate.isomap.5 <- isomap(dist(x.sampling), ndim=2, k=5)
isolate.isomap.4 <- isomap(dist(x.sampling), ndim=2, k=4)
isolate.isomap.3 <- isomap(dist(x.sampling), ndim=2, k=3)
isolate.isomap.2 <- isomap(dist(x.sampling), ndim=2, k=2)
# 由視覺化降維之後的資料，k選6
isolate.isomap <- c(paste(rep("isolate.isomap.", 8), 3:8, sep = ""))
isolate.isomap.vr <- lapply(1:6, function(i){ eval(parse(text = isolate.isomap[i]))})
par(mfrow = c(3,2))
num <- 3:8
isomap.k.plot <- lapply(1:6, function(i){
  plot(isolate.isomap.vr[[i]], 
       col = tim.colors(26)[class], 
       main = paste("k = ", num[i]))
})

isomap.class <- function(ndim){
  
  x.iso <- isomap(dist(x.sampling), ndim = ndim, k = 6)
  x.train <- as.data.frame(x.iso$points[index2, ])
  x.test<- as.data.frame(x.iso$points[!index2, ])
  model.iso <- svm(x.train, class.train)
  pred <- predict(model.iso, x.test)
  pred.table <- table(pred, class.test)
  acc <- sum(diag(pred.table))/length(class.test)
  
}
# ndim = 6
acc.isomap <- sapply(c(1:3, seq(4, 20, 2), seq(25, 70, 5)), isomap.class)
plot(c(1:3, seq(4, 20, 2), seq(25, 70, 5)), acc.isomap, type = "b", col = "navy", pch = "*",
     xlab = "ndim", ylab = "accuracy", main = "Isomap")  
axis(1, 6)
abline(h = 0.6, col = "red", lty = 5)
abline(v = 6, col = "black", lty = 2)

# compare
compare <- function(ncp, maxdim, ndim){
  acc <- c(pca.class(ncp),  mds.class(maxdim),  isomap.class(ndim))
}
acc.compare <- compare(ncp = 10, maxdim = 6, ndim = 6)
barplot(acc.compare, width = 1, names.arg = c("PCA\ndim=10", "MDS\ndim=6", "Isomap\ndim=6"),
        col = "navy", xlim = c(-3, 7), ylim = c(0, 1), ylab = "Accuracy")
axis(2, 0.7)
abline(h = 0.6, col = "red", lty = 5)
text(c(0.7, 1.8, 3), c(0.7, 0.65, 0.65), round(acc.compare, 3))
# coranking and LCMC

pca <- PCA(x.sampling, ncp = 10, graph = F)
pca.dim <- get_pca_ind(pca)$coord
Q.pca <- coranking(x.sampling, pca.dim, input = "data")
lcmc.pca <- LCMC(Q.pca, K = 5:10)

mds <- cmdscale(dist(x.sampling), k = 6)
Q.mds <- coranking(x.sampling, mds, input = "data")
lcmc.mds <- LCMC(Q.mds, K = 5:10)

x.iso <- isomap(dist(x.sampling), ndim = 6, k = 6)
iso.dim <- as.data.frame(x.iso$points)
Q.iso <- coranking(x.sampling, iso.dim, input = "data")
lcmc.iso <- LCMC(Q.iso, K = 5:10)

par(mfrow=c(1, 3))
imageplot(Q.pca)
imageplot(Q.mds)
imageplot(Q.iso)

lcmc <- rbind(lcmc.pca, lcmc.mds, lcmc.iso)
colnames(lcmc) <- c(paste(rep("k = ", 5), 5:10, sep = ""))
lcmc


