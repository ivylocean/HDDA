library(caTools)
library(e1071)
library(FactoMineR)
library(factoextra)
library(fields)
library(vegan)
library(tabplot)
# raw data
isolet5 <- read.table("data/isolet5", sep = ",")
dim(isolet5)
isolet1234 <- read.table("data/isolet1+2+3+4", sep = ",")
dim(isolet1234)
x <- rbind(isolet1234[,-618], isolet5[,-618])
y <- c(as.factor(isolet1234[,618]), as.factor(isolet5[,618]) )
dim(x)
length(y)
table(y)
# sampling
set.seed(12345)
index <- sample.split(y, 1/30)
x.sampling <- x[index,]
class <- as.factor(y[index])
dim(x.sampling)
length(class)
table(class)
# exploration
# tab plot
# choose top k variablles
bw.ratio <- function(x, y){
  tg <- table(y)
  gm <- tapply(x, y, mean)
  repm <- rep(gm, tg)
  wss <- sum((x - repm)^2)
  bss <- sum((gm-mean(x))^2)
  bw <- bss/wss
}
bw.values <- apply(x.sampling, 2, bw.ratio, class)
summary(sort(bw.values))
top <- sum(bw.values > 0.03) # 341
selected.var <- order(bw.values, decreasing = TRUE)[1:top]
x.bw <- x.sampling[, selected.var]
data.bw <- cbind(x.bw, class)
# tableplot
tableplot(data.bw, nBins = 260, sortCol = 342)
top2 <- 10 
selected.var <- order(bw.values, decreasing = TRUE)[1:top2]
x.bw <- x.sampling[, selected.var]
data.bw <- cbind(x.bw, class)
tableplot(data.bw, nBins = 260, sortCol = 11, decreasing = F,
                    pals = list(tim.colors(26)))
# heatmap
br <- two.colors(start = "blue", end = "red", middle = "white", alpha = 1)
x.mat <- as.matrix(x.sampling)
# scale01 <- function(x){
#   x.01 <- (x - min(x))/(max(x) - min(x))
# }
# x.mat.c <- apply(x.mat, 2, scale01) # Range Column Condition
# data.mat <- cbind(x.mat.c, class)
# data <- data.mat[order(data.mat[, 618]),]
# rc <- tim.colors(26)[data[, 618]]
# heatmap(data[, -618], col = br,  Colv = NA, Rowv = NA, RowSideColors = rc,  ylab = "Letters")
data.mat <- cbind(x.mat, class)
data <- data.mat[order(data.mat[, 618]),]
rc <- tim.colors(26)[data[, 618]]
heatmap(data[, -618], col = br,  Colv = NA, Rowv = NA, RowSideColors = rc,  ylab = "")
legend(24.5, 28, legend = letters[1:26], col = tim.colors(26)[1:26], pch = 19, bty = "n", ncol = 2)
rasterImage(t(br), 25, 0, 27, 1)
text(c(25, 27), c(1.7, 1.7), c(-1, 1))
# SVM 
model <- svm(x.sampling, class, cross = 10)
(acc.10.tot <- model$tot.accuracy)

pca <- PCA(x.sampling, ncp = 2, graph = F)
pca.dim <- get_pca_ind(pca)$coord
plot(pca.dim[, 1], pca.dim[, 2],
     col = tim.colors(26)[class],
     pch = c("o","+")[1:260 %in% model$index + 1],
     main="SVM to Isolet Data", xlab = "dim1", ylab = "dim2")
legend(-25, 22, letters[1:26], col = tim.colors(26)[1:26], 
       pch = 19, ncol = 5, bty="n")
text(13, 17, "Accuracy = 0.68", cex = 1)
