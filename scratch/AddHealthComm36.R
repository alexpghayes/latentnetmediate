# #### Add Health
#
#
# library(igraph)
# library(randnet)
# library(irlba)
# library(NetworkReg)
# library(RSpectra)
#
#
# USVT.orig <- function (A)
# {
#   n <- nrow(A)
#   K <- ceiling(n^(1/2))
#   SVD <- RSpectra::svds(A, k = K, nv = K, nu = K)
#   Km <- sum(SVD$d>(2.01*sqrt(n*max(rowMeans(A)))))
#   print(Km)
#   Ahat <- SVD$u[,1:Km] %*% (t(SVD$v[,1:Km]) * SVD$d[1:Km])
#   Ahat[Ahat > 1] <- 1
#   Ahat[Ahat < 0] <- 0
#   return(list(Ahat=Ahat,Km=Km))
# }
#
#
#
# load(here::here("data/AddHealth84Communities.Rda"))
#
#
#
#
#
# I <- 36
#
# A <- AddHealth[[I]]$A
# att <- AddHealth[[I]]$att
#
# # missing data imputation
#
# att$sex[att$sex==0] <- mean(att$sex[att$sex>0])
#
# att$grade[att$grade==0] <- mean(att$grade[att$grade>0])
#
#
# n <- nrow(A)
# head(att)
#
# table(att$grade)
#
# g <- graph.adjacency(A,mode="undirected")
#
#
# lo <- layout.fruchterman.reingold(g)
# plot(g,vertex.label=NA,vertex.size=3,layout=lo)
#
#
# X <- att[,-ncol(att)]
#
# X$race <- factor(X$race)
#
# dummy.race <- model.matrix( ~ race , data=X )
#
# race.proportion <- colSums(dummy.race)/n
# minor.index <- which(race.proportion<0.05)
#
#
# X <- X[,-which(names(X)=="race")]
#
# X <- as.matrix(X)
# X <- scale(X,TRUE,TRUE)
#
# if(length(minor.index)>0){
#   minor <- matrix(dummy.race[,minor.index],nrow=n)
#   all.minor <- rowSums(minor)
#
#   major.race <- matrix(dummy.race[,which(race.proportion>=0.05)],nrow=n)
#   colnames(major.race) <- colnames(dummy.race)[which(race.proportion>=0.05)]
#   X <- cbind(X,major.race,all.minor)
#   race.num <- ncol(major.race) + 1
#
#   colnames(X)[ncol(X)] <- "raceMix"
#
# }else{
#   X <- cbind(X,dummy.race)
#   race.num <- ncol(dummy.race)
#
# }
#
#
# Y <- matrix(att$level,ncol=1)
#
# Y[is.na(Y)] <- 0
#
#
# eig <- eigs_sym(A,k=9)
#
# plot(eig$values)
#
#
# usvt <- USVT.orig(A)
# K <- usvt$Km #min(c(BHMC.estimate(A,K.max=10)$K,ecv$sse.rank,ecv$auc.rank))
# if(K <1){
#   K <- 1
# }
# p <- ncol(X)
#
# ols <- lm(Y~X-1)
# ss <- summary(ols)
# ss
#
# K <- 9
#
# # is R a rank restriction on the moderation effect?
#
#
# fit <- SP.Inf(X,Y,A,K=K,boot.thr = TRUE)
#
# fit
#
# fit$beta
# fit$coef.mat
#
# fit2 <- SP.Inf(X,Y,A,K=K, r = 7)
# str(fit2)
#
# fit2$coef.mat
# rownames(fit2$theta) <- colnames(X)
# fit2$theta
#
# U <- eig$vectors
#
# dim(U)
# dim(X)
# X
# colnames(X)
#
# m <- lm(U ~ X + 0)
#
# o <- lm(Y ~ U + X + 0)
# summary(o)
#
# coef(m) %*% coef(o)[1:9]
#
#
#
#
