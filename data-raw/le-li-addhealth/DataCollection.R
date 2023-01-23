library(igraph)


AddHealth <- list()



for (k in 49:84) {
  print(k)
  edge.list <- read.table(paste("http://moreno.ss.uci.edu/comm", k, ".dat", sep = ""), skip = 4)
  test.file <- scan(paste("http://moreno.ss.uci.edu/comm", k, "_att.dat", sep = ""), what = "", sep = "\n")
  skip.num <- as.numeric(which(test.file == "DATA:"))
  att <- read.table(paste("http://moreno.ss.uci.edu/comm", k, "_att.dat", sep = ""), skip = skip.num)
  if (any(test.file == "\"school\"")) {
    names(att) <- c("sex", "race", "grade", "school")
  } else {
    names(att) <- c("sex", "race", "grade")
  }

  n <- nrow(att)
  W.mat <- matrix(0, n, n)
  W.mat[cbind(edge.list[, 1], edge.list[, 2])] <- edge.list[, 3]
  avg.level <- rowSums(W.mat) / apply(W.mat, 1, function(x) sum(x > 0.1))
  att$level <- avg.level

  g <- graph.data.frame(d = edge.list[edge.list[, 3] > 0, 1:2])
  cc <- clusters(g, mode = "weak")
  index <- which(cc$membership == which.max(cc$csize))
  A <- get.adjacency(g)
  node.names <- as.numeric(V(g)$name)
  att <- att[node.names, ]
  A <- A[index, index]
  att <- att[index, ]
  A <- as.matrix(A)
  n <- nrow(A)
  A.sym <- A + t(A)
  A <- matrix(0, n, n)
  A[A.sym > 0] <- 1
  AddHealth[[k]] <- list(A = A, att = att)
}


edge.list <- read.table("http://moreno.ss.uci.edu/comm1.dat", skip = 4)
test.file <- scan("http://moreno.ss.uci.edu/comm1_att.dat", what = "", sep = "\n")
att <- read.table("http://moreno.ss.uci.edu/comm1_att.dat", skip = 8, nrows = 113)
names(att) <- c("sex", "race", "grade")

n <- nrow(att)
W.mat <- matrix(0, n, n)
W.mat[cbind(edge.list[, 1], edge.list[, 2])] <- edge.list[, 3]
avg.level <- rowSums(W.mat) / apply(W.mat, 1, function(x) sum(x > 0.1))
att$level <- avg.level

g <- graph.data.frame(d = edge.list[edge.list[, 3] > 0, 1:2])
cc <- clusters(g, mode = "weak")
index <- which(cc$membership == which.max(cc$csize))
A <- get.adjacency(g)
node.names <- as.numeric(V(g)$name)
att <- att[node.names, ]
A <- A[index, index]
att <- att[index, ]
A <- as.matrix(A)
n <- nrow(A)
A.sym <- A + t(A)
A <- matrix(0, n, n)
A[A.sym > 0] <- 1
AddHealth[[1]] <- list(A = A, att = att)

save(AddHealth, file = "AddHealth84Communities.Rda")
