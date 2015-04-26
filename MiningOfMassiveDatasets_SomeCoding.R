# Coursera - Mining of Massive Datasets
# -------------------------------------
# mmds.org
# http://www.tonicebrian.com/2013/03/11/introduction-to-the-minhash-algorithm/

# Week 1
# ------

# Properties of Distributed File System (Google HFS, Hadoop HDFS):
# ----------------------------------------------------------------
# - 1) Chunk servers in clusters holds "chunks" of the file (16-64 MB typically),
#   usually replicated 2-3 times. Replicated, ideally NOT in the same rack
# - Chunk servers are also computatinal servers - compute close to the data!
# - 2) Master Node: A "name server", stores the chunk locations (metadata)
#   Server not by default replicated (so if MN fails, MR job might have to be restarted.
#   This server is called "Name Node" in Hadoop HDFS
# - 3) Client library for file access. Talks to the Master Node to find the correct chunk server,
#      or connects directly to chunk servers to find the data (peer-to-peer fashion without going
#      through the master node)

# MapReduce:
# ----------
# - When a mapper completes, it sends the location of its <R> outputs (one for each key=<R>educer) to the
#   Master Node, and the MN then shuffles the map output to the correct reducer (by cecking Key value)
# - Mappers and Reducers store their intermediate files on local filesystem (FS), not distributed filesystem

# Link analysis and Page Rank
# ---------------------------
# http://en.wikipedia.org/wiki/PageRank
# http://www.math.cornell.edu/~mec/Winter2009/RalucaRemus/Lecture3/lecture3.html
# http://www.sirgroane.net/google-page-rank/
# http://ilpubs.stanford.edu:8090/361/1/1998-8.pdf
# http://www4.ncsu.edu/~ipsen/ps/slides_man.pdf
# http://en.wikipedia.org/wiki/Strongly_connected_component
# http://machinelearningresource.com/wp-content/uploads/2014/10/Problem1ConceptsPart1.html

# See "Mining of Massive Datasets" book, section 5.2.2
# TIPS: https://class.coursera.org/mmds-001/forum/thread?thread_id=99
SetStandardOptions()

library(gridExtra)
library(igraph)

op <- par()
par(mar=c(1,1,2,1))

# -------------------------------------------------------------------------------------------------------------------

# Plot transition matrix M and converged result (page rank) r (or v)
PlotResult <- function(ret, graph.name=character(0), remove.zeroes=F) {
  df <- as.data.frame(as.table(ret$transition.matrix))
  df$Var1 <- with(df, factor(Var1, levels=rev(levels(Var1)))) # NOTE: Reverse the order of the y-axis
  
  # TEST:
  if (length(colnames(ret$transition.matrix)) == 0) {
    df$Var1 <- as.integer(df$Var1) # Convert A-Z back to 1-26
    df$Var2 <- as.integer(df$Var2)
  }
  
  if (remove.zeroes == T)
    df$Freq[df$Freq == 0] <- ""

  if (length(graph.name) > 0)
    graph.name <- paste0(" for ", graph.name)
  
  rotation <- 0
  if (max(nchar(as.character(df$Var1))) > 1)
    rotation <- 90
  
  g1 <- ggplot()
  g1 <- g1 + geom_tile(aes(x=Var2, y=Var1, fill=Freq), data=df, color="black", size=0.1) + 
    labs(x="", y="") +
    ggtitle(paste0("Transition Matrix", graph.name, " - ", sqrt(nrow(df)), " nodes")) +
    theme(plot.title=element_text(size=14, color="black")) +
    geom_text(aes(x=Var2, y=Var1, label=sprintf("%.2f", Freq)), data=df, size=3, colour="black") +
    scale_fill_gradient(low="white", high="cornflowerblue", name="Probability") +
    theme(panel.background=element_blank(), axis.ticks=element_blank(),
          panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + # Remove box/grid
    theme(axis.text=element_text(colour="black", size=10), 
          axis.title=element_text(face="bold", colour="black", size=12),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10)) +
    theme(axis.text.x=element_text(angle=rotation, hjust=1))
    
  if (length(colnames(ret$transition.matrix)) == 0)
    g1 <- g1 + scale_x_continuous(breaks=1:ncol(ret$transition.matrix)) +
    scale_y_continuous(breaks=1:ncol(ret$transition.matrix))
      
  if (length(rownames(ret$transition.matrix)) > 0)
    df2 <- data.frame(x=rownames(ret$transition.matrix), y=ret$page.rank)
  else
    df2 <- data.frame(x=1:ncol(ret$transition.matrix), y=ret$page.rank)
  
  #df2 <- data.frame(x=levels(df$Var1), y=ret$page.rank)
  
  g2 <- ggplot()
  g2 <- g2 + geom_bar(aes(x=x, y=y, fill=y), data=df2, stat="identity") + 
    labs(x="", y="") + ggtitle("Node Importance") +
    theme(plot.title=element_text(size=14, color="black")) +
    scale_fill_gradient(low="goldenrod1", high="goldenrod3", name="Importance") +
    theme_bw() +
    theme(axis.text=element_text(colour="black", size=10), 
          axis.title=element_text(face="bold", colour="black", size=12),
          legend.text=element_text(size=10),
          legend.title=element_text(size=10)) +
  theme(axis.text.x=element_text(angle=rotation, hjust=1))
  
  if (length(colnames(ret$transition.matrix)) == 0)
    g2 <- g2 + scale_x_continuous(breaks=1:ncol(ret$transition.matrix))
  
  grid.arrange(g1, g2)
}

# Create transition matrix with initial probability 1/nodes
CreateTransitionMatrix <- function(adj.matrix, node.names=character(0), teleport.constant=0, fix.dead.ends=F, start.prob=NA, do.epsilon=T,
                                   do.debug=F) {
  breakout <- 1000
  counter <- 0 # Breakout if counter > breakout
  
  nodes <- ncol(adj.matrix)
  edges <- diag(adj.matrix %*% t(adj.matrix)) # NOTE: This gives total number of in- and out-edges for each node
  epsilon <- 1.0e-16
  # Create a transition matrix
  transition.matrix <- t(adj.matrix)
  
  # Need to sum to 1, so divide by the sum of edges
  for (counter in 1:nodes)
    transition.matrix[, counter] <- transition.matrix[, counter] / edges[counter]
  # transition.matrix
  # Fix NaN
  temp <- sapply(transition.matrix, function(x) ifelse(is.nan(x), 0, x))
  transition.matrix <- matrix(temp, ncol=nodes, byrow=F)
  
  # Fix dead ends (column sums to 0)
  if (fix.dead.ends == T) {
    zero.cols <- which(colSums(transition.matrix) == 0)
    transition.matrix[, zero.cols] <- (1 /nodes)
  }
  
  old.result <- 0
  r <- rep(1 / nodes, nodes)
  
  if (teleport.constant == 0)
    result <- transition.matrix %*% r
  else {
    transition.matrix <- (teleport.constant * transition.matrix) # Incorporate B
    # NOTE: Initialize the vector r (or v) with equal prob. 1/nodes for all nodes
    result <- (transition.matrix %*% r) + ((1 - teleport.constant) * (1 / nodes))
  }
  # BMr + (1-B)1/n because:
  # With prob. B, follow a link at random
  # With prob. (1-B), jump to some random page
  # Since surfer has to decide (OR), we use prob(B) PLUS prob(1-B)
  
  while (T) {
    counter <- counter + 1
    old.result <- result
    if (teleport.constant == 0)
      result <- transition.matrix %*% old.result
    else
      result <- (transition.matrix %*% old.result) + ((1 - teleport.constant) * (1 / nodes))
    
    if (do.debug == T)
      print(result)
    
    difference <- sum(sapply(1:nodes, function(x) abs(result[x] - old.result[x])))
    
    if (counter > breakout)
      break
    
    if ((difference < epsilon) & (do.epsilon == T))
      break
    
    if (length(which(result == old.result)) == nodes) # Calculation has converged, so break loop
      break
  }
  
  # result # Converged
  # sum(result) # The sum should always be 1
  
  # The transition matrix is a column stochastic matrix M (all columns sum to 1): 
  # - Let page i have d<i> out-links.
  #  - if vertex i points to vertex j, then M<j,i> = 1/d<i>, otherwise M<j,i> = 0
  
  ret <- list()
  ret$page.rank <- result
  if (length(node.names) > 0) {
    colnames(transition.matrix) <- node.names
    rownames(transition.matrix) <- node.names
  }
  ret$transition.matrix <- transition.matrix
  return (ret)
}

# -------------------------------------------------------------------------------------------------------------------

# http://igraph.org/r/

# A <-> B
nodes <- 2 
adj.matrix.g1 <- matrix(c(0,1, 1,0), nrow=2, ncol=2, byrow=T)
g1 <- graph.adjacency(adj.matrix.g1, mode="directed")
V(g1)$name <- c("A","B")
plot(g1, main="PageRank")
edges <- diag(adj.matrix.g1 %*% t(adj.matrix.g1))
# NOTE: This gives total number of in- and out-edges for each node
edges/sum(edges)

# A -> B
nodes <- 2 
adj.matrix.g1_1 <- matrix(c(0,1, 0,0), nrow=2, ncol=2, byrow=T)
g1_1 <- graph.adjacency(adj.matrix.g1_1, mode="directed")
V(g1_1)$name <- c("A","B")
plot(g1_1, main="PageRank")
edges <- diag(adj.matrix.g1_1 %*% t(adj.matrix.g1_1))
# NOTE: This gives total number of in- and out-edges for each node
edges/sum(edges)

nodes <- 3 # 1:y, 2:a, 3:m
edges <- c(1,1, 1,2, 2,1, 2,3, 3,2)
g2 <- graph(edges, directed=T, n=nodes)
V(g2)$name <- c("y","a","m")
plot(g2, main="PageRank")

adj.matrix.g2 <- matrix(c(1,1,0, 1,0,1, 0,1,0), nrow=3, ncol=3, byrow=T)
g2 <- graph.adjacency(adj.matrix.g2, mode="directed")
V(g2)$name <- c("y","a","m")
plot(g2, main="PageRank")
edges <- diag(adj.matrix.g2 %*% t(adj.matrix.g2))
# NOTE: This gives total number of in- and out-edges for each node
edges/sum(edges) # This gives the same result as in video 2-8 PageRank Power Iteration (6/15,6/15,3/15)

adj.matrix.g2_1 <- matrix(c(1,1,0, 1,0,1, 0,0,1), nrow=3, ncol=3, byrow=T)
g2_1 <- graph.adjacency(adj.matrix.g2_1, mode="directed")
V(g2_1)$name <- c("y","a","m")
plot(g2_1, main="PageRank")

adj.matrix.g2_2 <- matrix(c(0,1,1, 1,0,1, 0,1,0), nrow=3, ncol=3, byrow=T)
g2_2 <- graph.adjacency(adj.matrix.g2_2, mode="directed")
V(g2_2)$name <- c("y","a","m")
plot(g2_2, main="PageRank")

# Dead end graph, with a -> m. m has zero out-degree, so col 3 of M will be all zeros = not stochastic matrix
# Node m can not pass its page-rank score, since it has no out-links, thus the overall score "leaks out" eventually
adj.matrix.g2_3 <- matrix(c(1,1,0, 1,0,1, 0,0,0), nrow=3, ncol=3, byrow=T)
g2_3 <- graph.adjacency(adj.matrix.g2_3, mode="directed")
V(g2_3)$name <- c("y","a","m")
plot(g2_3, main="PageRank")

# Loop  y -> a -> m -> y
adj.matrix.g2_4 <- matrix(c(0,1,0, 0,0,1, 1,0,0), nrow=3, ncol=3, byrow=T)
g2_4 <- graph.adjacency(adj.matrix.g2_4, mode="directed")
V(g2_4)$name <- c("y","a","m")
plot(g2_4, main="PageRank")

# Book, p. 166
# In this matrix, the order of the pages is the natural one, A, B, C, and D. Thus,
# the first column expresses the fact, already discussed, that a surfer at A has a
# 1/3 probability of next being at each of the other pages. The second column
# expresses the fact that a surfer at B has a 1/2 probability of being next at A
# and the same of being at D. The third column says a surfer at C is certain to
# be at A next. The last column says a surfer at D has a 1/2 probability of being
# next at B and the same at C.

# It is known that the distribution of the surfer approaches a limiting distribution
# v that satisfies v = Mv (or r = Mr as shown on slides), provided two conditions are met:
# 1. The graph is strongly connected; that is, it is possible to get from any node to any other node.
# 2. There are no dead ends: nodes that have no arcs out.

# The limit is reached when multiplying the distribution by M another time
# does not change the distribution. In other terms, the limiting v is an eigenvector
# of M (an eigenvector of a matrix M is a vector v that satisfies v = λMv for
# some constant eigenvalue λ). In fact, because M is stochastic, meaning that its
# columns each add up to 1, v is the principal eigenvector (its associated eigenvalue
# is the largest of all eigenvalues). Note also that, because M is stochastic,
# the eigenvalue associated with the principal eigenvector is 1.

nodes <- 4
adj.matrix.g3 <- matrix(c(0,1,1,1, 1,0,0,1, 1,0,0,0, 0,1,1,0), nrow=nodes, ncol=nodes, byrow=T)
g3 <- graph.adjacency(adj.matrix.g3, mode="directed")
V(g3)$name <- c("A","B","C","D")
E(g3)
plot(g3, main="G3 PageRank", layout=layout.fruchterman.reingold,
     vertex.label=V(g3)$name, vertex.color="red", vertex.label.color="black", vertex.frame.color="red",
     vertex.label.cex=1)

# Do a "dead end" graph by removing C->A
nodes <- 4
adj.matrix.g3_2 <- matrix(c(0,1,1,1, 1,0,0,1, 0,0,0,0, 0,1,1,0), nrow=nodes, ncol=nodes, byrow=T)
g3_2 <- graph.adjacency(adj.matrix.g3_2, mode="directed")
V(g3_2)$name <- c("A","B","C","D")
E(g3_2)
plot(g3_2, main="G3_2 PageRank")

# Do a one-node "spider-trap" graph by linking node C to itself
nodes <- 4
adj.matrix.g3_3 <- matrix(c(0,1,1,1, 1,0,0,1, 0,0,1,0, 0,1,1,0), nrow=nodes, ncol=nodes, byrow=T)
g3_3 <- graph.adjacency(adj.matrix.g3_3, mode="directed")
V(g3_3)$name <- c("A","B","C","D")
E(g3_3)
plot(g3_3, main="G3_3 PageRank")
plot(g3_3, main="G3_3 PageRank", layout=layout.fruchterman.reingold, edge.color="powderblue",
     vertex.label=V(g3_3)$name, vertex.color="wheat", vertex.label.color="brown", vertex.frame.color="black",
     vertex.label.cex=1)

# Do Teleporting, p. 174
B <- 0.8 # Typically set B to between 0.8-0.9
nodes <- 4
adj.matrix.g3_4 <- adj.matrix.g3_3
g3_4 <- graph.adjacency(adj.matrix.g3_4, mode="directed")
V(g3_4)$name <- c("A","B","C","D")
E(g3_4)
plot(g3_4, main="G3_4 PageRank - teleporting")

ret <- CreateTransitionMatrix(adj.matrix.g1, V(g1)$name)
PlotResult(ret, "G1")
ret <- CreateTransitionMatrix(adj.matrix.g1_1, V(g1_1)$name)
PlotResult(ret, "G1_1")
ret <- CreateTransitionMatrix(adj.matrix.g2, V(g2)$name)
PlotResult(ret, "G2")
ret <- CreateTransitionMatrix(adj.matrix.g2_1, V(g2_1)$name, do.epsilon=F)
PlotResult(ret, "G2_1")
ret <- CreateTransitionMatrix(adj.matrix.g2_2, V(g2_2)$name)
PlotResult(ret, "G2_2")
ret <- CreateTransitionMatrix(adj.matrix.g2_3, V(g2_3)$name, , fix.dead.ends=T, do.epsilon=F)
PlotResult(ret, "G2_3")
ret <- CreateTransitionMatrix(adj.matrix.g2_4, V(g2_4)$name, do.epsilon=T)
PlotResult(ret, "G2_4")
ret <- CreateTransitionMatrix(adj.matrix.g3, V(g3)$name, do.epsilon=T)
PlotResult(ret, "G3")
# "Spider-trap" (C is a dead-end node)
ret <- CreateTransitionMatrix(adj.matrix.g3_2, V(g3_2)$name, do.epsilon=F)
PlotResult(ret, "G3_2")
# One node "Spider-trap" (C links to itself)
ret <- CreateTransitionMatrix(adj.matrix.g3_3, V(g3_3)$name, do.epsilon=F)
PlotResult(ret, "G3_3")
# One node "Spider-trap" (C links to itself) and teleporting constant
ret <- CreateTransitionMatrix(adj.matrix.g3_4, V(g3_4)$name, teleport.constant=B, do.epsilon=F)
PlotResult(ret, "G3_4")

ret

# Watts-Strogatz small world game
par(mar=c(1,1,2,1))
g <- watts.strogatz.game(1, 20, 5, 0.05)
m <- as.matrix(g[])
colnames(m) <- LETTERS[1:ncol(m)]
rownames(m) <- LETTERS[1:ncol(m)]
degrees <- diag(m %*% m)
degrees.normalized <- degrees / (ncol(m) - 1)
V(g)$name <- paste(colnames(m), round(degrees.normalized, 2))
plot(g, main="Watts-Strogatz small world network", vertex.size=degrees)
ret <- CreateTransitionMatrix(m, colnames(m), do.epsilon=F)
PlotResult(ret, "Watts-Strogatz Small World Game", remove.zeroes=F) # TODO: Formatting problem with rz=T

# Erdos-Renyi random network
g <- erdos.renyi.game(12, .8, "gnp")
plot(g, main="Erdos-Renyi random network")
ret <- CreateTransitionMatrix(as.matrix(g[]), do.epsilon=F)
PlotResult(ret, "Erdos-Renyi random network")

# https://github.com/igraph/igraph/blob/master/nexus/download/padgett.R
library(network)
data(flo) # Florentine wedding network dataset (Medici influence)
head(flo)
nflo <- network(flo, directed=F)
all(nflo[,] == flo) # Trust, but verify
par(mar=c(1,1,1.5,1))
plot(nflo, displaylabels=TRUE, boxed.labels=FALSE, label.cex=0.75)
#diameter(nflo)
g.flo <- graph.adjacency(flo, mode="undirected")
degrees <- diag(flo %*% flo)
degrees.normalized <- degrees / (ncol(flo) - 1)
plot(g.flo, main="Florentine Marriage Network", layout=layout.fruchterman.reingold,
     vertex.label=V(g.flo)$name, vertex.color="goldenrod2", vertex.label.color="black",
     vertex.frame.color="goldenrod3",
     vertex.label.cex=1, vertex.size=degrees * 3.6)
data.frame(V(g.flo)$name, degree(g.flo))
# NOTE: Can get rid of the Pucci (unconnected) node, but not needed
pucci <- which(colnames(flo) == "Pucci")
flo <- flo[, -pucci]
pucci <- which(rownames(flo) == "Pucci")
flo <- flo[-pucci, ]
g.flo <- graph.adjacency(flo)
V(g.flo)$betweenness
B <- 0.85
page.rank(g.flo)
ret <- CreateTransitionMatrix(as.matrix(g.flo[]), node.names=colnames(flo), do.epsilon=T,
                              teleport.constant=B, do.debug=F)
ret <- CreateTransitionMatrix(flo, node.names=colnames(flo), do.epsilon=F, do.debug=F)

op <- par()
par(mfrow=c(2,1))
par(mar=c(2.5,4,2,1))
plot(page.rank(g.flo)$vector, pch=21, bg="cyan", col="blue", main="page.rank(g.flo) with damping=.85",
     cex.main=1, cex.lab=.8, cex.axis=.7, ylab="Value", xlab="") # default damping = 0.85
plot(ret$page.rank, pch=21, bg="cyan", col="blue", main="ret$page.rank with teleport=.85",
     cex.main=1, cex.lab=.8, cex.axis=.7, ylab="Value", xlab="") # B/teleport = 0.85
par(mfrow=c(1,1))
par <- op
    
PlotResult(ret, "Florentine Marriage Network")
write.table(file="C:/coding/Networks/FlorentineMarriages.csv", flo, sep=";",
          row.names=T, col.names=T)

# --------------------------------------------------------------------------------------------------
# Shortest paths (Djikstra's algorithm):
# http://www.inside-r.org/packages/cran/igraph/docs/shortest.paths
# http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
nodes <- 4
adj.matrix.shortest.path <- matrix(c(0,1,1,0, 0,0,1,0, 0,0,0,1, 1,0,0,0), nrow=nodes, ncol=nodes, byrow=T)
g.shortest <- graph.adjacency(adj.matrix.shortest.path, mode="undirected")
V(g.shortest)$name <- c("A","B","C","D")
E(g.shortest)
plot(g.shortest, main="Shortest path")
shortest.paths(g.shortest, mode="in")
shortest.paths(g.shortest, mode="out")
get.shortest.paths(g.shortest, "B")
get.all.shortest.paths(g.shortest, "B")
average.path.length(g.shortest)
ConfusionMatrix(shortest.paths(g.shortest), labels=c("A","B","C","D"))

# --------------------------------------------------------------------------------------------------
# Reading .gephi files into R
package.install("rgexf")
library(rgexf)
# g <- read.gexf("C:/coding/Networks/RandomGraph.gexf")
g <- read.csv("C:/coding/Networks/RandomGraph.csv", header=T, sep=";")
g <- g[,-1] # Remove the first col containing node names
rownames(g) <- colnames(g)
g <- as.matrix(g)
g <- graph.adjacency(g, mode="directed")
par(mar=c(1,1,2,1))
plot(g, main="Random Graph from Gephi CSV-export")
ret <- CreateTransitionMatrix(g, node.names=colnames(g), do.epsilon=T, do.debug=F,
                              teleport.constant=.85)
PlotResult(ret, "Random Graph from Gephi CSV-export")


# ---------------------------------------------------------------------------------------------------------------
# Quiz Week 1

# 1) Graph 1
# Suppose we compute PageRank with a β of 0.7, and we introduce the additional constraint that the sum of the
# PageRanks of the three pages must be 3, to handle the problem that otherwise any multiple of a solution will
# also be a solution. Compute the PageRanks a, b, and c of the three pages A, B, and C, respectively.
# Then, identify from the list below, the true statement.
nodes <- 3
B <- 0.7
adj.matrix.quiz1 <- matrix(c(0,1,1, 0,0,1, 0,0,1), nrow=3, ncol=3, byrow=T)
g.quiz1 <- graph.adjacency(adj.matrix.quiz1, mode="directed")
V(g.quiz1)$name <- c("A","B","C")
plot(g.quiz1, main="PageRank")
edges <- diag(adj.matrix.quiz1 %*% t(adj.matrix.quiz1))
edges/sum(edges) # This gives the same result as in video 2-8 PageRank Power Iteration
ret <- CreateTransitionMatrix(adj.matrix.quiz1, V(g.quiz1)$name, teleport.constant=B,
                              start.prob=rep(1, nodes), do.epsilon=F)
PlotResult(ret)
ret$page.rank * 3
sum(ret$page.rank * 3)
ret$page.rank <- ret$page.rank * 3
a <- ret$page.rank[1]
b <- ret$page.rank[2]
c <- ret$page.rank[3]
(b + c) == 3.25
(a + c) == 2.595
(a + c) == 1.985
(a + b) == 1.025
# Answer: (a + c) = 2.595  (OK)

# 2) Graph 2
# Suppose we compute PageRank with B=0.85. Write the equations for the PageRanks a, b, and c
# of the three pages A, B, and C, respectively. Then, identify in the list below, one of the equations.
B <- 0.85
adj.matrix.quiz2 <- matrix(c(0,1,1, 0,0,1, 1,0,0), nrow=3, ncol=3, byrow=T)
g.quiz2 <- graph.adjacency(adj.matrix.quiz2, mode="directed")
V(g.quiz2)$name <- c("A","B","C")
edges <- diag(adj.matrix.quiz2 %*% t(adj.matrix.quiz2))
edges/sum(edges) # This gives the same result as in video 2-8 PageRank Power Iteration
ret <- CreateTransitionMatrix(adj.matrix.quiz2, V(g.quiz2)$name, teleport.constant=B, do.epsilon=F)
ret
PlotResult(ret)
plot(g.quiz2, main="PageRank", vertex.size=ret$page.rank*80)
a <- ret$page.rank[1]
b <- ret$page.rank[2]
c <- ret$page.rank[3]
(.475 * a) + (.05 * c) == b
b + (.575 *a) == c
b + (.575 * a) == (.85 * c)
(.475 * a) + (.05 * c) == (.95 * b) 
# Answer: (.475 * a) + (.05 * c) == (.95 * b)  (OK)

# 3) Graph 3
# Assuming no "taxation," compute the PageRanks a, b, and c of the three pages A, B, and C, using iteration,
# starting with the "0th" iteration where all three pages have rank a = b = c = 1. Compute as far as the 5th
# iteration, and also determine what the PageRanks are in the limit. Then, identify the true statement from
# the list below.
adj.matrix.quiz3 <- adj.matrix.quiz2
g.quiz3 <- graph.adjacency(adj.matrix.quiz3, mode="directed")
V(g.quiz3)$name <- c("A","B","C")
plot(g.quiz3, main="PageRank")
edges <- diag(adj.matrix.quiz3 %*% t(adj.matrix.quiz3))
edges/sum(edges) # This gives the same result as in video 2-8 PageRank Power Iteration
ret <- CreateTransitionMatrix(adj.matrix.quiz3, V(g.quiz3)$name)
PlotResult(ret)
# Answer: In the limit, b = 3/5  (OK)
 

# ---------------------------------------------------------------------------------------------------------------
# Week 2

# 1) Finding similar sets

# Three essential techniques:
# - Shingling: Convert documents, emails etc. to sets
#   - Can give false positives (documents that we declare ar similar but arent't) or false negatives
#     (documents that we declare are NOT similar, but are in fact similar)

# - Minhashing: Convert large sets to short sigmatures (short integer vectors that represents the sets,
#   and reflect their similarity), while preserving similarity

# - LSH, Locality-Sensitive Hashing, focusing on pairs of signatures likely to be similar

# The process:
# ------------
# 1) Shingling (->set) -> Minhashing (->signatures) -> LSH (->candidate pairs)

# http://en.wikipedia.org/wiki/W-shingling
# k-gram or k-shingles:
document <- "abcab"
k <- 2
k.shingles <- sapply(1:(nchar(document) - (k - 1)), function(x) substr(document, x, x + (k - 1)))
k.shingles

document1 <- "The dog chased with the cat"
document2 <- "The dog that chased the cat"
document1 <- "The dog that chased the cat"
document2 <- "The dog that also chased the cat"
k <- 3
k.shingles.doc1 <- sapply(1:(nchar(document1) - (k - 1)), function(x) substr(document1, x, x + (k - 1)))
k.shingles.doc1
k.shingles.doc2 <- sapply(1:(nchar(document2) - (k - 1)), function(x) substr(document2, x, x + (k - 1)))
k.shingles.doc2
# TODO: Return unique(k.shingles.doc<n>) ?

# To compress long shingles, we can hash them to, say, 4 bytes. These hashes are called tokens.

# 2) Minhashing

# Jaccard Similarity Measure:
# The Jaccard similarity of two sets is the size of their INTERSECTION divided by the size of their UNION
k.shingles.doc1 <- unique(k.shingles.doc1)
k.shingles.doc2 <- unique(k.shingles.doc2)

jaccard.sim <- length(intersect(k.shingles.doc1, k.shingles.doc2)) / length(union(k.shingles.doc1, k.shingles.doc2))
jaccard.sim

setdiff(union(k.shingles.doc1, k.shingles.doc2), intersect(k.shingles.doc1, k.shingles.doc2))
d1 <- unlist(strsplit(document1, " "))
d2 <- unlist(strsplit(document2, " "))
setdiff(union(d1, d2), intersect(d1, d2))

# NOTE: Jaccard Distance is 1 - Jaccard Similarity:
jaccard.distance <- (1 - jaccard.sim)

# Example: Say, an Amazon book with two sets of customers with high Jaccard similarity:
# The sim. may not need to be very high for it to be significant for inclusion in "Customers who bought <X> also bought..."
# Since customers often buy books on a wide range of topics/themes.
# Even a Jaccard similarity like 20% might be unusual enough to identify customers with similar tastes. (Book p. 76)

# A good rule of thumb is to imagine that there are only 20 (commonly used) characters and estimate the number of k-shingles
# as 20^k. For large documents, such as research articles, choice k = 9 is considered safe. (Book p. 79)

GetJaccardSetSimilarity <- function(document1, document2, apply.shingling=T, k) {
  document1 <- tolower(document1)
  document2 <- tolower(document2)
  
  if (apply.shingling == T) {
    k.shingles.doc1 <- unique(sapply(1:(nchar(document1) - (k - 1)), function(x) substr(document1, x, x + (k - 1))))
    k.shingles.doc2 <- unique(sapply(1:(nchar(document2) - (k - 1)), function(x) substr(document2, x, x + (k - 1))))
    jaccard.sim <- length(intersect(k.shingles.doc1, k.shingles.doc2)) / length(union(k.shingles.doc1, k.shingles.doc2))
  } else {
    jaccard.sim <- length(intersect(document1, document2)) / length(union(document1, document2))
  }
  
  return (jaccard.sim)
}

document1 <- "The dog that chased the cat also chased it yesterday and the day before"
document2 <- "The dog which chased the cat also chased it yesterday and the day before"
document3 <- "The dog chased the cat which also chased it yesterday and the day before"
GetJaccardSetSimilarity(document1, document2, apply.shingling=T, 5)
GetJaccardSetSimilarity(document2, document3, apply.shingling=T, 5)

# http://www.dbinfoblog.com/post/142/union,-intersection,-and-difference-of-bags-/
GetJaccardBagSimilarity <- function(document1, document2, apply.shingling=T, k) {
  document1 <- tolower(document1)
  document2 <- tolower(document2)
  
  # NOTE: Need to do a different intersect/union here:
  # http://poincare.matf.bg.ac.rs/~vladaf/Courses/PmfBl%20TI%20IP/Materijali/Kent%20State/lecture2.ppt
  if (apply.shingling == T) {
    k.shingles.doc1 <- (sapply(1:(nchar(document1) - (k - 1)), function(x) substr(document1, x, x + (k - 1))))
    k.shingles.doc2 <- (sapply(1:(nchar(document2) - (k - 1)), function(x) substr(document2, x, x + (k - 1))))
    jaccard.sim <- length(BagIntersect(k.shingles.doc1, k.shingles.doc2)) / length(c(k.shingles.doc1, k.shingles.doc2))
  } else {
    jaccard.sim <- length(BagIntersect(document1, document2)) / length(c(document1, document2))
  }
  
  return (round(jaccard.sim, 2))
}

document1 <- c("a","a","a","b")
document2 <- c("a","a","b","b","c")
GetJaccardBagSimilarity(document1, document2, apply.shingling=F, 2)
document1 <- c("aaab")
document2 <- c("aabbc")
GetJaccardBagSimilarity(document1, document2, apply.shingling=T, 1)
# Book p. 77, should be 1/3 for first setup (c("a","a","a","b") etc.) CORRECT! :-)

# TODO: How do we get from the document with words to the 1/0 format?
k <- 5
# NOTE: Dropping shingling here
document1 <- "All the words do not appear in all documents altogether overall"
document2 <- "The words do not appear in all documents this is overall"
document1.split <- unlist(strsplit(tolower(document1), " "))
document2.split <- unlist(strsplit(tolower(document2), " "))
universal.set <- unique(unlist(c(strsplit(tolower(document1), " "), strsplit(tolower(document2), " "))))
input.matrix <- matrix(rep(0, length(universal.set) * 2), ncol=2, byrow=T)
input.matrix
for (counter in 1:length(universal.set)) {
  if (universal.set[counter] %in% document1.split)
    input.matrix[counter, 1] <- 1
  if (universal.set[counter] %in% document2.split)
    input.matrix[counter, 2] <- 1
}
input.matrix

# Minhashing example:
input.matrix <- matrix(c(1,1,0,0,0,1,1, 0,0,1,1,1,0,0,  1,0,0,0,0,1,1,  0,1,1,1,1,0,0), ncol=4, byrow=F)
input.matrix
input.matrix[7,1:4]
random.permutation1 <- c(3,4,7,6,1,2,5) # so, row 5 of the input matrix is the first
random.permutation2 <- c(4,2,1,3,6,7,5) # so, row 3 of the input matrix is the first
random.permutation3 <- c(1,3,7,6,2,5,4) # so, row 1 of the input matrix is the first
# Can also do
sample(1:7)
# to get a random permutation

DoMinhash <- function(input.matrix, random.permutation) {
  minhash <- rep(0, ncol(input.matrix))
  counter <- 0
  
  while (0 %in% minhash == T) {
    counter <- counter + 1
    which.row <- which(random.permutation == counter)
    row <- input.matrix[which.row, ]
    for (counter2 in 1:length(minhash))
      if (row[counter2] == 1 & minhash[counter2] == 0)
        minhash[counter2] <- counter
  }
  
  return (minhash)
}

sig1 <- DoMinhash(input.matrix, random.permutation1)
sig2 <- DoMinhash(input.matrix, random.permutation2)
sig3 <- DoMinhash(input.matrix, random.permutation3)
sig.matrix <- matrix(c(sig1, sig2, sig3), ncol=4, byrow=T)
sig.matrix

input.matrix <- matrix(c(1,1,0,0,0,1,1,0,1,0,1,0,0,0,0,1,0,1,0,0,1,0,1,0,0,1,1,1,1,0,1,
                         1,0,0,0,1,0,0,1,1,0,0,0,1,1,1,1,0,1,1,1,0,1,0,1,1,0,1,1,0,0,1,
                         1,1,0,0,0,1,1,0,1,0,1,0,0,0,0,1,0,1,0,0,1,0,1,0,0,0,1,1,1,0,1,
                         1,0,0,0,1,0,0,1,1,0,0,0,1,1,0,1,0,1,1,1,0,1,0,1,1,1,1,1,0,0,1), ncol=4, byrow=F)

n <- nrow(input.matrix)
m.cols <- ncol(input.matrix)
n.runs <- 250
sig.matrix <- matrix(rep(0, m.cols*n.runs), ncol=m.cols, byrow=F)
for (counter in 1:n.runs) {
  random.permutation <- sample(1:n)
  sig.matrix[counter, ] <- DoMinhash(input.matrix, random.permutation)
    
}
sig.matrix
image(t(sig.matrix), xaxt="n", yaxt="n", main="Signature Matrix", cex.main=1)

# Signature similarity:
# Example: Col1 and col3:
col1_3 <- sig.matrix[, c(1,3)]
sig.sim1_3 <- length(which(apply(col1_3, 1, function(x) x[1] == x[2])) == TRUE) / nrow(sig.matrix) # Equal sig's / total sig's
sig.sim1_3
# Example: Col2 and col3:
col2_3 <- sig.matrix[, c(2,3)]
sig.sim2_3 <- length(which(apply(col2_3, 1, function(x) x[1] == x[2])) == TRUE) / nrow(sig.matrix) # Equal sig's / total sig's
sig.sim2_3
# Example: Col1 and col2. NOTE: Col 1 and col 2 has NO common rows, thus sig.sim1_2 = 0:
col1_2 <- sig.matrix[, c(1,2)]
sig.sim1_2 <- length(which(apply(col1_2, 1, function(x) x[1] == x[2])) == TRUE) / nrow(sig.matrix) # Equal sig's / total sig's
sig.sim1_2
col2_4 <- sig.matrix[, c(2,4)]
sig.sim2_4 <- length(which(apply(col2_4, 1, function(x) x[1] == x[2])) == TRUE) / nrow(sig.matrix) # Equal sig's / total sig's
sig.sim2_4
plot(c(sig.sim1_3, sig.sim2_3, sig.sim1_2, sig.sim2_4), col=c(2:5), pch=19, ylab="Signature similarity",
     ylim=c(0, 1))

# 3) LSH
# http://www.mit.edu/~andoni/LSH/
# http://en.wikipedia.org/wiki/Locality-sensitive_hashing

# Hashing into buckets example:
input.matrix <- matrix(c(1,0,1,1,0,  0,1,1,0,1), ncol=2, byrow=F)

HashIntoBuckets <- function(input.matrix) {
  hx <- function(x) return (x %% 5)
  gx <- function(x) return (((2 * x) + 1) %% 5)
  n <- nrow(input.matrix)
  sig1 <- c(Inf, Inf)
  sig2 <- c(Inf, Inf)
  final.sig1 <- c(Inf, Inf)
  final.sig2 <- c(Inf, Inf)
  
  for (counter in 1:n) {
    x <- input.matrix[counter, 1]
    if (x == 1) {
      sig1 <- c(hx(counter), gx(counter))
      if (sig1[1] < final.sig1[1])
        final.sig1[1] <- sig1[1]
      if (sig1[2] < final.sig1[2])
        final.sig1[2] <- sig1[2]
    }
    x <- input.matrix[counter, 2]
    if (x == 1) {
      sig2 <- c(hx(counter), gx(counter))
      if (sig2[1] < final.sig2[1])
        final.sig2[1] <- sig2[1]
      if (sig2[2] < final.sig2[2])
        final.sig2[2] <- sig2[2]
    }
  }
  
  return (c(final.sig1, final.sig2))
}

HashIntoBuckets(input.matrix) # sig1=(1,2), sig2=(0,0)

input.matrix <- matrix(c(1,1,0,0,0,1,1, 1,0,1,1,1,0,0,  1,0,0,0,0,1,1,  1,0,1,1,1,0,0), ncol=4, byrow=F)
input.matrix2 <- input.matrix[, 1:2]
input.matrix2 <- input.matrix[, c(2,4)]
# Test for type a(1,1), b(1,0) or c(0,1) rows. (type d = (0,0)):
type.a.row <- length(which(apply(input.matrix2, 1, function(x) x[1] == 1 & x[2] == 1) == TRUE))
type.b.row <- length(which(apply(input.matrix2, 1, function(x) x[1] == 1 & x[2] == 0) == TRUE))
type.c.row <- length(which(apply(input.matrix2, 1, function(x) x[1] == 0 & x[2] == 1) == TRUE))
type.a.row / (type.a.row + type.b.row + type.c.row)
# NOTE: This gives a Jaccard similarity of 1 if the rows are equal, or lower if they are not
#       This is the same as the probability (over all permutations of the rows) that minhash(col1) == minhash(col2)


# 4) Distance Measures

# Euclidean versus Non-Euclidean:
# A Euclidean space has some number of real-valued dimensions and "dense" points. There is a notion of "average"
# of two points. A Euclidean distance is based on the locations of points in such a space.
# Any other space is Non-Euclidean. Distance measures for non-Euclidean spaces are based on properties of points,
# but not their "location" in a space.

# Distance measure d must satisfy:
# 1) d(x, y) >= 0
# 2) d(x, y) = 0 iff (x = y)
# 3) d(x, y) = d(y, x)
# 4) d(x, y) <= d(x, z) + d(z, y)
# (No. 4 is "triangle inequality": The distance from x to y can not be greater than the distance
# from x to some other point z, and then from z to y)

# Definition for L<2> norm:
# d(x,y) = Square root of the sum of squares of the differences between x and y in each dimension.
# Example:
x <- c(0,0)
y <- c(6,6)
sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2) # = 8.49, see also http://en.wikipedia.org/wiki/Taxicab_geometry

# Definition for L<1> norm: The sum of the differences in each dimension
# (= Manhattan Distance, the distance if you had to travel along coordinates only)
abs(x[1] - y[1]) + abs(x[2] - y[2]) # = 12, see also http://en.wikipedia.org/wiki/Taxicab_geometry

# Definition for L<Inf> norm:
# d(x,y) = The maximum of the differences between x and y in any dimension. NOTE: The max is the
# limit as r goes to <Inf> of the L<Inf> norm: What you get by taking the rth power of the differences,
# summing and taking the rth root.

# Four types of distances measures:
# - Jaccard distance for sets: 1 - Jaccard similarity
#   http://en.wikipedia.org/wiki/Jaccard_index
x <- c(1,2,3,4)
y <- c(1,3,5)
jaccard.dist <- 1 - (length(intersect(x,y)) / length(union(x,y)))
jaccard.dist
# - Cosine distance for vectors: The angle between the vectors
#   http://en.wikipedia.org/wiki/Cosine_similarity
v1 <- c(0,0,1,1,1)
v2 <- c(1,0,0,1,1)
v1 <- c(1,2,3,4,5)
v2 <- c(-1,-2,-3,-4,-5)
v1 <- c(0,0,5,0)
v2 <- c(0,0,-2,5)
v1 <- c(5,0)
v2 <- c(5,5)
cosine.distance <- sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum((v2^2))))
cosine.distance
(1 - cosine.distance) * 180 / pi
# - Edit distance for strings: Number of inserts or deletes to change one string into another
# http://en.wikipedia.org/wiki/Edit_distance
# http://en.wikipedia.org/wiki/Wagner%E2%80%93Fischer_algorithm
# http://wordaligned.org/articles/longest-common-subsequence
# http://www.geeksforgeeks.org/dynamic-programming-set-5-edit-distance/
x <- c("a","b","c","d","e") # NOTE: unlist(strsplit(string, "", length(string))) can be used
y <- c("b","c","d","u","v","e")
LCS <- intersect(x, y) # LCS = longest common substring
edit.distance <- length(x) + length(y) - (2 * length(LCS))
edit.distance
# - Hamming distance for vectors: the number of positions in which they differ
v1 <- c(1,0,1,0,1)
v2 <- c(1,0,0,1,1)
v1 <- c("this", "is", "cool", "stuff")
v2 <- c("this", "is", "cooler", "stuff")
v1 <- c("this", "was", "cool", "stuff")
v2 <- c("was", "this", "cool", "stuff")
v1 <- c("this", "was", "indeed","cool", "stuff") # NOTE: Try setdiff(v1, v2) on this instead
v2 <- c("this", "was", "cool", "stuff")
v1 <- c("this", "was", "cool", "stuff", "indeed") # NOTE: Try setdiff(v1, v2) on this instead
v2 <- c("this", "was", "cool", "stuff")
hamming.distance <- length(which((v1 == v2) == FALSE)) 
hamming.distance

# 5) Nearest Neighbor Learning

# 1-Nearest Neighbor classifier:
# We need 4 things:
# 1) Distance metric (e.g. Euclidean)
# 2) How many neighbors to look at? With 1-NN: One neighbor
# 3) Weighting function (optional, how do we weight the neighbors?)
# 4) How to fit with the local points? For 1-NN: Just predict the same output as the nearest neighbor.
#    For K-NN: Predict the average output among k nearest neighbors.

# TODO with example code: How to find nearest neighbors? One way is to use LSH. 


# 6) Frequent Itemsets

# 1) The Market-Basket model:
# - A large SET of ITEMS, e.g., things sold in a supermarket.
# - A large set of BASKETS, each of which is a small SET of the ITEMS, e.g., the things one customer
#   buys on one day.

# Example: Frequent itemsets:
Items <-c("milk", "coke", "pepsi", "beer", "juice")
support.threshold <- 3 # 3 Baskets
b <- list() # Baskets
n.baskets <- 8
b[[1]] <- c("milk", "coke", "beer","diapers")
b[[2]] <- c("milk", "pepsi", "juice")
b[[3]] <- c("milk", "beer","diapers")
b[[4]] <- c("coke", "juice")
b[[5]] <- c("milk", "diapers","pepsi", "beer")
b[[6]] <- c("diapers","milk", "coke", "beer","juice")
b[[7]] <- c("coke", "beer", "juice","diapers")
b[[8]] <- c("beer", "coke")
# Support for itemset I = the number of baskets containing all items in I
# TODO: Find frequent itemsets (items or sets of items that appear <support.threshold> or more
# times in the baskets 1-8

# Example: An association rule: {m,b}->c
# That is: "Baskets with milk and beer in them also contains coke"
found <- 0
total.found <- 0
items.to.check <- c("milk","beer")
items.to.find <- c(items.to.check,"coke")
items.to.check <- c("beer")
items.to.find <- c(items.to.check,"diapers") # The "beer and diapers" case... :-)
for (counter in 1:n.baskets) {
  result <- (unique(items.to.find %in% b[[counter]]) == TRUE)
  if (length(result) == 1 & result[1] == TRUE)
    found <- found + 1
  result <- (unique(items.to.check %in% b[[counter]]) == TRUE)
  if (length(result) == 1 & result[1] == TRUE)
    total.found <- total.found + 1
}
confidence <- found / total.found
confidence
# The task is: Find all association rules with support >= S and confidence >= C
# We decide the values of S and C

# Find all pairs in an itemset ("n choose 2", use choose(10, 2)):
n <- 10
all.pairs <- (n * (n - 1)) / 2
# For instance, for 4 items, the number of pairs is 6:
# 12 13 14 23 24 34
pairs <- list()
pos <- 1
for (counter1 in 1:(n - 1))
  for (counter2 in (counter1 + 1):n) {
    pairs[[pos]] <- c(counter1, counter2)
    pos <- pos + 1
  }
pairs

# Triangular Matrix approach:
# Find pair {i, j}, where i < j at the position <x>:
# Formula to find position <x>: (i - 1) * (n - (i / 2)) + j - i
n <- 10
i <- 3
j <- 5
(i - 1) * (n - (i / 2)) + j - i
# Found at position 19. Correct. Check position of {3, 5} in list above

# Time it takes to generate pairs is: n^k / factorial(k)
k <- 2
n^k / factorial(k)

# 7) A-Priori algorithm for frequent itemsets

# Good examples: http://en.wikipedia.org/wiki/Apriori_algorithm


# ---------------------------------------------------------------------------------------------------------------
# Quiz 1 Week 2, LSH

# Question 1)
str1 <- "he"
str2 <- "she"
str3 <- "his"
str4 <- "hers"
FindEditDistance <- function(x, y) {
  if (length(x) == 1)
    x <- unlist(strsplit(x, ""))
  if (length(y) == 1)
    y <- unlist(strsplit(y, ""))
  LCS <- intersect(x, y) # LCS = longest common substring. TODO: Can NOT use intersect()!
  edit.distance <- length(x) + length(y) - (2 * length(LCS))
  return (edit.distance)
}
FindEditDistance(str1, str2) # can not use these here...

# he->she: Add s: 1
# she->he: Remove h: 1
# he->his: Remove e: 1, add i: 2, add s: 3
# his->he: Remove i: 1, remove s:2, add e: 3
# he->hers: Add r: 1, add s: 2
# hers->he: Remove r: 1, remove s: 2
# she->his: Remove s: 1, remove e: 2, add i: 3, add s: 4
# his->she: Remove h: 1, remove i: 2, add h: 3, add e: 4
# she->hers: Remove s: 1, add r: 2, add s: 3
# hers->she: Remove s: 1, remove r: 2, add s: 3 
# his->hers: Remove i: 1, add e: 2, add r: 3
# hers->his: Remove r: 1, remove e: 2, add i: 3

# Answer: There is one pair at distance 4

# Question 2)
# Perform a minhashing of the data, with the order of rows: R4, R6, R1, R3, R5, R2. Which of the following is
# the correct minhash value of the stated column? Note: we give the minhash value in terms of the original
# name of the row, rather than the order of the row in the permutation. These two schemes are equivalent,
# since we only care whether hash values for two columns are equal, not what their actual values are.
q2 <- matrix(c(0,1,0,0,1,0,  1,0,1,0,0,1,  1,1,0,1,1,0,  0,1,1,0,0,0), ncol=4, byrow=F)
q2
random.permutation <- c(3,6,4,1,5,2) # so, row 3 of the input matrix is the first
DoMinhash(q2, random.permutation) # 5 2 1 4
# Answer: The minhash value for C3 is R4

# Question 3) 

# Question 4)
document1 <- "ABRACADABRA"
document2 <- "BRICABRAC"
k <- 2
k.shingles.doc1 <- unique(sapply(1:(nchar(document1) - (k - 1)), function(x) substr(document1, x, x + (k - 1))))
k.shingles.doc1
k.shingles.doc2 <- unique(sapply(1:(nchar(document2) - (k - 1)), function(x) substr(document2, x, x + (k - 1))))
k.shingles.doc2
intersect(k.shingles.doc1, k.shingles.doc2)
jaccard.sim <- length(intersect(k.shingles.doc1, k.shingles.doc2)) / length(c(k.shingles.doc1, k.shingles.doc2))
jaccard.sim
# Answer: There are 5 shingles in common.

# Question 5) 

# Question 6)
# For this problem, you should work out the conditions under which a point will be assigned to (0,0) when the
# L1 norm is used, but assigned to (100,40) when the L2 norm is used. Identify one of those points from the
# list below.
p1 <- c(52,13)
p2 <- c(55,5)
p3 <- c(51,15)
p4 <- c(57,5)
p.check.1 <- c(0,0)
p.check.2 <- c(100,40)

# Results L1 norm:
MyManhattanDistance(p1, p.check.1) # 65
MyManhattanDistance(p2, p.check.1) # 60
MyManhattanDistance(p3, p.check.1) # 66
MyManhattanDistance(p4, p.check.1) # 62 *

MyManhattanDistance(p1, p.check.2) # 75
MyManhattanDistance(p2, p.check.2) # 80
MyManhattanDistance(p3, p.check.2) # 74
MyManhattanDistance(p4, p.check.2) # 78

# Results L2 norm:
MyEuclideanDistance(p1, p.check.1) # 53.60
MyEuclideanDistance(p2, p.check.1) # 55.23
MyEuclideanDistance(p3, p.check.1) # 53.16
MyEuclideanDistance(p4, p.check.1) # 57.22

MyEuclideanDistance(p1, p.check.2) # 55.07
MyEuclideanDistance(p2, p.check.2) # 57.01
MyEuclideanDistance(p3, p.check.2) # 55.01
MyEuclideanDistance(p4, p.check.2) # 55.44 *

# Answer: p4 (57,5)

# ----------------------------------------------------------------------------------------------------------------
# Week 3

# 1) Community detection in graphs
# --------------------------------
# Think "overlapping tiles", where the tile overlaps have a greater edge density. Separate "tiles" into communities.
M <- matrix(sample(c(0,1), 100, replace=T), ncol=10)
M %*% M
image(M %*% M, axes=F)
axis(1, at=seq(0,1,length.out=ncol(M)), labels=1:ncol(M), las=1, tick=F, cex.axis=.7)
axis(2, at=seq(0,1,length.out=ncol(M)), labels=1:ncol(M), las=2, tick=F, cex.axis=.7)
g <- graph.adjacency(M, mode="directed")
ret <- CreateTransitionMatrix(M, do.epsilon=F)
ret
plot(g, main="Random Graph", vertex.size=(ret$page.rank*200))
PlotResult(ret)

# 2) The Community-Affiliation Graph Model (bipartite graphs, partitions)
#    http://infolab.stanford.edu/~crucis/pubs/paper-agmfit.pdf
#    http://en.wikipedia.org/wiki/Bipartite_graph
# ------------------------------------------------------------------------
# The network generating model: Given a set of nodes, how do communities "generate" edges of the network?
# AMG - Affiliation Graph Model:
# Nodes V, Communities C, Memberships M. Model is: B(V, C, M, {p<c>}) for graphs.
# p<c> is the probability per Community that a set of edges belongs to community c.

# Membership Strength: F<uA> - the strength (F) of node u's membership to community A
# F<uA> = 0: No membership to community A
# P<a>(u,v) = 1 - exp(-F<uA> * F<vA>)
par(mfrow=c(3,1))
par(mar=c(2.5,4.2,1.8,.8))
n <- 20
MS <- list()
MS$V <- 1:n
MS$Fa <- runif(n, .25) # Membership to community A strength
MS$Fb <- runif(n, .25) # Membership to community B strength
Pa <- numeric(0)
Pb <- numeric(0)
for (counter in 1:(n-1)) {
  Pa[counter] <- 1 - exp((-MS$Fa[counter]) * MS$Fa[counter + 1])
  Pb[counter] <- 1 - exp((-MS$Fb[counter]) * MS$Fb[counter + 1])
}
Pa[n] <- 1 - exp((-MS$Fa[n]) * MS$Fa[1]) # Last one...
Pb[n] <- 1 - exp((-MS$Fb[n]) * MS$Fb[1])
MS$Pa <- Pa # The probs that a pair of nodes u,v belongs to community A
MS$Pb <- Pb # The probs that a pair of nodes u,v belongs to community B

# 3) From AGM to BigCLAM
# ----------------------
membership.strength.matrix <- matrix(rep(0, n*2), ncol=2)
membership.strength.matrix[,1] <- MS$Pa
membership.strength.matrix[,2] <- MS$Pb
# Example: u = c(0, 1.2, 0, 0.2), v = c(0.5, 0, 0, 0.8)
# c(0, 1.2, 0, 0.2) %*% c(0.5, 0, 0, 0.8)
Pab <- numeric(0) # The probability that at leat one common Community links nodes a,b:
for (counter in 1:(n-1)) {
  dot.product <- membership.strength.matrix[counter,] %*% membership.strength.matrix[counter + 1,] 
  Pab[counter] <- 1 - exp(-dot.product)
}
MS$Pab <- Pab
MS$Pab

plot(MS$Fa, type="o", ylim=c(0, 1), main="AFM, community A", cex.main=1, cex.lab=.8, cex.axis=.7,
     xlab="")
grid()
lines(MS$Pa, type="o", col="blue")
abline(h=.5, col="gray")
plot(MS$Fb, type="o", ylim=c(0, 1), main="AFM, community B", cex.main=1, cex.lab=.8, cex.axis=.7,
     xlab="")
grid()
lines(MS$Pb, type="o", col="green4")
abline(h=.5, col="gray")
plot(MS$Pab, col="blue", type="o", main="AFM, P(C<ab>)", cex.main=1, cex.lab=.8,
     cex.axis=.7, xlab="", xlim=c(1, n))
grid()
par(mfrow=c(1,1))

# 4) Mining Data Streams
# ----------------------
# Sliding Windows:
par(mar=c(4.8,4.5,2,.8))
n <- 50
window.size <- 5
window.mean <- 0
window.means <- numeric(0)
elements <- round(runif(n, min=1, max=100))

for (counter in 1:n) {
  if (counter == window.size) # Average all elements
  {
    window.mean <- sum((elements[(counter - (window.size - 1)):counter]) / window.size)
    window.means <- c(window.means, window.mean)
  } else if (counter > window.size) {
    # New value i arrives in the stream: Just add (i - j) / N to the old average, where j is the oldest value
    window.mean <- window.mean + sum((elements[counter] - elements[counter - window.size]) / window.size)
    window.means <- c(window.means, window.mean)
  }
}

plot(elements, col="red", type="o", main="Sliding Window", cex.lab=.8, cex.axis=.7, cex.main=1)
lines(window.size:n, window.means, type="o", col="blue")

# 5) Defining the graph
# ---------------------

# Adjacency matrix:
adj.matrix <- matrix(c(0,1,1,0,1,0,  1,0,1,0,0,0,  1,1,0,1,0,0,  0,0,1,0,1,1,
                       1,0,0,1,0,1,  0,0,0,1,1,0), ncol=6, byrow=T)
adj.matrix

# Degree matrix:
degree.matrix <- adj.matrix %*% adj.matrix # Get diag degrees vector from matrix directly
degree.matrix <- adj.matrix %*% t(adj.matrix)
degree.vector <- diag(degree.matrix)
degree.matrix <- diag(degree.vector)
degree.matrix
g <- graph.adjacency(adj.matrix, mode="directed")
V(g)$name <- c("A","B","C","D","E","F")
plot(g, main="Adjacency")
edges <- diag(adj.matrix %*% t(adj.matrix))
edges/sum(edges) # This gives the same result as in video 2-8 PageRank Power Iteration
ret <- CreateTransitionMatrix(adj.matrix, V(g)$name)
PlotResult(ret)

# Laplacian matrix:
# http://en.wikipedia.org/wiki/Laplacian_matrix
laplacian.matrix <- degree.matrix - adj.matrix
laplacian.matrix


# -----------------------------------------------------------------------------------------------------------
# Week 4

# 1) Recommender Systems
# ----------------------

# Recommender Systems are important for online retailers, because the online retailer can stock so many more
# items (no need for renting physical shelfspace at a store). Therefore, it is much more difficult for a customer
# to find all the items available online (long tail phenomena).
# TODO: check: wired.com/wired/archive/12.10/tail.html

# Formal model: Utility function u: C x S -> R (C = set of customers, S = set of items, R = ordered set of ratings)

# Three approaches to recommender systems:
# - Content-based: Recommend items to customer X based on previous purchases of customer X
#   1) For each item, create an Item Profile (properties/features of the item). For text features, the profile
#   is the set of "important" words in the item/document (use TF-IDF to find them)
#   2) For each user, create a User Profile. Simple type: User has rated items with profiles i<1..n>. The user profile
#   is then a (weighted) average of rated item profiles i<1..n>.
#   For each item y, use cosine distance to find the relationship between the user profile x and the item profile y.
#   Recommend the items y with the smallest cosine distance to user profile x to user X.
#   NOTE: Content-based approach is not a very popular technique, many limitations.

# - User-User Collaborative Filtering: Find user x's "neighborhood" N (other users that like the same as user x, that is,
#   their ratings are similar as user x). Estimate x's ratings based on ratings of users in N.
ratings.matrix <- matrix(c(4,0,0,5,1,0,0,  5,5,4,0,0,0,0,  0,0,0,2,4,5,0,  0,3,0,0,0,0,3), ncol=7, byrow=T)
rownames(ratings.matrix) <- c("A","B","C","D")
colnames(ratings.matrix) <- c("HP1","HP2","HP3","TW","SW1","SW2","SW3")
ratings.matrix

# Option 1: Try Jaccard similarity:
# Does not give a good result, ignores the ratings values themselves (Ratings for A-B is more similar
# than A-C, but gets a lower Jaccard sim).
row.A <- 1
row.B <- 3
intersect.AB <- 0
for (counter in 1:ncol(ratings.matrix))
  if ((ratings.matrix[row.A,counter] > 0 & ratings.matrix[row.B,counter] > 0) == T)
    intersect.AB <- intersect.AB + 1
union.AB <- 0
for (counter in 1:ncol(ratings.matrix))
  if ((ratings.matrix[row.A,counter] > 0 | ratings.matrix[row.B,counter] > 0) == T)
    union.AB <- union.AB + 1
sim.AB <- intersect.AB / union.AB

# Option 2: Try Cosine Similarity (sim(A,B) = cos(r<A>,r<B>)):
# We see that sim(A,B) > sim(A,C), but not by much. Problem: Treats missing ratings (0) as negative ratings, but we
# do not know if the user would have rated low or high on the missing ratings.
a <- ratings.matrix[1,]
b <- ratings.matrix[2,]
round((a %*% b) / sqrt((a %*% a) * (b %*% b)), 2) # or use: sum(a * b), sum(a * a), etc.
a <- ratings.matrix[1,]
b <- ratings.matrix[3,]
round((a %*% b) / sqrt((a %*% a) * (b %*% b)), 2) # or use: sum(a * b), sum(a * a), etc. Or: {base} crossprod(a,b)

# Option 3: Try Centered Cosine Similarity: Normalize ratings by subtracting row mean
# NOTE: Each row sums to zero after normalizing, the ratings have been centered.
ratings.matrix.2 <- matrix(0, ncol=7, nrow=4)
rowmeans.A <- sum(ratings.matrix[1,]) / length(which(ratings.matrix[1,] > 0))
rowmeans.B <- sum(ratings.matrix[2,]) / length(which(ratings.matrix[2,] > 0))
rowmeans.C <- sum(ratings.matrix[3,]) / length(which(ratings.matrix[3,] > 0))
rowmeans.D <- sum(ratings.matrix[4,]) / length(which(ratings.matrix[4,] > 0))
for (counter in 1:ncol(ratings.matrix)) {
  ratings.matrix.2[1,counter] <- ifelse(ratings.matrix[1,counter] > 0, ratings.matrix[1,counter] - rowmeans.A, 0)
  ratings.matrix.2[2,counter] <- ifelse(ratings.matrix[2,counter] > 0, ratings.matrix[2,counter] - rowmeans.B, 0)
  ratings.matrix.2[3,counter] <- ifelse(ratings.matrix[3,counter] > 0, ratings.matrix[3,counter] - rowmeans.C, 0)
  ratings.matrix.2[4,counter] <- ifelse(ratings.matrix[4,counter] > 0, ratings.matrix[4,counter] - rowmeans.D, 0)
}
ratings.matrix.2
# Now, try Cosine similarity on the normalized/centered matrix:
a <- ratings.matrix.2[1,]
b <- ratings.matrix.2[2,]
round((a %*% b) / sqrt((a %*% a) * (b %*% b)), 2) # or use: sum(a * b), sum(a * a), etc.
a <- ratings.matrix.2[1,]
b <- ratings.matrix.2[3,]
round((a %*% b) / sqrt((a %*% a) * (b %*% b)), 2) # or use: sum(a * b), sum(a * a), etc. Or: {base} crossprod(a,b)
# We now see that the (A,B) similarity is better than (A,C). Missing ratings are treated as "average". Handles both
# "tough raters" and "easy raters" better. NOTE: Pearson Correlation = Centered Cosine Similarity!

# - Item-Item Collaborative Filtering: Find item x's "neighborhood" N (other items that a number of users have rated similarly):
ratings.matrix <- matrix(c(1,0,3,0,0,5,0,0,5,0,4,0,  0,0,5,4,0,0,4,0,0,2,1,3,  2,4,0,1,2,0,3,0,4,3,5,0,
                        0,2,4,0,5,0,0,4,0,0,2,0,  0,0,4,3,4,2,0,0,0,0,2,5,  1,0,3,0,3,0,0,2,0,0,4,0), ncol=12, byrow=T)
rownames(ratings.matrix) <- c("A","B","C","D","E","F")
colnames(ratings.matrix) <- c(1:12)
ratings.matrix
rowmeans.A <- sum(ratings.matrix[1,]) / length(which(ratings.matrix[1,] > 0))
rowmeans.B <- sum(ratings.matrix[2,]) / length(which(ratings.matrix[2,] > 0))
rowmeans.C <- sum(ratings.matrix[3,]) / length(which(ratings.matrix[3,] > 0))
rowmeans.D <- sum(ratings.matrix[4,]) / length(which(ratings.matrix[4,] > 0))
rowmeans.E <- sum(ratings.matrix[5,]) / length(which(ratings.matrix[5,] > 0))
rowmeans.F <- sum(ratings.matrix[6,]) / length(which(ratings.matrix[6,] > 0))
ratings.matrix.2 <- matrix(0, ncol=12, nrow=6)
for (counter in 1:ncol(ratings.matrix)) {
  ratings.matrix.2[1,counter] <- ifelse(ratings.matrix[1,counter] > 0, ratings.matrix[1,counter] - rowmeans.A, 0)
  ratings.matrix.2[2,counter] <- ifelse(ratings.matrix[2,counter] > 0, ratings.matrix[2,counter] - rowmeans.B, 0)
  ratings.matrix.2[3,counter] <- ifelse(ratings.matrix[3,counter] > 0, ratings.matrix[3,counter] - rowmeans.C, 0)
  ratings.matrix.2[4,counter] <- ifelse(ratings.matrix[4,counter] > 0, ratings.matrix[4,counter] - rowmeans.D, 0)
  ratings.matrix.2[5,counter] <- ifelse(ratings.matrix[5,counter] > 0, ratings.matrix[5,counter] - rowmeans.E, 0)
  ratings.matrix.2[6,counter] <- ifelse(ratings.matrix[6,counter] > 0, ratings.matrix[6,counter] - rowmeans.F, 0)
}
ratings.matrix.2
cosine.sim <- numeric(0)
for (counter in 1:nrow(ratings.matrix.2)) {
  a <- ratings.matrix.2[1,]
  b <- ratings.matrix.2[counter,]
  cosine.sim <- c(cosine.sim, round((a %*% b) / sqrt((a %*% a) * (b %*% b)), 2))
}
cosine.sim # We see that row 3 and row 6 has the best similarity to row 1, so we choose these:
sim1_3 <- cosine.sim[3]
sim1_6 <- cosine.sim[6]
# We want to impute the missing rating for user 5 on movie 1: Find the ratings for user 5 on movies 3 and 6:
rating1_5 <- ((sim1_3 * ratings.matrix[3,5]) + (sim1_6 * ratings.matrix[6,5])) / (sim1_3 + sim1_6)
round(rating1_5, 1) # 2.6 - correct!
# NOTE: Item-item outperforms user-user in many use cases, even if they are theoretically similar. Items are "simpler"
# than users; they usually belong to fewer categories, whereas users often have very varied tastes

# - TODO: Latent factor based



# - TODO: UV decomposition
M <- matrix(c(5,2,4,4,3,3,1,2,4,1,2,0,3,1,4,2,5,4,3,5,4,4,5,4,0), ncol=5, byrow=T)
M

# - Dimensionality reduction: Rank of a matrix
A <- matrix(c(1,2,1,-2,-3,1,3,5,0), ncol=3, byrow=T)
A
# Slide says: We can write A as two "basis" vectors: [1,2,1][-2,-3,1] and new coordinates [1,0][0,1][1,1]
A <- matrix(c(1,2,1,-2,-3,1), ncol=3, byrow=T)
# But shouldn't that be [-2,-3,1][3,5,0]:
A <- matrix(c(-2,-3,1,3,5,0), ncol=3, byrow=T)
A
c(1,1) %*% A # Row 1
c(1,0) %*% A # Row 2
c(0,1) %*% A # Row 3  if ((sum(x1) == sum(old.x1)) | (counter == 1000))
break


# - SVD
# Example: Ratings, users to movies, rows=users, cols=movies
A <- matrix(c(1,1,1,0,0,  3,3,3,0,0,  4,4,4,0,0,  5,5,5,0,0,  0,2,0,4,4,  0,0,0,5,5,  0,1,0,2,2),
            ncol=5, byrow=T)
A
# A = U*Sigma*t(V)
SVD <- svd(A)
plot(SVD$d, pch=19, type="b", col="blue")
# SVD$d diagonal matrix: Sorted after "strength". "Sci-fi strength" is higher than "romance strength"
# The first two values are the important ones, the rest is "noise"
# U is "user to concept" similarity matrix
# V is "movie to concept" similarity matrix

# TEST: Find the eigenvector and eigenvalue of a matrix by Power Iteration (book p.400)
# Only works for square matrices?
# http://ergodic.ugr.es/cphys/LECCIONES/FORTRAN/power_method.pdf
# http://www.uio.no/studier/emner/matnat/ifi/INF-MAT4350/h08/undervisningsmateriale/chap20slides.pdf
M <- matrix(c(3,2,2,6), ncol=2, nrow=2, byrow=T)
M

DoPowerIteration <- function(M) {
  if (ncol(M) != nrow(M)) # If not quadratic matrix, multiply
    M <- M %*% t(M)
  
  x1 <- rep(1, ncol(M)) # Init
  result <- M %*% x1
  counter <- 0
  old.x1 <- 0

  while (T) {
    Frobenius.norm <- sqrt(sum(result^2)) # NOTE: e.g.: sqrt(5^2 + 8^2)
    result <- (result / Frobenius.norm)
    old.x1 <- x1
    x1 <- result
    result <- M %*% x1
  
    if ((sum(x1) == sum(old.x1)) | (counter == 1000))
      break
    
    counter <- counter + 1
  }

  # Compute the principal eigenvalue
  principal.eigenvalue <- t(x1) %*% M %*% x1

  ret <- list()
  ret$eigenvector <- x1
  ret$principal.eigenvalue <- principal.eigenvalue
  return (ret)
}

ret <- DoPowerIteration(M)
# NOTE:
M %*% ret$eigenvector
# is the same as:
ret$principal.eigenvalue %*% t(ret$eigenvector)
# http://en.wikipedia.org/wiki/Eigenvalues_and_eigenvectors

# Get second eigenpair (book p.408):
M.star <- M - (as.numeric(ret$principal.eigenvalue) * (ret$eigenvector %*% t(ret$eigenvector)))
M.star # OK. Book has error in first number of M.star matrix (2.6 instead of correct 1.6)
ret2 <- DoPowerIteration(M.star)
ret$eigenvector %*% t(ret2$eigenvector) # Should be 0?? 

DoPowerIteration(A)

# -----------------------------------------------------------------------------------------------------------
# Week 5

# Clustering
# 1) Agglomerative approach (bottom up): Start with each point as a cluster, recursively combine with nearest points
# 2) Divisive approach (top down): Start with all points as one cluster, recursively split it

# Euclidean distance: centroid (average between two or more points)
# Non-euclidean distance: clustroid: (data)point closest to other points
# TEST: find the point to use as the "centroid" in the clustroid: The point that's nearest to the other points:
GetClustroid <- function(n, distance.type=1, point.numbers=F, clustroid.numbered=T, show.centroid=T) {
  op <- par()
  par(mar=c(5,5,2.3,1))
  n <- n * 2
  p <- matrix(runif(n), ncol=2, byrow=T)
  p
  distances <- list()
  min_temp_dist <- 0.0
  old_temp_dist <- Inf
  
  for (counter in 1:nrow(p)) {
    centroid <- c(p[counter,1],p[counter,2])
    temp_dist <- 0.0
    
    for (counter2 in 1:nrow(p)) {
      if (counter2 != counter) {
          temp_dist <- temp_dist + sqrt((c(p[counter2,1],p[counter2,2]) - centroid) %*%
                                          (c(p[counter2,1],p[counter2,2]) - centroid))
      }
    }

    # TODO:
    # if (distance.type == 2) # distance.type=2: Smallest average distance to other points
    # if (distance.type == 3) # distance.type=3: Smallest sum of squares of distances to other points
    
    if (temp_dist < old_temp_dist) {
      min_temp_dist <- c(p[counter,1],p[counter,2])
      old_temp_dist <- temp_dist
    }
    distances[[counter]] <- temp_dist
  }
  
  # Get centroid too
  centroid <- c(mean(p[,1]), mean(p[,2]))
  
  plot(p, pch=21, bg="wheat", col="goldenrod", main="Clustroid test", cex.main=1, cex.lab=.7,
       cex.axis=.7, xlab="X", ylab="Y", cex=3.5, xlim=c(min(p[,1])-.1, max(p[,1])+.1),
       ylim=c(min(p[,2])-.1, max(p[,2])+.1))
  min_points <- which(unlist(distances) == min(unlist(distances)))

  for (counter in min_points)
    points(p[counter,1], p[counter,2], pch=21, bg="cyan", col="darkcyan", cex=3.5)
  
  if (show.centroid == T) # Plot centroid too
    points(centroid[1], centroid[2], col="red", cex=1.5, pch="+")
  
  if (point.numbers == T)
    text(x=p[,1], y=p[,2],1:nrow(p), cex=.6) # , pos=3, offset=-.1)
  else # Just plot clustroid number
    for (counter in min_points)
      text(x=p[counter,1], y=p[counter,2], counter, cex=.6)
  
  par <- op
  return (unlist(distances))
}

n <- 6
GetClustroid(n, point.numbers=T)

# BFR (Bradley-Fayyad-Reina) Algorithm
# Assumes each cluster is normally distributed around a centroid in Euclidean space.
# All points do not fit in main memory, so need to keep statistics on the points we discard.
# Keep track of these points:
# - 1) Discard set (DS), 2) Compression set (CS), Retained set (RS)
# DS: N (number of points) and SUM (vector, sum of the coordinates of the points in the <i>th dimension),
#     SUMSQ (sum of squares of coordinates in the <i>th dimension)
# Size is: 2d+1 (d = number of dimensions)
ds.vector <- numeric(0)
cluster.x <- rnorm(100, sd=3)
cluster.y <- rnorm(100, sd=3)
par(mfrow=c(1,2))
plot(cluster.x, cluster.y, main="Discard set", pch=19, col="gray", cex.main=.8, cex.lab=.7, cex.axis=.7)
n <- length(cluster.x) # N
sum.x <- sum(cluster.x)
sum.y <- sum(cluster.y)
sumsq.x <- sum(cluster.x^2)
sumsq.y <- sum(cluster.y^2)
ds.vector <- c(n, sum.x, sum.y, sumsq.x, sumsq.y)
ds.vector
# Get centroid:
centroid.x <- (sum.x / n)
centroid.y <- (sum.y / n)
points(centroid.x, centroid.y, col="red", pch=19)
# Variance of a cluster's DS in <i>th dimension:
var.ds.x <- (sumsq.x / n) - (sum.x / n)^2
var.ds.y <- (sumsq.y / n) - (sum.y / n)^2
# Standard deviation is sqrt of that:
sd.ds.x <- sqrt(var.ds.x)
sd.ds.y <- sqrt(var.ds.y)
# To find out if a new point is close enough to an existing centroid (and thus can go into the DS set after being
# added to the current cluster), we use the Mahalanobis distance:
# The Mahalanobis distance should be smaller than a threshold we set.
new.point.x <- 4
new.point.y <- -6
# Get the normalized distance for the d dimensions
normalized.distance <- numeric(0)
normalized.distance[1] <- (new.point.x - centroid.x) / sd.ds.x
normalized.distance[2] <- (new.point.y - centroid.y) / sd.ds.y
mahalanobis.distance <- sqrt(sum(normalized.distance^2))
mahalanobis.distance
points(mahalanobis.distance, mahalanobis.distance, col="green2", pch=19)
hist(cluster.y, col="wheat", main="Mahalanobis distance", cex.main=.8, cex.lab=.7, cex.axis=.7)
abline(v=mahalanobis.distance, col="green2", lwd=2)
par(mfrow=c(1,1))

# Online algorithms: Computational advertising
# Uses bipartite graphs (Spark??), example: Assign a task to an available server.
# Which to choose for best overall utilization? Should be max tasks matched to available servers.
# Good polynomial-time offline algorithm: http://en.wikipedia.org/wiki/Hopcroft%E2%80%93Karp_algorithm
# Analyzing the greedy matching algorithm:
M.greedy <- 12
M.opt <- 20

# The Adwords problem (Performance-based advertising. Started by Overture, 2000, bought by Yahoo. Google created
# a similar system called AdWords in 2002)
# Advertisers bid on keywords. When a keyword is searched, the highest bidder "wins" that keyword--their ad
# is shown on the site. Advertiser is charged only if the ad is clicked on.
# What ads to show for a given query?
ads <- data.frame(advertiser=c("A","B","C"), bid=c(1.00, 0.75, 0.50), ctr=c(1.0, 2.0, 2.5))
# CTR = Click-through rate
ads$expected.revenue <- ads$bid * ads$ctr
ads

# The Balance algorithm
# For each query, pick the advertiser with the largest unspent budget.

# -----------------------------------------------------------------------------------------------------------
# Week 6

# Support Vector Machines:
# Prediction = sign(w * x + b)
# "Confidence of prediction on outcome var y<i>:
# y<i> = (w * x + b) * y<i>
# Maximize the margin:
# We want to find max<Gamma> such that: <All>i, y<i> * (w * x + b) >= <Gamma>

# Decision trees:
