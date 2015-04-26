# http://www.learner.org/courses/mathilluminated/interactives/network/
# http://cran.r-project.org/web/packages/igraph/igraph.pdf
# http://www.inside-r.org/packages/cran/igraph/docs
# http://igraph.wikidot.com/community-detection-in-r
# http://igraph.org/r/
# https://griffsgraphs.wordpress.com/tag/gephi/
# https://ludicanalytics.wordpress.com/tag/gephi/

#############################
# Week 1
#############################

# 1.1
# Calculate networks
nodes <- 4 # Also called: vertices
links <- (nodes * (nodes - 1)) / 2 # Also called: edges
links
networks <- nodes**links
networks

# Cycles: End up at the starting node
# Path: Only unique nodes, can not cross the same node twice
# Walk: Can use same node twice or more
# Geodesic: Shortest path from one node to another

# If no nodes are self-connected, the matrix diagonal (upper left to lower right) is all zeros

# 1.4
# Counting walks (end up at the starting node):
A <- matrix(c(0,1,0,1,1,0,0,1,0,0,0,1,1,1,1,0), nrow=4, ncol=4)
A

# Number of walks of length 2
A %*% A
# (1,1)=2 here, because you can walk from 1 to 2 and back, and from 1 to 4 and back

# Number of walks of length 2
A %*% A %*% A

# Network diameter (TODO: NEED TO UNDERSTAND CONCEPT BETTER!):
# http://en.wikipedia.org/wiki/Graph_diameter
# http://en.wikipedia.org/wiki/Network_science#Diameter_of_a_network
# Circle of n nodes: n/2 or (n-1)/2 (if odd number of nodes)
# Tree of nodes: K levels has n = (2^(K-1) - 1) nodes,
# so, K = log2(n + 1) - 1. Diameter is 2K
levels <- 3 # Not counting top node in tree (4 levels in tree counting top node)
nodes <- (2**(levels + 1)) - 1
nodes
diameter <- 2 * log2(nodes + 1)
diameter # NOTE: diameter is 2K
# IMPORTANT NOTE: diameter = number of *edges* in longest shortest path, NOT nodes!
# Other definition: The diameter is the average distance betwen pairs. It measures how
# near or far typical individuals are from each other.

# Neighborhood of node i:
# neigborhood.of.node.i.in.network.g = { j | ij in g } 
# neigborhood.of.node.i.in.network.g = The set of other nodes that i is linked to in g

# Degree of node i:
# di = #Ni(g)
# di = Counting the number of neighbors a given node i has in network g
#      Can do: Count the number of edges connected to that node/vertex

# Theorem of network structure (distance and diameter both converge towards 1??):
average.path.length.and.diameter <- log(nodes) / log(diameter)
average.path.length.and.diameter
# Not sure about this...

degree <- 4 # Node i has 4 neighbors
# Moving out l number of links from root in each direction:
l <- 10
result <- (degree * (degree - 1))**l
result
log(result) / log(degree)

# "Small Worlds, Six degree of separation":
nodes.population <- 6700000000 # Earth total population
degree.friends.and.relatives <- 44 # How many people one interacts with as an average
log(nodes.population) / log(degree.friends.and.relatives)
# It takes about 6 people to get from one person to another regardless where they are!
# See also: Milgrams letter experiment (small average path and diameter):
# http://en.wikipedia.org/wiki/Small-world_experiment

# The probability that node x has d links is binomial:
nodes <- 10
links <- 4
prob.x <- factorial(nodes - 1) / (factorial(links) * factorial(nodes - links - 1))
prob.x
# (n-1)! / (d!(n-d-1)!)
# Should be between 0 and 1, this is wrong....
# UCINET software: https://sites.google.com/site/ucinetsoftware/home

# Week 1, Problem Set, Q5:
# School 1: A university has 1,097 students; on average each student has 8 close friends.
# School 2: A middle school has 149 students; on average each student has 8 close friends.
nodes.school1 <- 1097
degree.school1 <- 8
nodes.school2 <- 149
degree.school2 <- 8
round((log(nodes.school1) / log(degree.school1)) / (log(nodes.school2) / log(degree.school2)), 2)

package.install("network")
library(network)
data(flo) # Florentine wedding network dataset (Medici influence)
head(flo)
nflo <- network(flo, directed=F)
all(nflo[,] == flo) # Trust, but verify
par(mar=c(1,1,1,1))
plot(nflo, displaylabels=TRUE, boxed.labels=FALSE, label.cex=0.75)
#diameter(nflo)

# http://cran.r-project.org/web/packages/igraph/igraph.pdf
library(igraph)
#g <- graph(flo)
#plot(g)

# http://www.learner.org/courses/mathilluminated/interactives/network/ (Clustering coeff):
# The clustering coefficient of a vertex indicates how concentrated the
# neighborhood of that vertex is. The clustering coefficient is the ratio
# of the number of actual edges there are between neighbors to the number
# of potential edges there are between neighbors (all possible edges
# between the vertices).
# Example from Networked Life: 4 - 3 - Lecture 7- III.Clustering of Connectivity (Week 2):
nodes <- 5
nodes1 <- 2
connected1 <- 1 # This is: The number of neighors' connections. For node 1 it is: 2<->5 = 1 connection
links1 <- connected1 / ((nodes1 * (nodes1 - 1)) / 2)
nodes2 <- 3
connected2 <- 2 # This is: The number of neighors' connections. For node 2 it is: 3<->5,5<->1 = 2 connections
links2 <- connected2 / ((nodes2 * (nodes2 - 1)) / 2)
nodes3 <- 3
connected3 <- 2 # This is: The number of neighors' connections. For node 3 it is: 4<->5,5<->2 = 2 connections
links3 <- connected3 / ((nodes3 * (nodes3 - 1)) / 2)
nodes4 <- 2
connected4 <- 1 # This is: The number of neighors' connections. For node 4 it is: 3<->5 = 1 connection
links4 <- connected4 / ((nodes4 * (nodes4 - 1)) / 2)
nodes5 <- 4
connected5 <- 3 # This is: The number of neighors' connections. For node 5 it is: 3<->4,3<->2,2<->1 = 3 connections
links5 <- connected5 / ((nodes5 * (nodes5 - 1)) / 2)
avg.clustering.coefficient <- (links1 + links2 + links3 + links4 + links5) / nodes # cc = 0.7666... - OK!
links <- 7
p <- links / ((nodes * (nodes - 1)) / 2) # p = 0.7 (p is 'edge density').
# Same as:
graph.density(g)
# This network is NOT highly clustered, p has to be MUCH higher than cc for the network to be highly clustered.

edges <- c(1,2, 1,5, 2,3, 2,5, 3,5, 3,4, 4,5)
g <- graph(edges, directed=F, n=nodes)
adj.matrix <- matrix(c(0,1,0,0,1, 1,0,1,0,1, 0,1,0,1,1, 0,0,1,0,1, 1,1,1,1,0), nrow=5, ncol=5)
g2 <- graph.adjacency(adj.matrix, mode="directed")
diag(adj.matrix %*% adj.matrix) # Get number of neighbors for vertices. Is this the same as degree(g)?
op <- par()
par(mar=c(1,1,1,1))
plot(g)
plot(g2)
par <- op

diameter(g)
are.connected(g, 1, 3)
get.edge(g, 1)
neighbors(g, 1, mode=1)
vcount(g)
ecount(g)
V(g) # get vertex sequence
E(g) # get edge sequence
betweenness(g, v=V(g), directed=TRUE, weights=NULL, nobigint=TRUE, normalized=FALSE)
edge.betweenness(g, e=E(g), directed=TRUE, weights=NULL)

# Same as above, as adjacency matrix:
adj.matrix <- matrix(c(0,1,1,1, 1,0,0,0, 1,0,0,0, 1,0,0,0), nrow=4, ncol=4)
g <- graph.adjacency(adj.matrix)
plot(g)
diameter(g)

# Adjacency matrix, with (2,2) and (4,4) pointing back to themselves:
adj.matrix <- matrix(c(0,1,1,1, 1,1,0,0, 1,0,0,0, 1,0,0,1), nrow=4, ncol=4)
# adj.matrix %*% adj.matrix
g <- graph.adjacency(adj.matrix)
plot(g)

# Another example, with A matrix above
g <- graph.adjacency(A)
plot(g)

# Loading PAJEK files with ipgraph:
library(igraph)
g = read.graph('C:/coding/R/Coursera/SocialAndEconomicNetworks/Week 1/assignment/ProblemSet1/METHODSC.NET',
               format='pajek')
g
str(g)
par(mar=c(1,1,1,1))
plot(g)
par(mar=c(3,3,3,3))

#############################
# Week 2
#############################

# Centrality: Four things to measure:
# - Degree: connectedness
# - Closeness, Decay: Ease of reacking other nodes
# - Betweenness: Role as an intermediary, connector
# - Influence, Prestige, Eigenvectors: "Not what you know, but who you know",
#   (or: you are important if your friends are important.)

# Closeness centrality: (n-1) / (sum(j) of I(i,j))
# I(i,j) = length of the shortest path between nodes i and j
# For instance, the Florentine network:
nodes <- 15
distances <- c(1, 2, 1, 1, 1, 2, 2, 3, 1, 1, 3, 2, 1, 3, 1) # I(i,j)
(nodes - 1) / sum(distances) # OK! Should be 14/25 (0.56) for Medici node

library(igraph)
g <- graph.ring(10)
g2 <- graph.star(10)
closeness(g)
closeness(g2, mode="in")
closeness(g2, mode="out")
closeness(g2, mode="all")

g3 <- graph(c(1,2,2,3,1,4,1,5), directed=F)
plot(g3, main="Closeness Centrality")
# Find closeness centrality of node 1:
nodes <- 5
closeness(g3, mode="all", normalized=F)
closeness(g3, mode="all", normalized=T) # Normalized: Multiply by (n-1), all other nodes
closeness.normalized <- (nodes - 1) * ((1 + 1 + 1 + 2)^(-1))
closeness.normalized
closeness.not.normalized <- ((1 + 1 + 1 + 2)^(-1))
closeness.not.normalized

# Decay centrality:
# Decay Centrality: C of node i distance (graph) = sum(j != i) of Delta^(I(i,j))
# Delta chosen will be between 0 and 1. Near 1 becomes component size, near 0 becomes degree
delta <- .5
(delta^1) + (delta^1) + (delta^1) + (delta^2)

# Between centrality (NOTE: Denominator can be (n-1)(n-2)/2 for undirected graphs)
# TIPS: https://class.coursera.org/networksonline-002/forum/thread?thread_id=160
el <- matrix( c("a","b","b","c","c","d","d","e","e","a","b","e"), nc=2, byrow=TRUE)
g4 <- graph.edgelist(el, directed=F)
plot(g4, main="Betweenness Centrality")
betweenness(g4, normalized=F) # Note: IS NOT normalized (IS NOT divided by (n-1)(n-2)/2))
betweenness(g4, normalized=T) # Note: IS normalized (IS divided by (n-1)(n-2)/2))

# Eigenvector Centrality = Bonacich Centrality
# Use: igraph's bonpow function?
bonpow(g4)
# Matlab: For eigenvectors, first get create the adjacency matrix, then do eig(m) on that in MatLab

# TIPS: https://class.coursera.org/networksonline-002/forum/thread?thread_id=172
#       https://class.coursera.org/networksonline-002/forum/thread?thread_id=160
#       https://class.coursera.org/networksonline-002/forum/thread?thread_id=173
#       http://press.princeton.edu/chapters/s2_8767.pdf

# Random Networks:
par(mfrow=c(1,1))
oldmar <- par()$mar
par(mar=c(1,1,1.5,1))
g <- erdos.renyi.game(50, 2/50, type=c("gnp", "gnm"), directed=F, loops=F)
degree.distribution(g)
plot(g, vertex.size=14, frame=F)
par(mar=oldmar)

# Visualization of random nodes i being created over time t (m = time node i is born): m < i < t
m <- 20 # Degree at birth of node 1
t <- 100
# Expected degree for node i born at m < i < t is:
func <- function(x) { m * (1 + log(t / x)) }
plot(func(1:t), pch=20, col="blue", xlim=c(1,200), main="Expected degree for node i born at m < i < t")
t <- 200
nodes.i <- t
# Expected degree for node i born at m < i < t is:
func <- function(x) { m * (1 + log(nodes.i / x)) }
points(func(1:t), pch=20, col="red")
# Above same as:
# points(m * (1 + log(nodes.i/(1:t))), pch=19, col="green")

# How many nodes have degree < 35?
# We want to find: m * (1 - log(t / i)) < 35
e <- exp(1)
degree <- 35
t <- 100
# Find nodes with degree < 35:
nodes <- round(t * e^(-(degree - m) / m), 1)
nodes
percent.born.later.t1 <- (t - nodes) / t
percent.born.later.t1
abline(v=nodes, col="green")
t <- 200
# Find nodes with degree < 35:
nodes <- round(t * e^(-(degree - m) / m), 1)
nodes
percent.born.later.t2 <- (t - nodes) / t
percent.born.later.t2
abline(v=nodes, col="cyan")
# Nodes born later than abline have degree < 35

# Continuous Time Approximation:
# ------------------------------
# deriv(degree<i>(t)) / deriv(t) = m/t
# and degree<i>(i) = m
# So, degree<i>(t) = m + (m * log(t(i))
# TODO: Above is a differential equation, check it!
# http://en.wikipedia.org/wiki/Differential_equation
# http://en.wikipedia.org/wiki/Heat_equation (example of important d.e.)

# Preferential Attachment (Price, 1975) - better random forming of degree distributions
# - Form links with prob. proportional to number of links a node already has: "Rich get richer"

# So: Newborn nodes form m links to exising nodes.
# We'll have tm links formed in total (t nodes with m links each).
# Total degree is 2tm (each different link is attached to 2 nodes).
# Probability of attaching to node i is: d<i>(t)/2tm (the nodes degree / total degree)

# Mean Field Approximation:
# -------------------------
# deriv(degree<i>(t)) / deriv(t) = m*d<i>(t)/2tm = d<i>(t)/2t (m cancels out in num. and denom.)
# So we have: d<i>(i)=m (a node gets m links when it is born)
# d<i>(t) = m*((t/i)^(1/2)))

m <- 20 # Degree at birth of node 1
t <- 100
# Expected degree for node i born at m < i < t is:
func1 <- function(x) { m * (1 + log(t / x)) } # Random Formula
func2 <- function(x) { m * ((t / x)^(1 / 2)) } # Preferential Attachment Formula
plot(func1(1:t), col="blue", xlim=c(1, t), ylim=c(0, 210),
     main="Expected degree for node i born at m < i < t", xlab="Time", ylab="Mean Field Approximation in red")
points(func2(1:t), col="red")

# Compare curves in a simple way:
log(10/1:10)
(10/1:10)^(1/2)

# How many nodes have degree < 35?
degree <- 35
t <- 100
m <- 20
# Find nodes with degree < 35:
nodes <- m * ((t/degree)^(1/2)) # NOTE: Not same result as in lecture... check Jackson's book?
nodes
abline(v=nodes, col="cyan")
abline(h=35, col="cyan")

# Power Law: f<t>(d) = 2m^2 / d^3
# Or:
# log(f(d)) = log(2m^2) - 3log(d)

# Hybrid Models:
# --------------
# Simple Hybrid, simple version of Jackson-Rogers (2007):
# - New node a meets m other nodes uniformly at random and directs links at them.
# - Then: Meets (1-a)m of their neighbors and attaches to them too.

# Relation to Preferential Attachment:
# Example: In a network where half the nodes have degree k and the other half have degree 2k:
# - Randomly select a link and then a node on one end of that link. There is a 2/3 chance
#   that it has degree 2k, and a 1/3 chance that it has degree k.

# Formula when d<i>(i)=m:
# Deriv(d<i>(t)) / Deriv(t) = a*m/t + (1-a)d<i>(t)/2*t
# This is:
# d<i>(t) = (m + 2*a*m / (1-a)) * (t/i)^((1-a)/2) - 2*a*m / (1-a)
# Fdist(d) = 1 - ((m + a*m*(2/(1-a))) / (d + a*m*x))^(2/(1-a))
m <- 20
t <- 100
i <- 1:100
d <- 1:100

par(mfrow=c(1,3))
a <- 0.001
func <- function(x) { ((m + 2*a*m) / (1-a)) * (t/x)^((1-a)/2) - ((2*a*m) / (1-a)) }
plot(func(i), pch=19, col="blue", main="a=.001")
a <- 0.5
plot(func(i), pch=19, col="red", main="a=.5")
a <- 0.999
plot(func(i), pch=19, col="green", main="a=.999")
par(mfrow=c(1,1))

# Below does not match what is shown at end of lecture "4 - 4 - 3.4- Hybrid Models (14-15)"
par(mfrow=c(1,3))
a <- 0.001
Fdist <- function(d) { 1 - ((m + a*m*(2/(1-a))) / (d + a*m*(2/(1-a))))^(2/(1-a)) }
plot(log(1 - Fdist(d)), pch=19, col="blue", main="a=.001")
a <- .5
plot(log(1 - Fdist(d)), pch=19, col="red", main="a=.5")
a <- 0.999
plot(log(1 - Fdist(d)), pch=19, col="green", main="a=.999")
par(mfrow=c(1,1))

# Stochastic Block Models:
# ------------------------
# Continuous covaariates:
# Example: link between j and i depends on their characteristics:
# B<i>X<i> + B<j>X<j> + B<ij>|X<i>-X<j>|
# For instance:
# log(prob<ij>/(1-prob<ij>)) = B<i>X<i> + B<j>X<j> + B<ij>|X<i>-X<j>|

# Exponential Random Graph Models (ERGMs):
# See: Jacob Levy Moreno and Helen Hall, 1938
# p<star> (p*) and ERG models:
# - Probability of network depends on number of links
#   AND also dependes of number of TRIANGLES
# (B<Links> * #Links(g)) + (B<Triangles> * #Triangles(g))
# - B is some form of parameters. # = "number of"
# - NOTE: It can be other things than triangles and links, say 2- and 3-stars connections, etc.
# We want probability of network to be:
# Prob(g) ~ exp[B<L>L(g) + B<T>T(g)]
# - Should always be positive (0 to 1)
# - See: Theorem by Hammersly and Clifford ('71)
# Formula: Pr(g) = exp[log(p/(1-p))L(g) - log(1/(1-p))n(n-1)/2]
# - p: probability of a link
# - L(g): number of links in g

# Statnet package: https://statnet.csde.washington.edu/trac/wiki/Installation
# Statnet tutorial: http://www.jstatsoft.org/v24/i09/paper
package.install("statnet")
library(statnet)
data(florentine)

oldmar <- par()$mar
par(mar=c(.5,.5,.5,.5))
plot(flomarriage, displaylabels=T, displayisolates=T)
betweenness(flomarriage)
degree(flomarriage)

data(flo)
nflo <- network(flo)
plot(nflo, vertex.cex=apply(flo,2,sum)+1, usearrows=FALSE,
     vertex.sides=3+apply(flo,2,sum),
     vertex.col=2+(network.vertex.names(nflo)=="Medici"))
par(mar=oldmar)

# Problem set 3
model1 <- ergm(flomarriage ~ edges)
summary(model1)
model2 <- ergm(flomarriage ~ edges + triangles)
summary(model2)
model3 <- ergm(flomarriage ~ edges + triangles + kstar(2))
summary(model3)

# Statistical Exponential Random Graph Models (SERGMs):
# -----------------------------------------------------
# (Chandrasekhar-Jackson 2012)
# Let N(s') be the number of networks with Statistic(g')=s'
# s = a network statistic (for instance: s = N(nodes) and N(edges) and N(stars))
# Other way of saying that: A certain density of links, clustering, average path lenghth, etc.
# s' ("s prime") = statistics in other networks
# Pr(s) = N(s)exp[<Beta>s] / (Sums of N(s')exp[<Beta>s'])

# Subgraph Generation Models (SUGMs):
# -----------------------------------
# Subnetworks of type j (links/triangles/stars) are formed, the network is a "biproduct"
# Created independently, with probability p<j>
# Then: Pr(S) = K(S)exp[<Beta>S] / (<Sum of s'>(K(s')exp[<Beta>s'])
# <Beta>j = log(p<j>/(1-p<j>)) and K^<n>(s) = TT<j>(Snj over Sj)
# Note: TT = "Product of" symbol (like symbol "E" = "Sum of")

# Strategic Network Models:
# -------------------------
# Example from Economic Analysis (Jackson & Wolinsky 1996):
# u<i>(g) = payoff to i if the network is g
# Undirected network formation
# Symmetric version:
# - Benefit from a friend is Delta < 1
# - Benefit from a friend of a friend is Delta^2
# - Cost of link is c > 0
adj.g6 <- matrix(c(0,1,1,0,0, 1,0,0,0,0, 0,0,0,0,0, 0,1,0,0,0, 0,0,0,0,0), nrow=5, ncol=5)
g <- graph.adjacency(adj.g6, mode="undirected")
plot(g, main="Symmetric version example 1")
# u<1> = 2Delta + Delta^2 - 2c
# u<2> = 2Delta + Delta^2 - 2c
# u<3> = Delta + Delta^2 + Delta^3 - c 
# u<4> = Delta + Delta^2 + Delta^3 - c
adj.g7 <- matrix(c(0,1,1,0,0, 1,0,1,0,0, 1,1,0,0,0, 0,1,0,0,1, 0,0,0,1,0), nrow=5, ncol=5)
g <- graph.adjacency(adj.g7, mode="undirected")
plot(g, main="Symmetric version example 2")
# u<1> = 2Delta + Delta^2 + Delta^3 - 2c
# u<2> = 3Delta + Delta^2 - 3c
# u<3> = 2Delta + Delta^2 + Delta^3 - 2c 
# u<4> = 2Delta + 2Delta^2 - 2c
# u<5> = Delta + Delta^2 + 2Delta^3 - c

# Pairwise Stability and Efficiency (Jackson book p.207):
# ----------------------------------
# A game example: Each node "shouts out" who they wish to connect to
# (http://en.wikipedia.org/wiki/Nash_equilibrium, http://en.wikipedia.org/wiki/John_Forbes_Nash,_Jr.)
# Nash equilibria:
# - Both nodes announce each other is an equilibrium (dyad, pairwise stable)
# - Neither node announces the other is an equilibrium (not pairwise stable)
# Pairwise stability:
# u<i>(g) > u<i>(g-ij) for i and ij in g
# (no agent gains from severing a link)
# u<i>(g+ij) > u<i>(g) implies u<j>(g+ij) < u<j>(g) for ij not in g
# (no two agents both gain from adding a link (at least one strictly))

# Efficiency:
# -----------
# A network g is Pareto efficient if there does not exist g' (other network) such that:
# u<i>(g') >= u<i>(g) for all i, and strictly so for some
# A network is strongly efficient if:
# g maximizes Sum of u<i>(g')
# This is Utilitarianism ("I care only about the total utility of the network")
# Example in Jackson book p.207:
Delta <- 4 # Payoff having a direct friend
Cost <- 1
adj.g8 <- matrix(c(0,1,0,0, 1,0,1,0, 0,1,0,1, 0,0,1,0), nrow=4, ncol=4)
g <- graph.adjacency(adj.g8, mode="undirected")
plot(g, main="Symmetric version example Pareto Efficient")
Node1 <- Delta + Delta^2 + Delta^3 - Cost
Node1
Node2 <- 2*Delta + Delta^2 - 2*Cost
Node2
Node3 <- 2*Delta + Delta^2 - 2*Cost
Node3
Node4 <- Delta + Delta^2 + Delta^3 - Cost
Node4
# TODO: Do not match book/slide numbers. How to calculate?? Check forum! Next video??

# -----------------------------------------------------------------------------------------------
# Network theory, distribution for network degree (number of neighbors the vertexes/nodes have)
# Create a probability density distribution for a normal distribution, and a heavy-tailed dist
# Degree distributions are heavy-tailed, and in many types of networks can be closely approcimated
# by the power law distribution. It fits a regular linear model quite well
x <- rnorm(100, mean=0)
prob.x <- exp(1)^(-((x-mean(x))^2))
prob.x.log <- -((x-mean(x))^2)
plot(prob.x ~ x, col="blue")
plot(prob.x.log ~ x, col="blue")
plot(prob.x.log ~ log(abs(x)), col="blue")
# Heavy-tailed distribution
B <- 3 # The power law distribution with exponent B
prob.x.heavy.tailed <- 1 / x^B
prob.x.heavy.tailed.log <- (-B) * log(abs(x))
plot(prob.x.heavy.tailed ~ x, col="blue")
plot(prob.x.heavy.tailed.log ~ x, col="blue") # Heavy curvature, fast decay and long tail
plot(prob.x.heavy.tailed.log ~ log(x), col="blue") # We get a straight line for the distribution!
# NOTE: Slope is negative(B) for plot above. Fits a linear model very well.

# --------------------------------------------------------------------------------------------------
# Reading .gephi files into R
package.install("rgexf")
library(rgexf)
# g <- read.gexf("C:/coding/Networks/RandomGraph.gexf")
g <- read.csv("C:/coding/Networks/RandomGraph.csv", header=T, sep=";")
g <- g[,-1] # Remove the first col containing node names
g <- as.matrix(g)
g <- graph.adjacency(g, mode="directed")
par(mar=c(1,1,2,1))
plot(g, main="Random Graph from Gephi CSV-export")

# --------------------------------------------------------------------------------------------------
# TODO: Get LinkedIn connections for graph
# http://linkurio.us/linkedin-inmaps-discontinued-visualize-network-now/
# http://www.dataiku.com/blog/2012/12/07/visualizing-your-linkedin-graph-using-gephi-part-1.html

# --------------------------------------------------------------------------------------------------
# http://rulesofreason.wordpress.com/2012/11/05/network-visualization-in-r-with-the-igraph-package/
# TODO: http://stackoverflow.com/questions/9876267/r-igraph-community-detection-edge-betweenness-method-count-list-members-of-e
par(mar=c(1,1,2,1))
bsk <- read.table("http://www.dimiter.eu/Data_files/edgesdata3.txt", sep='\t', dec=',', header=T)
# Transform the table into the required graph format:
bsk.network <- graph.data.frame(bsk, directed=F)
plot(bsk.network, main="BSK")

# Subset the data. If we want to exclude people who are in the network only tangentially (participate in one or two relationships only)
# we can exclude the by subsetting the graph on the basis of the 'degree':
bad.vs <- V(bsk.network)[degree(bsk.network)<3] #identify those vertices part of less than three edges
bsk.network <- delete.vertices(bsk.network, bad.vs) #exclude them from the graph
# Plot the data.Some details about the graph can be specified in advance.
# For example we can separate some vertices (people) by color:
V(bsk.network)$color <- ifelse(V(bsk.network)$name=='CA', 'blue', 'red')
# Useful for highlighting certain people. Works by matching the name attribute of the vertex to the one specified in the 'ifelse' expression
# We can also color the connecting edges differently depending on the 'grade': 
E(bsk.network)$color <- ifelse(E(bsk.network)$grade==9, "red", "grey")
# or depending on the different specialization ('spec'):
E(bsk.network)$color <- ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))
# Note: the example uses nested ifelse expressions which is in general a bad idea but does the job in this case
# Additional attributes like size can be further specified in an analogous manner, either in advance or when the plot function is called:
V(bsk.network)$size <- degree(bsk.network)/10
# Here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally 
# bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)
# Note that if the same attribute is specified beforehand and inside the function, the former will be overridden.
# And finally the plot itself:
par(mai=c(0,0,1,0))   	# this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(bsk.network,				# the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Organizational network example',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(bsk.network)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)

# ---------------------------------------------------------------------------------------------------------------------
# Normalize node degrees (divide node degree by N-1 number of nodes)
# Coursera Analysis of social networks, lecture 3A, Centrality
par(mar=c(1,1,2,1))
# Graph 1 on slide "normalization"
n <- 7
adj.matrix <- matrix(c(0,0,1,0,0,1,0,  0,0,0,0,1,0,1,  1,0,0,1,0,1,0,  0,0,1,0,1,0,0,  0,1,0,1,0,0,1,
                       1,0,1,0,0,0,0,  0,1,0,0,1,0,0),
                     nrow=n, ncol=n, byrow=T)
degrees <- diag(adj.matrix %*% t(adj.matrix))
degrees.normalized <- degrees / (ncol(adj.matrix) - 1)
g <- graph.adjacency(adj.matrix, mode="undirected")
V(g)$name <- round(degrees.normalized, 2)
plot(g, main="Normalization 1", vertex.color="wheat", vertex.size=(degrees * 5) + 20, vertex.label.cex=.8,
     vertex.label.font=2, vertex.label.dist=0, edge.curved=F, layout=layout.fruchterman.reingold)

# Graph 2 on slide "normalization"
n <- 6
adj.matrix <- matrix(c(0,1,1,1,1,1,  1,0,0,0,0,0,  1,0,0,0,0,0,  1,0,0,0,0,0,  1,0,0,0,0,0,  1,0,0,0,0,0),
                     nrow=n, ncol=n, byrow=T)
degrees <- diag(adj.matrix %*% adj.matrix)
degrees.normalized <- degrees / (ncol(adj.matrix) - 1)
g <- graph.adjacency(adj.matrix, mode="undirected")
V(g)$name <- round(degrees.normalized, 2)
plot(g, main="Normalization 2", vertex.color="wheat", vertex.size=(degrees * 5) + 20, vertex.label.cex=.8,
     vertex.label.font=2, vertex.label.dist=0, edge.curved=F, layout=layout.fruchterman.reingold)

# Graph 3 on slide "normalization"
n <- 5
adj.matrix <- matrix(c(0,1,0,0,0,  1,0,1,0,0,  0,1,0,1,0,  0,0,1,0,1,  0,0,0,1,0),
                     nrow=n, ncol=n, byrow=T)
degrees <- diag(adj.matrix %*% adj.matrix)
degrees.normalized <- degrees / (ncol(adj.matrix) - 1)
g <- graph.adjacency(adj.matrix, mode="undirected")
V(g)$name <- round(degrees.normalized, 2)
plot(g, main="Normalization 3", vertex.color="wheat", vertex.size=(degrees * 5) + 15, vertex.label.cex=.8,
     vertex.label.font=2, vertex.label.dist=0, edge.curved=F, layout=layout.fruchterman.reingold)

# Try on the florentine dataset
degrees <- diag(flo %*% flo)
degrees.normalized <- degrees / (ncol(flo) - 1)
g <- graph.adjacency(flo, mode="undirected")
V(g)$name <- paste(colnames(flo), round(degrees.normalized, 2))
plot(g, main="Florentine Marriages network", vertex.color="wheat", vertex.size=(degrees * 4) + 15, vertex.label.cex=.8,
     vertex.label.font=2, vertex.label.dist=0, edge.curved=F, layout=layout.fruchterman.reingold)

# ---------------------------------------------------------------------------------------------------------------------
