# Data Mining Specialization
# --------------------------

# Pattern Discovery in Data Mining
# --------------------------------

# Week 1
# ------
# NetFlix data mining: http://www.salon.com/2013/02/01/how_netflix_is_turning_viewers_into_puppets/?123
# Set notation, etc.: http://www.saburchill.com/math/chapters/0030020.html
# General R thread: https://class.coursera.org/patterndiscovery-001/forum/thread?thread_id=8#post-1667

package.install("arules")
library(arules)

set.seed(1101011)

sz <- 30
shopping_history <- data.frame(apple=rbinom(sz, 1, .7), banana=rbinom(sz, 1, .9),
                               kiwi=rbinom(sz, 1, .5), pear=rbinom(sz, 1, .6),
                               orange=rbinom(sz, 1, .4), durian=rbinom(sz, 1, .2))
shopping_history
barplot(colSums(shopping_history), main="Shopping carts", cex.main=.8, cex.lab=.7, cex.axis=.7, cex=.7,
        col="cornflowerblue", ylim=c(0, sz))

# Lecture 2.2: Frequent itemsets
# -----------------
# k-itemset: X = { X<1>...X<k> }
# Absolute support: The number of transactions that contain the itemset (e.g. 3)
# Relative support: The fraction of trnasactions that contain the itemset (e.g. 3/5)
# minsup threshold = sigma: Itemset is frequennt if sigma is >= threshold

# minconf c: The conditional possibility that X also contains Y (c = sup(X union Y) / sup(X))

transactions <- matrix(c("Beer","Nuts","Diapers","","","Beer","Coffee","Diapers","","","Beer","Diapers",
                         "Eggs","","","Nuts","Eggs","Milk","","","Nuts","Coffee","Diapers","Eggs","Milk"), ncol=5, byrow=T)
colnames(transactions) <- c("Item1","Item2","Item3","Item4","Item5")
transactions

foundBeerButNotDiapers <- 0
foundDiapersButNotBeer <- 0
foundBeerAndDiapers <- 0
foundBeer <- 0
foundDiapers <- 0

for (counter in 1:nrow(transactions)) {
  if ("Beer" %in% transactions[counter,] & "Diapers" %in% transactions[counter,]) {
    foundBeerAndDiapers <- foundBeerAndDiapers + 1
    foundBeer <- foundBeer + 1
  }
  
  if ("Beer" %in% transactions[counter,] & !("Diapers" %in% transactions[counter,]))
    foundBeerButNotDiapers <- foundBeerButNotDiapers + 1
  
  if ("Diapers" %in% transactions[counter,] & !"Beer" %in% transactions[counter,])
    foundDiapersButNotBeer <- foundDiapersButNotBeer + 1
  
  if ("Diapers" %in% transactions[counter,])
    foundDiapers <- foundDiapers + 1
}

# Percentage of transactions where those who bought beer also bought diapers
(foundBeerAndDiapers / (foundBeerAndDiapers + foundBeerButNotDiapers)) * 100
# Percentage of transactions where those who bought diapers also bought beer
(foundBeerAndDiapers / (foundBeerAndDiapers + foundDiapersButNotBeer)) * 100
# Support: Percentage of transactions where people bought both beer and diapers
supportBeer <- (foundBeerAndDiapers / nrow(transactions)) * 100
supportBeer
supportDiapers <- (foundBeerAndDiapers / nrow(transactions)) * 100
supportDiapers
# Confidence Beer -> Percentage of transactions where people bought Beer and Diapers / Transactions with Beer
foundBeer <- (foundBeer / nrow(transactions)) * 100
confidenceBeer <- (foundBeerAndDiapers / foundBeer) * 100 # support(Beer U Diapers) / support(Beer) 
confidenceBeer
# Confidence Diapers -> Percentage of transactions where people bought Beer and Diapers / Transactions with Diapers
foundDiapers <- (foundDiapers / nrow(transactions)) * 100
confidenceDiapers <- (foundBeerAndDiapers / foundDiapers) * 100 # support(Beer U Diapers) / support(Diapers)
confidenceDiapers

# Lecture 2.3/4: Compressed representation - Closed patterns and max-patterns
# ---------------------------------------------------------------------------
# Closed patterns: A pettern (itemset) X is CLOSED if X is frequent, and there
# exists no super-pattern Y <superpattern of> X, with the same support as X
transaction.1 <- c(1:50)
transaction.2 <- c(1:100)
minsup <- 1
closed.patterns <- 2
#t1 and t2, because all others are contained in one of them:
# ({1,2} is in no. 1, {51:52} is in no. 2, etc.)

# Max patterns: A pattern X is a MAX PATTERN if X is frequent, and there exists
# no frequenct pattern Y <superpattern of> X. Difference from closed patterns:
# We do not care about the real support of the sub-patterns of the max pattern
max.pattern <- 1 # {1:100} is the only one
# Max pattern is a lossy compression, because we do not know the real support of 
# for instance {1:40}. We only know that it is frequent, when minsup = 1.
# So in many applications mining closed patterns is the most desirable.

# Lecture 2.5: Efficient methods: The Downward Closure ("Apriori") property
# -------------------------------------------------------------------------
# Apriori: Any SUBSET of a frequenct itemset, must also be frequent
# Itemset {beer, diapers, nuts}: {beer,nuts} is also frequent
# Apriori pruning principle:
# If there is any itemset that is INFREQUENT, its superset shoud never be generated/considered.

# Lecture 2.6: The Apriori algorithm
# ----------------------------------
# Some tips: http://nikhilvithlani.blogspot.no/2012/03/apriori-algorithm-for-data-mining-made.html
minsup <- 2
tdb <- matrix(c("A","C","D","","B","C","E","","A","B","C","E","B","E","",""), ncol=4, byrow=T)
tdb
items <- table(tdb) # Gets the support for every single item
items <- names(items[items >= minsup & nchar(names(items)) > 0]) # Filter thos with minsup >= 2
combn(items, 2) # Get all pairs from set
