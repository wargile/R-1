# Data Analysis and Statistical Inference

# Week 1
# ------

# http://www.gapminder.org/data/
# http://www.gapminder.org/world/

library(scales)

par(mar=c(5,4,2.5,.8))
par(cex.lab=.7)
par(cex.axis=.7)
par(cex.main=.8)
par(cex=1)
par(mgp=c(1.5, .5, 0)) # Axis labels and main heading distance from plot

folder <- "C:/coding/R/Coursera/DataAnalysisAndStatisticalInference/Docs and extra content/os2_data/openintroData/"
cars <- read.csv(paste0(folder, "cars.txt"), header=T, sep="\t")
head(cars)

# Square-root example (cars) to get x/y on the same scale:
par(mfrow=c(1,2))
plot(cars$weight ~ cars$mpgCity, pch=19, col="red", main="Normal")
plot(sqrt(cars$weight) ~ cars$mpgCity, pch=19, col="blue", main="Square root")
par(mfrow=c(1,1))

# Inverse example (mtcars) to change x/y slope:
par(mfrow=c(1,2))
plot(cars$weight ~ cars$mpgCity, pch=19, col="red", main="Normal")
plot(1/cars$weight ~ cars$mpgCity, pch=19, col="blue", main="Inverse")
par(mfrow=c(1,1))

# -----------------------------------------------------------------------------------------------------------------------------
# Plots for "Saving is easy/difficult", video 1-12 example:

df <- data.frame(Under.40K=c(128,54,17,3,0), Between.40K.80K=c(63,71,7,6,1), Over.80K=c(31,61,27,5,0), Refused=c(9,10,7,0,0))
sum(df)
rownames(df) <- c("Very","Somewhat","Not very","Not at all","Not sure")
barplot(as.matrix(df), col=c("goldenrod3","violetred3","wheat","cornflowerblue","orange"), main="Is it easy to save money?",
        xlab="Income level", ylab="Number of respondents") # NOTE: Draws nice stacked plot!
legend("topright", legend=rownames(df), cex=.7, y.intersp=1.2,
       fill=c("goldenrod3","violetred3","wheat","cornflowerblue","orange"), box.col="gray")

oldmar <- par()$mar
par(mar=c(3,3,2,5.5), xpd=TRUE) # NOTE: xpd: prevent clipping
df2 <- as.data.frame(sapply(df, function(x) x / sum(x)))
rownames(df2) <- rownames(df)
barplot(as.matrix(df2), col=c("goldenrod3","violetred3","wheat","cornflowerblue","orange"), main="Is it easy to save money?",
        xlab="Income level", ylab="Number of respondents") # NOTE: Draws nice stacked plot!
legend("topright", legend=rownames(df2), cex=.7, inset=c(-0.2,0), # NOTE: Inset param
       fill=c("goldenrod3","violetred3","wheat","cornflowerblue","orange"), box.col="gray")
# TIP: Legend outside plot area:
# http://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
par(mar=oldmar)

mosaicplot(t(as.matrix(df)), col=c("goldenrod4","violetred3","wheat","cornflowerblue","orange"), las=2, main="Saving")

# ------------------------------------------------------------------------------------------------------------------------------
# Promotion example with cards, video 1-13:
# Gender discrimination
# 48 male bank supervisors given the same personnel file,asked to judge whether the person should be promoted.
# Files were identical, except for gender of applicant random assignment
# 35 / 48 promoted
# Are females unfairly discriminated against?
df <- data.frame(Promoted=c(21,14), Not.Promoted=c(3,10))
rownames(df) <- c("Males","Females")
barplot(as.matrix(df), col=c("powderblue","pink"), main="Gender discrimination?",
        xlab="Promoted?", ylab="Sex") # NOTE: Draws nice stacked plot!
legend("topright", legend=rownames(df), cex=.7,
       fill=c("powderblue","pink"), box.col="gray")

# Simulate:
promoted <- rep(1, sum(df$Promoted))
not.promoted <- rep(0, sum(df$Not.Promoted))
all <- c(promoted, not.promoted)
promotion.diff <- numeric(0)

for (counter in 1:250) {
  pos <- sample(x=1:length(all), size=(length(all) / 2))
  females <- sort(all[pos]) 
  males <- sort(all[-pos])
  ratio.females <- length(females[females == 1]) / (length(all) / 2) # Females promoted
  ratio.males <- length(males[males == 1]) / (length(all) / 2) # Males promoted
  promotion.diff[counter] <- ratio.females - ratio.males
}

par(mfrow=c(1,2))
hist(round(promotion.diff, 2), col="orange1", main="Promotions diff. males and females", xlab="Difference")
barplot(table(round(promotion.diff, 2)), col="violetred3", main="Promotions diff. males/females", xlab="Difference")
par(mfrow=c(1,1))

dotplot(table(promotion.diff))

df <- data.frame(table(promotion.diff))
names(df) <- c("Diff","Freq")
df2 <- NULL

for (counter in 1:nrow(df)) {
  temp <- rep(1, df$Freq[counter])
  for (counter2 in 1:length(temp))
    df2 <- rbind(df2, data.frame(Diff=df$Diff[counter], Freq=1))
}
ggplot(data=df2, aes(x=round(as.numeric(Diff), 2), y=Freq)) + geom_dotplot(fill="powderblue", dotsize=1)


# -------------------------------------------------------------------------------------------------------------------------
# Markdown template for 1st peer assessment
download.file("http://bit.ly/dasi_project_template",
              destfile="C:/coding/R/Coursera/DataAnalysisAndStatisticalInference/Week 1/PeerAssessment/PeerAssessment1.Rmd")

# ---------------------------------------------------------------------------------------------------------------------
# Data for lab 1:
source("http://www.openintro.org/stat/data/cdc.R") # Data taken from: http://www.cdc.gov/brfss
names(cdc)
head(cdc)
dim(cdc)

pairs(cdc, col="blue")
cor(sapply(cdc, as.numeric))
CorrelationPlot(as.data.frame(sapply(cdc, as.numeric)))
CorrelationPlot2(as.data.frame(sapply(cdc, as.numeric)))

par(mfrow=c(2,1))
par(mar=c(3.4,3.4,2,1))
hist(cdc$height[cdc$gender == "f"], col="powderblue", main="Female height", breaks=20, xlim=c(50, 90))
abline(v=mean(cdc$height[cdc$gender == "f"]), col="red")
abline(v=median(cdc$height[cdc$gender == "f"]), col="green4")
hist(cdc$height[cdc$gender == "m"], col="wheat", main="Male height", breaks=20, xlim=c(50, 90))
abline(v=mean(cdc$height[cdc$gender == "m"]), col="red")
abline(v=median(cdc$height[cdc$gender == "m"]), col="green4")
par(mfrow=c(1,1))

par(mar=c(2,2,2,1))
barplot(table(cdc$gender[cdc$smoke100 == 1]), col=c("powderblue", "pink"), main="Smoke 100 by gender")
plot(table(cdc$genhlth, cdc$hlthplan), col=c("Red", "Green4"), main="Health Plans versus General Health")
plot(table(cdc$genhlth, cdc$exerany), col=c("Red", "Green4"), main="Exercise versus General Health")
par(mar=c(5,5,3,1))

plot(height ~ wtdesire, data=cdc, col=alpha(c("blue", "red"), .3), pch=19, main="Height versus Desired Weight")
plot(wtdesire ~ gender, data=cdc, col=alpha(c("blue", "red"), .3), pch=19, main="Gender versus Desired Weight")
plot(weight ~ wtdesire, data=cdc, col=alpha(c("blue", "red"), .3), pch=19, main="Weight versus Desired Weight")

# ---------------------------------------------------------------------------------------------------------------------

# Quiz 1 question 12
n <- 107 # Our sample size
died <- 18
survived <- n - died
control <- 52
treatment <- n - control
#survival <- rbinom(n, 100, prob=died/n)
diff.groups <- rep(0, n)
for (counter in 1:n) {
  survival <- rep(0, n)
  died.rows <- sample(1:n, died, rep=F)
  survival[died.rows] <- 1
  control.rows <- sample(1:n, control, rep=F)
  treatment.rows <- sample(1:n, treatment, rep=F)
  died.control <- survival[control.rows]
  died.treatment <- survival[treatment.rows]
  diff.groups[counter] <-  (sum(died.treatment)/treatment) - (sum(died.control)/control)
}
hist(diff.groups, col="wheat", main="Tuberculosis treatment simulation",
     cex.main=.8, cex.lab=.8, cex.axis=.7, breaks=10)

# Get some GapMinder data:
df <- read.csv("c:/coding/R/testdata/indicator life_expectancy_at_birth.csv", header=T, sep=";",
               colClasses=c("character", rep("numeric", 205)))
dim(df)
summary(df)
head(df)
df.2013 <- df[, c("Country","X2013")]
df.2013 <- df.2013[!is.na(df.2013$X2013), ]
head(df.2013)
par(mar=c(3,3,2,1))
ggplot(data=df.2013, aes(x=df.2013$Country, y=df.2013$X2013, colour=df.2013$X2013)) +
  geom_point() + ggtitle("Life expectancy 2013") + xlab("Country") + ylab("Years") +
  theme(plot.title=element_text(size=18, colour="steelblue4")) +
  theme(axis.text.x=element_text(angle=90, hjust=1))

df.Countries <- df[df$Country %in% c("Norway", "Afghanistan", "United States"), ]
Norway <- as.numeric(df.Countries[df.Countries$Country == "Norway", 2:ncol(df.Countries)])
Afghanistan <- as.numeric(df.Countries[df.Countries$Country == "Afghanistan", 2:ncol(df.Countries)])
US <- as.numeric(df.Countries[df.Countries$Country == "United States", 2:ncol(df.Countries)])
plot(Norway, type="l", col="blue", ylim=c(1,100), main="Life expectancy")
# TODO: xaxt with years from-to
lines(US, col="red")


# ----------------------------------------------------------------------------------------------------------------------
# Week 2
# ------

# Cool site for probability visualization:
# http://setosa.io/conditional/

# Disjoint events: Events that can not happen at the same time, e.g. pass or fail a class,
# get heads or tails on a coin flip):

# P(A and B) = 0
# Non-disjoint events:
# P(A and B) <> 0

# Union of disjoint events:

# What is the probability of drawing a Jack or a Three from a well shuffled deck of cards?
cards <- 52
jacks <- 4 # There are 4 Jacks in the deck
threes <- 4 # There are 4 threes in the deck
prob <- jacks/cards + threes/cards
round(prob, 2)
# So: P(A or B) = P(A) + P(B)

# What is the probability of drawing a Red Jack? (NOTE: We have an overlap here: jack AND red)
reds <- 13 * 2
jacks <- 4
red_jacks <- 2
prob <- ((jacks + reds) / cards) - (red_jacks / cards)
round(prob, 2)
# So: P(A or B) = P(A) + P(B) - P(A and B)

# From the NRK "Siffer" program, episode 3:
# There is a 50% chance of rain on Saturday, and a 50% chance of rain on Sunday. What's the probability
# that it'll rain over the weekend?
rain.saturday <- .5
rain.sunday <- .5
prob.rain.during.weekend <- (rain.saturday + rain.sunday) - (rain.saturday * rain.sunday)
prob.rain.during.weekend # So, a 75% chance
# Or think possible outcomes: 1) rain/no rain, 2) no rain/rain, 3) rain/rain, 4) no rain/no rain
# 3 of 4 outcomes contains rain, that is 3/4 chance = 75% chance of rain

# Sample space: A collection of all possible outcomes of a trial

# Checking for independence:
# If P(A | B) = P(A), then A and B are independent
# P(A | B) = "probability of event A happening GIVEN that event B has already happened"

# Product rule for independent events:
# If A and B are independent, P(A and B) = P(A) * P(B)
# Example: Say you toss a coin twice, what is the prob. of getting two tails in a row?
sides <- 2
toss1 <- 1/sides
toss2 <- 1/sides
P.two.tails.in.a.row <- toss1 * toss2
P.two.tails.in.a.row
# Example: 33.5% of West Virginians are obese. What's the prob. that two randomly selected WV's
# are obese? The two persons are independent events.
P.obese <- 0.335
VW.person1 <- P.obese
VW.person2 <- P.obese
P.two.people.are.obese <- VW.person1 * VW.person2
P.two.people.are.obese

# The World Values Survey is an ongoing worldwide survey that polls the world 
# population about perceptions of life, work, family, politics, etc.
# The most recent phase of the survey that polled 77,882 people from 57 
# countries estimates that a 36.2% of the world's population agree with the 
# statement "Men should have more right to a job than women."
# The survey also estimates that 13.8% of people have a university degree or higher, 
# and that 3.6% of people fit both criteria.
P.agree <- 0.362
P.uni.degree <- 0.138
P.uni.degree.and.agree <- 0.036
# (1) Are agreeing with the statement "Men should have more right to a job than 
# women" and having a university degree or higher disjoint events?
# Answer: Since P(agree & uni.degree) = 0.036 != 0, -> not disjoint.
agree.and.no.uni.degree <- P.agree - P.uni.degree.and.agree
agree.and.no.uni.degree
disagree.and.uni.degree <- P.uni.degree - P.uni.degree.and.agree
disagree.and.uni.degree
# (3) What is the probability that a randomly drawn person has a 
# university degree or higher or agrees with the statement about men having 
# more right to a job than women?
# Answer: General addition rule: P(A or B) = P(A) + P(B) - P(A and B)
P.uni.degree + P.agree - P.uni.degree.and.agree
# or (see Wenn diagram of two intersecting circles):
agree.and.no.uni.degree + P.uni.degree.and.agree + disagree.and.uni.degree
# (4) What percent of the world population do not have a university 
# degree and disagree with the statement about men having more 
# right to a job than women?
1 - (P.uni.degree + P.agree - P.uni.degree.and.agree)

# Conditional probability:
# ------------------------
# Create a contingency table for easier visualization:
df <- data.frame(obj.working.class=c(0,8,32,8,0), obj.upper.middle.class=c(0,0,13,37,0))
rownames(df) <- c("subj.poor", "subj.working.class", "subj.middle.class",
                  "subj.upper.middle", "subj.upper.class")
df
df["subj.middle.class", "obj.working.class"]
# What is the probability that a student's objective social class position is upper middle class?
round(sum(df[, "obj.upper.middle.class"]) / sum(df), 2)
# What is the probability that a student who is  objectively in the working class associates with upper middle class?
round(df["subj.upper.middle", "obj.upper.middle.class"] / sum(df), 2)
# What is the probability that a student who is objectively in the working class associates with upper middle class?
# P(subj.UMC | obj.WC)
round(df["subj.upper.middle", "obj.working.class"] / sum(df[, "obj.working.class"]), 2)
obj.WC <- sum(df[, "obj.working.class"])
subj.UMC.and.obj.WC <- sum(df["subj.upper.middle", "obj.working.class"])
# We calculate conditional probability using Bayes' Theorem:
# P(A | B) = P(A and B) / P(B)
# or: P(A | B) = (P(A) x P(B)) / P(B)
# Note: If A and B are independent, P(A|B) = P(A). Note that P(B) cancels out here, leaving P(A).
round((subj.UMC.and.obj.WC / sum(df)) / (obj.WC / sum(df)), 2)

# The 2010 American Community Survey estimates that 14.6% of Americans live below 
# the poverty line, 20.7% speak a language other that English at home, and 4.2% fall into both categories.
# Based on this information, what percent of Americans live below the poverty line given 
# that they speak a language other than English at home?
# Use Bayes' theorem here, just exctract data from the text above: P(below PL | speak non-Eng) = ?
below.pov.line.and.other.language <- 0.042
speak.non.eng <- 0.207
round(below.pov.line.and.other.language / speak.non.eng, 2)
# So, living below the poverty line is more prevalent in households where they speak another language than
# English (20%), compared to 14.6% of all Americans. Language and povery is dependent.

# Product rule for independent events: P(A and B) = P(A) x P(B)
# General product rule, Bayes' way:
# P(A and B) = P(A | B) x P(B)

# Probability Tree:
# -----------------
# TODO: Do some visualizing of probability (decision) trees! See Swaziland HIV example from week 2 video 7

# Bayesian Inference:
# -------------------
# Two dice, a 6 sided die and a 12 sided die. What is the prob. of rolling a value >= 4 with these two dice? It is:
value <- 4
bad.die.sides <- 6
good.die.sides <- 12
bad.die.roll.ge4.prob <- (bad.die.sides - (value - 1)) / bad.die.sides
good.die.roll.ge4.prob <- (good.die.sides - (value - 1)) / good.die.sides
# So, if the good die is in your right hand, the prob of rolling >= 4 with the die in your right hand is
# <good.die.prob>, and the prob of not rolling >= 4 is <1 - good.die.prob>
# Do a probability tree: Rolled number >= 4, is good die in the right hand or bad die in the right hand?
good.die.in.right.hand <- .5 # prior
bad.die.in.right.hand <- .5 # prior
prob.good.die.right.hand.roll.ge4 <- good.die.in.right.hand * good.die.roll.ge4.prob
prob.good.die.right.hand.roll.le4 <- good.die.in.right.hand * (1 - good.die.roll.ge4.prob)
prob.bad.die.right.hand.roll.ge4 <- bad.die.in.right.hand * bad.die.roll.ge4.prob
prob.bad.die.right.hand.roll.le4 <- bad.die.in.right.hand * (1 - bad.die.roll.ge4.prob)
# Posterior probability: P(good.die.in.right.hand AND roll.ge.4) | P(roll.ge.4)
# Posterior probability: P(hyphotesis | data)
prob.good.die.right.hand.roll.ge4 / (prob.good.die.right.hand.roll.ge4 + prob.bad.die.right.hand.roll.ge4)


# Normal distribution:
# --------------------
# A college admissions officer wants to determine which of the two 
# applicants scored better on their standardized test with respect to the 
# other test takers: Pam, who earned an 1800 on her SAT, or Jim, who 
# scored a 24 on his ACT?
# SAT scores ~ N(mean = 1500, SD = 300)
# ACT scores ~ N(mean = 21, SD = 5)

par(mfrow=c(1,2))
#hist(rnorm(10000, mean=1500, sd=300), col="powderblue", main="Pam")
plot(density(rnorm(10000, mean=1500, sd=300)), col="blue", main="Pam")
abline(v=1800, lwd=2, col="red")
#hist(rnorm(10000, mean=21, sd=5), col="wheat", main="Jim")
plot(density(rnorm(10000, mean=21, sd=5)), col="blue", main="Jim")
abline(v=24, lwd=2, col="red")
par(mfrow=c(1,1))
# How many sd's are they above the mean?
# Create Z-scores (Z = (observation - mean) / SD) for Pam and Jim:
Pam <- (1800 - 1500) / 300
Pam # Pam is 1 Z-score (= the number of SD's above or below the mean) above the mean.
Jim <- (24 - 21) / 5
Jim # Jim is 0.6 Z-score above the mean. So: Pam's result is best!
Pam.pnorm <- pnorm(1800, mean=1500, sd=300)
round(Pam.pnorm, 2) # = 0.84. So, Pam scored better than 84% of the SAT takers.

# A friend of yours tells you that she scored in the top 10% on the SAT. 
# What is the lowest possible score she could have gotten?
# Since the area under the curve is 1, the precentile score for the cutoff value is:
# 1 - 0.10 = 0.90
# Z-Score associated with .90 from the table is 1.28
# Z = 1.28 = (X - 1500) / 300
# - multiply by 300 on each side:
x <- (1.28 * 300) + 1500
x # x = 1884, so if you score above 1884, you're in the top 10%
# Or by using qnorm function in R:
qnorm(0.90, 1500, 300)

# Suppose weights of checked baggage of airline passengers follow a nearly normal 
# distribution with mean 45 pounds and standard deviation 3.2 pounds. Most airlines 
# charge a fee for baggage that weigh in excess of 50 pounds. 
# What percent of airline passengers are expected to incur this fee?
# baggage ~ N(mean = 45, SD = 3.2)
1 - pnorm(50, mean=45, sd=3.2)

# Binomial distribution:
# Example of "n choose k" with prob = 0.35
n <- 4
k <- 1
p <- .35
p^k * (1 - p)^(n - k)
# How many scenarios yield 1 success in 4 trials? Use "n choose k":
n <- 4
k <- 1
factorial(n) / (factorial(k) * factorial(n - k))
# How many scenarios yield 2 success in 9 trials?
n <- 9
k <- 2
factorial(n) / (factorial(k) * factorial(n - k))
# Easy way to do this manually:
# 9 * 8 * 7! / 2 * 1 * 7!
# 7! cancels out, so we're left with: (9 * 8) / (2 * 1) = (72 / 2) = 36
# Or, using choose function in R:
choose(9, 2)

# According to a 2013 Gallup poll, worldwide only 13% of employees are 
# engaged at work (psychologically committed to their jobs and likely to be 
# making positive contributions to their organizations). Among a random sample
# of 10 employees, what is the probability that 8 of them are engaged at work?
n <- 10
k <- 8
p.engaged <- 0.13
p.not.engaged <- 1 - p.engaged # 0.87
# So: P(k = 8) = (10 choose 8) * 0.13^8 * 0.87^2
(factorial(n) / (factorial(k) * factorial(n - k))) * p.engaged^k * p.not.engaged^(n - k)
# We get a very small probability, because we're asking for the probability that 8 out of 10
# are engaged at work (that is, 80%), when in fact worldwide it's only 13% as an average that
# are in fact engaged at work. So there's just a tiny chance of actually finding 8 out of 10
# in a random sample.
# Using dbinom in R we get the same result:
dbinom(8, size=10, p=0.13)
# Expected value (mean) of binomial distribution: mu = n*p
n * p.engaged
# Standard deviation of binomial distribution: sigma = sqrt(n * p * (1 - p))
sqrt(n * p.engaged * (1 - p.engaged))
# Test it: http://bit.ly/dist_calc

# Normal approximation to binomial (np >= 10 and n(1-p) >= 10):
barplot(dbinom(0:10, size=10, p=0.25), col="steelblue4") # Right skewed
barplot(dbinom(0:20, size=20, p=0.25), col="steelblue4") # Less right skewed
barplot(dbinom(0:100, size=100, p=0.25), col="steelblue4") # Looking very normal!
# Facebook friends example (approximation to the normal dist:
round(sum(dbinom(70:245, size=245, p=0.25)), 3)
# What is the minimum required for a binomial distribution with 
# p = 0.25 to closely follow a normal distribution?
# We need the max of n * 0.25 >= 10 and n * 0.75 >= 10
# So, solve for n:
n1 <- 10/0.25
n2 <- 10/0.75
max(n1, n2)

# According to a 2014 Gallup poll, 56% of uninsured Americans who plan to get health 
# insurance say they will do so through a government health insurance exchange. What 
# is the probability that in a random sample of 10 people exactly 6 plan to get health 
# insurance through a government health insurance exchange?
round(choose(10, 6) * 0.56^6 * (1 - 0.56)^4, 3)
round(dbinom(6, size=10, p=0.56), 3)
barplot(dbinom(0:10, size=10, p=0.56), col="gray")

# What is the probability that at least 60 out of a random sample of 100 uninsured 
# Americans plan to get health insurance through a government health insurance exchange?
sum(dbinom(60:100, size=100, p=0.56))

# While it is often assumed that the probabilities of having a boy or a girl are the same,
# the actual probability of having a boy is slightly higher at 0.51. Suppose a couple plans
# to have 3 children. What is the probability that exactly 2 of them will be boys?
p.boy <- 0.51
p.girl <- 1 - p.boy
n <- 3
k <- 2
scenarioes <- factorial(n) / (factorial(k) * factorial(n - k)) # or: choose(n, k)
# (3*2*1)/(2*1): 2*1 cancels out, leaving 3
# Sum all possible scenarioes:
((p.boy * p.boy * p.girl) + (p.boy * p.girl * p.boy) + (p.girl * p.boy * p.boy))
# Compare to dbinom:
dbinom(2, 3, p.boy)

# Example: Create a binomial density plot:
my.dbinom <- function(n, p, k) {
  # choose(n, k) = (factorial(n) / factorial(k) * factorial(n - k))
  # return (choose(n, k) * p^k * ((1 - p)^(n - k)))
  return ((factorial(n) / (factorial(k) * factorial(n - k))) * p^k * (1 - p)^(n - k))
}

plot(my.dbinom(40, 0.5, 1:40), type="o", pch=21, bg="cyan", col="blue", cex.axis=.8, cex.main=.8, main="BinomDensity")
plot(my.dbinom(40, 0.2, 1:40), type="o", pch=21, bg="cyan", col="blue", cex.axis=.8, cex.main=.8, main="BinomDensity")
plot(my.dbinom(40, 0.1, 1:40), type="o", pch=21, bg="cyan", col="blue", cex.axis=.8, cex.main=.8, main="BinomDensity")

barplot(my.dbinom(10, 0.25, 1:10), col="steelblue4", cex.axis=.8, cex.main=.8) # Right skewed
barplot(my.dbinom(20, 0.25, 1:20), col="steelblue4", cex.axis=.8, cex.main=.8) # Less right skewed
barplot(my.dbinom(100, 0.25, 1:100), col="steelblue4", cex.axis=.8, cex.main=.8) # Looking very normal!

# Week 3
# ------
# Central limit theorem: The distribution of sample statistics is nearly normal, centered
# at the population mean, and with a standard deviation equal to the population standard
# deviation divided by the square root of the sample size.
my.population <- rnorm(100000)
my.population <- rbeta(100000, 5, 2)
my.sample.means <- numeric()
my.population.sd <- sd(my.population)
my.population.mean <- mean(my.population)
my.std.error.pop <- numeric()
my.std.error.sample <- numeric()

for (counter in 1:200) {
  # NOTE: Here we sample without replacement. If sampling without, n must be < 10% of population
  my.sample <- sample(my.population, 25, replace=F) # NOTE: If pop is skewed, use n > 30
  rows <- match(my.sample, my.population, nomatch=NA)
  my.population <- my.population[-rows] # We need to remove the elements we've taken from the population
  my.sample.means[counter] <- mean(my.sample)
  my.std.error.pop[counter] <- my.population.sd / sqrt(length(my.sample))
  my.std.error.sample[counter] <- sd(my.sample) / sqrt(length(my.sample))
}

se.pop <- round(mean(my.std.error.pop), 2)
se.sample <- round(mean(my.std.error.sample), 2)

par(mfrow=c(1,2))
hist(my.sample.means, col="lightblue", cex.axis=.8, cex=.8, cex.main=.8,
     main=paste0("Sample(SE.p=", se.pop, " SE.s=", se.sample, ")"))
abline(v=my.population.mean, col="red", lwd=2)
abline(v=mean(my.sample.means), col="blue", lwd=2)
hist(my.population, col="wheat", cex.axis=.8, cex=.8, cex.main=.8,
     main="Population")
#lines(density(my.population, bw=1), col="red", lwd=3)
par(mfrow=c(1,1))

# Find critical value for a 95% confidence interval:
# (conf.interval = value +/- critical value)
# (The area covering 95% of the normal distribution curve)
abs(qnorm((1 - 0.95) / 2))

# Backtracking to n for a given ME (Marginal Error):
# ME = z * (s / sqrt(n))
# n = ((z * s) / ME)^2

# A sample of 50 college students were asked how many exclusive relationships they've 
# been in so far. The students in the sample had an average of 3.2 exclusive 
# relationships, with a standard deviation of 1.74. In addition, the sample distribution 
# was only slightly skewed to the right. Estimate the true average number of exclusive 
# relationships based on this sample using a 95% confidence interval.
confint.level <- 1.96 # For 95% ci
x.bar <- 3.2
n <- 50
stddev <- 1.74
SE <- stddev / sqrt(n)
ci.lower <- x.bar + confint.level * SE
ci.upper <- x.bar - confint.level * SE
c(ci.lower, ci.upper)
# So: We are 95% confident that college students on average have been in 2.72 to 3.68 exclusive relationships.
# Not sure if below is correct for reproducing confint result:
college.student.sample <- rnorm(n, sd=stddev, mean=x.bar)
df <- data.frame(x=1:n, y=college.student.sample)
fit <- lm(y ~ x, data=df)
confint(fit)

# p-value:
# --------
