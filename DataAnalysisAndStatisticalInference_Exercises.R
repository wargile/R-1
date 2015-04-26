# Data Analysis and Statistical Inference - Exercises

folder <- "C:/coding/R/Coursera/DataAnalysisAndStatisticalInference/Docs and extra content/os2_data/openintroData/"
op <- par()
par(mar=c(5,4.6,2,.8))

# Unit 1, Introduction to Data
# Chapter 1

# Part 1 - Designing studies: 1.1, 1.3, 1.7, 1.15, 1.17, 1.21
# -----------------------------------------------------------

# Exercise 1.1 Of the 224 patients in the treatment group, 45 had a stroke by the
# end of the first year. Using these two numbers, compute the proportion of patients
# in the treatment group who had a stroke by the end of their first year.
treatment.group <- 224
had.stroke <- 45
round(had.stroke / treatment.group, 2)

# Example 1.3 Data were collected about students in a statistics course. Three
# variables were recorded for each student: number of siblings, student height, and
# whether the student had previously taken a statistics course. Classify each of the
# variables as continuous numerical, discrete numerical, or categorical.
# Answer: number of siblings: discrete numerical, student height: numerical, previous course or not: categorical
# (NOTE: If you can count a numerical variable (like siblings above), it is discrete)

# Section 1.2.2, county data
# NOTE: The county data set is an observational study with confounding variables,
# and its data cannot easily be used to make causal conclusions.
county <- read.csv(paste0(folder, "countyComplete.txt"), header=T, sep="\t")
head(county, n=1)
names(data)

plot(county$fed_spending ~ county$poverty, col="blue", main="County data, federal spending by poverty",
     cex.axis=.8, cex.lab=.8, cex.main=1, xlab="Poverty", ylab="Federal spending", cex=.5, pch=21, bg="gray")
plot(county$home_ownership ~ county$housing_multi_unit, col="blue", cex=.5, pch=21, bg="gray",
     main="County data, home ownership versus multi-unit housing", cex.axis=.8, cex.lab=.8, cex.main=1,
     xlab="Multi-unit housing", ylab="Home ownership types")

boxplot(county$median_household_income, horizontal=T, col="purple", pch=21, bg="gray", xlab="Income",
        main="County data, median household income", cex.axis=.8, cex.lab=.8, cex.main=1)
boxplot(county$mean_work_travel, horizontal=T, col="orange", pch=21, bg="gray", xlab="Mean work travel distance",
        main="County data, mean work travel distance", cex.axis=.8, cex.lab=.8, cex.main=1)

d <- density(county$mean_work_travel)
plot(d, col="orange", pch=21, bg="gray", xlab="Mean work travel distance",
        main="County data, mean work travel distance (density)", cex.axis=.8, cex.lab=.8, cex.main=1)
polygon(d, col="wheat")

state.pa <- county[county$state == "Pennsylvania", ]
state.pa$name <- substr(state.pa$name, 1, nchar(as.character(state.pa$name)) - 7)
plot(county$households ~ county$fed_spending, pch=21, bg="gray", col="blue", cex=.5,
     main="County data, households versus federal spending", cex.axis=.8, cex.lab=.8, cex.main=1,
     xlab="Federal spending", ylab="Households")

ggplot(data=state.pa, aes(x=name, y=fed_spending, fill=fed_spending)) + geom_bar(stat="identity") +
  ggtitle("Federal spending PA, by county") + theme(axis.text.x=element_text(angle=90, hjust=1, size=7)) +
  xlab("County name") + ylab("Federal spending")
ggplot(data=state.pa, aes(x=name, y=households, fill=fed_spending)) + geom_bar(stat="identity") +
  ggtitle("Federal spending PA, household density") + theme(axis.text.x=element_text(angle=90, hjust=1, size=7)) +
  xlab("Households") + ylab("Federal spending")

# Exercise 1.7 For the second and third questions above, identify the target population
# and what represents an individual case.
# 1. What is the average mercury content in swordfish in the Atlantic Ocean?
# 2. Over the last 5 years, what is the average time to complete a degree for Duke under-
#    graduate students?
# 3. Does a new drug reduce the number of deaths in patients with severe heart disease?
# Answer (TIP: Read the research question CAREFULLY to get all population properties! See 1st answer below):
# 2) Target population: All Duke under-graduate students WHO completed a degree AND graduated within the last 5 years.
#    Individual case: An under-graduate student at Duke.
# 3) Target population: All patients with severe heart disease.
#    Individual case: A patient with severe heart disease.

# When individuals are randomly assigned to a group, the experiment is called a randomized experiment
# In general, association does not imply causation, and causation can only be inferred from a randomized experiment.
# A confounding variable is a variable that is correlated with both the explanatory and response variables.

# Observational studies come in two forms: prospective (collect information as the events/study unfolds)
# and retrospective studies (collect data after events have taken place, e.g. researchers may review past
# events in medical records).

# Sampling types:
# Simple random samling: Random picks from the whole population
# Stratified sampling: Separate population subgroups with SIMILAR PROPERTIES into designated strata.
# Sample randomly within the stratas. NOTE: We have to randomly sample from ALL stratas.
# Clustered sampling: Separate population into clusters. Sample randomly from within the clusters.
# NOTE: It is NOT a requirement to sample from ALL clusters, as opposed to stratified sampling.

# Memorize chapter 1.5.1 "Principles of experimental design", p. 17.
# Randomized experiments are generally built on four principles: Controlling. Randomization. Replication. Blocking.

# Exercise 1.14 Look back to the study in Section 1.1 where researchers were testing whether stents
# were effective at reducing strokes in at-risk patients. Is this an experiment? Was the study blinded?
# Was it double-blinded?
# Answer: Experimental study (patients were assigned to treatment groups).
# Not blinded, not double-blinded (both patients and researchers could distinguish the treatment given).

# Exercise 1.15: What do scatterplots reveal about the data, and how might they be useful?
# Answer: They reveal the spread of the data, the correlation (neg or pos, if correlation present) between
# the explanatory and response variable. It can reveal a simple or complex variable relationship.

# Exercise 1.17: Describe two variables that would have a horseshoe shaped association in a scatterplot.
# Answer: Generally, a variable relationship that is good "in moderation", that is, good at medium values,
# but bad at very low or very high values (like health/sun exposure or health/water consumption).

# Exercise 1.21: (Getting the mean by dividing by n:) What was n in this sample of emails?
email50 <- read.csv(paste0(folder, "email50.txt"), header=T, sep="\t")
nrow(email50)
colors <- SetColors(gradient=T, range=nrow(email), palette.no=1, setAsPalette=F)
plot(email$num_char ~ email$line_breaks, pch=19, main="Num char by line breaks", col=colors, cex=1.4)
# Answer: n = 50

# Part 2 - Exploratory data analysis: 1.27, 1.39, 1.41, 1.45, 1.49
# ----------------------------------------------------------------

# Exercise 1.27 Height measurements of young students and adult teachers at a K-3
# elementary school were taken. How many modes would you anticipate in this height
# data set?
# Answer: Bimodal (students vary in heights, so one peak there, and another for the teacher variation.
# There are two peaks because students and teachers have very different height distributions):
#        /\
# __/\__|  |__

# Exercise 1.39: What does 0.458 represent in Table 1.35? What does 0.059 represent in Table 1.36?
# Answer: 0.458: the ratio of emails with spam and small numbers, to the total of spam emails
#         0.059: The ratio between total spam emails and emails with spam and small numbers 

# Exercise 1.41: Column properties is most interesting.

# Exercise 1.45: What components of each plot in Figure 1.43 do you find most useful?
# Answer: Boxplot which shows higher median of income as population increases
# Exercise 1.49: Answer: Sample size might be too small to determine if there is a real difference
# in promotion between males and females

# Part 3 - Introduction to inference via simulation: 1.51, 1.53
# -------------------------------------------------------------
# TODO....

# ---------------------------------------------------------------------------------------------------------------

# Unit 2 - Probability and distributions
# Suggested reading:
# OpenIntro Statistics, Chapter 2: Sections 2.1 and 2.2
# OpenIntro Statistics, Chapter 3: Sections 3.1, 3.2, and 3.4
# Suggested exercises:
# Part 2 - Conditional probability: 2.17, 2.21, 2.25
# Part 3 - Normal distribution: 3.3, 3.5, 3.9, 3.11, 3.19
# Part 4 - Binomial distribution: 3.27, 3.29, 3.31, 3.39

# Addition rule of disjoint outcomes (two or more outcomes that can not happen at the same time):
# P(A1 or A2) = P(A1) + P(A2)
# Example: Roll a die twice: Prob. of getting a 1 and a 2 is: 1/6 + 1/6 = 1/3


par <- op
