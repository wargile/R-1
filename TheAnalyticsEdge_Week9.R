# The Analytics Edge week 9 - Integer Optimization
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/optimization/
# ------------------------------------------------------------------------------------

# http://en.wikipedia.org/wiki/Integer_programming

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(ggplot2)
library(ggmap)

# Set locale to US, to ensure that there aren't formatting issues in assignments/inputs:
Sys.setlocale("LC_ALL", "C")

SetStandardOptions()

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 9/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# 1) SPORTS SCHEDULING: AN INTRO TO INTEGER OPTIMIZATION
# ------------------------------------------------------

# 2) eHARMONY - MAXIMIZING THE PROBABILITY OF LOVE
# ------------------------------------------------

# 3) OPERATING ROOM SCHEDULING - MAKING HOSPITALS RUN SMOOTHLY
# ------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------

# Quick questions 1 (SPORTS SCHEDULING: AN INTRO TO INTEGER OPTIMIZATION)
# -----------------------------------------------------------------------

# 1) a) Suppose that you are trying to schedule 3 games between 6 teams (A, B, C, D, E, and F) that will occur simultaneously.
#       Which of the following are feasible schedules? Select all that apply.
#       Answer: A plays B, C plays D, and E plays F, A plays F, B plays E, and C plays D, A plays D, B plays E, and C plays F
#    b) How many different feasible schedules are there?
#       Answer: 15 (6 take 2: combn(6, 2), or use: factorial(6)/(factorial(2)*factorial(6-2)))

# 2) For each of the decisions below, indicate if the decision variables would be binary, integer, or neither.
#    a) We have 20 students, and we want to assign them to one of two groups.
#       Answer: Binary
#    b) The owner of 5 clothing stores needs to decide how many shirts, pants, and hats to send to each store,
#       given historical sales data.
#       Answer: Integer
#    c) After try-outs, the coach of a basketball team needs to decide which people should make the team (15 people tried out).
#       Answer: binary
#    d) A fertilizer company is trying to decide how much (in grams) of three different compounds to add to each bag of fertilizer.
#       Answer: Neither

# 3) TODO!

# 4) TODO!

# Quick questions 2 (eHARMONY - MAXIMIZING THE PROBABILITY OF LOVE)
# -----------------------------------------------------------------

# 1) eHarmony has a distinguishing feature that makes it stand out from other online dating websites. What is this feature?
#    Answer: eHarmony only suggests matches to users, instead of allowing users to browse. 

# 2) In the previous video, we saw a small example with 3 men and 3 women. We defined a "match" as an assignment of each man
#    to exactly one woman, and each woman to exactly one man. The optimal match in the previous video was to assign man 1 to
#    woman 3, man 2 to woman 1, and man 3 to woman 2.
#    a) How many different feasible matches are there in the example with 3 men and 3 women? Answer: 6
factorial(3)
#    b) How many different feasible matches are there in the example with 5 men and 5 women? Answer: 120
factorial(5)

# 3) On slide 4 of the previous video, we gave the following confusion matrix to predict compatibility between users.
#    What is the accuracy of the model, according to the confusion matrix below? Answer: 0.7607
cf <- matrix(c(1030,227,126,92), nrow=2, byrow=T)
cf
accuracy <- sum(diag(cf)) / sum(cf)
accuracy


# ---------------------------------------------------------------------------------------------------------------------------------
# HOMEWORK:

# 1) SELECTING PROFITABLE HOTEL SITES
# -----------------------------------
# This problem is based on: http://pubsonline.informs.org/doi/abs/10.1287/inte.20.2.12

# The final regression model is given by:
# Profitability = 39.05 - 5.41*(State Population per Inn) + 5.86*(Price of the Inn) - 
#   3.09*(Square Root of the Median Income of the Area) + 1.75*(College Students in the Area)

# PROBLEM 1.1 - SELECTING THE MOST PROFITABLE HOTELS. Answer: See below.
# According to the regression equation given above, which variables positively affect Profitability? Select all that apply.
# Answer: Price of the Inn + College Students in the Area (= the positive coefficients)

# PROBLEM 1.2 - SELECTING THE MOST PROFITABLE HOTELS. Answer: Profitability of hotel 1 = 44.26
State.Population.per.Inn <- -1.00
Price.of.the.Inn <- -0.30
Square.Root.of.the.Median.Income.of.the.Area <- -0.81
College.Students.in.the.Area <- -0.54
profitability <- 39.05 - (5.41*State.Population.per.Inn) + (5.86*Price.of.the.Inn) -
  (3.09*Square.Root.of.the.Median.Income.of.the.Area) + (1.75*College.Students.in.the.Area)
exp(profitability) / (1 + exp(profitability))
1 / (1 + exp(profitability))
profitability # NOTE: This is the answer!

# PROBLEM 1.3 - SELECTING THE MOST PROFITABLE HOTELS. Answer: Hotel 2 has the highest predicted profitability.
# In your spreadsheet, compute the predicted profitability for all hotels. Which hotel has the highest predicted profitability?

# PROBLEM 1.4 - SELECTING THE MOST PROFITABLE HOTELS. Answer: Hotel 8 has the lowest predicted profitability.
# In your spreadsheet, compute the predicted profitability for all hotels. Which hotel has the lowest predicted profitability?

# PROBLEM 1.5 - SELECTING THE MOST PROFITABLE HOTELS. Answer: 1 hotel (price of hotel 2 is 10.000.000, thus using up the budget)
# La Quinta has a budget of $10,000,000 to spend on hotels. Suppose we just used a "greedy" approach where we selected the most
# profitable hotels until we ran out of budget. So we would start by buying the hotel we predict to be the most profitable, and
# then if we had enough budget left, we would buy the hotel we predict to be the second most profitable, etc.
# How many hotels would we purchase with this approach?

# PROBLEM 1.6 - SELECTING THE MOST PROFITABLE HOTELS. Answer: 53.38 (profitability of hotel 2)
# What would our total predicted profitability be? (This is the sum of the predicted profitability of all hotels we purchase.)

# PROBLEM 2.1 - AN OPTIMIZATION APPROACH. Answer: Objective value = 269.924681 (see SelectingHotels.ods)
# Now, build an optimization model in your spreadsheet to select hotels. The decision variables are whether or not a hotel is
# selected (binary variables). The objective is to maximize the total predicted profitability. We have two constraints: the
# decision variables should be binary, and the total cost should not exceed the budget of $10,000,000. Formulate and solve
# this model in LibreOffice.
# What is the objective value of the solution?

# PROBLEM 2.2 - AN OPTIMIZATION APPROACH. Answer: 7 hotels
# How many hotels are selected in the solution?

# PROBLEM 2.3 - AN OPTIMIZATION APPROACH. Answer: 6 hotels
# How many hotels located in South Lake Tahoe are selected in the solution?

# PROBLEM 2.4 - AN OPTIMIZATION APPROACH. Answer: Objective value = 205.7009044 (see SelectingHotels_2.ods)
# La Quinta thinks that buying too many hotels in one city is probably not a good idea, and would prefer to diversify in other
# cities, even though it will decrease the sum of the predicted profitability. Add a constraint to limit the number of hotels
# selected in South Lake Tahoe to 2.
# What is the objective value of the solution now?

# PROBLEM 2.5 - AN OPTIMIZATION APPROACH. Answer: 6 hotels are selected in the solution now.
# How many hotels (in total) are selected in the solution now?

# PROBLEM 2.6 - AN OPTIMIZATION APPROACH. Answer: Eureka, Fresno, Los Angeles, South Lake Tahoe
# In which cities do we buy at least one hotel? Select all that apply.

# PROBLEM 2.7 - AN OPTIMIZATION APPROACH. Answer: See below.
# In this problem, we compared the greedy approach with an optimization approach, and saw that the optimization approach was
# much better. This is true in many situations, but not always. In which of the following situations would the greedy approach
# perform as well as the optimization approach? Select all that apply.
# Answer: Selected:
# a) Instead of maximizing the sum of the profitability of the hotels we select, we wanted to maximize the average profitability
#    of the hotels we select.
# b) Instead of having a budget constraint, we had a constraint on the number of different hotels we can select (for example,
#    we want to maximize profitability given that we can only select 2 hotels).


# 2) ASSIGNING SALES REGIONS AT PFIZER TURKEY
# -------------------------------------------

# PROBLEM 1.1 - FORMULATING THE PROBLEM. Answer: 88 decision vars (22 bricks * 4SRs)

# PROBLEM 1.2 - FORMULATING THE PROBLEM. Answer:
# Answer: Minimize the sum of d<i,j> times the decision variables, summed over all i and j. 

# PROBLEM 1.3 - FORMULATING THE PROBLEM. Answer: x<1,1>+x<2,1>+x<3,1>+x<4,1>=1 (NOTE: x<(All SRs),(Brick 1)> == 1)
# We have three main types of constraints. The first is that each brick must be assigned to exactly one SR.
# Which of the following constraints models this restriction for brick 1?

# PROBLEM 1.4 - FORMULATING THE PROBLEM. Answer: See below.
# The second main type of constraint tries to balance the workload between the SRs. The sum of the index values of the bricks
# of an SR correspond to his/her total workload and should be approximately 1. To model this, we'll constrain the workload of
# each SR to range between 0.8 and 1.2. Denote the index value of brick <j> by I<j>. Which of the following constraints do we
# want to add to our model for SR 1?
# Answer: 0.8 <= (I1*x<1,1> + I2*x<1,2> + ... + I22*x<1,22>) <= 1.2

# PROBLEM 1.5 - FORMULATING THE PROBLEM. Answer: 
# TODO....


# 3) CLASS ASSIGNMENTS IN AN ELEMENTARY SCHOOL
# --------------------------------------------
# This problem is based on a case study: http://pubsonline.informs.org/doi/abs/10.1287/ited.2013.0111

# PROBLEM 1.1 - SOLVING THE BASIC PROBLEM. Answer:

# PROBLEM 1.2 - SOLVING THE BASIC PROBLEM. Answer:

# PROBLEM 1.3 - SOLVING THE BASIC PROBLEM. Answer:

# PROBLEM 1.4 - SOLVING THE BASIC PROBLEM. Answer:

# PROBLEM 2.1 - ADDING LOGICAL CONSTRAINTS. Answer:

# PROBLEM 2.2 - ADDING LOGICAL CONSTRAINTS. Answer:

# PROBLEM 2.3 - ADDING LOGICAL CONSTRAINTS. Answer:

# PROBLEM 2.4 - ADDING LOGICAL CONSTRAINTS. Answer:
