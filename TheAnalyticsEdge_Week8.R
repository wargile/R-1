# The Analytics Edge week 8 Linear Optimization
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/wiki/15.071x_2/
# https://courses.edx.org/courses/MITx/15.071x_2/1T2015/courseware/99c0f9582cd64cd489558ef44a402b83/
# --------------------------------------------------------------------------------------------------

# Courses:
# http://theopenacademy.com/content/convex-optimization-i
# http://theopenacademy.com/content/linear-algebra-mit
# https://www.khanacademy.org/math/algebra/systems-of-eq-and-ineq

library(scales)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)


SetStandardOptions()
# Set locale to US, to ensure that there aren't formatting issues in assigmnents/inputs:
Sys.setlocale("LC_ALL", "C")
options(digits=7)

folder <- "C:/coding/R/Coursera/edX_TheAnalyticsEdge/Week 8/Assignment/"

# ---------------------------------------------------------------------------------------------------------------------------------
# Lectures:

# 1) AIRLINE REVENUE MANAGEMENT: AN INTRODUCTION TO LINEAR OPTIMIZATION
# ---------------------------------------------------------------------

# 2) RADIATION THERAPY: AN APPLICATION OF LINEAR OPTIMIZATION
# -----------------------------------------------------------
install.packages("lpSolveAPI")
library(lpSolveAPI)

Radiation <- make.lp(5,6)
set.column(Radiation, 1, c(2,0,0,0,0))
set.column(Radiation, 2, c(0,1,0,0,2))
set.column(Radiation, 3, c(0,0,1.5,1.5,0))
set.column(Radiation, 4, c(0,2,1,0,0))
set.column(Radiation, 5, c(1,0,0,1,2))
set.constr.type(Radiation, c(">=",">=",">=",">=", "<="))
set.rhs(Radiation, c(7,7,7,7,5))
set.objfn(Radiation, c(3,4.5,2.5,1,2,4))
lp.control(Radiation, sense='min')
solve(Radiation)
get.objective(Radiation)
get.variables(Radiation)

# 3) GOOGLE ADWORDS: OPTIMIZING ONLINE ADVERTISING
# ------------------------------------------------
# PPC * CTR = Average price per display
ppc <- 25 # $25 must be paid to Google by advertiser per click
# So, if two users click on the ad, Verizon must pay google $50
ctr <- 0.2 # Click-through-rate for the users (example: 10 users saw the ad, 2 clicked: ctr = 2 / 10 = 0.2)
ppc * ctr # = Average of $5 per user per display

# ---------------------------------------------------------------------------------------------------------------------------------

# Quick questions 1 (AIRLINE REVENUE MANAGEMENT: AN INTRODUCTION TO LINEAR OPTIMIZATION):
# ---------------------------------------------------------------------------------------

# 1) Suppose that, as in the previous video, regular seats cost $617 and discount seats cost $238. We are selling 166 seats.
#    The demand for regular seats is 150 and the demand for discount seats is 150.
# a) How many discount seats should we sell? Answer: 16
# b) What would our total revenue be, for both regular and discount seats, assuming that we have a full plane?
# Answer: $96358
total.seats <- 166
price.regular <- 617
price.discount <- 238
demand.regular <- 150
demand.discount <- 150
(demand.regular * price.regular) + ((total.seats - demand.regular) * price.discount)

# 2) a) How many decision variables would we have if there were 4 different types of tickets? Answer: 4 (the four different
#       seat types to sell. The variables are sometimes called decision variables because the problem is to decide what value
#       each variable should take)
#    b) How many constraints would we have if there were 4 different types of tickets (with two different types of tickets,
#       our model has 5 constraints: one capacity constraint, two demand constraints, and two non-negativity constraints)?
#       Answer: 1 capacity constraint + 4 demand constraints + 4 non-negativity constraints = 9 constraints

# 3) In the previous video, we solved our optimization problem in LibreOffice. In your spreadsheet, change the demand for
#    regular seats to 50 (cell D5). Then re-solve the model. (See Airline_RM.ods).
#    a) What is the new optimal objective value? Answer: 58458
#    b) Now change the demand of regular seats to 200. What is the new optimal objective value? Answer: 102422

# 4) a) Suppose that our demand for regular seats remains the same (100) but our demand for discount seats goes down to 100.
#       Will our optimal solution change? Answer: No. 
#    b) Now suppose that our demand for regular seats remains the same (100) but our demand for discount seats goes down to 50.
#       Will our optimal solution change? Answer: Yes
#    Explanation: In the first case, our optimal solution will not change because we are only offering 66 discount seats. So even
#    if the demand goes down to 100, we are not meeting the demand. But in the second case, we can only offer 50 discount seats.
#    So our airplane will not be full, and our optimal solution will change to 100 regular seats and 50 discount seats.

# 5) In your spreadsheet, change the capacity to 250 in the capacity constraint, the regular demand to 150, and the discount
#    demand to 150. Then re-solve the model. What is the objective value of the optimal solution?
#    Answer: 116350

# 6) *** TODO! ***

# Quick questions 2 (RADIATION THERAPY: AN APPLICATION OF LINEAR OPTIMIZATION):
# -----------------------------------------------------------------------------

# 1) In the next video, we'll be formulating the IMRT problem as a linear optimization problem.
#    What do you think the decision variables in our problem will be? Answer: The intensities of the beamlets.

# 2) In the previous video (2), we constructed the optimization problem (see the last slide). If the beamlet intensity
#    of the first beamlet is set to 3, how much radiation will that beamlet deliver to tumor voxels?
#    a) Answer: 6
#    b) Answer: 9 (Beamlet 1 hits one tumor voxel, and two healthy tissue voxels. At unit intensity, it delivers
#       a dose of 2 to the tumor voxel, a dose of 2 to the first healthy tissue voxel, and a dose of 1 to the second healthy
#       tissue voxel. At intensity 3, this means that it will deliver a dose of 2*3 = 6 to the tumor voxel, and 2*3 + 1*3 = 9
#       to the healthy tissue voxels.)

# 3) In our optimal solution, we are giving the maximum allowed dose to the spinal cord (5). If we were to relax this,
#    how much could we decrease the objective? Change the right-hand-side (RHS) of the spinal cord constraint to 6,
#    and re-solve the model. By how much did we decrease the objective? (Hint: the previous objective value was 22.75)
#    Answer: The objective was decreased with 0.5833333333

# 4) In the previous video, we discussed a Head and Neck case with 132,878 voxels total, 9,777 voxels in the tumor,
#    and 328 beamlets. How many decision variables does our optimization model have? Answer: 328 (the beamlets)

# 5) In your spreadsheet from Video 3, make sure that you have solved the original small example problem (change the spinal
#    cord limit back to 5 and re-solve if you have changed it, and make sure the objective value is 22.75).
#    Now, change the weight for the spinal cord term in the objective to 5.
#    a) Without re-solving, what does the objective value of the current solution change to?
#    Answer: a) 42.75
#    b) Now re-solve the model. What does the objective change to?
#    Answer: b) 25.67

# -----------------------------------------------------------------------------------------------------------------
# HOMEWORK UNIT 8:

# HOMEWORK 1: INVESTMENT MANAGEMENT UNDER TAXATION
# ------------------------------------------------
# http://en.wikipedia.org/wiki/Portfolio_optimization

# PROBLEM 1.1 - FORMULATING THE PROBLEM. Answer: Decision variables: 8 (the number of different stocks to sell)

# PROBLEM 1.2 - FORMULATING THE PROBLEM. Answer: See below
# 1) We'll assume for this problem that you can't sell more shares of stock than you own, and you can't buy additional
#    shares. What is the maximum value your decision variables can be? Answer: 150
# 2) What is the minimum value your decision variables can be? Answer: 0

# PROBLEM 1.3 - FORMULATING THE PROBLEM. Answer: See below
# Your objective is to maximize the estimated value of your stock portfolio next year. To do this, you should sum the
# estimated value of each stock next year. Suppose you sell x shares of your stock in Microsoft. What is the estimated
# value of your Microsoft stock next year? Answer: $34.55 * (150 - x)

# PROBLEM 1.4 - FORMULATING THE PROBLEM. Answer: 1165.985
# You need to make sure you get $10,000 in cash from selling your stocks, after taxes and transaction costs. How much would
# you get in cash, after taxes and transaction costs, if you sell 50 shares of your Intel stock?
# (NOTE: Change Intel stock number to 50 and look at the "Net cashflow" result in Investment.ods)

# PROBLEM 2.1 - ANALYZING THE SOLUTION. Answer:

# PROBLEM 2.2 - ANALYZING THE SOLUTION. Answer:

# PROBLEM 2.3 - ANALYZING THE SOLUTION. Answer:

# PROBLEM 3.1 - ADJUSTING THE FORMULATION. Answer:

# PROBLEM 3.2 - ADJUSTING THE FORMULATION. Answer:

# PROBLEM 3.3 - ADJUSTING THE FORMULATION. Answer:

# PROBLEM 3.4 - ADJUSTING THE FORMULATION. Answer:


# HOMEWORK 2: FILATOI RIUNITI
# ---------------------------

# PROBLEM 1.1 - FORMULATING THE OPTIMIZATION PROBLEM. Answer:

# PROBLEM 1.2 - FORMULATING THE OPTIMIZATION PROBLEM. Answer:

# PROBLEM 1.3 - FORMULATING THE OPTIMIZATION PROBLEM. Answer:

# PROBLEM 1.4 - FORMULATING THE OPTIMIZATION PROBLEM. Answer:

# PROBLEM 2.1 - SENSITIVITY ANALYSIS. Answer:

# PROBLEM 2.2 - SENSITIVITY ANALYSIS. Answer:

# PROBLEM 2.3 - SENSITIVITY ANALYSIS. Answer:

# PROBLEM 2.4 - SENSITIVITY ANALYSIS. Answer:

# PROBLEM 3.1 - DATA ESTIMATES. Answer:

# PROBLEM 3.2 - DATA ESTIMATES. Answer:

# PROBLEM 3.3 - DATA ESTIMATES. Answer:


# HOMEWORK 3: GASOLINE BLENDING
# -----------------------------

# PROBLEM 1.1 - THE FORMULATION. Answer:

# PROBLEM 1.2 - THE FORMULATION. Answer:

# PROBLEM 2.1 - THE SOLUTION. Answer:

# PROBLEM 2.1 - THE SOLUTION. Answer:

# PROBLEM 2.2 - THE SOLUTION. Answer:

# PROBLEM 2.3 - THE SOLUTION. Answer:

# PROBLEM 2.4 - THE SOLUTION. Answer:

# PROBLEM 3.1 - SENSITIVITY ANALYSIS AND SHADOW PRICES. Answer:

# PROBLEM 3.2 - SENSITIVITY ANALYSIS AND SHADOW PRICES. Answer:

# PROBLEM 3.3 - SENSITIVITY ANALYSIS AND SHADOW PRICES. Answer:

# PROBLEM 3.4 - SENSITIVITY ANALYSIS AND SHADOW PRICES. Answer:


# HOMEWORK 4: EVEN' STAR ORGANIC FARM (OPTIONAL)
# ----------------------------------------------
# https://www.facebook.com/EvenStarOrganicFarm/info

# PROBLEM 1.1 - FORMULATING THE PROBLEM. Answer:

# PROBLEM 1.2 - FORMULATING THE PROBLEM. Answer:

# PROBLEM 1.3 - FORMULATING THE PROBLEM. Answer:

# PROBLEM 1.4 - FORMULATING THE PROBLEM. Answer:

# PROBLEM 1.5 - FORMULATING THE PROBLEM. Answer:

# PROBLEM 1.6 - FORMULATING THE PROBLEM. Answer:

# PROBLEM 2.1 - SOLVING THE MODEL. Answer:

# PROBLEM 2.2 - SOLVING THE MODEL. Answer:

# PROBLEM 2.3 - SOLVING THE MODEL. Answer:

# PROBLEM 2.4 - SOLVING THE MODEL. Answer:

# PROBLEM 3.1 - SENSITIVITY ANALYSIS. Answer:

# PROBLEM 3.2 - SENSITIVITY ANALYSIS. Answer:

# PROBLEM 3.3 - SENSITIVITY ANALYSIS. Answer:

# PROBLEM 3.4 - SENSITIVITY ANALYSIS. Answer:
