# Coursera - Game Theory
# ----------------------
# http://no.wikipedia.org/wiki/Spillteori

# Week 1
# ------

# TCP Backoff Game (http://gametheory.cs.ubc.ca/tcpbackoff)
# ---------------------------------------------------------
# Sending TCP packets: NodeStart->Node1->Node2->Node3->...->Node<n>->NodeEnd
# TCP acknowledgment:  NodeEnd->Node<n>->...->Node3->Node2-Node1->NodeStart
# If one or more nodes are overwhelmed by messages, throws some away without notifying anyone
# Computer waits a certain time for acknowledgement, and if no ack received, sends message again
# In case of no ack first time, it waits a little longer next time for ack before resending,
# because it "understands" that bombarding the network with fewer messages might be good for reducing congestion.
# Other computers on the internet are also doing the same thing.
# That's why the net is rarely saturated.
# This process is called the "TCP Backoff Mechanism".

# Question: Should you run your computer with a correctly implemented TCP (that has the "Backoff" mechanism),
# or install a "booster" mechanism (advertised by a browser popup) that does not implement it?

# This problem is an example of what we call a two-player game:
# * both use a correct implementation: both get 1 ms delay
# * one correct, one defective: 4 ms for correct, 0 ms for defective
# * both defective: both get a 3 ms delay
# (This problem scales very well to a larger number of players)


# Self-Interested Agents and Utility Theory
# -----------------------------------------
# An agent's preferences can be described by utility functions.
# - A single-dimenensional and linear function is enough (x:wealth, y:health).
# - An agent's response to uncertainty can be captured purely by an expected value. 
# von Neumann & Morgenstern 1944, famous theorem on this
# (https://archive.org/stream/theoryofgamesand030098mbp/theoryofgamesand030098mbp_djvu.txt)


# Defining Games - Key Ingredients
# --------------------------------
# Players, Actions, Payoffs
# Two standard representations:
# - Normal Form (a.k.a. Matrix form, Strategic Form): List what payoffs players get as a function of their actions
#   Assumes that all players move simultaneously
# - Extensive Form: Includes timing of moves, players move sequentially (think Chess), keeps track of other player
#   before making decision (think Poker, bet sequentially)
tcp.players <- list(CC=c(-1,-1), CD=c(-4,0), DC=c(0,-4), DD=c(-3,-3))
tcp.backoff.game <- matrix(tcp.players, ncol=2)
tcp.backoff.game[1:4]
tcp.backoff.game[[1]][1]

# In-video quiz:
# Task: Consider the normal form: N={1,2}. A<i> = {Movie, Theater}. Each player chooses an action
# of either going to the movies or the theater. Player 1 prefers to see a movie with Player 2 over going
# to the theater with player 2. Player 2 prefers going to the theatre with player 1 over seeing a movie
# with player 1. Players get a payoff of 0 if they end up at a different place than the other player.
# Which restrictions should a,b,c,d satisfy?
# Matrix, Movie/theater: [a,b][0,0][0,0][c,d]
# a) a > c, b > d
# b) a > d, b < c
# c) a > c, b > d
# d) a < c, b < d
# Answer: (c) (a > c, b < d) is true.
# Since Player 1 prefers to seeing a movie over going to the theater, then Player 1's payoff
# under (Movie, Movie) has to be larger than the payoff under (Theater, Theater). Thus, a > c.
# Since Player 2 prefers to go to the theater over seeing a movie, then Player 2's payoff under
# (Theater, Theater) has to be larger than the payoff under (Movie, Movie). Thus, b < d.

# Types of games:
# ---------------
# 1) Non-conflicting games: Both Players cooperate for best payoff:
#   For instance: Walking on the sidewalk: TURN LEFT/TURN RIGHT(1,1 - 0,0 - 1,1 - 0,0):
#   If we walk on the same sidewalk towards each other, if both turn right or turn left, we do not collide (1,1),
#   if we turn different directions, either left or right, we collide (0,0)
# 2) Conflicting games: Players choose opposite alternative to get best payoff (0,1 - 0,0 - 1,0 - 0,0).
#    No payoff if both Players choose the same Action
# 3) Combination of cooperation and competition: "Battle of the Sexes Game": (2,1 - 0,0 - 1,2 - 0,0)
#    Husband and wife wants to go to the movies together (primary payoff), but want to see different movies.
#    If the choose his movie, he gets the better U/Payoff (2,1), and vice versa (1,2), but they both gain
#    something (going to the movies together). If they end up seeing different movies, they both loose (0,0)

# Nash Equilibrium Intro (John Nash):
# -----------------------------------
# 1) John Maynard Smith: Beauty Contest Game (stylised version of a game described below):
#    Stock Market prediction: When is it time to sell (price is going up, but is company value inflated, and
#    is the stock then going to drop suddenly?)
#    Newspaper beauty contest competition: You guess what OTHER readers think is the most beautiful woman.
#    Keynes likened investment to this concept: It's not only what YOU think of the stock, but what you think
#    OTHER investors think about the stock
#    Example:
#    1) Each player names an integer between 1 and 100.
#    2) The player who names the integer closest to two thirds of the *average* integer wins a prize,
#       the other players get nothing.
#    3) Ties are broken uniformly at random (roll a dice, flip a coin, etc. to decide who is then the winner)
#    Strategy: Many people probably choose a mean of 50, so I will choose 2/3 of that (33). Others think:
#    Most people probably choose 2/3 of 50 (33), so I will choose 2/3 of that again (22). 22 won. But the Nash
#    Equilibrium is 1. On the second round, many people guess 1.

# Best Response:
# --------------
# Agent Vector: {a<-i>, a<i>}
# a<-i> is everyone else's response. a<i> is you.
# Your best response (a<i>*) is:
# NOTE: BR = Best Response Set, [IS PART OF] = E symbol, A<i> = all your Actions, [ALL] = upside-down A symbol
# a<i>* [IS PART OF] BR(a<-i>) iff [ALL]A<i>, u<i>(a<i>*, a<-i>) > ui(a<i>, a<-i>)

# The definition of Nash Equilibrium:
# a = {a<1> ... a<n>} is a "pure strategy" Nash Equilibrium iff [ALL]i, a<i> [IS PART OF] BR(a<-i>)

# Example 1: Prisoner's Dilemma: COOPERATE/DEFECT(-1,-1 - -4,0 - 0,-4 - -3,-3)
# The dominant strategy is both agents defecting (-3,-3 - both not cooperating, both given worst punishment (u))
# This is the Nash Equlibrium in this game.
# Example 2: Walking on the sidewalk: TURN LEFT/TURN RIGHT(1,1 - 0,0 - 1,1 - 0,0):
# This has TWO "Pure Strategy" Nash Eqilibria (1,1). 
# Example 3: Combination of cooperation and competition: "Battle of the Sexes Game": (2,1 - 0,0 - 1,2 - 0,0)
# This also has TWO "Pure Strategy" Nash Eqilibria (2,1 - 1,2). Although a difference here compared to ex. 2.
# (See later for "Mixed Strategies" games for the difference between ex 2) and ex 3))
# Example 4: Matching pennies: HEADS/TAILS(1,-1 - -1,1 - -1,1 - 1,-1)
# Here it is NO "Pure Strategy" Nash Eqilibrium, because this game cycles forever:
# You want to play tails to win, I lose (-1,1) -> If I play tails too, I win (1,-1), but then you loose,
# so you want to play heads (-1,1) -> But then I lose... etc.

# Dominant Strategies:
# --------------------
# Strategy: Choice of Action.
# Let s<i> and s'<i> be two strategies for player i, and let S<-i> be the set of stragey profiles
# for all the other players.
# Definition:
# s<i> strictly dominates s'<i> if [ALL]s<-i> [IS PART OF] S<-i>, u<i>(s<i>, s<-i>) > u<i>(s'<i>, s<-i>)
# Explanation: The Player has better utility when he plays s<i> than s'<i>
# s<i> very weakly dominates s'<i> if [ALL]s<-i> [IS PART OF] S<-i>, u<i>(s<i>, s<-i>) >= u<i>(s'<i>, s<-i>)
# Dominant stragey for Prisoner's Dilemma is D regardless of wether Player chooses C or D here:
PD <- list(CC=c(-1,-1), CD=c(-4,0), DC=c(0,-4), DD=c(-3,-3))
prisoners.dilemma <- matrix(PD, ncol=2)
# Why? Because -3 is better than -4, and 0 is better than -1

# Pareto Optimality:
# ------------------
# Analyzing games from the outside as an observer, can some outcomes be said to be better?
# An outcome o that is AT LEAST as good for every player as another outcome o':
# The outcome o then pareto-dominates the outcome o'.
# Example: o={7,8} versus o'={7,2}
# Definition (Pareto Optimality):
# An outcome o<*> is Pareto-optimal if there is no other outcome that Pareto-dominates it.
# - Sidewalk Game: Pareto optimal = 1,1 and 1,1
# - Battle of the Sexes Game: Pareto optimal = 1,2 and 2,1
# - Pennies game: None (eternally cycling best outcome)
# - Prisoner's Dilemma Game: Pareto optimal = -1,-1 and -4,0 and 0,-4 (-1,-1 dominates -3,-3)
