# Lagerstyringsprinsipper
# -----------------------

# For Simple Moving Average (glidende gjennomsnitt), se TimeSeries3.R

SetStandardOptions()

# 1) Min-Max:
# -----------
# http://www.logistikk-ledelse.no/2009/ma/ma0901.htm
# Se ogs� C:\coding\R\Coursera\edX_CTL.SC1x_SupplyChainAndLogisticsFundamentals\Week 5\Lectures
# I Wilsons formel (EOQ - Economic Order Quantity) bruker vi f�lgende faktorer
# (MERK: Betegnelser kan variere, ingen internasjonal standard!):
D <- sample(5000, 1) # �rlig forbruk
K <- sample(100, 1) # Ordres�rkostnad
C <- sample(100, 1) # Pris pr. stk.
I <- 25/100 # �rlig lagerrente (25% i eksemplet her)

interval <- 20
Q <- seq(0,1600, by=interval)

total.cost <- D / Q * K + Q / 2 * C * I
EOQ <- floor(sqrt((2 * D * K) / (C * I)))
total.cost <- total.cost[-which(total.cost == Inf)]
plot(total.cost, col="blue", type="o", ylim=c(min(total.cost), max(total.cost)), main=paste("EOQ:", EOQ),
     xlab="Ordrest�rrelse", ylab="Totalkostnad")
abline(v=EOQ/interval, col="red")
