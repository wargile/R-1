# Lagerstyringsprinsipper
# -----------------------

# For Simple Moving Average (glidende gjennomsnitt), se TimeSeries3.R

# 1) Min-Max:
# -----------
# http://www.logistikk-ledelse.no/2009/ma/ma0901.htm
# I Wilsons formel (EOQ - Economic Order Quantity) bruker vi f�lgende faktorer
# (MERK: Betegnelser kan variere, ingen internasjonal standard!):
D <- 8000 # �rlig forbruk
K <- 50 # Ordres�rkostnad
C <- 40 # Pris pr. stk.
I <- 25/100 # �rlig lagerrente (25% i eksemplet her)

interval <- 20
Q <- seq(0,1600, by=interval)

total.cost <- D / Q * K + Q / 2 * C * I
EOQ <- floor(sqrt((2 * D * K) / (C * I)))
plot(total.cost, col="blue", type="o", ylim=c(0, 20000), main=paste("EOQ:", EOQ), cex.lab=.7, cex.axis=.7,
     cex.main=.8, xlab="Ordrest�rrelse", ylab="Totalkostnad")
abline(v=EOQ/interval, col="red")