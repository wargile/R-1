# Lagerstyringsprinsipper
# -----------------------

# For Simple Moving Average (glidende gjennomsnitt), se TimeSeries3.R

# 1) Min-Max:
# -----------
# http://www.logistikk-ledelse.no/2009/ma/ma0901.htm
# I Wilsons formel (EOQ - Economic Order Quantity) bruker vi følgende faktorer
# (MERK: Betegnelser kan variere, ingen internasjonal standard!):
D <- 8000 # Årlig forbruk
K <- 50 # Ordresærkostnad
C <- 40 # Pris pr. stk.
I <- 25/100 # Årlig lagerrente (25% i eksemplet her)

interval <- 20
Q <- seq(0,1600, by=interval)

total.cost <- D / Q * K + Q / 2 * C * I
EOQ <- floor(sqrt((2 * D * K) / (C * I)))
plot(total.cost, col="blue", type="o", ylim=c(0, 20000), main=paste("EOQ:", EOQ), cex.lab=.7, cex.axis=.7,
     cex.main=.8, xlab="Ordrestørrelse", ylab="Totalkostnad")
abline(v=EOQ/interval, col="red")
