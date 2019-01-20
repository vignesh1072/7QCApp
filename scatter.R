library(qcc)
library(ggpubr)
library(scatterplot3d)

MyRaneData <- read.csv(file="E:/R-demo/01 Nov 2018.csv", encoding = "UTF-8", header=TRUE, sep=",")

myRaneDf <- data.frame(MyRaneData)
head(myRaneDf)

plot(myRaneDf$Leakage.CW..lpm.,myRaneDf$NPD..bar., main = "Scatter Plot", xlab = "CW leak", ylab = "leaked pressure drop", las=1, xlim = c(0,2.5), pch=8, col=2)
abline(lm(myRaneDf$NPD..bar.~myRaneDf$Leakage.CW..lpm.), col=4)
lines(smooth.spline(myRaneDf$NPD..bar.,myRaneDf$Leakage.CW..lpm.), lty=6, lwd=10)

ggscatter(myRaneDf, x = "Leakage.CW..lpm.", y = "NPD..bar.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "CW leak", ylab = "leaked pressure drop")


scatterplot3d(myRaneDf$Leakage.CW..lpm., myRaneDf$NPD..bar., myRaneDf$Leakage.CW..lpm., pch = 16)

