"
Guessing and Forgetting

Analyzing Simulated Data	
Ken Cor and Gaurav Sood

Last Edited: 2/08/16 by GS

"

# Load data
fake <- read.csv("data/sim_data.csv")
params <- read.csv("data/sim_data_item_params.csv")

# n items
nitems <- (length(fake) -1)/4

# Columns without guessing
t1true	<- paste0("true.t1", 1:nitems)
t2true	<- paste0("true.t2", 1:nitems)

# Vectors of Names
t1 <- paste0("guess.t1", 1:nitems)
t2 <- paste0("guess.t2", 1:nitems)
		
# Raw and True
raw    <- colMeans(nona(fake[,t2]) - nona(fake[,t1]))
true   <- colMeans(nona(fake[,t2true]) - nona(fake[,t1true]))

# LCA correction
# Convert NA to 'd'
fake[,t1][is.na(fake[,t1])] <- "d"
fake[,t2][is.na(fake[,t2])] <- "d"
fake[,t1] <- sapply(fake[,t1], as.character)
fake[,t2] <- sapply(fake[,t2], as.character)

transmatrix <- multi_transmat(fake[,t1], fake[,t2], force9=T)
lca <- guesstimate(transmatrix)$est.learning
lca <- lca[1:(length(lca) -1)] #just items

# LCA and Raw Bias
lcabias <- lca - true
rawbias <- raw - true

# Contextualize the bias
mean(rawbias/true)
mean(lcabias/true)

# Squared Error
lcase <- (lca - true)^2
rawse <- (raw - true)^2

# proportion of times lcase < rawse,....
mean(lcase < rawse)

# Percentage
mean(lcabias/true)
mean(rawbias/true)

# Correlations
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~	
cor(lcabias, params$diff)
cor(rawbias, params$diff)

# Plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load libs
library(ggplot2)
library(grid)

# Plot
pdf("figs/fakeBiasDKDensity.pdf")
par(mar=c(4,2,2,2), xaxs="i", yaxs="i") 
plot(density(lcabias, bw=.12), 
			xlim=c(-.5,.5), ylim=c(0,3.7), 
			type="n", 
			main="", 
			xaxt="n", yaxt="n", 
			ylab="",
			yaxs="i", 
			xlab="Difference Between Estimate and True Effect")
			
polygon(density(lcabias, bw=.12), col=rgb(.40, .40, .40, .25), border=0)
polygon(density(rawbias, bw=.12), col=rgb(.00, .00, .00, .35), border=0)
abline(v=median(lcabias), col=rgb(.2, .2, .2,.85), lty=2, lwd=1.5)
text(.13, 3.5, "Median LCA - True Diff.", cex=.8, col=rgb(.2, .2, .2, .85))
abline(v=median(rawbias), col=rgb(.20, .2, .20, .85), lty=2, lwd=1.5)
text(-.16, 3.3, "Median Raw - True Diff.", cex=.8, col=rgb(.20, .2, .20, .85))	
axis(2, labels=F,tck = 0)
axis(1, seq(-.5,.5,.1), labels=nolead0s(seq(-.5,.5,.1)), cex.axis=.85, tck = -.01)
legend(.22,3.3, c("LCA - True Diff.", "Raw - True Diff."), fill=c(rgb(.40, .40, .40, .25), rgb(.00, .00, .00, .35)), cex=.85, bty="n")
dev.off()