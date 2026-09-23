"
Guessing and Forgetting

US: The National Issues Convention
Ken Cor and Gaurav Sood

Last Edited: 2/05/16 by GS

"
# Load data
delib <- read.csv("data/nic1.csv")

# 0, 1, and NA
t1raw <- c("efact1raw", "ffact1raw", "ftfact11raw", "ftfact21raw", "ftfact31raw", "ftfact41raw", "repfact1raw", "demfact1raw")
t2raw <- c("efact2raw", "ffact2raw", "ftfact12raw", "ftfact22raw", "ftfact32raw", "ftfact42raw", "repfact2raw", "demfact2raw")

# Fix
delib[,t2raw][!is.na(delib[,t2raw]) & delib[,t2raw]==TRUE] <- 1
delib[,t2raw][!is.na(delib[,t2raw]) & delib[,t2raw]==FALSE] <- 0

# Lucky		
lucky <- c(.50, .50, .50, .50, .25, .25, .429, .429)

# Reliability
t1_alpha <- round(ltm::cronbach.alpha(nona(delib[,t1raw]))$alpha,3)
t2_alpha <- round(ltm::cronbach.alpha(nona(delib[,t2raw]))$alpha,3)
write.table(t(c("US: The National Issues Convention", t1_alpha, t2_alpha)), file="results/reliability.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# Raw and Stnd 
raw   <- colMeans(nona(delib[,t2raw]) - nona(delib[,t1raw]))
stnd  <- stndcor(delib[,t1raw], delib[,t2raw], lucky)

# Store for stnderr calc
pre_test <- delib[,t1raw]
pst_test <- delib[,t2raw]

# LCA correction
# Convert NA to 'd'
delib[,t1raw][is.na(delib[,t1raw])] <- "d"
delib[,t2raw][is.na(delib[,t2raw])] <- "d"

transmatrix <- multi_transmat(delib[,t1raw], delib[,t2raw])
lca_res <- guesstimate(transmatrix)
lca <- lca_res$est.learning[1:(length(lca_res$est.learning) -1)] #just items

# person level results
indiv <- plevel(delib[,t1raw], delib[,t2raw], delib$female, lucky)

# Write out results
# item results
res   <- cbind(poll="US: The National Issues Convention", itemID=names(stnd$learn), raw = raw, lca = lca, stnd = stnd$learn)

write.table(res, file="results/item_level.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# person results to facilitate gender comparison
res   <- cbind(poll="US: The National Issues Convention", personID=c(1:nrow(delib)), indiv)

write.table(res, file="results/person_level.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# Poll results
# Calculate stnderrs for Poll results
stnderrs <- guess_stnderr(pre_test,pst_test,lucky)$stnderrs.effects

# Write out results

res  <- c("US: The National Issues Convention", n=nrow(delib), nitems = length(raw), raw = mean(raw), lca= mean(lca), stnd = mean(stnd$learn), 
	                                                                        raw_se = stnderrs[1], lca_se = stnderrs[2], stnd_se = stnderrs[3])
write.table(t(res), file="results/pollresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# Goodness of Fit
res <-  fit_dk(delib[,t1raw], delib[,t2raw], lca_res$param.lca[8,], lca_res$param.lca[1:7,])
write.table(t(c(poll="US: The National Issues Convention", lca=mean(res[2,] < .05))), file="results/fitresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")
