"
Guessing and Forgetting

Australia Constitutional Referendum
Ken Cor and Gaurav Sood

Last Edited: 02/07/16 by KC
"

# Load data
delib <- read.csv("data/aus.csv")

# 0, 1, and NA			
t1raw <- paste0(c(paste0(c("rp", "qr", "gg", "pr","bus", "wel"), "fact1"), "flagchg1", "anthem1", "wdroyal1", "pargame1"), "r")
t2raw <- paste0(c(paste0(c("rp", "qr", "gg", "pr","bus", "wel"), "fact2"), "flagchg2", "anthem2", "wdroyal2", "pargame2"), "r")

# Lucky
lucky <- c(.25, .25, .25, .50, .25, .50, .20, .20, .333, .333)

# Reliability
t1_alpha <- round(ltm::cronbach.alpha(nona(delib[,t1raw]))$alpha,3)
t2_alpha <- round(ltm::cronbach.alpha(nona(delib[,t2raw]))$alpha,3)
write.table(t(c("Australia Constitutional Referendum", t1_alpha, t2_alpha)), file="results/reliability.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

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
res   <- cbind(poll="Australia Constitutional Referendum", itemID=names(stnd$learn), raw = raw, lca = lca, stnd = stnd$learn)

suppressWarnings(write.table(res, file="results/item_level.csv", col.names = T, row.names = F, quote=F, append=T, sep=",", qmethod = "double"))

# person results to facilitate gender comparison
res   <- cbind(poll="Australia Constitutional Referendum", personID=c(1:nrow(delib)), indiv)

suppressWarnings(write.table(res, file="results/person_level.csv", col.names = T, row.names = F,  quote=F, append=T, sep=",", qmethod = "double"))

# Poll results
# Calculate stnderrs for Poll results
stnderrs <- guess_stnderr(pre_test,pst_test,lucky)$stnderrs.effects

# Write out results

res  <- c("Australia Constitutional Referendum", n=nrow(delib), nitems = length(raw), raw = mean(raw), lca= mean(lca), stnd = mean(stnd$learn), 
	                                                                                 raw_se = stnderrs[1], lca_se = stnderrs[2], stnd_se = stnderrs[3])
suppressWarnings(write.table(t(res), file="results/pollresults.csv", col.names = T, row.names = F, append=T, quote=F, sep=",", qmethod = "double"))

# Goodness of Fit
res <-  fit_dk(delib[,t1raw], delib[,t2raw], lca_res$param.lca[8,], lca_res$param.lca[1:7,])
suppressWarnings(write.table(t(c(poll="Australia Constitutional Referendum", lca=mean(res[2,] < .05))), file="results/fitresults.csv", col.names = T, row.names = F, quote=F, append=T, sep=",", qmethod = "double"))
