"
Guessing and Forgetting

Denmark: Euro
Ken Cor and Gaurav Sood

Last Edited: 2/05/16 by GS

"

# Load data
delib <- read.csv("data/dk.csv")

# 0, 1, and NA
t1raw <- paste0(c(paste0("t0pk", 1:6), paste0("t0ppk", 7), paste0("t0ppk", 9), paste0("t0ppk", 11)), "raw")
t2raw <- paste0(c(paste0("t2pk", 1:6), paste0("t2ppk", 7), paste0("t2ppk", 9), paste0("t2ppk", 11)), "raw")

# Lucky
lucky <- c(.333, .333, .25, .25, .25, .25, .50, .50, .50)

# Reliability
t1_alpha <- round(ltm::cronbach.alpha(nona(delib[,t1raw]))$alpha,3)
t2_alpha <- round(ltm::cronbach.alpha(nona(delib[,t2raw]))$alpha,3)
write.table(t(c("Denmark: Euro", t1_alpha, t2_alpha)), file="results/reliability.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

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
res   <- cbind(poll="Denmark: Euro", itemID=names(stnd$learn), raw = raw, lca = lca, stnd = stnd$learn)

write.table(res, file="results/item_level.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# person results to facilitate gender comparison
res   <- cbind(poll="Denmark: Euro", personID=c(1:nrow(delib)), indiv)

write.table(res, file="results/person_level.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# Poll results
# Calculate stnderrs for Poll results
stnderrs <- guess_stnderr(pre_test,pst_test,lucky)$stnderrs.effects

# Write out results

res  <- c("Denmark: Euro", n=nrow(delib), nitems = length(raw), raw = mean(raw), lca= mean(lca), stnd = mean(stnd$learn), 
	                                                                        raw_se = stnderrs[1], lca_se = stnderrs[2], stnd_se = stnderrs[3])
write.table(t(res), file="results/pollresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# Goodness of Fit
res <-  fit_dk(delib[,t1raw], delib[,t2raw], lca_res$param.lca[8,], lca_res$param.lca[1:7,])
write.table(t(c(poll="Denmark: Euro", lca=mean(res[2,] < .05))), file="results/fitresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")
