"
Guessing and Forgetting

EU 2007	
Ken Cor and Gaurav Sood

Last Edited: 2/05/16 by GS

"
# Load data
delib <- read.csv("data/eu2007.csv")

#3 Subsets (Raw with DK/REF as NA)
t1raw <- c(paste0("t1q",16:24,"r"), "t1q33ar", "t1q33br")
t2raw <- c(paste0("t3q",19:27,"r"), "t3q36ar", "t3q36br")

# Lucky				
lucky <- c(.25, .25, .25, .25, .25, .2, .2, .2, .25, .4, .4)

# Reliability
t1_alpha <- round(ltm::cronbach.alpha(nona(delib[,t1raw]))$alpha,3)
t2_alpha <- round(ltm::cronbach.alpha(nona(delib[,t2raw]))$alpha,3)
write.table(t(c("EU 2007", t1_alpha, t2_alpha)), file="results/reliability.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")
			
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
res   <- cbind(poll="EU 2007", itemID=names(stnd$learn), raw = raw, lca = lca, stnd = stnd$learn)

write.table(res, file="results/item_level.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# person results to facilitate gender comparison
res   <- cbind(poll="EU 2007", personID=c(1:nrow(delib)), indiv)

write.table(res, file="results/person_level.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# Poll results
# Calculate stnderrs for Poll results
stnderrs <- guess_stnderr(pre_test,pst_test,lucky)$stnderrs.effects

# Write out results

res  <- c("EU 2007", n=nrow(delib), nitems = length(raw), raw = mean(raw), lca= mean(lca), stnd = mean(stnd$learn), 
	                                                                        raw_se = stnderrs[1], lca_se = stnderrs[2], stnd_se = stnderrs[3])
write.table(t(res), file="results/pollresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")

# Goodness of Fit
res <-  fit_dk(delib[,t1raw], delib[,t2raw], lca_res$param.lca[8,], lca_res$param.lca[1:7,])
write.table(t(c(poll="EU 2007", lca=mean(res[2,] < .05))), file="results/fitresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")
