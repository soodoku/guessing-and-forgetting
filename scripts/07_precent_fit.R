

percent_fit<- function(nitems = NULL, poll_fits = NULL, subset = NULL)
{
  res<-1-sum(poll_fits*nitems)/sum(nitems)
  res
}

res <- percent_fit(read.csv("results/pollresults.csv")$nitems,read.csv("results/fitresults.csv")$lca)
write.table(t(c("Overall", round(res,3))), file="results/fitresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")