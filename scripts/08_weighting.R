invar_wt<- function(poll_effects = NULL, poll_stnderrs = NULL, subset = NULL)
{
  #Produces mean, meadian, and inverse variance weighted average across polls
  #s.e. is for the inverse variance weighted average
  
  require(rmeta)
  res<-matrix(ncol=3, nrow=4)
  for (i in 1:3)
  {
    met<-meta.summaries(poll_effects[,i],poll_stnderrs[,i])
    res[1,i]<-mean(poll_effects[,i])
    res[2,i]<-median(poll_effects[,i])
    res[3,i]<-met$summary
    res[4,i]<-met$se.summary
  }
  row.names(res)<-c("mean", "median", "inverse variance wieghted avg", "s.e. invar_wt")
  round(res,3)
}

res <- invar_wt(read.csv("results/pollresults.csv")[,4:6],read.csv("results/pollresults.csv")[,7:9])
res1 <- c("", "", res[1,], "", "", "")
res2 <- c( "", "", res[2,], "", "", "")
res3 <- c("", "", res[3,1], res[4,1], res[3,2], res[4,2], res[3,3], res[4,3])

write.table(t(c("Mean", res1)), file="results/pollresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")
write.table(t(c("Median", res2)), file="results/pollresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")
write.table(t(c("Inverse Variance", res3)), file="results/pollresults.csv", col.names = F, row.names = F, append=T, sep=",", qmethod = "double")