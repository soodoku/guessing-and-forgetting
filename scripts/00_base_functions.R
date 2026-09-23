"
Guessing and Forgetting

Base Functions
Ken Cor and Gaurav Sood

Last Edited: 2/12/16 by GS

"

options(stringsAsFactors=FALSE)

# From package goji: https://github.com/soodoku/goji/
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Convert Missing to 0
nona <- function(x){
	x[is.na(x)] <- 0; 
	x
}

# Take out lead 0s
# Useful for Graphs
nolead0s <- function(x) {
	gsub("0\\.","\\.", x)
}

# Rescale 0 to 1
zero1 <- function(x, minx=NA, maxx=NA){
	stopifnot(identical(typeof(as.numeric(x)), 'double'))
	if(typeof(x)=='character') x <- as.numeric(x)
	res <- NA
	if(is.na(minx)) res <- (x - min(x,na.rm=T))/(max(x,na.rm=T) -min(x,na.rm=T))
	if(!is.na(minx)) res <- (x - minx)/(maxx -minx)
	res
}

# From package guess: https://github.com/soodoku/guess
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transition Matrix
# ~~~~~~~~~~~~~~~~~~~~~~~
transmat <- function(pre_test_var, pst_test_var, subgroup=NULL, force9=FALSE) {	

	if (!is.null(subgroup))
	{
		pre_test_var <-subset(pre_test_var, subgroup)
		pst_test_var <-subset(pst_test_var, subgroup)
	}
	
	# No NAs
	pre_test_nona <- nona(as.character(pre_test_var))
	pst_test_nona <- nona(as.character(pst_test_var))
	
	# Check if the vector has only one of the 4 values
	if (!all(unique(c(pre_test_nona, pst_test_nona)) %in% c(NA, "1", "0", "d"))) stop("The input vectors can only contain: 0, 1, NA, d")

	pre_pst <- paste0(pre_test_nona, pst_test_nona)

	# Get the numbers
	x00     <- sum(pre_pst=="00")
	x10     <- sum(pre_pst=="10")  
	x01     <- sum(pre_pst=="01")  
	x11     <- sum(pre_pst=="11")  
	xd0     <- sum(pre_pst=="d0") 
	xd1     <- sum(pre_pst=="d1") 
	x1d     <- sum(pre_pst=="1d") 
	xdd     <- sum(pre_pst=="dd")
	x0d     <- sum(pre_pst=="0d")
	
	res <- c(x00, x01, x10, x11)
	names(res) <- c("x00", "x01", "x10", "x11")

	if ((xd0 + xd1 + x1d + xdd != 0) | force9) {

		res <- c(x00, x01, x0d, x10, x11, x1d, xd0, xd1, xdd)
		names(res) <- c("x00", "x01", "x0d", "x10", "x11", "x1d", "xd0", "xd1", "xdd")
	} 

    return(invisible(res))

}

# Data.frame transition matrix
multi_transmat <- function (pre_test = NULL, pst_test=NULL, subgroup=NULL, force9=FALSE, agg=TRUE) 
{

	# Checks
	if (!is.data.frame(pre_test)) stop("Specify pre_test data.frame.") # pre_test data frame is missing
	if (!is.data.frame(pst_test)) stop("Specify pst_test data.frame.") # post_test data frame is missing
	if(length(pre_test)!=length(pst_test)) stop("Lengths of pre_test and pst_test must be the same.") # If different no. of items
	
	# Subset
	if (!is.null(subgroup))
	{
		pre_test <- subset(pre_test, subgroup)
		pst_test <- subset(pst_test, subgroup)
	}
	
	# No. of items
	n_items <- length(pre_test)

	# Initialize results
	res <- list()
	
	# Get transition matrix for each item pair
	for (i in 1:n_items)
	{
		# cat("\n Item", i, "\n")
		res[[i]] <- transmat(pre_test[,i], pst_test[,i], force9=force9)
	}

	# Prepping results
	row_names <- paste0("item", 1:n_items) 
	col_names <- names(res[[1]])

	res       <- matrix(unlist(res), nrow=n_items, byrow=T, dimnames=list(row_names, col_names))

	if (agg==TRUE) {
		res       <- rbind(res, colSums(res, na.rm=T))
		rownames(res)[nrow(res)] <- "agg"
	}

	return(invisible(res))
}

# Standard Correction for Guessing

stndcor <- function(pre_test=NULL, pst_test=NULL, lucky=NULL, item_names=NULL)
{	
	if (!is.data.frame(pre_test)) stop("Specify pre_test data.frame.") # pre_test data frame is missing
	if (!is.data.frame(pst_test)) stop("Specify pst_test data.frame.") # post_test data frame is missing
	if (is.null(lucky))    stop("Specify lucky vector.")           # lucky vector is missing
	
	if (length(unique(length(pre_test), length(pst_test), length(lucky)))!=1) stop("Length of input varies. Length of pre_test, pst_test, and lucky must be the same.")
	n_items <- length(pre_test) # total number of items
	pre_test_cor  <- pst_test_cor <- stnd_cor <- NA
	
	pre_test_cor <- mapply(function(x, y) sum(x==1, na.rm = TRUE) - sum(x == 0, na.rm = TRUE)/(1/y - 1), pre_test, lucky)
	pst_test_cor <- mapply(function(x, y) sum(x==1, na.rm = TRUE) - sum(x == 0, na.rm = TRUE)/(1/y - 1), pst_test, lucky)
	stnd_cor     <- pst_test_cor - pre_test_cor

	# Names of the return vector
	if (is.null(item_names)) {
		names(pre_test_cor) <- names(pst_test_cor) <- names(stnd_cor) <- names(pre_test)
	} else {
		names(pre_test_cor) <- names(pst_test_cor) <- names(stnd_cor) <- item_names
	}

	pre   <- pre_test_cor/nrow(pre_test)
	pst   <- pst_test_cor/nrow(pst_test)
	learn <- stnd_cor/nrow(pre_test)

	return(list(pre=pre, pst=pst, learn=learn))
}


# Likelihood Functions
eqn1dk = function(x, g1=NA, data) {
	sum(x[1:7])
}

guessdk_lik <- function(x, g1=x[8], data) 
{
	lgg <- x[1]
	lgk <- x[2] 
	lgc <- x[3]
	lkk <- x[4]
	lcg <- x[5]
	lck <- x[6]
	lcc <- x[7]
			
	vec <- NA
	vec[1] <- (1 - g1)*(1 - g1)*lgg
	vec[2] <- (1 - g1)*g1*lgg + (1 - g1)*lgk
	vec[3] <- (1 - g1)*lgc
	vec[4] <- (1 - g1)*g1*lgg
	vec[5] <- g1*g1*lgg+g1*lgk+lkk
	vec[6] <- g1*lgc
	vec[7] <- (1 - g1)*lcg
	vec[8] <- g1*lcg + lck
	vec[9] <- lcc
	
	-sum(data*log(vec))
}

# Estimate LCA Correction + Params and produce results
guesstimate <- function(transmatrix=NULL, nodk_priors=c(.3,.1,.1,.25), dk_priors=c(.3,.1,.2,.05,.1,.1,.05,.25)) 
{
		
	# Initialize results mat
	nitems	<- nrow(transmatrix)
	nparams <- ifelse(ncol(transmatrix)==4, 4, 8)
	est.opt <- matrix(ncol=nitems, nrow=nparams)
	
	# priors
	nodk_priors <- nodk_priors
	dk_priors   <- dk_priors

	# effects
	effects	<- matrix(ncol=nitems, nrow=1)

	# calculating parameter estimates
	if (nparams == 4) {
		for (i in 1:nitems) {
			est.opt[,i]	 <- tryCatch(solnp(nodk_priors, guess_lik, eqfun = eqn1, eqB = c(1), LB = rep(0,4), UB = rep(1,4), data=transmatrix[i,])[[1]], error=function(e) NULL)
		}

		effects[,1:nitems] <- est.opt[2,]

	} else {
		for (i in 1:nitems) {
			est.opt[,i]	 <- tryCatch(solnp(dk_priors, guessdk_lik, eqfun = eqn1dk, eqB = c(1), LB = rep(0,8), UB = rep(1,8), data=transmatrix[i,])[[1]], error=function(e) rep(NA,8))
		}
		
		effects[,1:nitems] 	<- est.opt[2,] + est.opt[6,]
	}
	
	# Assign row names
	if (nrow(est.opt) == 8){
		row.names(est.opt) 	<- c("lgg", "lgk", "lgc", "lkk", "lcg", "lck", "lcc", "gamma")
	} else {
		row.names(est.opt) 	<- c("lgg", "lgk",  "lkk", "gamma")
	}

	res <- list(param.lca=est.opt, est.learning=effects)

	return(invisible(res))
}

# Standard Error
guess_stnderr <- function(pre_test=NULL, pst_test=NULL, lucky=NULL, nsamps=100, seed = 31415) 
{
  
  # pre_test <- alldat[,t1]; pst_test <-  alldat[,t2]; nsamps=10; seed = 31415
  
  # build a df
  df 		<- data.frame(cbind(pre_test, pst_test))
  
  # set nitems and nparams based on df		
  nitems 	    <- ncol(df)/2	
  nparams     <- ifelse(sum(is.na(pre_test))==0, 4, 8)	
  
  #define matrices to store samples and sample results		
  resamps.results <- t1.t2 <- list()
  resamps.eff  <- matrix(ncol=3, nrow=nsamps)
  
  stnderrs.lca.params <- matrix(ncol=nitems, nrow=nparams)
  
  stnderrs.effects <- avg.effects <- matrix(ncol=3, nrow=1)
  
  resamps.lca.params <- rep(list(matrix(nrow = nsamps, ncol = nparams)), nitems)
  
  #extracting samples from the data	
  set.seed(seed)
  resamples <- lapply(1:nsamps, function(i) df[sample(1:nrow(df), replace = T),])
  
  # Looping through the samples; estimating based one each
  for(i in 1:length(resamples)) {
    t1.t2[[i]]<- resamples[[i]]
    t1.t2[[i]][is.na(t1.t2[[i]])] <- "d"
    transmatrix_i           <- multi_transmat(t1.t2[[i]][,1:nitems],t1.t2[[i]][,(nitems+1):(2*nitems)], force9=TRUE)
    resamps.results[[i]] 	<- guesstimate(transmatrix_i)
    resamps.eff[i,1]  <- mean(colMeans(nona(resamples[[i]][,(nitems+1):(2*nitems)]) - nona(resamples[[i]][,1:nitems])))
    resamps.eff[i,2] 	<- mean(resamps.results[[i]]$est.learning[1:(length(resamps.results[[i]]$est.learning) -1)])
    resamps.eff[i,3] 	<- mean(stndcor(resamples[[i]][,1:nitems], resamples[[i]][,(nitems+1):(2*nitems)], lucky)$learn)
    
    for(j in 1:nitems) {
      resamps.lca.params[[j]][i,]    <- resamps.results[[i]]$param.lca[,j]
    }	
  }
  
  # Now getting standard error and means of different effects
  stnderrs.effects[1,]	<- sapply(as.data.frame(resamps.eff), sd, na.rm=T)			
  avg.effects[1,]			<-	sapply(as.data.frame(resamps.eff), mean, na.rm=T)
  
  for (j in 1:nitems) {
    stnderrs.lca.params[,j]	<- sapply(as.data.frame(resamps.lca.params[[j]]), sd, na.rm=T)						
  }
  
  # Rounding off for print
  #resamps.agg 			<- round(resamps.agg, 0)
  #stnderrs.lca.params	<- round(stnderrs.lca.params, 3)
  #avg.effects			<- round(avg.effects, 3)
  #stnderrs.effects 		<- round(stnderrs.effects, 3)
  
  # Assigning row names
  colnames(avg.effects) <- colnames(stnderrs.effects) <- c("raw","lca","stnd")
  
  if(nrow(stnderrs.lca.params)==8) {
    row.names(stnderrs.lca.params) <- c("lgg", "lgk", "lgc", "lkk", "lcg", "lck", "lcc", "gamma")
  } else { 
    row.names(stnderrs.lca.params) <- c("lgg", "lgk",  "lkk", "gamma")
  }
  
  # Get results out			    
  res 		<- list(stnderrs.lca.params, avg.effects, stnderrs.effects)
  names(res)	<- c("stnderrs.lca.params", "avg.effects", "stnderrs.effects") 
  
  res
}

# Person Level LCA adjustment
# pre <- delib[,t1raw]; pst <- delib[,t2raw]; female <- delib$female; lucky<-lucky
plevel <- function(pre=NULL, pst=NULL, female=NULL, lucky=NULL) 
{

  n <- nrow(pre)
  
  transmatrix <- multi_transmat(pre, pst)  
  lca_res     <- guesstimate(transmatrix)
 
  # LCA gamma
  lca_gamma <- lca_res$param.lca["gamma",(1:(length(lca_res$est.learning) -1))]

  # Adj
  t1_stnd <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1), pre, lucky)
  t1_lca  <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pre, lca_gamma)

  t2_stnd <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pst, lucky)
  t2_lca  <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pst, lca_gamma)

  t1_stnd_f <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pre[!is.na(female) & female==1,], lucky)
  t1_stnd_m <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pre[!is.na(female) & female==0,], lucky)

  t1_lca_f  <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pre[!is.na(female) & female==1,], lca_gamma)
  t1_lca_m  <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pre[!is.na(female) & female==0,], lca_gamma)

  t2_stnd_f <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pst[!is.na(female) & female==1,], lucky)
  t2_stnd_m <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pst[!is.na(female) & female==0,], lucky)

  t2_lca_f  <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pst[!is.na(female) & female==1,], lca_gamma)
  t2_lca_m  <- 1 - mapply(function(x, y) (sum(x==0)/(1/y-1))/sum(x==1),  pst[!is.na(female) & female==0,], lca_gamma)

  d_t1_stnd <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pre, t1_stnd))
  d_t1_lca  <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pre, t1_lca))
  
  d_t2_stnd <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pst, t2_stnd))
  d_t2_lca  <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pst, t2_lca))
  
  d_t1_stnd_f <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pre[!is.na(female) & female==1,], t1_stnd_f))
  d_t1_stnd_m <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pre[!is.na(female) & female==0,], t1_stnd_m))
  
  d_t2_stnd_f <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pst[!is.na(female) & female==1,], t2_stnd_f))
  d_t2_stnd_m <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pst[!is.na(female) & female==0,], t2_stnd_m))
  
  d_t1_lca_f <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pre[!is.na(female) & female==1,], t1_lca_f))
  d_t1_lca_m <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pre[!is.na(female) & female==0,], t1_lca_m))

  d_t2_lca_f <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pst[!is.na(female) & female==1,], t2_lca_f))
  d_t2_lca_m <- as.data.frame(mapply(function(x, y) ifelse(x==1, y, x), pst[!is.na(female) & female==0,], t2_lca_m))

  # person level results
  d_t1_stnd <- as.data.frame(sapply(d_t1_stnd, function(x){ x[x=='d'] <- 0; as.numeric(x)}))
  d_t1_lca  <- as.data.frame(sapply(d_t1_lca,  function(x){ x[x=='d'] <- 0; as.numeric(x)}))
  
  d_t2_stnd <- as.data.frame(sapply(d_t2_stnd, function(x){ x[x=='d'] <- 0; as.numeric(x)}))
  d_t2_lca  <- as.data.frame(sapply(d_t2_lca, function(x) { x[x=='d'] <- 0; as.numeric(x)}))
  
  d_t1_stnd_f <- as.data.frame(sapply(d_t1_stnd_f, function(x){ x[x=='d'] <- 0; as.numeric(x)}))
  d_t1_lca_f  <- as.data.frame(sapply(d_t1_lca_f,  function(x){ x[x=='d'] <- 0; as.numeric(x)}))

  d_t2_stnd_f <- as.data.frame(sapply(d_t2_stnd_f, function(x){ x[x=='d'] <- 0; as.numeric(x)}))
  d_t2_lca_f  <- as.data.frame(sapply(d_t2_lca_f, function(x) { x[x=='d'] <- 0; as.numeric(x)}))

  d_t1_stnd_m <- as.data.frame(sapply(d_t1_stnd_m, function(x){ x[x=='d'] <- 0; as.numeric(x)}))
  d_t1_lca_m  <- as.data.frame(sapply(d_t1_lca_m,  function(x){ x[x=='d'] <- 0; as.numeric(x)}))

  d_t2_stnd_m <- as.data.frame(sapply(d_t2_stnd_m, function(x){ x[x=='d'] <- 0; as.numeric(x)}))
  d_t2_lca_m  <- as.data.frame(sapply(d_t2_lca_m, function(x) { x[x=='d'] <- 0; as.numeric(x)}))

  t1_raw  <- rowMeans(pre==1)
  t1_lca  <- rowMeans(d_t1_lca)
  t1_stnd <- rowMeans(d_t1_stnd)

  raw <-  rowMeans(pst==1) - rowMeans(pre==1)
  lca <-  rowMeans(d_t2_lca) - rowMeans(d_t1_lca)
  stnd <- rowMeans(d_t2_stnd) - rowMeans(d_t1_stnd)
  
  indiv   <- cbind(t1_raw, t1_lca, t1_stnd, raw, lca, stnd, female)
  colnames(indiv) <- c("t1_raw", "t1_lca", "t1_stnd","raw", "lca", "stnd", "female")

  res <- indiv
  res
  
}

# Goodness of fit

fit_dk <- function(pre_test, pst_test, g, est.param, force9=FALSE) 
{

	data    <- multi_transmat(pre_test, pst_test, force9=force9)
	data    <- data[(1:nrow(data)-1),] # remove the agg.
	expec	<- matrix(ncol=nrow(data), nrow=9)
	fit		<- matrix(ncol=nrow(data), nrow=2)
	colnames(fit) <- rownames(data)
	rownames(fit) <- c("chi-square", "p-value")

	for(i in 1:nrow(data)){
		gi			<- g[[i]]
		expec[1, i]	<- (1 - gi)*(1-gi)*est.param[1,i]*sum(data[i,])
		expec[2, i]	<- ((1 - gi)*gi*est.param[1,i] + (1 - gi)*est.param[2,i])*sum(data[i,])
		expec[3, i]	<- ((1 - gi)*est.param[3,i])*sum(data[i,])
		expec[4, i]	<- ((1 - gi)*gi*est.param[1,i])*sum(data[i,])
		expec[5, i]	<- (gi*gi*est.param[1,i]+gi*est.param[2,i]+est.param[4,i])*sum(data[i,])
		expec[6, i]	<- (gi*est.param[3,i])*sum(data[i,])
		expec[7, i]	<- ((1 - gi)*est.param[5,i])*sum(data[i,])
		expec[8, i]	<- (gi*est.param[5,i] + est.param[6,i])*sum(data[i,])
		expec[9, i]	<- est.param[7,i]*sum(data[i,])
		test 		<- suppressWarnings(chisq.test(expec[,i], p=data[i,]/sum(data[i,])))
		fit[1:2,i]	<- round(unlist(test[c(1,3)]),3)
	}


	fit
}