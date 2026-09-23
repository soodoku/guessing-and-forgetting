"
Guessing and Forgetting

Simulate Data
Ken Cor and Gaurav Sood

Last Edited: 2/08/16 by GS

"

# Inference should be at item level

# Parameter Distribution and Ranges:
	# Ability 
		# rnorm, roughly -3,3 
		# we may want to draw a fresh respondent sample for each item
	# Difficulty 
		# uniform, item-level, covers most of the range of theta (-2.5,2.5)
	# Learning
		# Each item: A base chance that everyone who doesn't know the item gets (this varies between (.05 and .6))
		# By person: b*ability; b varies from 0 to 1.5 (so covers all scenarios)
	# Gamma 
		# uniform, covers broad range (.1, .6), orthogonal to item diff.
		# item level
	# Confession
		# uniform, covers broad range (.1, .6), orthogonal to item diff.

# Creating data
	# 1. sample ability: random normal (just do it once for now)
	# 2. sample difficulty: random uniform
	# 3. Produce time 1 data using IRT
	# 4. sample alpha (learn): random uniform
	# 5. sample beta (learn): random uniform
	# 6. t2: Convert 0s to 1s as impact of learning, rbinom = alpha + beta*(ability at t1)
	# 7. sample gamma. random uniform.
	# 8. sample confess. random uniform.
	# 9. create observed: convert 0s to 1s probabilistically using gamma
	# 10. create observed: convert 0s to NA probabilistically using confess

# Load libraries
	library(ltm)

	set.seed(76543)				# set seed for reproducibility
	
	n 			<- 500 		# number of respondents
	nitems		<- 1000		# number of items
	
	theta	<- rnorm(n)												# standard normal abilities
	diff 	<- seq(-3, 3, 	 length.out=nitems)						# difficulty
	gamma  	<- seq(.1, .60,  length.out=nitems)[sample(1:nitems)]	# lucky guessing
	alpha	<- seq(.1,.50,   length.out=nitems)[sample(1:nitems)] 	# base level of learning by item
	beta	<- seq(0, .60,   length.out=nitems)[sample(1:nitems)]	# rich get richer process
	confess <- seq(0, .15, 	 length.out=nitems)[sample(1:nitems)] 	# confession rates

	# Creating data
		wave1		<- rmvlogis(n, cbind(diff, 1), z.vals = theta)
		wave2		<- wave1
		
	# Setting the knowledge gains
	for(i in 1:ncol(wave1)){
		probab 		<- ifelse(range(alpha[i] + beta[i]*theta)[2] >= 1 | range(alpha[i] + beta[i]*theta)[1] < 0 , zero1(alpha[i] + beta[i]*theta), alpha[i] + beta[i]*theta) 
		wave2[,i]	<- ifelse(wave1[,i]==0, rbinom(n, 1, probab), wave1[,i])
	}
			
	# Posthoc Introduction of guessing and confessing
	# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	# Combine data from diff. waves; create observed data
		waves 	<- cbind(wave1, wave2)
		wavem 	<- matrix(ncol=ncol(waves), nrow=nrow(waves))
	
	# Add confessions
		# Confess by item
		confess2   <- rep(confess, 2)		

		for(i in 1:ncol(waves)){
			wavem[,i] <- ifelse(waves[,i]==0, ifelse(rbinom(n, 1, confess2[i]), NA, 0), waves[,i])
		}
	
	# Add Guessing
		# Gamma by item
		gamma2   <- rep(gamma, 2)		

		# Only the ignorant can guess
		# Those who guess are lucky gamma_i of the time on the ith item
		for(i in 1:ncol(waves)){
			wavem[,i] <- ifelse(!is.na(wavem[,i]) & wavem[,i]==0, rbinom(n, 1, gamma2[i]), wavem[,i])
		}
		
	# Check
		mean(rowMeans(wave2 - wave1, na.rm=T))
		mean(rowMeans(wavem[,(nitems+1):ncol(wavem)] - wavem[,1:nitems], na.rm=T))
	
	# Create fake data dataset
		rawdata 	<- waves
		guessdata	<- wavem
		
		alldat 									<- data.frame(rawdata, guessdata)
		names(alldat)[1:nitems]					<- paste0("true.t1", 1:nitems)
		names(alldat)[(nitems +1):(2*nitems)]	<- paste0("true.t2", 1:nitems)
		names(alldat)[(2*nitems+1):(3*nitems)]	<- paste0("guess.t1", 1:nitems)
		names(alldat)[(3*nitems +1):(4*nitems)]	<- paste0("guess.t2", 1:nitems)
	
	# Output data
		write.csv(alldat, file="data/sim_data.csv")
		write.csv(cbind(diff, alpha, beta, gamma, confess), file="data/sim_data_item_params.csv")
	