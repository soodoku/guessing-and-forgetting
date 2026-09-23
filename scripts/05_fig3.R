"
   											
Guessing and Forgetting	
Figure 4: Male/Female
Ken Cor and Gaurav Sood						
Last Edited: 2/14/16 by GS

"
		
# Load data
data <- read.csv("results/person_level.csv")
data <- subset(data, !is.na(female))
data$female <- ifelse(data$female==1, "female", "male")

# Get the diffs
data$lca_diff <- data$lca - data$raw
data$stnd_diff <- data$stnd - data$raw

# Melt
#iteml 		 <- melt(data[,c("poll", "female", "lca", "raw", "stnd")], id=c("poll", "female"))
iteml 		 <- melt(data[,c("poll", "female", "lca_diff", "stnd_diff")], id=c("poll", "female"))
		
iteml$gender_type <- factor(with(iteml, paste0(variable, female)))

levels(iteml$gender_type) 	<- c("LCA - Raw\nFemale", "LCA - Raw\nMale", "Stnd - Raw\nFemale", "Stnd - Raw\nMale")
iteml$gender_type 			<- factor(iteml$gender_type, levels=c("Stnd - Raw\nFemale", "Stnd - Raw\nMale", "LCA - Raw\nFemale", "LCA - Raw\nMale"))

ggplot(data=iteml, aes(factor(gender_type), value)) + 
	   geom_boxplot(fatten=1) + 
	   geom_hline(yintercept=0, linetype="dotted", size=.3, color="#333333") + 
			scale_y_continuous(breaks=round(seq(-1, 1, .2), 2), labels= nolead0s(round(seq(-1, 1, .2), 2))) + 
			labs(x="",y="Differences Between Different Estimators by Gender", size=10) + 
			theme_bw() + 
			theme(axis.text    = element_text(size=8),
				  axis.title   = element_text(size=10),
				  axis.ticks.length = unit(.1,"cm"),
				  panel.grid.major = element_blank(),
				  panel.grid.minor = element_blank())					
ggsave(file="figs/item_male_female.pdf", dpi = 600, width=5, height=5)






