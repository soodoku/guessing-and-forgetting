"
   											
Guessing and Forgetting	
Figure 3: LCA/Stnd. Diff.
Ken Cor and Gaurav Sood						
Last Edited: 2/04/16 by GS

"

# Load data
item <- read.csv("results/item_level.csv")

# Diff.
item$lcadiff <- item$lca - item$raw
item$stnddiff <- item$stnd - item$raw

iteml <- melt(item[,c("itemID", "lcadiff", "stnddiff")], id="itemID")
levels(iteml$variable) <- c("CLCA - Raw", "Standard - Raw")

# 
ggplot(data=iteml, aes(factor(variable), value)) + 
	   geom_boxplot(fatten=1) + 
	   geom_hline(yintercept=0, linetype="dotted", size=.3, color="#333333") + 
	   scale_y_continuous(breaks=round(seq(-.2, .8, .2), 2), labels= nolead0s(round(seq(-.2, .8, .2), 2))) + 
	   labs(x="",y="Distribution of Differences Between Other Estimates and the Raw Estimate", size=10) + 
	   theme_bw() + 
	   theme(axis.text    = element_text(size=8),
			 axis.title   = element_text(size=10),
			 axis.ticks.length = unit(.1,"cm"),
		     panel.grid.major = element_blank(),
			 panel.grid.minor = element_blank())
					
ggsave(file="figs/item_lca_stnd.pdf", dpi = 600, width=5, height=5)

