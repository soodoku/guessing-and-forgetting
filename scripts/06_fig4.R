"
   											
Guessing and Forgetting	
Fig 2: Item Level Plot
Ken Cor and Gaurav Sood						
Last Edited: 2/08/16 by GS

"
# Load data
poll <- read.csv("results/pollresults.csv")

# Melt the data
polll 					<- melt(poll[1:23,c("X", "raw", "lca")], id="X")
levels(polll$variable) 	<- c("Raw", "LCA")

pollnames      <-  c("AU Monarchy", "BTP 2004 Primaries", "BTP 2004 GE", "BTP 2005", "BTP 2007",  "Bulgaria", "Ca. Ref.", "CPL", "Denmark", "EU 2007", "EU 2009", "N. Ireland", "Michigan", "NIC",  "San Mateo", "SWEPCO", "UK BGE", "UK Crime",  "UK EU", "UK Health", "UK Monarchy", "Vermont", "WTU")
polll$X 	   <-  rep(pollnames, 2)

ggplot(data = polll, aes(y = X, x = value, colour = variable)) +
		geom_point() + 
		scale_colour_manual("", values = c("#000000", "#aaaaaa")) +
		scale_x_continuous(breaks=round(seq(0, .5, .1), 2), labels= nolead0s(round(seq(0, .5, .1), 2))) + 
		labs(x="Mean",y="", size=10) + 
		theme_bw() +
		theme(panel.grid.major.y = element_line(colour = "#efefef", linetype = "dotted"),
		      panel.grid.minor.x = element_blank(),
		      panel.grid.major.x = element_line(colour = "#f7f7f7", linetype = "solid"),
		      panel.border       = element_blank(),
		      legend.position  = "bottom",
		      legend.key       = element_blank(),
		      legend.key.width = unit(1,"cm"),
		      axis.title   = element_text(size=10),
		      axis.text    = element_text(size=8),
		      axis.ticks.y = element_blank(),
		      axis.line.x  = element_line(colour = 'red', size = 3, linetype = 'dashed'))

ggsave(file="figs/poll_raw_lca_bw.pdf", dpi = 600, width=5, height=5)

