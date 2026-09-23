## Replicating Guessing and Forgetting

Start by downloading the replication materials. 

Change your working directory to the replication materials folder:

```{r}
# setwd(replication_dir)
```

The replication folder ships with two subfolders: `data` and `scripts`. Two notes about the Deliberative Polling data:

1. The data were collected by James Fishkin and his colleagues. Learn more at http://cdd.stanford.edu. 
2. In the data that ships, we have coded `don't knows' as NAs. Existing NAs in the data set (extremely few) were converted to 0s.  

The scripts are configured such that for them to work, you need to create two additional folders in your replication materials folder:

```{r}
dir.create("figs")
dir.create("results")
```

### Dependencies, Session Info

Next, install and load R packages `ltm`, `ggplot2`, `Rsolnp`.

```{r}
library(lme4)
library(ltm)
library(Rsolnp)
library(ggplot2)
library(grid)
library(reshape2)
library(rmeta)
```

```{r}
sessionInfo()
```

```
# R version 3.2.3 (2015-12-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 10586)

# locale:
# [1] LC_COLLATE=English_United States.1252 
# [2] LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    

# attached base packages:
# [1] grid      stats     graphics  grDevices utils     datasets  methods  
# [8] base     

# other attached packages:
#  [1] rmeta_2.16     reshape2_1.4.1 ggplot2_2.0.0  Rsolnp_1.16    ltm_1.0-0     
#  [6] polycor_0.7-8  sfsmisc_1.0-29 mvtnorm_1.0-5  msm_1.6        MASS_7.3-45   
# [11] lme4_1.1-11    Matrix_1.2-3  

# loaded via a namespace (and not attached):
#  [1] Rcpp_0.12.3      magrittr_1.5     splines_3.2.3    munsell_0.4.3   
#  [5] colorspace_1.2-6 lattice_0.20-33  minqa_1.2.4      stringr_1.0.0   
#  [9] plyr_1.8.3       tools_3.2.3      parallel_3.2.3   nlme_3.1-124    
#  [13] gtable_0.1.2     survival_2.38-3  nloptr_1.0.4     stringi_1.0-1   
#  [17] scales_0.3.0     expm_0.999-0     truncnorm_1.0-7
```

Next, source the base functions needed to run the scripts:

```{r}
source("scripts/00_base_functions.R")
```


### Reproducing Figures and Tables 

**Figure 1: Distribution of Difference Between LCA and Raw Estimates and True Effect**

Start by simulating data using [01_sim_data.R](01_sim_data.R). The file generates [data/sim_data.csv](data/sim_data.csv) and [data/sim_data_item_params](data/sim_data_item_params). To analyze the simulated data and reproduce Figure 1, run [scripts/02_analyze_sim_data.R](scripts/02_analyze_sim_data.R).

```{r}
source("scripts/01_sim_data.R")
source("scripts/02_analyze_sim_data.R")
```

**Figures 2-4, Table 3 and Table F1**

These tables and figures plot results of the analyses of Deliberative Polls. All poll level scripts are in a [separate subfolder](scripts/05_poll_scripts/). The scripts append respective poll's results to [person level results](results/person_level.csv), [item level results](results/item_level.csv), [poll level results](pollresults.csv), and [goodness of fit results](results/fitresults.csv). Each script has annotations that note which portion of the script produces results for which table. You can run all poll level scripts in one go as follows:

```{r}
# Iterate through the polls
poll_scripts_dir <- "scripts/03_poll_scripts/"
for (i in dir(poll_scripts_dir)) { source(paste0(poll_scripts_dir, i)) }
```

**Figures 2: Distribution of Differences Between Guessing Adjusted Estimates and Raw Estimates**

To plot the item level results, use [scripts/04_fig2.R](scripts/04_fig2.R).

```{r}
# Average
source("scripts/04_fig2.R")
```

**Figure 3: Distribution of Differences in Average Learning Between Males and Females by Different Estimators**

To plot the item level results split by gender, use [scripts/05_fig3.R](scripts/05_fig3.R).

```{r}
# Average
source("scripts/05_fig3.R")
```

**Figure 4**

To plot the poll level results, use [scripts/06_fig4.R](scripts/06_fig4.R).

```{r}
# Average
source("scripts/06_fig4.R")
```

**Table 3**

The results of the goodness of fit analyses are stored in [goodness of fit results](results/fitresults.csv). (See section on Figures 2-4 that describes the scripts that produce these results.) To add the line noting the overall average:

```{r}
# Average
source("scripts/07_precent_fit.R")
```

**Table F1**

Poll level results are stored in [poll level results](results/pollresults.csv). (See section on Figures 2-4 that describes the scripts that produce these results.)  To add the line noting the overall simple and inverse variance weighted averages:

```{r}
# Average
source("scripts/08_weighting.R")
```

**Reproducing in-text numbers (numbers that are without tables)**

1. Comparing T1 Differences Between Men and Women

```{r}
# Mean and s.e.
# Load data
data <- read.csv("results/person_level.csv")
data <- subset(data, !is.na(female))
data$female <- ifelse(data$female==1, "female", "male")

# Get the diffs
data$t1_lca_diff  <- data$t1_lca - data$t1_raw
data$t1_stnd_diff <- data$t1_stnd - data$t1_raw

summary(with(data, lmer(t1_raw ~ female + (1|poll))))
summary(with(data, lmer(t1_stnd ~ female + (1|poll))))
summary(with(data, lmer(t1_lca ~ female + (1|poll))))

t1_lca_mod   <- with(data, lmer(t1_lca_diff ~ female + (1|poll)))
t1_stnd_mod  <- with(data, lmer(t1_stnd_diff ~ female + (1|poll)))
```

2. Comparing Differences in Learning Between Men and Women

```{r}
# Get the diffs
data$lca_diff <- data$lca - data$raw
data$stnd_diff <- data$stnd - data$raw

summary(with(data, lmer(raw ~ female + (1|poll))))
summary(with(data, lmer(stnd ~ female + (1|poll))))
summary(with(data, lmer(lca ~ female + (1|poll))))

lca_mod   <- with(data, lmer(lca_diff ~ female + (1|poll)))
stnd_mod  <- with(data, lmer(stnd_diff ~ female + (1|poll)))
```

