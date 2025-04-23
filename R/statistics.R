#mixed anova on all strategy groups

#add columns
strategies_aligned$rotation_size <- 60
strategies_aligned$phase <- "aligned"
strategies_rotated$rotation_size <- 60
strategies_rotated$phase <- "rotated"

strategies_aligned50$rotation_size <- 50
strategies_aligned50$phase <- "aligned"
strategies_rotated50$rotation_size <- 50
strategies_rotated50$phase <- "rotated"

strategies_aligned40$rotation_size <- 40
strategies_aligned40$phase <- "aligned"
strategies_rotated40$rotation_size <- 40
strategies_rotated40$phase <- "rotated"

strategies_aligned30$rotation_size <- 30
strategies_aligned30$phase <- "aligned"
strategies_rotated30$rotation_size <- 30
strategies_rotated30$phase <- "rotated"

strategies_aligned20$rotation_size <- 20
strategies_aligned20$phase <- "aligned"
strategies_rotated20$rotation_size <- 20
strategies_rotated20$phase <- "rotated"

#sample size in each group (60~9, 50~7, 40~6, 30~2, 20~1)

all_strategies <- rbind(
  strategies_aligned, strategies_rotated,
  strategies_aligned50, strategies_rotated50,
  strategies_aligned40, strategies_rotated40,
  strategies_aligned30, strategies_rotated30,
  strategies_aligned20, strategies_rotated20
)

all_strategies$unique_id <- paste0("P", all_strategies$participant_id, "_", all_strategies$rotation_size)

library(afex)

anova_result <- aov_ez(
  id = "unique_id",
  dv = "aimdeviation_deg", 
  data = all_strategies,
  within = "phase",
  between = "rotation_size",
  type = 3
)
summary(anova_result)

#Results:
Univariate Type III Repeated-Measures ANOVA Assuming Sphericity

Sum Sq num Df Error SS den Df F value    Pr(>F)    
(Intercept)         3400.8      1   862.76     20 78.8351 2.246e-08 ***
  rotation_size        621.5      4   862.76     20  3.6017   0.02284 *  
  phase               3938.4      1   878.93     20 89.6191 7.886e-09 ***
  rotation_size:phase  687.3      4   878.93     20  3.9099   0.01669 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


#Rotation: There are differences between rotation sizes
#Phase: There is a highly significant different between rotation sizes (aligned vs rotated)
#Interaction between rotation and phase: the strategy shift from aligned to rotated differs across rotation sizes.
                                        
                                        
#Post hoc tests needed!
#Tukey test for rotation size: which aim deviations signifcantly differ per rotation group?                                        

emm_rotation_size <- emmeans(anova_result, pairwise ~ rotation_size)
summary(emm_rotation_size)
#Only 60 and 30 rotation groups (P=0.0442)



