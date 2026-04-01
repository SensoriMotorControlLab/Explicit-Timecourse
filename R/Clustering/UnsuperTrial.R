### Unsupervised learning with trial by trial features
#OVERALL PARTICIPANT BY APRTICIPANT FEATURES!!1

#### 1 WHEN DO THEY ARRIVE AT STRATEGY
# take strategy from last 16 trials - this is the strategy they learn
# first 104 trials and divide by last 16
# everyones at 1 when they reach their strategy 


# count back from final 16 strategies (based on sd) -  average final strategy level
# don't use as data anymore
# determine features based on first 104 trials
# then start taking 8 trials (aim dev) at end and compare to the last 16 (stable) aim deviations via t-test (prob wont be different)
# then shift 8 over then another t-test (rolling window but determine when the value in this rolling window is sig lower than final 16),
# see when the 8 trials are lower than final 16 -- cut off point for when they havent stabilized
    # that trial idx is a feature (when they arrive at that point 
####NVM GO FORWARD, LAST TRIAL THAT DOES NOT INTO FINAL 16 AVERAGE (SAMPLE)
#####WAIT LOOK AT AVERAGE AND THEN LOOK AT SD OF PART BEFORE SETTLED STRATEGY


##### 2 
#take trials before the strategy arrival and find the SD before
#this should reveal erratic people

#### 3
# find first trial where they start deviating
# do t-test thing (rolling window backwards)
# trial-by trial and keep doing t-test and the moment its different from 0,
# they are outside of baseline noise level - see if that happens rapidly or delayed


###4
# take sd from point where they start deviating from 0 and to the point where they start settling on a startegy
# feature 
# high sd here is erratic people

#5 
#initial sd before aim change
#might be low

#6
#take difference betwee twopoints and it should be postive but if its not then they made a single step

#7 - experimental - use diff! find absolute and mean - one # for everyone
# good for detecting gradual change
# diff(erratic) largeee
#diff(graudal) diffs are not as large
#whats the means(abs(differratic))
#could distinguish between erratic and gradual and stepwise
#diff(stepwise(7:8) - 7 is where they dont have a step 8 is their final stabilized trial so its from no step to step
#erratic you have 0, explore, final - so look at in between period from when they start changing to when they stop changing


#if its short, its aha learning, if its not short, take mean abs diff between aims 

#8
# difference between tow points
# low for stepwise
#when number is high, take all trials in between when they start changing and stop changing and look at size of changed they make inbetween






# relabel - stepwise (one-three), erratic, gradual  


library(zoo)

getFeatures <- function () {
strategy_data <- read.csv("data/strategy_only_participants.csv")
strategy_data <- strategy_data =='rotated'
strategy_data <- strategy_data[which(strategy_data$trial_idx<121)]

# block-by-block features
# avg aimdeviation_deg per W trials
# SDaim sd per W trials
# abs difference with W trials back?
    # 8 trials per block


###ROLLING WINDOW IN TRIAL BY TRIAL FEATRUES BUT USE FIRST 60 TRIALS


#W is the window

W <- 8
Nblocks <- max(participant_data$trial_idx)/W 

for (id in unique(strategy_data$participant_id)) {
 
  participant_data <- strategy_data %>%
    filter(participant_id=id)
  
  for(block in c(1:Nblocks))
   participant <- c(participant,)
  
if(is.matrix(features) {
  features <- rbind(features,pmat)
} else {
  participant <- c(participant,id
                   )
 pmat <- matric(c(avg,_aim,aim_sd,ncol=2*length(avh_aim))) 
features <- rbind(features,pmat)
  








### 
  
trial_features <- strategy_data %>%
  filter(trial_idx <=32) %>%
  group_by(participant_id) %>%
  arrange(trial_idx, .by_group = TRUE) %>%
  mutate(
    trial_of_change = {
      t <- which(aimdeviation_deg > 7 | aimdeviation_deg < -7)
      if (length(t) > 0) t[1] else NA
    }
  ) %>%
  ungroup() %>%
  select(participant_id, aimdeviation_deg, trial_of_change)


trial_summary <- trial_features %>%
  group_by(participant_id) %>%
  summarise(
    trial_of_change = ifelse(all(is.na(trial_of_change)), 120, mean(trial_of_change, na.rm = TRUE)),
    sd_aim   = sd(aimdeviation_deg, na.rm = TRUE),
    mean_aim = mean(aimdeviation_deg, na.rm = TRUE),
    prop_negative = mean(aimdeviation_deg < -3, na.rm = TRUE)
  ) %>%
  mutate(
    trial_of_change = trial_of_change * 8
  )
return(list(trial_features = trial_features,
            trial_summary = trial_summary))
}



PCA <- function () {
  trial_summary <- getFeatures()
  trial_scaled <- trial_summary %>%
  select(mean_aim, sd_aim, trial_of_change, prop_negative) %>%
  
  scale(center = TRUE, scale = TRUE)


pca <- prcomp(trial_scaled, center = TRUE, scale. = TRUE)
trial_pca <- as.data.frame(pca$x[, 1:4])
trial_pca$participant_id <- trial_summary$participant_id

km <- kmeans(trial_pca[, 1:4], centers = 3, nstart = 50)
trial_pca$cluster <- factor(km$cluster)


strategy_data_clusters <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  select(participant_id, trial_after_rot, aimdeviation_deg) %>%  
  left_join(trial_pca %>% select(participant_id, cluster), by = "participant_id")

cluster_summary <- trial_pca %>%
  select(participant_id, cluster) %>%
  distinct() %>%   
  count(cluster) 

print(cluster_summary)
}



##plot
UnsuperPlot <- function () {
strategy_labels <- c(
  "1" = "Rapid strategy onset",
  "2" = "Delayed strategy onset",
  "3" = "Erratic strategy onset"
)


cluster_colors <- c(
  "1" = "#E64B35", 
  "2" = "#4DBBD5",  
  "3" = "#00A087"   
)

strategy_data_clusters <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  select(participant_id, trial_after_rot, aimdeviation_deg) %>%  
  left_join(trial_pca %>% select(participant_id, cluster), by = "participant_id")
 
ggplot(strategy_data_clusters, 
       aes(x = trial_after_rot, y = aimdeviation_deg, 
           group = participant_id, color = factor(cluster))) +
  geom_line(alpha = 0.6, linewidth = 0.7) +
  facet_wrap(~cluster, ncol = 1, labeller = as_labeller(strategy_labels)) +
  scale_color_manual(values = cluster_colors, guide = "none") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),          # remove grid lines
    strip.text = element_text(size = 14, face = "bold"),  # facet titles
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Trial-by-trial aiming deviation by strategy cluster",
    x = "Trial after rotation",
    y = "Aiming deviation (°)"
  )
}


