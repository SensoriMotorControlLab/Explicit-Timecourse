#####clustering

### so we have four strategy types (first 32-50 trials after rotation): rapid-onset, delayed-onset, erratic-onset, and exponential.

#rapid onset will have an aiming strategy stable increase (close to peak aim) within 1-10 trials of rotation
#delayed onset will have an aiming strategy stable increase (close to peak aim) after 10 trials
#erratic onset will include negative aiming degrees (on right side of target)
#exponential will just be a gradual increase (maybe increasing 5 deg every trial till peak)

strategy_data <- strategy_data %>%
  filter(trial_type.x == "rotated") %>%
  
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%   # optional: ensure trials are in order
  mutate(trial_after_rot = row_number() - 1) %>%  # starts at 0
  ungroup()


strategy_data_clean <- strategy_data %>%
  filter(!is.na(aimdeviation_deg)) %>%
  group_by(participant_id, rotation) %>%
  filter(abs(aimdeviation_deg) <= 2 * rotation) %>%
  ungroup()


##set parameters for rule based classification

features <- strategy_data_clean %>%
  group_by(participant_id, group, rotation) %>%
  summarise(
    # first aim deviation past ±7°
    early_mean = aimdeviation_deg[which(abs(aimdeviation_deg) > 7)[1]],
    # trial number of that first meaningful change
    time_to_first_change = which(aimdeviation_deg > 8 | aimdeviation_deg < -8)[1],
    early_neg = sum(aimdeviation_deg[trial_after_rot <= 16] <= -7, na.rm = TRUE),
    early_slope = coef(lm(aimdeviation_deg[trial_after_rot <= 20] ~ trial_after_rot[trial_after_rot <= 20]))[2],
    n_increases = sum(diff(aimdeviation_deg[trial_after_rot <= 20]) > 0, na.rm = TRUE),
    early_var = var(aimdeviation_deg[trial_after_rot <= 3]),
    .groups = "drop"
  ) %>%
  mutate(
    strategy_type = case_when(
      early_neg > 1 ~ "erratic",
      time_to_first_change <= 10 ~ "rapid-onset",
      time_to_first_change > 10  ~ "delayed-onset", 
      n_increases > 3 ~ "exponential", 
    )
  )

table(features$strategy_type)


#cluster
features_for_clust <- features %>%
  select(participant_id, n_increases, early_mean, time_to_first_change, early_neg, early_slope, early_var)

features_for_clust_clean <- features_for_clust %>% filter(complete.cases(.))
features_scaled <- scale(features_for_clust_clean %>% select(-participant_id))

rownames(features_scaled) <- features_for_clust_clean$participant_id

library(factoextra)
fviz_nbclust(features_scaled,kmeans,method = "wss") + 
  labs(subtitle="Elbow mehtod")

km.out <- kmeans(features_scaled,centers = 3, nstart = 50)
clusters <- km.out$cluster

fviz_cluster(
  list(data = features_scaled, cluster = clusters),
  palette = c("#FF69B4", "#9ac0cd", "salmon"),  # colors for each cluster
  geom = "text",  
  ggtheme = theme_minimal(),
)

table(clusters)

cluster_labels <- recode(as.character(clusters),
                         `1` = "Delayed-onset",
                         `2` = "Erratic-onset",
                         `3` = "Rapid-onset")

#visualize
cluster_df <- data.frame(
  participant_id = features_for_clust_clean$participant_id,
  cluster = factor(cluster_labels,
                   levels = c("Delayed-onset", "Erratic-onset", "Rapid-onset"))
)


features_clustered <- features %>%
  inner_join(cluster_df, by = "participant_id")

strategy_data_clustered <- strategy_data_clean %>%
  inner_join(cluster_df, by = "participant_id")

sum(features_clustered$cluster=='Rapid-onset') #38
sum(features_clustered$cluster=='Delayed-onset') #14
sum(features_clustered$cluster=='Erratic-onset') #5


cluster_labels <- c(
  "Rapid-onset"   = "Rapid-onset (n=38)",
  "Delayed-onset" = "Delayed-onset (n=14)",
  "Erratic-onset" = "Erratic-onset (n=5)"
)

ggplot(strategy_data_clustered , 
       aes(x = trial_after_rot, y = aimdeviation_deg, group = participant_id, color = cluster)) +
  geom_line(alpha = 0.4, size = 0.55) +                     # individual participants
  geom_smooth(aes(group = cluster), se = TRUE, size = 1.2, method = "loess", span = 0.3) +
  facet_wrap(~ cluster, ncol = 2, labeller = as_labeller(cluster_labels)) +  
  scale_color_manual(values = c(
    "Erratic-onset" = "#FF69B4",
    "Rapid-onset" = "#9ac0cd",
    "Delayed-onset" = "salmon"
  )) +
  theme_minimal() +
  labs(
    title = "Aiming Deviation Patterns by Cluster",
    x = "Trial (after rotation onset)",
    y = "Aiming Deviation (°)"
  ) +
  theme(panel.grid = element_blank()) +
  scale_x_continuous(limits = c(0, 50))


#does cluster vary with rotation size?

clusterROT <- table(features_clustered$cluster, features_clustered$rotation)
chisq.test(clusterROT)

fisher.test(clusterROT)

##plot 1
features_clustered$rotation <- factor(
  features_clustered$rotation,
  levels = c("20", "30", "40", "50", "60")
)

ggplot(features_clustered, aes(x = as.factor(cluster), fill = as.factor(rotation))) +
  geom_bar(position = position_dodge(preserve = "single"), width = 0.85,
           color = "grey40") +
  scale_fill_manual(
    values = c(
      "20" = "salmon",
      "30" = "#EEE98F",
      "40" = "#D1EEEE",
      "50" = "#EEE0E5",
      "60" = "#FF69B4"
    )
  ) +
  scale_y_continuous(limits = c(0, 20)) + 
  labs(
    x = "Cluster",
    y = "Count",
    fill = "Rotation Size"
  ) +
  theme_minimal(base_size = 14) + 
  annotate("text", x = 1, y = 12, label = "
Fisher p = 0.087", size = 5, color = "black")




#two-staged approach:
#(1) collect data and see strategies in many ways - observe that originally
#(2) do supervised and unsupervised clustering 
#    - include variance and sd across last 8 trials (mean of erratic would be 0 but Sd would be bugger than 0)
#    - pre and post variance estimator  

#- get patterns of ways (single step, rapid, double step, non erattic-one step, non-erattic but two-step, erratic)
# - we need measure of erratic - AFTER THYE LEARN, ERRATIC SHOULD DISSAPEAR - DIFFERENCE IN VARIANCE BEFORE AND AFTER THE 
#               - with step function, add constarint, where the variance is larger before step --- measure of fit for variance to be higher after step (erratic step vs normal step)
#               - 


#sueprvised
#- function or models that fit each way of learning 
# #- get patterns of ways (single step, rapid, double step, non erattic-one step, non-erattic but two-step, erratic)
# - we need measure of erratic - AFTER THYE LEARN, ERRATIC SHOULD DISSAPEAR - DIFFERENCE IN VARIANCE BEFORE AND AFTER THE 
#               - with step function, add constarint, where the variance is larger before step --- measure of fit for variance to be higher after step (erratic step vs normal step)
# 


#unsueprvised
#     - focus on two appraoches: (1) use knowledge by saying that we can use 8 trials and take average strtagey, sd , and trial-to trial change within 8 blocks



