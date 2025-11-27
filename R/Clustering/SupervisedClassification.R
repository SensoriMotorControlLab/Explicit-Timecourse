# Based on clusters from unsupervised trial feats

#-------------------------------#
# Participants in Cluster 1: mostly rapid with veryyyy slight exploratory (1-3 negatives)
# 05484c -  delayed 126,  19.25 (2 negs)
# 0b1dca - rapid 118, 16.50, def 2 step. (2 negs at first)
# 0f6fbf -  rapid 9 at 120 but two negatives
# 13cb04 -  two negatives at first but stable positive. rapid 12 at 116
# 14893b -  rapid - 27 deg at trial 118 
# 1896cb -  rapid with some erratic - 11.75 at trial 115, gets noisy stable 
# 19e7ed - rapaid, one neg 114, 99.75
# 1e2a6b -  semi erratic 119, 8.75 but moreso rapid
# 20d744 - rapid 118, 17.00
# 2525df - 1 negative (-8 at 117), but rapid 18 at trial 118
# 2528e1 - 1 negative (-10 at 123), but rapid 10.75 at trial 124
# 2c2f44 - rapid 11.00 at trial 114
# 33e532 - rapid: 1 negative (-23.00 at trial 91) but 14.25 at trial 92
# 35f115 - delayed trial 125, 10.50 **
# 360ea6 - rapid 16.25 at trial 117 (g2) 
# 396716 - erratic negative 144 at trial 115 **
# 3e3a73 - erratic negatvie 144 at trial 114 **
# 4093e8 - g1. one neg, trial 98, -35.75, but. stabilizes
# 4a6642 - rapid (looks exp) g1 - 11.50 at trial 97
# 52ef4e - erratic, 115, -14.00 ***
# 54044d - two negative values but gets stable 22.25 at trial 108 (g1) so delayed
# 58e451 - rapid but 3 negative; -12.75 at trial 11. stabilizes at trial 121 (7.75)
# 622518 - one negative, 37.75 at trial 127 so delayed
# 70e8cb.- rapid, trial 114, 19.00
# 7cd1bd - rapid - 24.50 at trial 115
# 7eec53 - rapid -33.00 at trial 90 but its not erratic, just went right first ** (kind of noisy)
# 811eae - rapid 114, 18.50 deg
# 81d984 - looks exp a a bit but rapid - 8.00 at trial 94
# 8d426d - rapid but one negative 17.00 at trial 115
# 901482 - delayed - 19.50 at trial 101 ~ noisy aims **
# 96634a - delayed, 8.50 at trial 125 **
# 9db7b0 - rapid - 13.75 at trial 114 
# 9eabc1.- rapid - 147.25 at trial 122
# 9fb9fe - rapid 8.25 at trial 114 but two negative aims
# a18f63 - rapid trial 115, 7.50 deg
# a23b35 - rapid (g1) one negative -24.50 at trial 92 but 18 at trial 98 and stable
# abf95a.- rapid trial 115, 20.50. deg
# ba0a7c - rapid but. one neg. first trial. 116, -14.25
# ba8f14 - rapid, 30 at 119. but one negative aim
# bb04a2 - two negs. then positive trial 115 -9.50 deg but rapid
# bd8518 - rapid, one negative at trial 118. but goes up and down
# c0144b - rapid 21.75 114 
# d0d19c - rapid (g1) 10.00 at trial 91
# d1436b - rapid 10 at trial 116 
# d6141d - rapid 58.50 at trial 115
# d9ff04 - rapid 57.25 at trial 92, one negative value after but immediately back to positive 
# dd50ef - starts at -7 at trial 115 and then one negative value after at trial 119 but stabilizes positive
# e066de - rapid 22.75 at trial 121
# fd5cd5 -neg at first. trial 115 -15.50, but changes (16 deg at trial1 16) so rapid
# ffa337 - rapid, 22.25 at trial 116, BUT ONE NEGATIVE at trial 120



#-------------------------------#


#-------------------------------#
# Participants in cluster 2: delayed
# 13d986 - delayed (g1) 15 at trial 119
# 15f2a1 - delayed (g1) stabilizes at 51.50 at trial 146
# 194dab - delayed (g2) -47.5 at trial 148
# 31b753 -  delayed (g1) 20.75 at trial 109
# 422c52 - delayed (g2) at trial 154, -8 but goes pos
# 54c6f3 - very delayed
# 7eacfc - delayed, 16.75 at trial 134
# 94709f - delayed (g1) 12.25 at trial 133
# 98e5cb - very erratic at end
# a02c67 - delayed (g1) but erratic stats at trial 126
# af8328 - delayed (g2) 24.25 at 151
# b4d36d - delayed (g2) 16.25 at trial 170
# bccf6e -  delayed 10.75 at trial 131
# bde44b - (g1) 7.50 at trial 106 so delayed
# ee29d6 - delayed: 9.25 at trial 132
#-------------------------------#



#-------------------------------#
# Participants in Cluster 3: the more higher erratic is here, but we see some rapid one step too
# f275ca - rapid and erratic - 101 at trial 115 
# bdb042 - erratic
# a5310d - g1 erratic
# 654648 - erratic early
# 3091de - very erratic
# 0c7728 -  slight erratic
# 205194 -  early semi erratic, 3 negs, stabilizes at 122,  44.25
# 59a9dd - pretty early erratic but stabilizes **
# 83456b - delayed 124, 19.7 and erratic **
# ab3b79 - erratic early 118, 11.25. deg **


library(randomForest)
library(caret)
library(dplyr)
strat_data <- read.csv("data/strategy_only_participants.csv")

onset_labels <- data.frame(
  participant_id = c("0c7728", "0f6fbf", "13cb04", "2525df", "2528e1", 
                     "2c2f44", "33e532", "360ea6", "396716", "3e3a73", "4a6642",
                     "54044d", "58e451", "622518", "8d426d", "9fb9fe", "a23b35",
                     "ba8f14", "bd8518", "bdb042", "c0144b","d0d19c", "d1436b","d6141d",
                    "d9ff04", "dd50ef","e066de", "ffa337", "7eec53", "9db7b0", "7cd1bd",
                    "14893b", "1896cb","59a9dd", "4093e8", "81d984", "f275ca", "96634a", 
                    "9eabc1", "901482", "13d986", "15f2a1", "194dab", "422c52", "94709f",
                    "a02c67", "af8328", "b4d36d", "ee29d6", "bccf6e", "31b753", "7eacfc",
                    "bde44b", "98e5cb", "3091de", "654648", "a5310d",
                    "54c6f3","fd5cd5","bb04a2","ba0a7c","abf95a","ab3b79","a18f63","83456b","811eae",
                    "70e8cb","52ef4e","35f115","20d744","205194",
                    "1e2a6b", "19e7ed","0b1dca", "05484c"),
  
    label = c("erratic", "rapid", "rapid", "rapid", "delayed", 
            "rapid", "rapid", "rapid", "erratic", "erratic", "rapid", "delayed",
            "erratic", "delayed", "rapid", "rapid", "rapid", "rapid", "rapid", "erratic",
            "rapid", "rapid", "rapid", "rapid", "rapid", "rapid", "rapid","rapid", "rapid", 
            "rapid", "rapid","rapid",  "erratic", "erratic", "rapid", "rapid", "erratic", "delayed",
            "delayed","delayed", "delayed", "delayed", "delayed",  "delayed", "delayed",
            "delayed", "delayed", "delayed", "delayed", "delayed", "delayed", "delayed",
            "delayed", "erratic", "erratic", "erratic", "erratic",
            "delayed",  "rapid",  "rapid", "rapid", "rapid", "erratic", "rapid",
            "erratic", "rapid","rapid", "rapid", "delayed", "rapid", "erratic",
            "erratic",  "rapid", "rapid", "delayed")
)

getFeatures <- function (strat_data) {
  strategy_data <- strat_data
  strategy_data <- strategy_data %>%
    filter(trial_type.x == "rotated") %>%
    
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%   
    mutate(trial_after_rot = row_number() - 1) %>%  
    ungroup()
  
  trial_features <- strategy_data %>%
    filter(trial_after_rot <= 32) %>% 
    group_by(participant_id) %>%
    arrange(cutrial_no, .by_group = TRUE) %>%
    mutate(
      trial_of_change = {
        t <- which(aimdeviation_deg > 7 | aimdeviation_deg < -7)
        if (length(t) > 0) t[1] else NA
      }
    ) %>%
    ungroup() %>%
    select(participant_id, trial_after_rot, aimdeviation_deg, trial_of_change)
  
  
  trial_summary <- trial_features %>%
    group_by(participant_id) %>%
    summarise(
      trial_of_change = ifelse(all(is.na(trial_of_change)), 32, mean(trial_of_change, na.rm = TRUE)),
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

# labels with features
participantFeatures <- function () {
participant_features <- trial_features %>%
  group_by(participant_id) %>%
  summarise(
    across(where(is.numeric), 
           list(mean = mean, sd = sd), 
           .names = "{.col}_{.fn}")) %>%
  inner_join(onset_labels, by = "participant_id")

participant_features_clean <- participant_features %>%
  select(-participant_id)
participant_features_clean$label <- as.factor(participant_features_clean$label)
return(participant_features )
}


##------- Leave out method -------##

##we can generate trial features within each participant

RFmodel <- function(trial_features, onset_labels) {
  
  library(randomForest)

  participant_features <- trial_features %>%
    group_by(participant_id) %>%
    summarise(
      across(where(is.numeric), 
             list(mean = mean, sd = sd), 
             .names = "{.col}_{.fn}")) %>%
    inner_join(onset_labels, by = "participant_id")
  
  participant_features_clean <- participant_features %>%
    select(-participant_id)
  
  participant_features_clean$label <- as.factor(participant_features_clean$label)
  participant_features_clean[is.na(participant_features_clean)] <- 0
  participant_features_clean$participant_id <- participant_features$participant_id
  
  # LOO method
  participants <- unique(participant_features_clean$participant_id)
  predictions <- data.frame(participant_id = character(),
                            true_label = character(),
                            predicted_label = character(),
                            stringsAsFactors = FALSE)
  
  set.seed(42)
  for (p in participants) {
    train_data <- participant_features_clean %>% filter(participant_id != p)
    test_data  <- participant_features_clean %>% filter(participant_id == p)
    
    rf_model <- randomForest(label ~ . -participant_id, data = train_data, ntree = 500)
    pred <- predict(rf_model, test_data)
    
    predictions <- rbind(predictions,
                         data.frame(participant_id = p,
                                    true_label = as.character(test_data$label),
                                    predicted_label = as.character(pred)))
  }
  
  SuperTable <- table(predictions$true_label, predictions$predicted_label)
  
  return(list(predictions = predictions, SuperTable = SuperTable))
}


##plot 
SuperPlot <- function () {
predictions <- predictions %>%
  select(participant_id, predicted_label)
trial_features_clustered <- trial_features %>%
  left_join(predictions, by = "participant_id") %>%
  rename(cluster = predicted_label)


cluster_means <- trial_features_clustered %>%
  group_by(cluster, trial_after_rot) %>%
  summarise(mean_aim = mean(aimdeviation_deg, na.rm = TRUE))

trial_labels <- c(
  "erratic" = "Erratic strategy onset",
  "rapid" = "Rapid strategy onset",
  "delayed" = "Delayed strategy onset"
)

trial_colors <- c(
  "erratic" = "#E64B35",
  "rapid" = "#4DBBD5",  
  "delayed" = "#00A087"   
)

ggplot(trial_features_clustered, 
       aes(x = trial_after_rot, y = aimdeviation_deg, 
           group = participant_id, color = factor(cluster))) +
  geom_line(alpha = 0.4, linewidth = 0.7) +
  facet_wrap(~cluster, ncol = 1, labeller = as_labeller(trial_labels)) +
  scale_color_manual(values = trial_colors, guide = "none") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),               # remove grid lines
    strip.text = element_text(size = 14, face = "bold"),  # facet titles
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "",
    x = "Trial after rotation",
    y = "Aiming deviation (°)"
  ) +
  coord_cartesian(ylim = c(-80, 100))
}



####compare LOO to unsupervised trial-trial features!
SupersChi <- function () {
rf_labels <- predictions %>%
  select(participant_id, predicted_label) %>%
  distinct(participant_id, .keep_all = TRUE)

unsupervised_labels <- strategy_data_clusters %>%
  select(participant_id, unsupervised_cluster = cluster) %>%
  distinct(participant_id, .keep_all = TRUE)


compare_df <- rf_labels %>%
  inner_join(unsupervised_labels, by = "participant_id")

chisq.test(compare_df$predicted_label, compare_df$unsupervised_cluster)
}



#does rotation predict strategy type?

ProportionSuperPlot <- function () {
strategy_summary <- strategy_data %>%
  group_by(participant_id) %>%
  summarise(rotation = unique(rotation))

table_data <- strategy_summary %>%
  inner_join(onset_labels %>% select(participant_id, label),
             by = "participant_id") %>%
  distinct(participant_id, .keep_all = TRUE)

prop <- table_data %>%
  count(label, rotation) %>%  
  rename(n = n)



filtered_data <- table_data %>%
  filter(rotation != 20)

plot_data <- filtered_data %>%
  group_by(label, rotation) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(label) %>%
  mutate(prop = n / sum(n)) 


ggplot(plot_data, aes(x = label, y = prop, fill = as.factor(rotation))) +
  geom_col(position = "dodge") +
  labs(x = "Strategy Type", y = "Proportion", fill = "Rotation Size") + 
  scale_color_manual(values = c(
    "20"="#B9D3EE","30"="#85adf3","40"="#87CEEB","50"="#4682B4","60"="cadetblue"
  )) +
  scale_fill_manual(values = c(
    "20"="#B9D3EE","30"="#85adf3","40"="#87CEEB","50"="#4682B4","60"="cadetblue"
  )) +
  theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),     
  )
}  


SuperRotationChi <- function () {
cont_table <- table(filtered_data$label, filtered_data$rotation)
chi_sq_result <- chisq.test(cont_table)
chi_sq_result
}