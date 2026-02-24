total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)

total_learners_data <- total_group_data %>%
  semi_join(learner_id, by = c("rotation", "participant_id")) %>%
  select(participant_id, cutrial_no, aimdeviation_deg, reachdeviation_deg, rotation, trial_type, group)

flag_outliers <- function(x) {
  m <- mean(x, na.rm = TRUE)
  abs(x) > 3 * abs(m)
}

cleaned_data <- total_learners_data %>%
  group_by(trial_type) %>%
  mutate(is_outlier = flag_outliers(reachdeviation_deg)) %>%
  ungroup() %>% 
  filter(!is_outlier, trial_type != "lefthandrotated")
  

##20##
reach20_summary <- cleaned_data %>%
  filter(rotation == 20) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))


rotated20_mean <- cleaned_data %>%
  filter(rotation == 20, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_high <- t.test(rotated20_mean$mean_reach_dev, mu = 20)

t_high

baseline20_mean <- cleaned_data %>%
  filter(rotation == 20) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline20_mean$mean_reach_dev, mu = 0)


washout20_g2 <- cleaned_data %>%
  filter(group == "Group 2",
         rotation == 20,
         trial_type == "nocursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))


t_result <- t.test(washout20_g2$mean_reach_dev, mu = 0)

t_result

#plot
reachci_20 <- cleaned_data %>% 
  filter(rotation == 20) %>%
  group_by(cutrial_no) %>%
  summarise(
    n = n(),
    mean_dev = mean(reachdeviation_deg, na.rm = TRUE),
    ci_lower = if(n > 1) getConfidenceInterval(reachdeviation_deg)[1] else NA,
    ci_upper = if(n > 1) getConfidenceInterval(reachdeviation_deg)[2] else NA
  ) %>%
  arrange(cutrial_no) %>%
  filter(!is.na(ci_lower) & !is.na(ci_upper))

plot(reachci_20$cutrial_no, reachci_20$mean_dev, type = "l",
     ylim = range(-15:25),
     xlab = "Trial number", ylab = "Reach deviation (deg)")

polygon(
  c(reachci_20$cutrial_no, rev(reachci_20$cutrial_no)),
  c(reachci_20$ci_upper, rev(reachci_20$ci_lower)),
  col = rgb(0, 0, 1, 0.2), border = NA
)


###30###
reach30_summary <- cleaned_data %>%
  filter(rotation== 30) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))

rotated30_mean <- cleaned_data %>%
  filter(rotation == 30, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(rotated30_mean$mean_reach_dev, mu = 15, alternative = "greater")

t_high <- t.test(rotated30_mean$mean_reach_dev, mu = 30)

t_high

baseline30_mean <- cleaned_data %>%
  filter(rotation == 30) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>% 
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline30_mean$mean_reach_dev, mu = 0)

washout30_g1 <- cleaned_data %>%
  filter(rotation== 30,
         trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout30_g2 <- cleaned_data %>%
  filter(group == "Group 2",
         rotation== 30,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout30_combined <- bind_rows(washout30_g1, washout30_g2)

t_result <- t.test(washout60_combined$mean_reach_dev, mu = 0)

#plot
reachci_30 <- cleaned_data %>% 
  filter(rotation== 30) %>%
  group_by(cutrial_no) %>%
  summarise(
    n = n(),
    mean_dev = mean(reachdeviation_deg, na.rm = TRUE),
    ci_lower = if(n > 1) getConfidenceInterval(reachdeviation_deg)[1] else NA,
    ci_upper = if(n > 1) getConfidenceInterval(reachdeviation_deg)[2] else NA
  ) %>%
  arrange(cutrial_no) %>%
  filter(!is.na(ci_lower) & !is.na(ci_upper))

plot(reachci_30$cutrial_no, reachci_30$mean_dev, type = "l",
     ylim = range(-15:35),
     xlab = "Trial number", ylab = "Reach deviation (deg)")

polygon(
  c(reachci_30$cutrial_no, rev(reachci_30$cutrial_no)),
  c(reachci_30$ci_upper, rev(reachci_30$ci_lower)),
  col = rgb(0, 0, 1, 0.2), border = NA
)



###40###
reach40_summary <- cleaned_data %>%
  filter(rotation== 40) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))

#compare rotated trials to 40
rotated40_mean <- cleaned_data %>%
  filter(rotation== 40, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_high <- t.test(rotated40_mean$mean_reach_dev, mu = 40)


#compare aligned trials to 0
baseline40_mean <- cleaned_data %>%
  filter(rotation== 40) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline40_mean$mean_reach_dev, mu = 0)

#compare after-rotationtrials to 0
washout40_g1 <- cleaned_data %>%
  filter(rotation== 40,
         trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout40_g2 <- cleaned_data %>%
  filter(group == "Group 2",
         rotation== 40,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout40_combined <- bind_rows(washout40_g1, washout40_g2)

t_result <- t.test(washout40_combined$mean_reach_dev, mu = 0)

t_result

#plot
reachci_40 <- cleaned_data %>% 
  filter(rotation== 40) %>%  
  group_by(cutrial_no) %>% 
  summarise(
    mean_dev = mean(reachdeviation_deg, na.rm = TRUE),
    ci_lower = getConfidenceInterval(reachdeviation_deg[!is.na(reachdeviation_deg)])[1],
    ci_upper = getConfidenceInterval(reachdeviation_deg[!is.na(reachdeviation_deg)])[2],
    .groups = "drop"
  )

poly_data4 <- reachci_40 %>%
  filter(!is.na(ci_lower), !is.na(ci_upper))

plot(reachci_40$cutrial_no, reachci_40$mean_dev, type = "l",
     ylim = range(-15:40),
     xlab = "Trial number", ylab = "Reach deviation (deg)")

polygon(
  c(poly_data4$cutrial_no, rev(poly_data4$cutrial_no)),
  c(poly_data4$ci_upper, rev(poly_data4$ci_lower)),
  col = rgb(0.54, 0.81, 0.94, alpha = 0.3), border = NA
)


###50###

reach50_summary <- cleaned_data %>%
  filter(rotation== 50) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))

#compare rotated reach to 50
rotated50_mean <- cleaned_data %>%
  filter(rotation== 50, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_high <- t.test(rotated50_mean$mean_reach_dev, mu = 50)


#compare aligned reaches to 0
baseline50_mean <- cleaned_data %>%
  filter(rotation== 50) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline50_mean$mean_reach_dev, mu = 0)

#compare reaches to 0 (after rotation)
washout50_g1 <- cleaned_data %>%
  filter(rotation== 50,
         trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout50_g2 <- cleaned_data %>%
  filter(group == "Group 2",
         rotation== 50,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout50_combined <- bind_rows(washout50_g1, washout50_g2)

t_result <- t.test(washout50_combined$mean_reach_dev, mu = 0)

t_result

#baseline & washout had biases so compare
baseline50_mean <- baseline50_mean %>% rename(baseline = mean_reach_dev)
washout50_combined <- washout50_combined %>% rename(washout = mean_reach_dev)
paired_data <- merge(baseline50_mean, washout50_combined, by = "participant_id")

t_result <- t.test(paired_data$baseline, paired_data$washout, paired = TRUE)

t_result
paired_data %>%
  summarise(
    mean_baseline = mean(baseline, na.rm = TRUE),
    sd_baseline   = sd(baseline, na.rm = TRUE),
    mean_washout = mean(washout, na.rm = TRUE),
    sd_washout   = sd(washout, na.rm = TRUE)
  )

#plot
reachci_50 <- cleaned_data %>% 
  filter(rotation== 50) %>%  
  group_by(cutrial_no) %>% 
  summarise(
    mean_dev = mean(reachdeviation_deg, na.rm = TRUE),
    ci_lower = getConfidenceInterval(reachdeviation_deg[!is.na(reachdeviation_deg)])[1],
    ci_upper = getConfidenceInterval(reachdeviation_deg[!is.na(reachdeviation_deg)])[2],
    .groups = "drop"
  )

poly_data5 <- reachci_50 %>%
  filter(!is.na(ci_lower), !is.na(ci_upper))

plot(reachci_50$cutrial_no, reachci_50$mean_dev, type = "l",
     ylim = range(-15:50),
     xlab = "Trial number", ylab = "Reach deviation (deg)")

polygon(
  c(poly_data5$cutrial_no, rev(poly_data5$cutrial_no)),
  c(poly_data5$ci_upper, rev(poly_data5$ci_lower)),
  col = rgb(1, 0.5, 0, alpha = 0.3), border = NA
)

###60###

#compare rotated reaches to 60
reach60_summary <- cleaned_data %>%
  filter(rotation== 60) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))

rotated60_mean <- cleaned_data %>%
  filter(rotation== 60, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(rotated60_mean$mean_reach_dev, mu = 60)

#compare baseline reaches to 0
baseline60_mean <- cleaned_data %>%
  filter(rotation== 60) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline60_mean$mean_reach_dev, mu = 0)

#compare washout reaches to 0
washout60_g1 <- cleaned_data %>%
  filter(rotation== 60,
    cleaned_data$trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout60_g2 <- cleaned_data %>%
  filter(rotation== 60,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout60_combined <- bind_rows(washout60_g1, washout60_g2)

t_result <- t.test(washout60_combined$mean_reach_dev, mu = 0)

t_result


#baseline & washout had biases so compare
baseline60_mean <- baseline60_mean %>% rename(baseline = mean_reach_dev)
washout60_combined <- washout60_combined %>% rename(washout = mean_reach_dev)
paired_data <- merge(baseline60_mean, washout60_combined, by = "participant_id")

t_result <- t.test(paired_data$baseline, paired_data$washout, paired = TRUE)

t_result

paired_data %>%
  summarise(
    mean_baseline = mean(baseline, na.rm = TRUE),
    sd_baseline   = sd(baseline, na.rm = TRUE),
    mean_washout = mean(washout, na.rm = TRUE),
    sd_washout   = sd(washout, na.rm = TRUE)
  )

#plot
reachci_60 <- cleaned_data %>% 
  filter(rotation== 60) %>%  
  group_by(cutrial_no) %>% 
  summarise(
    mean_dev = mean(reachdeviation_deg, na.rm = TRUE),
    ci_lower = getConfidenceInterval(reachdeviation_deg[!is.na(reachdeviation_deg)])[1],
    ci_upper = getConfidenceInterval(reachdeviation_deg[!is.na(reachdeviation_deg)])[2],
    .groups = "drop"
  )

poly_data <- reachci_60 %>%
  filter(!is.na(ci_lower), !is.na(ci_upper))

plot(reachci_60$cutrial_no, reachci_60$mean_dev, type = "l",
     ylim = range(-15:60),
     xlab = "Trial number", ylab = "Reach deviation (deg)")

polygon(
  c(poly_data$cutrial_no, rev(poly_data$cutrial_no)),
  c(poly_data$ci_upper, rev(poly_data$ci_lower)),
  col = rgb(1, 0.41, 0.71, alpha = 0.3), border = NA
)



#####anova 1#### do asymptotic reaches differ depending on rotation size?
rotated_means <- cleaned_data %>%
  filter(trial_type == "rotated") %>%
  group_by(rotation, participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>% 
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE), .groups = "drop")

anova_res <- aov(mean_reach_dev ~ factor(rotation), data = rotated_means)
summary(anova_res)
TukeyHSD(anova_res)


####anova 2##### Do participants detect and start compensating earlier for small rotations than large ones?”

baseline_means <- cleaned_data %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id, rotation) %>%
  summarise(mean_aligned = mean(reachdeviation_deg, na.rm = TRUE), .groups = "drop")

rotated_trials <- cleaned_data %>%
  filter(trial_type == "rotated") %>%
  left_join(baseline_means, by = c("participant_id", "rotation")) %>%
  group_by(participant_id, rotation) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  summarise(
    first_change_trial = cutrial_no[which(abs(reachdeviation_deg - mean_aligned) > 5)[1]],
    .groups = "drop"
  )

rotated_trials %>%
  group_by(rotation) %>%
  summarise(n = n())

# Run one-way ANOVA 
anova_res <- aov(first_change_trial ~ factor(rotation), data = rotated_trials)
summary(anova_res)
TukeyHSD(anova_res)
