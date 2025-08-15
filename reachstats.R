total_learners_data <- total_group_data %>%
  semi_join(learner_id, by = c("rotation", "participant_id")) %>%
  select(participant_id, cutrial_no, reachdeviation_deg, rotation, trial_type)

reach20_summary <- total_learners_data %>%
  filter(rotation == 20) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))


rotated20_mean <- total_learners_data %>%
  filter(rotation == 20, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(rotated20_mean$mean_reach_dev, mu = 10, alternative = "greater")

t_high <- t.test(rotated20_mean$mean_reach_dev, mu = 20)

t_high

baseline20_mean <- total_learners_data %>%
  filter(rotation == 20) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline20_mean$mean_reach_dev, mu = 0)


washout20_g1 <- total_learners_data %>%
  filter(rotation == 20,
         trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout20_g2 <- total_learners_data %>%
  filter(group == "Group 2",
         rotation == 20,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout20_combined <- bind_rows(washout20_g1, washout20_g2)

t_result <- t.test(washout20_combined$mean_reach_dev, mu = 0)

t_result

plot(reach20_summary,type='l')


reach30_summary <- total_learners_data %>%
  filter(rotation == 30) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))

rotated30_mean <- total_learners_data %>%
  filter(rotation == 30, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(rotated30_mean$mean_reach_dev, mu = 15, alternative = "greater")

t_high <- t.test(rotated30_mean$mean_reach_dev, mu = 30)

t_high

baseline30_mean <- total_learners_data %>%
  filter(rotation == 30) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>% 
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline30_mean$mean_reach_dev, mu = 0)

washout30_g1 <- total_learners_data %>%
  filter(rotation == 30,
         trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout30_g2 <- total_learners_data %>%
  filter(group == "Group 2",
         rotation == 30,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout30_combined <- bind_rows(washout30_g1, washout30_g2)

t_result <- t.test(washout60_combined$mean_reach_dev, mu = 0)

t_result

plot(reach30_summary,type='l')


reach40_summary <- total_learners_data %>%
  filter(rotation == 40) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))

rotated40_mean <- total_learners_data %>%
  filter(rotation == 40, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(rotated40_mean$mean_reach_dev, mu = 20, alternative = "greater")

t_high <- t.test(rotated40_mean$mean_reach_dev, mu = 40)


t_high

baseline40_mean <- total_learners_data %>%
  filter(rotation == 40) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline40_mean$mean_reach_dev, mu = 0)

washout40_g1 <- total_learners_data %>%
  filter(rotation == 40,
         trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout40_g2 <- total_learners_data %>%
  filter(group == "Group 2",
         rotation == 40,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout40_combined <- bind_rows(washout40_g1, washout40_g2)

t_result <- t.test(washout40_combined$mean_reach_dev, mu = 0)

t_result

plot(reach40_summary,type='l')


reach50_summary <- total_learners_data %>%
  filter(rotation == 50) %>%
  group_by(cutrial_no) %>%
  summarise(mean_reach_deviation = mean(reachdeviation_deg, na.rm = TRUE))

rotated50_mean <- total_learners_data %>%
  filter(rotation == 50, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(rotated50_mean$mean_reach_dev, mu = 25, alternative = "greater")

t_high <- t.test(rotated50_mean$mean_reach_dev, mu = 50)

t_high


baseline50_mean <- total_learners_data %>%
  filter(rotation == 50) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline50_mean$mean_reach_dev, mu = 0)


washout50_g1 <- total_learners_data %>%
  filter(rotation == 50,
         trial_type == "zero-clamp rotated",
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout50_g2 <- total_learners_data %>%
  filter(group == "Group 2",
         rotation == 50,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout50_combined <- bind_rows(washout50_g1, washout50_g2)

t_result <- t.test(washout50_combined$mean_reach_dev, mu = 0)

t_result


#baseline & washout had biases so. compare
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

plot(reach50_summary,type='l')


reach60_summary <- total_learners_data %>%
  filter(rotation == 60) %>%
  group_by(cutrial_no, participant_id) %>%
  summarize(mean_reach_dev =mean)


rotated60_mean <- total_learners_data %>%
  filter(rotation == 60, trial_type == "rotated") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(rotated60_mean$mean_reach_dev, mu = 60)


t_low
t_high

baseline60_mean <- total_learners_data %>%
  filter(rotation == 60) %>%
  filter(trial_type == "aligned") %>%
  group_by(participant_id) %>%
  arrange(cutrial_no, .by_group = TRUE) %>%
  slice_tail(n = 16) %>%  # last 8 trials for each participant
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

t_low  <- t.test(baseline60_mean$mean_reach_dev, mu = 0)


washout60_g1 <- total_learners_data %>%
  filter(group == "Group 1",
    rotation == 60,
         cutrial_no >= 209 & cutrial_no <= 216) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout60_g2 <- total_learners_data %>%
  filter(group == "Group 2",
         rotation == 60,
         trial_type == "no cursor",
         cutrial_no >= 233 & cutrial_no <= 240) %>%
  group_by(participant_id) %>%
  summarise(mean_reach_dev = mean(reachdeviation_deg, na.rm = TRUE))

washout60_combined <- bind_rows(washout60_g1, washout60_g2)

t_result <- t.test(washout60_combined$mean_reach_dev, mu = 0)

t_result


plot(reach60_summary)

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


