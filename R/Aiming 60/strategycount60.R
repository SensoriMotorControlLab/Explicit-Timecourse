aim60_data_fixed <- lapply(aim60_data, function(df) {
  # Remove cursortype if it exists
  if ("cursortype" %in% colnames(df)) {
    df$cursortype <- NULL
  }
  # Add zeroclamped_bool if it's missing
  if (!"zeroclamped_bool" %in% colnames(df)) {
    df$zeroclamped_bool <- NA
  }
  return(df)
})

df60_aim <- NULL

# Loop through each participant's data
for (file_index in 1:length(aim60_data_fixed)) {
  df <- aim60_data_fixed[[file_index]]
  df$participant_id <- file_index  # assign participant ID
  
  
  # Add to combined dataframe
  if (is.null(df60_aim)) {
    df60_aim <- df
  } else {
    df60_aim <- rbind(df60_aim, df)
  }
}

##extract last 8 aligned, first 8 rotated, last 8 rotated

last_60aligned <- rbind(df60_aim[df60_aim$participant_id %in% c(1,2,4,5,8,9,10,11,12) &
                                df60_aim$cutrial_no %in% 81:88, ], 
                     df60_aim[df60_aim$participant_id %in% c(3,6,7,13) &
                                df60_aim$cutrial_no %in% 105:112, ])

first_60rotated <- rbind(df60_aim[df60_aim$participant_id %in% c(1,2,4,5,8,9,10,11,12) &
                                 df60_aim$cutrial_no %in% 89:96, ], 
                     df60_aim[df60_aim$participant_id %in% c(3,6,7,13) &
                                df60_aim$cutrial_no %in% 113:120, ])
                     

last60_rotated <- rbind(df60_aim[df60_aim$participant_id %in% c(1,2,4,5,8,9,10,11,12) &
                                df60_aim$cutrial_no %in% 201:208, ], 
                    df60_aim[df60_aim$participant_id %in% c(3,6,7,13) &
                               df60_aim$cutrial_no %in% 225:232, ])

#mean of last 8 aligned
aligned_avg <- last_60aligned %>%
  group_by(participant_id) %>%
  summarise(mean_aligned = mean(aimdeviation_deg, na.rm = TRUE),
            sd_aligned = sd(aimdeviation_deg, na.rm = TRUE))

#mean of first 8 rotated
first_rot_avg <- first_60rotated %>%
  group_by(participant_id) %>%
  summarise(mean_first_rot = mean(aimdeviation_deg, na.rm = TRUE))

#mean of last 8 rotated
last_rot_avg <- last60_rotated %>%
  group_by(participant_id) %>%
  summarise(mean_last_rotated = mean(aimdeviation_deg, na.rm = TRUE),
            sd_last_rotated = sd(aimdeviation_deg, na.rm = TRUE))


CI <- function(last_60aligned) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last_60aligned, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

aligned_CI <- CI(last_60aligned)



CI <- function(last60_rotated) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last60_rotated, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

rotated_CI <- CI(last60_rotated)


#if a participant was consistently applying a strategy in the rotated phase, we
#expect their aim deviation in that phase to be well outside the CI range of the 
#aligned phase.

ci_compare <- merge(aligned_CI, rotated_CI, by = "participant_id", suffixes = c("_aligned", "_rotated"))
ci_compare$shift_amount <- ci_compare$aimdeviation_deg_rotated[,1] - ci_compare$aimdeviation_deg_aligned[,2]

# flag whether there's a strategy shift (if difference > 15 degrees)
ci_compare$aim_shift <- ifelse(ci_compare$shift_amount > 15, "Yes", "No")
ci_compare$aim_shift[ci_compare$participant_id == 10] <- "No"
print(ci_compare[, c("participant_id", "aimdeviation_deg_aligned", "aimdeviation_deg_rotated", "shift_amount", "aim_shift")])

#use plots to see if consistent with table 
df2 <- df60_aim[df60_aim$participant_id == 9, ] #1,4,5,6,7,8,10,11,12,13
plot(df2$aimdeviation_deg, type="l")
#10 was a "noisy strategy", so lets exclude it


strategies_aligned <- rbind(df60_aim[df60_aim$participant_id %in% c(1,4,5,8,11,12) &
                                       df60_aim$cutrial_no %in% 81:88, ], 
                            df60_aim[df60_aim$participant_id %in% c(6,7,13) &
                                       df60_aim$cutrial_no %in% 105:112, ])

nonstrategies_aligned <- rbind(df60_aim[df60_aim$participant_id %in% c(2,9) &
                                          df60_aim$cutrial_no %in% 81:88, ], 
                               df60_aim[df60_aim$participant_id %in% 3 &
                                          df60_aim$cutrial_no %in% 105:112, ])

strategies_rotated <- rbind(df60_aim[df60_aim$participant_id %in% c(1,4,5,8,11,12) &
                                       df60_aim$cutrial_no %in% 201:208, ], 
                            df60_aim[df60_aim$participant_id %in% c(6,7,13) &
                                       df60_aim$cutrial_no %in% 225:232, ])

nonstrategies_rotated <- rbind(df60_aim[df60_aim$participant_id %in% c(2,9) &
                                       df60_aim$cutrial_no %in% 201:208, ], 
                            df60_aim[df60_aim$participant_id %in% 3 &
                                       df60_aim$cutrial_no %in% 225:232, ])

t.test(strategies_rotated$aimdeviation_deg,strategies_aligned$aimdeviation_deg, paired=TRUE)
t.test(strategies_rotated$aimdeviation_deg,nonstrategies_rotated$aimdeviation_deg)






}

plot_last60_rotated <- merge(last60_rotated, ci_compare[, c("participant_id", "aim_shift")], by="participant_id")


ggplot(plot_last60_rotated, aes(x=factor(participant_id), y=aimdeviation_deg, color=aim_shift)) +
  geom_jitter(width=0.1, size=3, alpha=0.7) +
  scale_color_manual(values=c("Yes"="deeppink", "No"="black")) +
  labs(
    title = "Last 8 Reaches (Rotated Phase) by Strategy Group",
    x = "Participant ID",
    y = "Aiming Deviation (Degrees)",
    color = "Strategy Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  ylim(0, 60)