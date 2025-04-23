aim30_data_fixed <- lapply(aim30_data, function(df) {
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


df30_aim <- NULL  

for (file_index in 1:length(aim30_data_fixed)) {
  df <- aim30_data_fixed[[file_index]]
  df$participant_id <- file_index  # assign participant ID
  
  
  if (is.null(df30_aim)) {
    df30_aim <- df
  } else {
    df30_aim <- rbind(df30_aim, df)
  }
  
}


last_30aligned <- rbind(df30_aim[df30_aim$participant_id %in% c(1,2,3,4) &
                                   df30_aim$cutrial_no %in% 105:112, ])

first_30rotated <- rbind(df30_aim[df30_aim$participant_id %in% c(1,2,3,4) &
                                    df30_aim$cutrial_no %in% 113:130, ])


last30_rotated <- rbind(df30_aim[df30_aim$participant_id %in% c(1,2,3,4) &
                                   df30_aim$cutrial_no %in% 225:232, ])


CI <- function(last_30aligned) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last_30aligned, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

aligned_CI30 <- CI(last_30aligned)



CI <- function(last30_rotated) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last30_rotated, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

rotated_CI30 <- CI(last30_rotated)


#if a participant was consistently applying a strategy in the rotated phase, we
#expect their aim deviation in that phase to be well outside the CI range of the 
#aligned phase.

ci_compare30 <- merge(aligned_CI30, rotated_CI30, by = "participant_id", suffixes = c("_aligned", "_rotated"))
ci_compare30$shift_amount <- ci_compare30$aimdeviation_deg_rotated[,1] - ci_compare30$aimdeviation_deg_aligned[,2]

# flag whether there's a strategy shift (if difference > 15 degrees)
ci_compare30$aim_shift <- ifelse(
  (ci_compare30$aimdeviation_deg_rotated[,1] - ci_compare30$aimdeviation_deg_aligned[,2] > 10) |
    (ci_compare30$aimdeviation_deg_rotated[,2] - ci_compare30$aimdeviation_deg_aligned[,1] > 10),
  "Yes", "No")
#ci_compare30$aim_shift[ci_compare30$participant_id == 5] <- "No" #participant 5 has a noisy strategy

print(ci_compare30[, c("participant_id", "aimdeviation_deg_aligned", "aimdeviation_deg_rotated", "shift_amount", "aim_shift")])

#see individual plots
df5 <- df30_aim[df30_aim$participant_id == 3, ] 
plot(df5$aimdeviation_deg, type="l")    


strategies_aligned30 <- rbind(df30_aim[df30_aim$participant_id %in% c(1,2) &
                                         df30_aim$cutrial_no %in% 105:112, ])

nonstrategies_aligned30 <- rbind(df30_aim[df30_aim$participant_id %in% c(3,4) &
                                            df30_aim$cutrial_no %in% 105:112, ])

strategies_rotated30 <- rbind(df30_aim[df30_aim$participant_id %in% c(1,2) &
                                         df30_aim$cutrial_no %in% 225:232, ])

nonstrategies_rotated30 <- rbind(df30_aim[df30_aim$participant_id %in% c(3,4) &
                                            df30_aim$cutrial_no %in% 225:232, ])

t.test(strategies_rotated30$aimdeviation_deg,strategies_aligned$aimdeviation_deg, paired=TRUE)
t.test(strategies_rotated30$aimdeviation_deg,nonstrategies_rotated30$aimdeviation_deg)


plot_last30_rotated <- merge(last30_rotated, ci_compare30[, c("participant_id", "aim_shift")], by="participant_id")

ggplot(plot_last30_rotated, aes(x=factor(participant_id), y=aimdeviation_deg, color=aim_shift)) +
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
  ylim(-5, 30)  