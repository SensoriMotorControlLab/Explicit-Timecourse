aim20_data_fixed <- lapply(aim20_data, function(df) {
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


df20_aim <- NULL  

for (file_index in 1:length(aim20_data_fixed)) {
  df <- aim20_data_fixed[[file_index]]
  df$participant_id <- file_index  # assign participant ID
  
  
  if (is.null(df20_aim)) {
    df20_aim <- df
  } else {
    df20_aim <- rbind(df20_aim, df)
  }
  
}


last_20aligned <- rbind(df20_aim[df20_aim$participant_id %in% c(1,2,3,4) &
                                   df20_aim$cutrial_no %in% 105:112, ])

first_20rotated <- rbind(df20_aim[df20_aim$participant_id %in% c(1,2,3,4) &
                                    df20_aim$cutrial_no %in% 113:120, ])


last20_rotated <- rbind(df20_aim[df20_aim$participant_id %in% c(1,2,3,4) &
                                   df20_aim$cutrial_no %in% 225:232, ])


CI <- function(last_20aligned) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last_20aligned, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

aligned_CI20 <- CI(last_20aligned)



CI <- function(last20_rotated) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last20_rotated, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

rotated_CI20 <- CI(last20_rotated)


#if a participant was consistently applying a strategy in the rotated phase, we
#expect their aim deviation in that phase to be well outside the CI range of the 
#aligned phase.

ci_compare20 <- merge(aligned_CI20, rotated_CI20, by = "participant_id", suffixes = c("_aligned", "_rotated"))
ci_compare20$shift_amount <- ci_compare20$aimdeviation_deg_rotated[,1] - ci_compare20$aimdeviation_deg_aligned[,2]

# flag whether there's a strategy shift (if difference > 15 degrees)
ci_compare20$aim_shift <- ifelse(
  (ci_compare20$aimdeviation_deg_rotated[,1] - ci_compare20$aimdeviation_deg_aligned[,2] > 15) |
    (ci_compare20$aimdeviation_deg_rotated[,2] - ci_compare20$aimdeviation_deg_aligned[,1] > 15),
  "Yes", "No")
#ci_compare20$aim_shift[ci_compare20$participant_id == 5] <- "No" #participant 5 has a noisy strategy

print(ci_compare20[, c("participant_id", "aimdeviation_deg_aligned", "aimdeviation_deg_rotated", "shift_amount", "aim_shift")])

#see individual plots
df4 <- df20_aim[df20_aim$participant_id == 4, ] 
plot(df4$aimdeviation_deg, type="l")    


strategies_aligned20 <- rbind(df20_aim[df20_aim$participant_id %in% 4 &
                                         df20_aim$cutrial_no %in% 105:112, ])

nonstrategies_aligned20 <- rbind(df20_aim[df20_aim$participant_id %in% c(1,2,3) &
                                            df20_aim$cutrial_no %in% 105:112, ])

strategies_rotated20 <- rbind(df20_aim[df20_aim$participant_id %in% 4 &
                                         df20_aim$cutrial_no %in% 225:232, ])

nonstrategies_rotated20 <- rbind(df20_aim[df20_aim$participant_id %in% c(1,2,3) &
                                            df20_aim$cutrial_no %in% 225:232, ])

t.test(strategies_rotated$aimdeviation_deg,strategies_aligned$aimdeviation_deg, paired=TRUE)
t.test(strategies_rotated$aimdeviation_deg,nonstrategies_rotated$aimdeviation_deg)


plot_last20_rotated <- merge(last20_rotated, ci_compare20[, c("participant_id", "aim_shift")], by="participant_id")

ggplot(plot_last20_rotated, aes(x=factor(participant_id), y=aimdeviation_deg, color=aim_shift)) +
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
  ylim(0, 20)     
