aim40_data_fixed <- lapply(aim40_data, function(df) {
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


df40_aim <- NULL  

for (file_index in 1:length(aim40_data_fixed)) {
  df <- aim40_data_fixed[[file_index]]
  df$participant_id <- file_index  # assign participant ID
  
  
  if (is.null(df40_aim)) {
    df40_aim <- df
  } else {
    df40_aim <- rbind(df40_aim, df)
  }
  
}


last_40aligned <- rbind(df40_aim[df40_aim$participant_id %in% c(1,2,3,4,5,6,7,8,11,12,13) &
                                   df40_aim$cutrial_no %in% 81:88, ], 
                        df40_aim[df40_aim$participant_id %in% c(9,10,14) &
                                   df40_aim$cutrial_no %in% 105:112, ])

first_40rotated <- rbind(df40_aim[df40_aim$participant_id %in% c(1,2,3,4,5,6,7,8,11,12,13) &
                                    df40_aim$cutrial_no %in% 89:96, ], 
                         df40_aim[df40_aim$participant_id %in% c(9,10,14) &
                                    df40_aim$cutrial_no %in% 113:120, ])


last40_rotated <- rbind(df40_aim[df40_aim$participant_id %in% c(1,2,3,4,5,6,7,8,11,12,13) &
                                   df40_aim$cutrial_no %in% 201:208, ], 
                        df40_aim[df40_aim$participant_id %in% c(9,10,14) &
                                   df40_aim$cutrial_no %in% 225:232, ])


CI <- function(last_40aligned) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last_40aligned, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

aligned_CI40 <- CI(last_40aligned)



CI <- function(last40_rotated) {
  aggregate(aimdeviation_deg ~ participant_id, 
            data = last40_rotated, 
            FUN = function(x) Reach::getConfidenceInterval(x))
}

rotated_CI40 <- CI(last40_rotated)


#if a participant was consistently applying a strategy in the rotated phase, we
#expect their aim deviation in that phase to be well outside the CI range of the 
#aligned phase.

ci_compare40 <- merge(aligned_CI40, rotated_CI40, by = "participant_id", suffixes = c("_aligned", "_rotated"))
ci_compare40$shift_amount <- ci_compare40$aimdeviation_deg_rotated[,1] - ci_compare40$aimdeviation_deg_aligned[,2]

# flag whether there's a strategy shift (if difference > 15 degrees)
ci_compare40$aim_shift <- ifelse(
  (ci_compare40$aimdeviation_deg_rotated[,1] - ci_compare40$aimdeviation_deg_aligned[,2] > 15) |
    (ci_compare40$aimdeviation_deg_rotated[,2] - ci_compare40$aimdeviation_deg_aligned[,1] > 15),
  "Yes", "No")
#ci_compare40$aim_shift[ci_compare40$participant_id == 5] <- "No" #participant 5 has a noisy strategy

print(ci_compare40[, c("participant_id", "aimdeviation_deg_aligned", "aimdeviation_deg_rotated", "shift_amount", "aim_shift")])

#see individual plots
df4 <- df40_aim[df40_aim$participant_id == 11, ] #3,6,7,8,12,10, 11?
plot(df4$aimdeviation_deg, type="l")    


strategies_aligned40 <- rbind(df40_aim[df40_aim$participant_id %in% c(3,6,7,8,12) &
                                         df40_aim$cutrial_no %in% 81:88, ], 
                              df40_aim[df40_aim$participant_id %in% 10 &
                                         df40_aim$cutrial_no %in% 105:112, ])

nonstrategies_aligned40 <- rbind(df40_aim[df40_aim$participant_id %in% c(1,2, 4,5, 11, 13) &
                                            df40_aim$cutrial_no %in% 81:88, ], 
                                 df40_aim[df40_aim$participant_id %in% c(9,14) &
                                            df40_aim$cutrial_no %in% 105:112, ])

strategies_rotated40 <- rbind(df40_aim[df40_aim$participant_id %in% c(3,6,7,8,12) &
                                         df40_aim$cutrial_no %in% 201:208, ], 
                              df40_aim[df40_aim$participant_id %in% 10 &
                                         df40_aim$cutrial_no %in% 225:232, ])

nonstrategies_rotated40 <- rbind(df40_aim[df40_aim$participant_id %in% c(1,2, 4,5, 11, 13) &
                                            df40_aim$cutrial_no %in% 201:208, ], 
                                 df40_aim[df40_aim$participant_id %in% c(9,14) &
                                            df40_aim$cutrial_no %in% 225:232, ])

t.test(strategies_rotated$aimdeviation_deg,strategies_aligned$aimdeviation_deg, paired=TRUE)
t.test(strategies_rotated$aimdeviation_deg,nonstrategies_rotated$aimdeviation_deg)


plot_last40_rotated <- merge(last40_rotated, ci_compare40[, c("participant_id", "aim_shift")], by="participant_id")

ggplot(plot_last40_rotated, aes(x=factor(participant_id), y=aimdeviation_deg, color=aim_shift)) +
  geom_jitter(width=0.1, size=3, alpha=0.7) +
  scale_color_manual(values=c("Yes"="deeppink", "No"="black")) +
  labs(
    title = "Last 8 aimes (Rotated Phase) by Strategy Group",
    x = "Participant ID",
    y = "Aiming Deviation (Degrees)",
    color = "Strategy Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  ylim(0, 40)     
