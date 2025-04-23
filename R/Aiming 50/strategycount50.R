aim50_data_fixed <- lapply(aim50_data, function(df) {
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


      df50_aim <- NULL  
  
  for (file_index in 1:length(aim50_data_fixed)) {
    df <- aim50_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(df50_aim)) {
      df50_aim <- df
    } else {
      df50_aim <- rbind(df50_aim, df)
    }
    
  }
      
      
      last_50aligned <- rbind(df50_aim[df50_aim$participant_id %in% c(1,4,5,6,7, 8,9,11,12,14) &
                                         df50_aim$cutrial_no %in% 81:88, ], 
                              df50_aim[df50_aim$participant_id %in% c(2,3,10,13) &
                                         df50_aim$cutrial_no %in% 105:112, ])
      
      first_50rotated <- rbind(df50_aim[df50_aim$participant_id %in% c(1,4,5,6,7, 8,9,11,12, 14) &
                                          df50_aim$cutrial_no %in% 89:96, ], 
                               df50_aim[df50_aim$participant_id %in% c(2,3,10,13) &
                                          df50_aim$cutrial_no %in% 113:120, ])
      
      
      last50_rotated <- rbind(df50_aim[df50_aim$participant_id %in% c(1,4,5,6,7,8,9,11,12,14) &
                                         df50_aim$cutrial_no %in% 201:208, ], 
                              df50_aim[df50_aim$participant_id %in% c(2,3,10,13) &
                                         df50_aim$cutrial_no %in% 225:232, ])
      
      
      CI <- function(last_50aligned) {
        aggregate(aimdeviation_deg ~ participant_id, 
                  data = last_50aligned, 
                  FUN = function(x) Reach::getConfidenceInterval(x))
      }
      
      aligned_CI50 <- CI(last_50aligned)
      
      
      
      CI <- function(last50_rotated) {
        aggregate(aimdeviation_deg ~ participant_id, 
                  data = last50_rotated, 
                  FUN = function(x) Reach::getConfidenceInterval(x))
      }
      
      rotated_CI50 <- CI(last50_rotated)
      
      
      #if a participant was consistently applying a strategy in the rotated phase, we
      #expect their aim deviation in that phase to be well outside the CI range of the 
      #aligned phase.
      
      ci_compare50 <- merge(aligned_CI50, rotated_CI50, by = "participant_id", suffixes = c("_aligned", "_rotated"))
      ci_compare50$shift_amount <- ci_compare50$aimdeviation_deg_rotated[,1] - ci_compare50$aimdeviation_deg_aligned[,2]
      
      # flag whether there's a strategy shift (if difference > 15 degrees)
      ci_compare50$aim_shift <- ifelse(
        (ci_compare50$aimdeviation_deg_rotated[,1] - ci_compare50$aimdeviation_deg_aligned[,2] > 15) |
          (ci_compare50$aimdeviation_deg_rotated[,2] - ci_compare50$aimdeviation_deg_aligned[,1] > 15),
        "Yes", "No")
      ci_compare50$aim_shift[ci_compare50$participant_id == 5] <- "No" #participant 5 has a noisy strategy
      
      print(ci_compare50[, c("participant_id", "aimdeviation_deg_aligned", "aimdeviation_deg_rotated", "shift_amount", "aim_shift")])
      
  #see individual plots
      df3 <- df50_aim[df50_aim$participant_id == 5, ] #2,3,5,6 8,9,12,13, but 5 is noisy!
      plot(df3$aimdeviation_deg, type="l")    
      
      
      strategies_aligned50 <- rbind(df50_aim[df50_aim$participant_id %in% c(6,8,9,12) &
                                             df50_aim$cutrial_no %in% 81:88, ], 
                                  df50_aim[df50_aim$participant_id %in% c(2,3,13) &
                                             df50_aim$cutrial_no %in% 105:112, ])
      
      nonstrategies_aligned50 <- rbind(df50_aim[df50_aim$participant_id %in% c(1,4,5,7,11,14) &
                                                df50_aim$cutrial_no %in% 81:88, ], 
                                     df50_aim[df50_aim$participant_id %in% 10 &
                                                df50_aim$cutrial_no %in% 105:112, ])
      
      strategies_rotated50 <- rbind(df50_aim[df50_aim$participant_id %in% c(6,8,9,12) &
                                             df50_aim$cutrial_no %in% 201:208, ], 
                                  df50_aim[df50_aim$participant_id %in% c(2,3,13) &
                                             df50_aim$cutrial_no %in% 225:232, ])
      
      nonstrategies_rotated50 <- rbind(df50_aim[df50_aim$participant_id %in% c(1,4,5,7,11,14) &
                                                df50_aim$cutrial_no %in% 201:208, ], 
                                     df50_aim[df50_aim$participant_id %in% 10 &
                                                df50_aim$cutrial_no %in% 225:232, ])
      
      t.test(strategies_rotated$aimdeviation_deg,strategies_aligned$aimdeviation_deg, paired=TRUE)
      t.test(strategies_rotated$aimdeviation_deg,nonstrategies_rotated$aimdeviation_deg)
      
    
      plot_last50_rotated <- merge(last50_rotated, ci_compare50[, c("participant_id", "aim_shift")], by="participant_id")
      
      ggplot(plot_last50_rotated, aes(x=factor(participant_id), y=aimdeviation_deg, color=aim_shift)) +
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
        ylim(0, 50)     
      
