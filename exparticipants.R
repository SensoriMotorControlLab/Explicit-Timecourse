strategy_data <- read.csv("data/strategy_only_participants.csv")


#- multi-step -#

## 
pid <- "0b1dca"
df <- strategy_data[strategy_data$participant_id == pid, ]

aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)

aligned_mean <- mean(aligned_trials, na.rm = TRUE)
aligned_sd   <- sd(aligned_trials, na.rm = TRUE)

aligned_trials_clean <- aligned_trials[
  abs(aligned_trials - aligned_mean) <= aligned_sd
]

rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 120)

transition_aim <- c(aligned_trials_clean, rotated_trials)

x_aligned <- seq(-length(aligned_trials_clean), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)

plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "cyan", ylim = c(-25, 80),
     xlab = "", ylab = "",
     main = "",
     bty = "n",
     cex.axis = 1.5,
     cex.lab = 1.5,
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 0, col = "grey", lty = 2, lwd = 2)

##

pid <- "e066de" 
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "cyan", ylim = c(-25, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 0, col = "grey", lty = 2, lwd = 2)




#-- step --#

pid <- "24c273"  
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "orchid", ylim = c(-25, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)
abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 0, col = "grey", lty = 2, lwd = 2)


####ba0a7c afaaf4

pid <- "7eacfc"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "orchid", ylim = c(-25, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)
abline(v = 0, col = "grey", lty = 2, lwd = 2)  





##--exploratory--##


pid <- "73230b"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "#EAA178", ylim = c(-25, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 0, col = "grey", lty = 2, lwd = 2)


##
pid <- "803175"
df <- strategy_data[strategy_data$participant_id == pid, ]
aligned_trials <- tail(df$aimdeviation_deg[df$trial_type == "aligned"], 16)
rotated_trials <- head(df$aimdeviation_deg[df$trial_type == "rotated"], 100)
transition_aim <- c(aligned_trials, rotated_trials)
transition_trials <- 1:length(transition_aim)

x_aligned <- seq(-length(aligned_trials), -1, 1)
x_rotated <- seq(0, length(rotated_trials) - 1, 1)
transition_trials <- c(x_aligned, x_rotated)


plot(transition_trials, transition_aim, type = "l", lwd = 3,
     col = "#EAA178", ylim = c(-25, 80),
     xlab = "", ylab = "",
     main = paste(""),
     bty = "n",       
     cex.axis = 1.5,   
     cex.lab = 1.5,    
     cex.main = 1.8)

abline(v = 0, col = "grey", lty = 2, lwd = 2)  
abline(h = 0, col = "grey", lty = 2, lwd = 2)

