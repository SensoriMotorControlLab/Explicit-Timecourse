first_rot_trial <- which(df$trial_type == "rotated")[1]

step1_fit <- fit_onestep_model(trials, aim, first_rot_trial)
step2_fit <- fit_twostep_model(trials, aim, first_rot_trial)
exp_fit   <- fit_exponential_model(trials, aim)


############EXP 
df <- total_learners_data[total_learners_data$participant_id == "9fb9fe", ] 
rot_start <- 113
df_sub <- df[trials >= rot_start, ]
trials_sub <- seq_along(df_sub$aimdeviation_deg)  # reindex for fitting
fit <- fit_exponential_model(trials_sub, df_sub$aimdeviation_deg)

lambda <- fit$pred["lambda"]
N0 <- fit$pred["N0"]
pred <- N0 * (1 - exp(-lambda * (trials - rot_start)))

plot(df$aimdeviation_deg, type = "l", main = "Exp Winner",
     ylim = c(-10, 50), xlim = c(0, last_rot_trial),
     col = "grey", lwd = 1)
abline(v = rot_start, col = "red", lty = 2, lwd = 1)
abline(h = 30, col = "red", lty = 2, lwd = 1)
lines(trials, pred, col = "blue", lwd = 2)



###
df <- total_learners_data[total_learners_data$participant_id == "a02c67", ] 
rot_start <- 89

df_sub <- df[df$cutrial_no >= rot_start, ]
trials_sub <- seq_along(df_sub$aimdeviation_deg)
fit <- fit_exponential_model(trials_sub, df_sub$aimdeviation_deg)

lambda <- fit$pred["lambda"]
N0 <- fit$pred["N0"]
pred <- N0 * (1 - exp(-lambda * (trials_sub - 1)))

plot(df$cutrial_no, df$aimdeviation_deg, type = "l",
     main = "Exp Fit", ylim = c(-10, 60),
     xlim = c(0, max(df$cutrial_no)),
     col = "grey", lwd = 1)
lines(df_sub$cutrial_no, pred, col = "blue", lwd = 2)
abline(v = rot_start, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)



## this is not exp....
df <- total_learners_data[total_learners_data$participant_id == "13cb04", ] 
rot_start <- 113
df_sub <- df[trials >= rot_start, ]
trials_sub <- seq_along(df_sub$aimdeviation_deg)  # reindex for fitting
fit <- fit_exponential_model(trials_sub, df_sub$aimdeviation_deg)

lambda <- fit$pred["lambda"]
N0 <- fit$pred["N0"]
pred <- N0 * (1 - exp(-lambda * (trials - rot_start)))

plot(df$aimdeviation_deg, type = "l", main = "Exp Winner",
     ylim = c(-10, 70), xlim = c(0, 232),
     col = "grey", lwd = 1)
abline(v = rot_start, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)
lines(trials, pred, col = "blue", lwd = 2)


############
df <- total_learners_data[total_learners_data$participant_id == "13cb04", ] 

rot_start <- 113

# subset from rotation onset
df_sub <- df[df$cutrial_no >= rot_start, ]
trials_sub <- df_sub$cutrial_no 
aim_sub    <- df_sub$aimdeviation_deg

# fit exponential model
fit_params <- Reach::exponentialFit(
  signal     = aim_sub,
  timepoints = trials_sub,
  mode       = "learning",
  gridpoints = 11,
  gridfits   = 10
)

# ---- helper to compute predictions ----
exp_predict <- function(params, trials) {
  # Try both orders until we know which is correct
  lambda <- params[1]
  N0     <- params[2]
  pred <- N0 * (1 - exp(-lambda * (trials - min(trials))))
  return(pred)
}

# generate predictions
pred <- exp_predict(fit_params, trials_sub)

# ---- plot ----
plot(df$cutrial_no, df$aimdeviation_deg, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(0, 232),
     col = "darkgrey", lwd = 2,
     cex.axis=2,
     bty='n')

# Add reference lines
abline(v = rot_start, col = "black", lty = 2, lwd = 0.5)
abline(h = 40, col = "black", lty = 2, lwd = 0.5)

# Add exponential fit
lines(trials_sub, pred, col = "darkmagenta", lwd = 3.5)
lines(x = c(0, rot_start), y = c(0, 0), col = "darkmagenta", lwd = 3.5)

legend("bottomleft", legend = "Exponential Model Prediction",
       col = "darkmagenta", lwd = 2, bty = "n",cex=1.2)



df <- total_learners_data[total_learners_data$participant_id == "8d426d", ] 

rot_start <- 113

# Create relative trial axis: rotation = 0
df$trial_rel <- df$cutrial_no - rot_start

# Subset data: 8 trials before rotation and 60 after
df_sub <- df[df$trial_rel >= -8 & df$trial_rel <= 60, ]

# Subset baseline (last 8 trials before rotation)
baseline_sub <- df_sub[df_sub$trial_rel < 0, ]

# Subset post-rotation for exponential fit
fit_sub <- df[df$cutrial_no >= rot_start, ]
trials_fit <- fit_sub$cutrial_no
aim_fit    <- fit_sub$aimdeviation_deg

# Fit exponential model
fit_params <- Reach::exponentialFit(
  signal     = aim_fit,
  timepoints = trials_fit,
  mode       = "learning",
  gridpoints = 11,
  gridfits   = 10
)

# Prediction helper
exp_predict <- function(params, trials) {
  lambda <- params[1]
  N0     <- params[2]
  pred <- N0 * (1 - exp(-lambda * (trials - min(trials))))
  return(pred)
}

pred <- exp_predict(fit_params, trials_fit)

# ---- plot ----
plot(df_sub$trial_rel, df_sub$aimdeviation_deg, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(-8, 60),
     col = "darkgrey", lwd = 2,
     cex.axis=2,
     bty='n',
     xaxt = "n")

# x-axis ticks (include 0)
tick_positions <- seq(-8, 60, by = 8)
axis(1, at = tick_positions, labels = FALSE)

# Reference lines
abline(v = 0, col = "black", lty = 2, lwd = 0.5)  # rotation onset
abline(h = 40, col = "black", lty = 2, lwd = 0.5)

# Exponential fit (shift trials to relative axis)
lines(trials_fit - rot_start, pred, col = "darkmagenta", lwd = 3.5)

# Optional flat line for baseline
lines(c(-8, 0), c(0, 0), col = "darkmagenta", lwd = 3.5)

legend("topright", legend = c("Exponential Model Prediction", " "),
       col = c("darkmagenta"),
       lwd = c(3, NA),
       bty = "n", cex=2)


############
# ---- subset participant ----
df <- total_learners_data[total_learners_data$participant_id == "8d426d", ] 

rot_start <- 113

# Relative trial axis (rotation onset = 0)
df$trial_rel <- df$cutrial_no - rot_start

# Subset data: 8 trials before rotation to 60 after
df_sub <- df[df$trial_rel >= -8 & df$trial_rel <= 60, ]
trials_sub <- df_sub$trial_rel
aim_sub    <- df_sub$aimdeviation_deg

# ---- fit exponential model ----
fit_params <- Reach::exponentialFit(
  signal     = aim_sub,
  timepoints = trials_sub,
  mode       = "learning",
  gridpoints = 11,
  gridfits   = 10
)

# ---- generate predictions ----
# Split into pre-rotation (flat) and post-rotation (exponential)
pre_rot <- trials_sub[trials_sub <= 0.9]
post_rot <- trials_sub[trials_sub > -0.9]

pred_pre <- rep(0, length(pre_rot))
pred_post <- fit_params[2] * (1 - exp(-fit_params[1] * (post_rot - 0)))

# ---- plot ----
plot(trials_sub, aim_sub, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(-8, 60),
     col = "darkgrey", lwd = 2,
     cex.axis = 2,
     bty = "n",
     xaxt = "n")  # remove numbers

# Use the same axis style as your one-step plot
tick_positions <- pretty(c(-8, 60), n = 8)
axis(1, at = tick_positions, labels = FALSE)

# Reference lines
abline(v = 0, col = "black", lty = 2, lwd = 0.5)  # rotation onset
abline(h = 40, col = "black", lty = 2, lwd = 0.5)

# Exponential fit lines (flat before rotation)
lines(pre_rot, pred_pre, col = "darkmagenta", lwd = 3.5)
lines(post_rot, pred_post, col = "darkmagenta", lwd = 3.5)

# Legend
legend("topright", legend = "Exponential Model Prediction",
       col = "darkmagenta", lwd = 3.5, bty = "n", cex = 2)


##
df <- total_learners_data[total_learners_data$participant_id == "8d426d", ] 
rot_start <- 113
df_sub <- df[trials >= rot_start, ]
trials_sub <- seq_along(df_sub$aimdeviation_deg)  # reindex for fitting
fit <- fit_exponential_model(trials_sub, df_sub$aimdeviation_deg)

lambda <- fit$pred["lambda"]
N0 <- fit$pred["N0"]
pred <- N0 * (1 - exp(-lambda * (trials - rot_start)))

plot(df$aimdeviation_deg, type = "l", main = "Exp Winner",
     ylim = c(-10, 70), xlim = c(0, 232),
     col = "grey", lwd = 1)
abline(v = rot_start, col = "red", lty = 2, lwd = 1)
abline(h = 30, col = "red", lty = 2, lwd = 1)
lines(trials, pred, col = "blue", lwd = 2)


############
# ---- subset participant ----
df <- total_learners_data[total_learners_data$participant_id == "a02c67", ] 

rot_start <- 113

df$trial_rel <- df$cutrial_no - rot_start

df_sub <- df[df$trial_rel >= -8 & df$trial_rel <= 60, ]
trials_sub <- df_sub$trial_rel
aim_sub    <- df_sub$aimdeviation_deg

# ---- fit exponential model ----
fit_params <- Reach::exponentialFit(
  signal     = aim_sub,
  timepoints = trials_sub,
  mode       = "learning",
  gridpoints = 11,
  gridfits   = 10
)

exp_predict <- function(params, trials, rot_trial = 0) {
  lambda <- params[1]
  N0     <- params[2]
  pred <- numeric(length(trials))
  # flat before rotation
  pred[trials <= rot_trial] <- 0
  # exponential after rotation
  pred[trials > rot_trial] <- N0 * (1 - exp(-lambda * (trials[trials > rot_trial] - rot_trial)))
  return(pred)
}

pred <- exp_predict(fit_params, trials_sub, rot_trial = 0)

# ---- plot ----
plot(trials_sub, aim_sub, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(-8, 60),
     col = "darkgrey", lwd = 2,
     cex.axis = 2,
     bty = "n",
     xaxt = "n")  # suppress numbers

# Custom ticks from -8 to 60, no numbers
tick_positions <- seq(-8, 60, by = 8)
axis(1, at = tick_positions, labels = FALSE)

# Reference lines
abline(v = 0, col = "black", lty = 2, lwd = 0.5)   # rotation onset
abline(h = 40, col = "black", lty = 2, lwd = 0.5)

# Exponential fit with flat baseline
lines(trials_sub, pred, col = "darkmagenta", lwd = 3.5)

# Legend
legend("topright", legend = "Exponential Model Prediction",
       col = "darkmagenta", lwd = 3.5, bty = "n", cex = 2)


##
df <- total_learners_data[total_learners_data$participant_id == "98e5cb", ] 
rot_start <- 89

df_sub <- df[df$cutrial_no >= rot_start, ]
trials_sub <- seq_along(df_sub$aimdeviation_deg)
fit <- fit_exponential_model(trials_sub, df_sub$aimdeviation_deg)

lambda <- fit$pred["lambda"]
N0 <- fit$pred["N0"]
pred <- N0 * (1 - exp(-lambda * (trials_sub - 1)))

plot(df$cutrial_no, df$aimdeviation_deg, type = "l",
     main = "Exp Fit", ylim = c(-10, 70),
     xlim = c(0, max(df$cutrial_no)),
     col = "grey", lwd = 1)
lines(df_sub$cutrial_no, pred, col = "blue", lwd = 2)
abline(v = rot_start, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)


##############ONE STEP#############


##
df <- total_learners_data[total_learners_data$participant_id == "15f2a1", ] 
plot(df$aimdeviation_deg, type = "l", main = "One step Winner", ylim = c(-10, 70),
     xlim = c(0, 208),
     col= "hotpink", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 40, col = "red", lty = 2, lwd = 1)

step1_trial <- 119
step1_size  <- 6
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


####prediction doesnt match rly step is at 40
df <- total_learners_data[total_learners_data$participant_id == "94709f", ] 
plot(df$aimdeviation_deg, type = "l", main = "Delayed Onset", ylim = c(-10, 50),
     xlim = c(0, 208),
     col= "#FF69B4", lwd = 2)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)

step1_trial <- 118
step1_size  <- 16
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##
df <- total_learners_data[total_learners_data$participant_id == "af8328", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, last_rot_trial),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)

step1_trial <- 143
step1_size  <- 6.2
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##
df <- total_learners_data[total_learners_data$participant_id == "b4d36d", ] 
plot(df$aimdeviation_deg, type = "l", main = "Delayed Onset", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "#FF69B4", lwd = 2)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)

step1_trial <- 145
step1_size  <- 6
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2

####swell fit
df <- total_learners_data[total_learners_data$participant_id == "2528e1", ] 
plot(df$aimdeviation_deg, type = "l", main = "One step Winner", ylim = c(-10, 70),
     xlim = c(0, 232),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 40, col = "red", lty = 2, lwd = 1)

step1_trial <- 124
step1_size  <- 23
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2



#### 
df <- total_learners_data[total_learners_data$participant_id == "a23b35", ] 
plot(df$aimdeviation_deg, type = "l", main = "Rapid-onset", ylim = c(-10, 60),
     xlim = c(0, 208),
     col= "salmon", lwd = 2)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)

step1_trial <- 96
step1_size  <- 27
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2



##rly bad fit bc of erraticness
df <- total_learners_data[total_learners_data$participant_id == "194dab", ] 
plot(df$aimdeviation_deg, type = "l", main = "One Step Winner", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)

step1_trial <- 143
step1_size  <- 6
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2



##<3
df <- total_learners_data[total_learners_data$participant_id == "7eec53", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 60),
     xlim = c(0, 208),
     col= "hotpink", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)

step1_trial <- 90
step1_size  <- 44
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##<3333
df <- total_learners_data[total_learners_data$participant_id == "7cd1bd", ] 

plot(df$aimdeviation_deg, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(105, 232),
     col = "salmon", lwd = 2,
     cex.axis = 2)

tick_positions <- pretty(c(-8, 60), n = 8)
axis(1, at = tick_positions, labels = FALSE)

abline(v = 113, col = "black", lty = 2, lwd = 0.5)
abline(h = 60, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 114
step1_size  <- 51

lines(c(0, 113), c(0, 0), col = "darkorange", lwd = 3.5)                       
lines(c(113, step1_trial), c(0, 0), col = "darkorange", lwd = 3.5)             
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "darkorange", lwd = 3.5) 
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "darkorange", lwd = 3.5)   

legend("bottomright", legend = c("One-Step Model Prediction"),
       col = c("darkorange"), lwd = 2, bty = "n", cex=2)


df <- total_learners_data[total_learners_data$participant_id == "7cd1bd", ]

# Relative trial axis: rotation onset (113) = 0
df$trial_rel <- df$cutrial_no - 113

# Subset data: from -8 (pre-rotation) to +60 (post-rotation)
df_sub <- df[df$trial_rel >= -8 & df$trial_rel <= 60, ]

# ---- plot ----
plot(df_sub$trial_rel, df_sub$aimdeviation_deg, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(-8, 60),
     col = "darkgrey", lwd = 2,
     cex.axis = 2,
     bty='n',
     xaxt = "n")

tick_positions <- pretty(c(-8, 60), n = 8)
axis(1, at = tick_positions, labels = FALSE)

# Reference lines
abline(v = 0, col = "black", lty = 2, lwd = 0.5)   # rotation onset
abline(h = 60, col = "black", lty = 2, lwd = 0.5)

# One-step model
step1_trial <- 114 - 113  # relative trial = 1
step1_size  <- 51

# Model lines in relative x
lines(c(-8, 0), c(0, 0), col = "darkorange", lwd = 3.5)            # baseline
lines(c(0, step1_trial), c(0, 0), col = "darkorange", lwd = 3.5)   # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "darkorange", lwd = 3.5)  # vertical jump
lines(c(step1_trial, max(df_sub$trial_rel)), c(step1_size, step1_size), col = "darkorange", lwd = 3.5)  # flat after jump

# Legend
legend("bottomright", legend = "One-Step Model Prediction",
       col = "darkorange", lwd = 2, bty = "n", cex = 2)



## - explroation
df <- total_learners_data[total_learners_data$participant_id == "654648", ] 
plot(df$aimdeviation_deg, type = "l", main = "r", ylim = c(-10, 70),
     xlim = c(0, 232),
     col= "grey", lwd = 1.5,
     xlab = "Trial",
     ylab = "Aim Deviation (degrees)")
abline(v = 113, col = "black", lty = 2, lwd = 0.5)
abline(h = 60, col = "black", lty = 2, lwd = 0.5)
step1_trial <- 114
step1_size  <- 40
lines(c(0, 113), c(0, 0), col = "deeppink2", lwd = 2)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "deeppink2", lwd = 2)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "deeppink2", lwd = 2) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "deeppink2", lwd = 2)   # flat at step2

legend("bottomright", legend = c("One-Step Model Prediction"),
       col = c("deeppink2"), lwd = 2, bty = "n")




#####excellent fit tbh
df <- total_learners_data[total_learners_data$participant_id == "ba8f14", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 60),
     xlim = c(0, 208),
     col= "hotpink", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 30, col = "red", lty = 2, lwd = 1)
step1_trial <- 118
step1_size  <- 18
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##
df <- total_learners_data[total_learners_data$participant_id == "1896cb", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 60),
     xlim = c(0, last_rot_trial),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)

step1_trial <- 116
step1_size  <- 30
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##PRETTY GOOD - erratic
df <- total_learners_data[total_learners_data$participant_id == "e066de", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 70),
     xlim = c(0, 232),
     col= "salmon", lwd = 2,
     xlab = "Trial",
     ylab = "Aim Deviation (degrees)")
abline(v = 113, col = "black", lty = 2, lwd = 0.5)
abline(h = 50, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 120
step1_size  <- 25
lines(c(0, 113), c(0, 0), col = "deeppink2", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "deeppink2", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "deeppink2", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "deeppink2", lwd = 1)   # flat at step2

legend("bottomright", legend = c("One-Step Model Prediction"),
       col = c("deeppink2"), lwd = 2, bty = "n")


##
df <- total_learners_data[total_learners_data$participant_id == "d9ff04", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, 208),
     col= "hotpink", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)

step1_trial <- 91
step1_size  <- 29
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##
df <- total_learners_data[total_learners_data$participant_id == "3e3a73", ] 
plot(df$aimdeviation_deg, type = "l", main = "Rapid-onset", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "salmon", lwd = 2)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 40, col = "red", lty = 2, lwd = 1)

step1_trial <- 116
step1_size  <- 21.4
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2



## looks exponential
df <- total_learners_data[total_learners_data$participant_id == "bde44b", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, last_rot_trial),
     col= "hotpink", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 40, col = "red", lty = 2, lwd = 1)
step1_trial <- 114
step1_size  <- 13
lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2



##
df <- total_learners_data[total_learners_data$participant_id == "bccf6e", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, last_rot_trial),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 40, col = "red", lty = 2, lwd = 1)
step1_trial <- 122
step1_size  <- 8
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##beauty
df <- total_learners_data[total_learners_data$participant_id == "96634a", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 40, col = "red", lty = 2, lwd = 1)

step1_trial <- 123
step1_size  <- 16
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2






####
df <- total_learners_data[total_learners_data$participant_id == "2c2f44", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, last_rot_trial),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 30, col = "red", lty = 2, lwd = 1)
step1_trial <- 114
step1_size  <- 20
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


## - erratic?
df <- total_learners_data[total_learners_data$participant_id == "ee29d6", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 20, col = "red", lty = 2, lwd = 1)
step1_trial <- 137
step1_size  <- 9
lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 1)   # flat at step2


##
df <- total_learners_data[total_learners_data$participant_id == "c0144b", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 20, col = "red", lty = 2, lwd = 1)
step1_trial <- 119
step1_size  <- 5


lines(c(0, 113), c(0, 0), col = "blue", lwd = 2)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 2)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 2) # vertical jump to step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 2)   # flat at step2


####BEAUTIFUL - slow insight
df <- total_learners_data[total_learners_data$participant_id == "54044d", ] 
plot(df$aimdeviation_deg, type = "l", main = "Two-Step Winner", ylim = c(-10, 50),
     xlim = c(0, last_rot_trial),
     col= "hotpink", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)

step1_trial <- 106
step1_size  <- 20

lines(c(0, 89), c(0, 0), col = "blue", lwd = 2)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 2)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 2) # vertical jump to step1
lines(c(step1_trial, step1_trial), c(step1_size, step1_size), col = "blue", lwd = 2) # flat at step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 2)   # flat at step2



#### Initial Erratic Aiming ~ Exploration
df <- total_learners_data[total_learners_data$participant_id == "a5310d", ] 
df$aimdeviation_deg[df$cutrial_no > rot_start & df$aimdeviation_deg >= 0 & df$aimdeviation_deg <= 5] <- NA


plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 80),
     xlim = c(81, 208),
     col= "darkgrey", lwd = 2,
     xlab = "",
     ylab = "",
     cex.axis = 2,
     bty='n',
     xaxt = "n")

axis(1, at = seq(65, 232, by = 24), labels = FALSE)

abline(v = 89, col = "black", lty = 2, lwd = 0.5)
abline(h = 60, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 90
step1_size  <- 68

lines(c(0, 89), c(0, 0), col = "violetred", lwd = 3.5)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "violetred", lwd = 3.5)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "violetred", lwd = 3.5) # vertical jump to step1
lines(c(step1_trial, step1_trial), c(step1_size, step1_size), col = "violetred", lwd = 3.5) # flat at step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "violetred", lwd = 3.5)   # flat at step2

legend("bottomright", legend = c("One-Step Model Prediction"),
       col = c("violetred"), lwd = 2, bty = "n", cex=2)



df <- total_learners_data[total_learners_data$participant_id == "a5310d", ] 
rot_start <- 89

# Remove near-zero aim values after rotation
df$aimdeviation_deg[df$cutrial_no > rot_start & 
                      df$aimdeviation_deg >= 0 & 
                      df$aimdeviation_deg <= 5] <- NA

# Create relative trial axis: rotation = 0
df$trial_rel <- df$cutrial_no - rot_start

# Subset data: 8 trials before rotation and 60 trials after
df_sub <- df[df$trial_rel >= -8 & df$trial_rel <= 60, ]

# Subset baseline (last 8 trials before rotation)
baseline_sub <- df_sub[df_sub$trial_rel < 0, ]

# ---- plot ----
plot(df_sub$trial_rel, df_sub$aimdeviation_deg, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(-8, 60),
     col = "darkgrey", lwd = 2,
     cex.axis=2,
     bty='n',
     xaxt = "n")

# Reference lines
abline(v = 0, col = "black", lty = 2, lwd = 0.5)  # rotation onset
abline(h = 60, col = "black", lty = 2, lwd = 0.5)

tick_positions <- pretty(c(-8, 60), n = 8)
axis(1, at = tick_positions, labels = FALSE)

# One-step model parameters
step1_trial <- 90 - rot_start  # relative trial = 1
step1_size  <- 68

# One-step model lines (relative x)
lines(c(-8, 0), c(0, 0), col = "violetred", lwd = 3.5)            # baseline
lines(c(0, step1_trial), c(0, 0), col = "violetred", lwd = 3.5)   # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "violetred", lwd = 3.5)  # vertical jump
lines(c(step1_trial, max(df_sub$trial_rel)), c(step1_size, step1_size), col = "violetred", lwd = 3.5)  # flat after jump

# Legend
legend("bottomright", legend = c("One-Step Prediction"),
       col = c("violetred" ),
       lwd = c(3, NA),
       bty = "n", cex=1.8)


####PERFECTION
df <- total_learners_data[total_learners_data$participant_id == "622518", ] 

# Plot raw aim deviation
plot(df$aimdeviation_deg, type = "l",
     main = "",
     xlab = "Trial",
     ylab = "Aim Deviation (degrees)",
     ylim = c(-15, 70),
     xlim = c(0, 232),
     col = "grey", lwd = 1.5)  

abline(v = 113, col = "black", lty = 2, lwd = 0.5)
abline(h = 60, col = "black", lty = 2, lwd = 0.5)


step1_trial <- 125
step1_size  <- 27

lines(c(0, 113), c(0, 0), col = "deeppink2", lwd = 2)
lines(c(113, step1_trial), c(0, 0), col = "deeppink2", lwd = 2)
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "deeppink2", lwd = 2)
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "deeppink2", lwd = 2)

legend("bottomright", legend = c("One-Step Model Prediction"),
       col = c("deeppink2"), lwd = 2, bty = "n")


### - slow remove 0s
df <- total_learners_data[total_learners_data$participant_id == "0f6fbf", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "hotpink", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)

step1_trial <- 130
step1_size  <- 24

lines(c(0, 113), c(0, 0), col = "blue", lwd = 2)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 2)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 2) # vertical jump to step1
lines(c(step1_trial, step1_trial), c(step1_size, step1_size), col = "blue", lwd = 2) # flat at step1
lines(c(step1_trial, nrow(df)), c(step1_size, step1_size), col = "blue", lwd = 2)   # flat at step2


#########TWO STEP <3 fast
df <- total_learners_data[total_learners_data$participant_id == "4a6642", ] 
plot(df$aimdeviation_deg, type = "l", main = "One-Step Winner", ylim = c(-10, 50),
     xlim = c(0, 208),
     col= "lightblue", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 40, col = "red", lty = 2, lwd = 1)
step1_trial <- 95
step1_size  <- 10
step2_trial <- 101
step2_size  <- 26

lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2

####NOT BAD FIT
df <- total_learners_data[total_learners_data$participant_id == "81d984", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 70),
     xlim = c(0, 208),
     col= "grey", lwd = 1.5,
     xlab = "Trial",
     ylab = "Aim Deviation (degrees)",)
abline(v = 89, col = "black", lty = 2, lwd = 0.5)
abline(h = 40, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 92
step1_size  <- 20
step2_trial <- 109
step2_size  <- 32

lines(c(0, 89), c(0, 0), col = "blue", lwd = 2)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 2)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 2) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 2) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 2) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 2)   # flat at step2

legend("bottomright", legend = c("Two-Step Model Prediction"),
       col = c("blue"), lwd = 2, bty = "n")

#####
df <- total_learners_data[total_learners_data$participant_id == "4093e8", ] 
plot(df$aimdeviation_deg, type = "l", main = "Two-Step Winner", ylim = c(-10, 60),
     xlim = c(0, 208),
     col= "lightblue", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)

step1_trial <- 97
step1_size  <- 39
step2_trial <- 98
step2_size  <- 41

lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2

###<3 slow insight
df <- total_learners_data[total_learners_data$participant_id == "13d986", ] 
plot(df$aimdeviation_deg, type = "l", main = "Two-Step Winner", ylim = c(-10, 60),
     xlim = c(0, 208),
     col= "lightblue", lwd = 1)
abline(v = 89, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)

step1_trial <- 117
step1_size  <- 37
step2_trial <- 125
step2_size  <- 41

lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2




##
df <- total_learners_data[total_learners_data$participant_id == "3091de", ] 
plot(df$aimdeviation_deg, type = "l", main = "Erratic Winner", ylim = c(-10, 60),
     xlim = c(0, 232),
     col= "lightblue", lwd = 2)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)
step1_trial <- 118
step1_size  <- 6
step2_trial <- 159
step2_size  <- 19

lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2

##
df <- total_learners_data[total_learners_data$participant_id == "9eabc1", ] 
plot(df$aimdeviation_deg, type = "l", main = "Two-Step Winner", ylim = c(-10, 70),
     xlim = c(0, 232),
     col= "lightblue", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)

step1_trial <- 126
step1_size  <- 50
step2_trial <- 132
step2_size  <- 55

lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2

###<3
df <- total_learners_data[total_learners_data$participant_id == "f275ca", ] 
plot(df$aimdeviation_deg, type = "l", main = "Erratic Winner", ylim = c(-10, 60),
     xlim = c(0, 232),
     col= "lightblue", lwd = 2)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 60, col = "red", lty = 2, lwd = 1)

step1_trial <- 136
step1_size  <- 43
step2_trial <- 180
step2_size  <- 27

lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2


## predicted two step at trial 100 and 107 but i changed it
df <- total_learners_data[total_learners_data$participant_id == "31b753", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 80),
     xlim = c(0, 208),
     col= "darkgrey", lwd = 2,
     xlab = "",
     ylab = "",
     cex.axis = 2,
     bty='n')
abline(v = 89, col = "black", lty = 2, lwd = 0.5)
abline(h = 40, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 107
step1_size  <- 20
step2_trial <- 140
step2_size  <- 30

lines(c(0, 89), c(0, 0), col = "red2", lwd = 3.5)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "red2", lwd = 3.5)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "red2", lwd = 3.5) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "red2", lwd = 3.5) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "red2", lwd = 3.5) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "red2", lwd = 3.5)   # flat at step2


legend("bottomright", legend = c("Two-Step Model Prediction"),
       col = c("red2"), lwd = 2, bty = "n",cex=1.2)


df <- total_learners_data[total_learners_data$participant_id == "31b753", ] 
rot_start <- 89

# Relative trial axis: rotation = 0
df$trial_rel <- df$cutrial_no - rot_start

# Subset data: -8 to +60
df_sub <- df[df$trial_rel >= -8 & df$trial_rel <= 60, ]

# Subset baseline (last 8 trials before rotation)
baseline_sub <- df_sub[df_sub$trial_rel < 0, ]

# ---- plot ----
plot(df_sub$trial_rel, df_sub$aimdeviation_deg, type = "l",
     main = "",
     xlab = "",
     ylab = "",
     ylim = c(-10, 80),
     xlim = c(-8, 60),
     col = "darkgrey", lwd = 2,
     cex.axis = 2,
     bty = 'n',
     xaxt = "n")

tick_positions <- pretty(c(-8, 60), n = 8)
axis(1, at = tick_positions, labels = FALSE)

# Reference lines
abline(v = 0, col = "black", lty = 2, lwd = 0.5)   # rotation onset
abline(h = 40, col = "black", lty = 2, lwd = 0.5)

# Two-step model parameters (convert to relative trial indices)
step1_trial <- 107 - rot_start   # = 18
step1_size  <- 20
step2_trial <- 140 - rot_start   # = 51
step2_size  <- 30

# Two-step model lines
lines(c(-8, 0), c(0, 0), col = "red2", lwd = 3.5)                  # baseline
lines(c(0, step1_trial), c(0, 0), col = "red2", lwd = 3.5)         # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "red2", lwd = 3.5)  # jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "red2", lwd = 3.5) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "red2", lwd = 3.5) # jump to step2
lines(c(step2_trial, max(df_sub$trial_rel)), c(step2_size, step2_size), col = "red2", lwd = 3.5) # flat at step2

# Legend
legend("topright", legend = c("Two-Step Model Prediction"),
       col = c("red2"),
       lwd = c(3, NA),
       bty = "n", cex = 2)



## SLOW INSIGHT
df <- total_learners_data[total_learners_data$participant_id == "d0d19c", ]
df$aimdeviation_deg[df$cutrial_no > rot_start & df$aimdeviation_deg >= -2 & df$aimdeviation_deg <= 5] <- NA

plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 70),
     xlim = c(0, last_rot_trial),
     col= "grey", lwd = 1.5,
     xlab = "Trial",
     ylab = "Aim Deviation (degrees)",)
abline(v = 89, col = "black", lty = 2, lwd = 0.5)
abline(h = 60, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 90
step1_size  <- 20
step2_trial <- 120
step2_size  <- 25

lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2

legend("bottomright", legend = c("Two-Step Model Prediction"),
       col = c("blue"), lwd = 2, bty = "n")

###
df <- total_learners_data[total_learners_data$participant_id == "7eacfc", ] 
plot(df$aimdeviation_deg, type = "l", main = "Two-Step Winner", ylim = c(-10, 50),
     xlim = c(0, 232),
     col= "lightblue", lwd = 1)
abline(v = 113, col = "red", lty = 2, lwd = 1)
abline(h = 50, col = "red", lty = 2, lwd = 1)
step1_trial <- 125
step1_size  <- 13.7
step2_trial <- 131
step2_size  <- 16.1

lines(c(0, 113), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(113, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2



##
df <- total_learners_data[total_learners_data$participant_id == "901482", ] 
plot(df$aimdeviation_deg, type = "l", main = "Two-Step Winner", ylim = c(-10, 70),
     xlim = c(0, 208),
     col= "grey", lwd = 1.5)
abline(v = 89, col = "black", lty = 2, lwd = 0.5)
abline(h = 50, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 100
step1_size  <- 32
step2_trial <- 109
step2_size  <- 36

lines(c(0, 89), c(0, 0), col = "blue", lwd = 1)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "blue", lwd = 1)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "blue", lwd = 1) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "blue", lwd = 1) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "blue", lwd = 1) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "blue", lwd = 1)   # flat at step2

#####honestly not bad (slow bc wrong strat at first)
df <- total_learners_data[total_learners_data$participant_id == "0c7728", ] 
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 70),
     xlim = c(0, 232),
     col= "grey", lwd = 1.5,
     xlab = "",
     ylab = "",
     cex.axis = 2)
abline(v = 113, col = "black", lty = 2, lwd = 0.5)
abline(h = 60, col = "black", lty = 2, lwd = 0.5)


step1_trial <- 113
step1_size  <- 6
step2_trial <- 155
step2_size  <- 20

lines(c(0, 89), c(0, 0), col = "mediumorchid", lwd = 2)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "mediumorchid", lwd = 2)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "mediumorchid", lwd = 2) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "mediumorchid", lwd = 2) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "mediumorchid", lwd = 2) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "mediumorchid", lwd = 2)   # flat at step2

legend("bottomright", legend = c("Two-Step Model Prediction"),
       col = c("mediumorchid"), lwd = 2, bty = "n")



## immediate two-step insight
df <- total_learners_data[total_learners_data$participant_id == "33e532", ]
df$aimdeviation_deg[df$cutrial_no > rot_start & df$aimdeviation_deg >= -2 & df$aimdeviation_deg <= 5] <- NA
plot(df$aimdeviation_deg, type = "l", main = "", ylim = c(-10, 70),
     xlim = c(0, 208),
     col= "salmon", lwd = 2,
     xlab = "Trial",
     ylab = "Aim Deviation (degrees)",)
abline(v = 89, col = "black", lty = 2, lwd = 0.5)
abline(h = 60, col = "black", lty = 2, lwd = 0.5)

step1_trial <- 91
step1_size  <- 15
step2_trial <- 95
step2_size  <- 33

lines(c(0, 89), c(0, 0), col = "mediumorchid", lwd = 2)                       # baseline
lines(c(89, step1_trial), c(0, 0), col = "mediumorchid", lwd = 2)             # flat until step1
lines(c(step1_trial, step1_trial), c(0, step1_size), col = "mediumorchid", lwd = 2) # vertical jump to step1
lines(c(step1_trial, step2_trial), c(step1_size, step1_size), col = "mediumorchid", lwd = 2) # flat at step1
lines(c(step2_trial, step2_trial), c(step1_size, step2_size), col = "mediumorchid", lwd = 2) # vertical jump to step2
lines(c(step2_trial, nrow(df)), c(step2_size, step2_size), col = "mediumorchid", lwd = 2)   # flat at step2

legend("bottomright", legend = c("Two-Step Model Prediction"),
       col = c("mediumorchid"), lwd = 2, bty = "n")

