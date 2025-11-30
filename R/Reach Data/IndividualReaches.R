###visualization: histograms

dfreach <- function () {
  total_learners_data <- read.csv("data/total_learners_data.csv", stringsAsFactors =FALSE)
  learner_id <- suppressMessages(getLearners())
  
  last_8_aligned <- total_learners_data[
    (total_learners_data$cutrial_no %in% 85:88 & total_learners_data$group == 'Group 1') |
      (total_learners_data$cutrial_no %in% 109:112 & total_learners_data$group == 'Group 2'),]
  
  last_8_aligned <- last_8_aligned %>%
    group_by(group, participant_id) %>%
    arrange(cutrial_no) %>%
    mutate(time = seq(-n(), -1)) %>%
    ungroup()
  
  dfAlignedReach <- data.frame(
  x = last_8_aligned$time,
  y = last_8_aligned$reachdeviation_deg,
  rotation_group = last_8_aligned$rotation
  )

  
  first_32_rotated <- total_learners_data[
    (total_learners_data$cutrial_no %in% 89:120 & total_learners_data$group == 'Group 1') |
      (total_learners_data$cutrial_no %in% 105:136 & total_learners_data$group == 'Group 2'),]
  
  
  first_32_rotated <- first_32_rotated %>%
    group_by(group, participant_id) %>%
    arrange(cutrial_no) %>%
    mutate(time = seq(0, n()-1)) %>%
    ungroup()
  
  dfRotatedReach <- data.frame(
  x = first_32_rotated$time,
  y= first_32_rotated$reachdeviation_deg ,
  rotation_group = first_32_rotated$rotation
  )

all_hist_data_reach <- rbind(dfAlignedReach, dfRotatedReach)
}

#60 reach histogram
plot60reach <- function () {
  all_hist_data_reach  <- dfreach ()
   aim_60_hist_reach <- all_hist_data_reach %>%
    filter(rotation_group == '60')
  
  plot(NA,
       main='Reach Deviation With a 60° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,60), 
       ax=F, bty='n')
  
  
  img_info <- hist2d(x=aim_60_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,60,2.5)))
  img <- log(img_info$freq2D + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "#d5d5d5", "#858f94", "#49525e"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,60,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 59.5, 59.5), 
        col='navy', lty=3, lwd=2)
  avg_aim60r <- aggregate(y ~ x, data=aim_60_hist_reach, FUN=mean)
  #lines(avg_aim60r$x, avg_aim60r$y, col="#a93154", lwd=2)
  text(x = -8, y = 63, labels = "", adj = c(0,2), col = "black", cex = 1)
}

#reach 50
plot50reach <- function () {
  all_hist_data_reach  <- dfreach ()
  aim_50_hist_reach <- all_hist_data_reach %>%
    filter(rotation_group == '50')
  
  plot(NA,
       main='Reach Deviation With a 50° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,50), 
       ax=F, bty='n')
  
  
  img_info <- hist2d(x=aim_50_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,50,2.5)))
  img <- log(img_info$freq2D + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("white", "#d5d5d5", "#858f94", "#49525e"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,50,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 49.5, 49.5), 
        col='navy', lty=3, lwd=2)
  avg_aim50r <- aggregate(y ~ x, data=aim_50_hist_reach, FUN=mean)
 # lines(avg_aim50r$x, avg_aim50r$y, col="#a93154", lwd=2)
  text(x = -8, y = 63, labels = "", adj = c(0,2), col = "black", cex = 1)
}

#reach 40
plot40reach <- function() {
  all_hist_data_reach  <- dfreach ()
  aim_40_hist_reach <- all_hist_data_reach %>%
    filter(rotation_group == '40')
  
  plot(NA,
       main='Reach Deviation With a 40° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,40), 
       ax=F, bty='n')
  
  
  img_info <- hist2d(x=aim_40_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,40,2.5)))
  img <- log(img_info$freq2D + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("#ffffff", "#d5d5d5", "#858f94", "#49525e"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,40,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 39.5, 39.5), 
        col='navy', lty=3, lwd=2)
  avg_aim40r <- aggregate(y ~ x, data=aim_40_hist_reach, FUN=mean)
  #lines(avg_aim40r$x, avg_aim40r$y, col="#a93154", lwd=2)
  text(x = -8, y = 63, labels = "", adj = c(0,2), col = "black", cex = 1)
}

plot30reach <- function() {
  all_hist_data_reach  <- dfreach ()
  aim_30_hist_reach <- all_hist_data_reach %>%
    filter(rotation_group == '30')
  
  plot(NA,
       main='Reach Deviation With a 30° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,30), 
       ax=F, bty='n')
  
  
  img_info <- hist2d(x=aim_30_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,30,2.5)))
  img <- log(img_info$freq2D + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("#ffffff", "#d5d5d5", "#858f94", "#49525e"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,30,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 29.5, 29.5), 
        col='navy', lty=3, lwd=2)
  avg_aim30r <- aggregate(y ~ x, data=aim_30_hist_reach, FUN=mean)
 # lines(avg_aim30r$x, avg_aim30r$y, col="#a93154", lwd=2)
  text(x = -8, y = 63, labels = "", adj = c(0,2), col = "black", cex = 1)
}

plot20reach <- function() {
  all_hist_data_reach  <- dfreach ()
  aim_20_hist_reach <- all_hist_data_reach %>%
    filter(rotation_group == '20')
  
  plot(NA,
       main='Reach Deviation With a 20° Rotation',
       xlab='Trial', ylab='Aim Deviation (°)',
       xlim=c(-8,32), ylim=c(-15,20), 
       ax=F, bty='n')
  
  
  img_info <- hist2d(x=aim_20_hist_reach, nbins=NA, edges=list(seq(-8,31.5,1), seq(-15,20,2.5)))
  img <- log(img_info$freq2D + 1)
  
  image(x=img_info$x.edges,
        y=img_info$y.edges,
        col=colorRampPalette(c("#ffffff", "#d5d5d5", "#858f94", "#49525e"))(100),
        z=img,
        add=TRUE)
  
  axis(side=1, at=c(-8, 0,8, 16, 24, 32), labels=c(-8, 0,8, 16, 24, 32))
  axis(side=2, at=seq(-10,20,10))
  lines(x=c(-8, 0, 0, 32), 
        y=c(-0.5, -0.5, 19.5, 19.5), 
        col='navy', lty=3, lwd=2)
  avg_aim20r <- aggregate(y ~ x, data=aim_20_hist_reach, FUN=mean)
 # lines(avg_aim20r$x, avg_aim20r$y, col="#a93154", lwd=2)
  text(x = -8, y = 63, labels = "", adj = c(0,2), col = "black", cex = 1)
}

par(cex.axis = 1.5)


##model fits

fitReachModels <- function() {
  strat_data <- read.csv("data/strategy_only_participants.csv")
  
  results <- data.frame(
    participant = character(),
    group = character(),
    exp_aic = numeric(),
    step1_aic = numeric(),
    step2_aic = numeric(),
    best_model = character(),
    stringsAsFactors = FALSE
  )
  
  participants <- unique(strat_data$participant_id)
  
  for (pid in participants) {
    dat <- strat_data %>% filter(participant_id == pid)
    group <- unique(dat$group)
    
    baseline_trials <- dat %>% filter(trial_type.x == 'aligned') %>% tail(8)
    rotated_trials  <- dat %>% filter(trial_type.x == 'rotated') %>% head(32)
    
    all_trials <- rbind(baseline_trials, rotated_trials) %>% arrange(cutrial_no)
    
    trials <- 0:(nrow(all_trials) - 1)      
    aim    <- all_trials$reachdeviation_deg
    rotation_vals <- all_trials$rotation
    first_rot_trial <- which(all_trials$trial_type.x == "rotated")[1]
    
    ################## one-step model with noise + stability
    fit_onestep_model <- function(trials, aim, first_rot_trial, min_step=6, noise_sd=4){
      neg_log_likelihood <- function(par){
        mean2  <- par[1]                       # plateau after step
        trial1 <- par[2] + first_rot_trial - 1
        sd1    <- abs(par[3])
        sd2    <- abs(par[4])
        
        # Enforce minimum stable step
        if(mean2 < min_step) return(1e6)
        
        pred <- ifelse(trials < trial1, 0, mean2)
        residuals <- aim - pred
        
        # Downweight extreme outliers (don’t inflate)
        extreme_mask <- !is.na(residuals) & (abs(residuals) > 2 * max(rotation_vals, na.rm=TRUE))
        residuals[extreme_mask] <- residuals[extreme_mask] / noise_sd
        
        ll <- ifelse(trials < trial1,
                     dnorm(residuals, mean=0, sd=sd1, log=TRUE),
                     dnorm(residuals, mean=0, sd=sd2, log=TRUE))
        return(-sum(ll, na.rm=TRUE))
      }
      
      # Try multiple starts
      init_list <- list(
        c(mean2=10, trial1=3, sd1=3, sd2=5),
        c(mean2=20, trial1=5, sd1=3, sd2=5),
        c(mean2=40, trial1=10, sd1=3, sd2=5)
      )
      
      fits <- lapply(init_list, function(init) {
        optim(init, neg_log_likelihood, method="L-BFGS-B",
              lower=c(min_step, 1, 1, 1),
              upper=c(max(rotation_vals, na.rm=TRUE)*2, length(trials), 10, 10))
      })
      
      best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
      names(best_fit$par) <- c("mean2", "trial1", "sd1", "sd2")
      return(list(mse=best_fit$value, params=best_fit$par, k=4))
    }
    
    ################## two-step model with noise + stability
    fit_twostep_model <- function(trials, aim, first_rot_trial, min_step1=6, min_step2=6, noise_sd=4){
      neg_log_likelihood <- function(par){
        mean2  <- par[1]                       # plateau after step 1
        mean3  <- par[2]                       # plateau after step 2
        trial1 <- par[3] + first_rot_trial - 1
        trial2 <- par[4] + first_rot_trial - 1
        sd1    <- abs(par[5])
        sd2    <- abs(par[6])
        
        # Enforce constraints
        if(mean2 < min_step1) return(1e6)
        if((mean3 - mean2) < min_step2) return(1e6)
        if(trial2 <= trial1) return(1e6)  # step 2 must come after step 1
        
        pred <- ifelse(trials < trial1, 0,
                       ifelse(trials < trial2, mean2, mean3))
        residuals <- aim - pred
        
        extreme_mask <- !is.na(residuals) & (abs(residuals) > 2 * max(rotation_vals, na.rm=TRUE))
        residuals[extreme_mask] <- residuals[extreme_mask] / noise_sd
        
        ll <- ifelse(trials < trial1,
                     dnorm(residuals, mean=0, sd=sd1, log=TRUE),
                     dnorm(residuals, mean=0, sd=sd2, log=TRUE))
        return(-sum(ll, na.rm=TRUE))
      }
      
      init_list <- list(
        c(mean2=10, mean3=20, trial1=3, trial2=10, sd1=3, sd2=5),
        c(mean2=15, mean3=30, trial1=5, trial2=15, sd1=3, sd2=5),
        c(mean2=20, mean3=40, trial1=8, trial2=20, sd1=3, sd2=5)
      )
      
      fits <- lapply(init_list, function(init) {
        optim(init, neg_log_likelihood, method="L-BFGS-B",
              lower=c(min_step1, min_step1+min_step2, 1, 2, 1, 1),
              upper=c(max(rotation_vals, na.rm=TRUE)*2, max(rotation_vals, na.rm=TRUE)*2,
                      length(trials), length(trials), 10, 10))
      })
      
      best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
      names(best_fit$par) <- c("mean2", "mean3", "trial1", "trial2", "sd1", "sd2")
      return(list(mse=best_fit$value, params=best_fit$par, k=6))
    }
    
    ################## exponential model 
    fit_exponential_model <- function(trials, aim) {
      
  
      exp_func <- function(time, A, tau) {
        A * (1 - exp(-time / tau))
      }
      
      neg_log_likelihood <- function(par) {
        A   <- par[1]
        tau <- abs(par[2])        
        sd  <- abs(par[3])        
        
        pred <- exp_func(trials, A, tau)
        residuals <- aim - pred
        
        -sum(dnorm(residuals, mean=0, sd=sd, log=TRUE), na.rm=TRUE)
      }
      
      init_list <- list(
        c(20,  5, 5),
        c(40, 10, 5),
        c(60, 20, 5)
      )
      
      fits <- lapply(init_list, function(init) {
        optim(init, neg_log_likelihood, method="L-BFGS-B",
              lower=c(0, 0.01, 0.1),
              upper=c(200, 100, 20))
      })
      
      best_fit <- fits[[which.min(sapply(fits, function(f) f$value))]]
      
      return(list(
        mse = best_fit$value,   # mse = NLL here for  AIC
        params = best_fit$par,
        k = 3                   
      ))
    }
    
    
    ################## AIC 
    compute_aic <- function(mse, k, n) {
      Reach::AIC(mse, k, n)
    }
    
    
    step1_fit <- fit_onestep_model(trials, aim, first_rot_trial)
    step2_fit <- fit_twostep_model(trials, aim, first_rot_trial)
    exp_fit   <- fit_exponential_model(trials, aim)
    
    n_trials <- length(trials)
    
    
    step1_aic <- compute_aic(step1_fit$mse, step1_fit$k, n_trials)
    step2_aic <- compute_aic(step2_fit$mse, step2_fit$k, n_trials)
    exp_aic   <- compute_aic(exp_fit$mse, exp_fit$k, n_trials)
    
    best_model <- c("exponential", "one-step", "two-step")[which.min(c(exp_aic, step1_aic, step2_aic))]
    
    
    rotation_onset <- if (group == 'Group 1') 89 else if (group == 'Group 2') 113 else stop("Unknown group!")
    
    
    if (best_model %in% c("one-step", "two-step")) {
      step1_trial_val  <- round(step1_fit$params["trial1"])   # relative to onset
      step1_cut_trial  <- rotation_onset + step1_trial_val - 1
      step1_size       <- step1_fit$params["mean2"]
      step1_aim        <- dat$reachdeviation_deg[dat$cutrial_no == step1_cut_trial]
    } else {
      step1_cut_trial <- NA
      step1_size <- NA
      step1_aim <- NA
    }
    
    if (best_model == "two-step") {
      step2_trial_val  <- round(step2_fit$params["trial2"])
      step2_cut_trial  <- rotation_onset + step2_trial_val - 1
      step2_size       <- step2_fit$params["mean3"]
      step2_aim        <- dat$reachdeviation_deg[dat$cutrial_no == step2_cut_trial]
    } else {
      step2_cut_trial <- NA
      step2_size <- NA
      step2_aim <- NA
    }
    
    
    results <- rbind(results, data.frame(
      participant = pid,
      group = group,
      exp_aic = exp_aic,
      step1_aic = step1_aic,
      step2_aic = step2_aic,
      best_model = best_model
    ))
    

    summary_overall <- as.data.frame(table(results$best_model))
    colnames(summary_overall) <- c("best_model", "count")
    
  }
  return(list(
    results = results,
    summary_overall = summary_overall
  ))
}
