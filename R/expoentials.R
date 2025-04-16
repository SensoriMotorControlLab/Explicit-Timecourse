
exponentialModel <- function(par, timepoints) {
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  
  output = par['N0'] -( par['N0'] * (1-par['lambda'])^timepoints )
  
  
  return(data.frame(trial=timepoints,
                    output=output))
  
}


exponentialMSE <- function(par, signal, timepoints=c(0:(length(signal)-1)) ) {
  
  MSE <- mean((exponentialModel(par, timepoints)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

gridpoints=10
gridfits=10
asymptoteRange=NULL

#there are files within aim60_data that have different col names. so the rbind function wont work
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

exponentialFit <- function(aim60_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
 
  
  subdf60_reach <- NULL  
  
  for (file_index in 1:length(aim60_data_fixed)) {
    df <- aim60_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID

    
    if (is.null(subdf60_reach)) {
      subdf60_reach <- df
    } else {
      subdf60_reach <- rbind(subdf60_reach, df)
    }
    subdf60_reach <- subdf60_reach[subdf60_reach$reachdeviation_deg >= 0 & subdf60_reach$reachdeviation_deg <= 60, ]
    
  }
  
# remove outliers
 
  # Average reach deviation per trial number across participants
  agdf60_reach <- aggregate(reachdeviation_deg ~ cutrial_no, data=subdf60_reach, FUN=mean, na.rm=TRUE)
  signal <-  agdf60_reach$reachdeviation_deg
  signal <- signal[signal >= 0 & signal <= 60]
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials <- function() {
  
  exp_par60_reach <- exponentialFit(aim60_data_fixed)
  print(exp_par60_reach)  # Ensure exp_par is properly printed and accessible
  
  N060_reach <- exp_par60_reach['N0']
  lambda60_reach <- exp_par60_reach['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf60_reach$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values60_reach <- N060_reach - (N060_reach * (1 - lambda60_reach)^timepoints)
  
  # Main plot setup
 
  
   plot(agdf60_reach$cutrial_no, agdf60_reach$reachdeviation_deg, type = "n", col = "cornflowerblue", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Reach Deviation",
       xlim = range(agdf$cutrial_no), ylim = c(0,60))
   
   CI <- function(subdf60_reach) {
     aggregate(reachdeviation_deg ~ cutrial_no, 
               data = subdf60_reach, 
               FUN = function(x) Reach::getConfidenceInterval(x))
   }
   
   conf_subdf60_reach <- CI(subdf60_reach)
   
   
   lo <- conf_subdf60_reach$reachdeviation_deg[, 1]
   hi <- conf_subdf60_reach$reachdeviation_deg[, 2]
   
   # Plot the polygon representing the confidence intervals
  # polygon(x = c(conf_subdf$cutrial_no, rev(conf_subdf$cutrial_no)),
        #   y = c(lo, rev(hi)),
        #   border = NA,
         #  col = rgb(0.678, 0.847, 0.902, 0.2))
  
  # Calculate confidence intervals for exponential fit
  lambdaCI <- quantile(lambda, c(0.025, 0.975))
  N0CI     <- quantile(N0, c(0.025, 0.975))
  
  # Calculate the fitted curves for both lower and upper CI
  N0_lower <- unname(N0CI[1])
  lambda_lower <- unname(lambdaCI[1])
  par_lower <- c('N0' = N0_lower, 'lambda' = lambda_lower)
  fittedcurve_lower <- exponentialModel(par = par_lower, timepoints = timepoints)
  
  N0_upper <- unname(N0CI[2])
  lambda_upper <- unname(lambdaCI[2])
  par_upper <- c('N0' = N0_upper, 'lambda' = lambda_upper)
  fittedcurve_upper <- exponentialModel(par = par_upper, timepoints = timepoints)
  
  fittedcurve <- exponentialModel(par=exp_par,
                                  timepoints=timepoints)
  
  X <- c(X , rev(timepoints)+1)
  Y <- c(Y , rev(fittedcurve$output))
  
  lines(x=timepoints+1,
        y= fitted_values60_reach, col='navy')
  
  polygon( x = X,
           y = Y,
           border=NA,
           col = rgb(0.678, 0.847, 0.902, 0.1))
  
  # Add the fitted line on top of the polygon
  #lines(timepoints, fitted_values, col = "black", lwd = 2, lty = 1)
 
}




#aim

exponentialFit2 <- function(aim60_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf <- NULL  
  
  for (file_index in 1:length(aim60_data_fixed)) {
    df <- aim60_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf)) {
      subdf <- df
    } else {
      subdf <- rbind(subdf, df)
    }
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf2 <- aggregate(aimdeviation_deg ~ cutrial_no, data=subdf, FUN=mean, na.rm=TRUE)
  signal <- agdf2$aimdeviation_deg
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials2 <- function() {
  
  exp_par2 <- exponentialFit2(aim60_data_fixed)
  print(exp_par2)  # Ensure exp_par is properly printed and accessible
  
  N02 <- exp_par2['N0']
  lambda2 <- exp_par2['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf2$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values2 <- N02 - (N02 * (1 - lambda2)^timepoints)
  
  # Main plot setup
  plot(agdf2$cutrial_no, agdf2$aimdeviation_deg, type = "n", col = "deeppink", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Aim Deviation",
       xlim = range(agdf2$cutrial_no), ylim = c(0,60))
  
  
  fittedcurve2 <- exponentialModel(par=exp_par2,
                                  timepoints=timepoints)
  
  X2 <- c(X , rev(timepoints)+1)
  Y2 <- c(Y , rev(fittedcurve2$output))
  
  
  polygon( x = X2,
           y = Y2,
           border=NA,
           col = rgb(255/255, 105/255, 180/255, 0.2))
  
  # Add the fitted line on top of the polygon
  lines(timepoints, fitted_values2, col = "black", lwd = 2, lty = 1)
  #lines(x=timepoints+1,
      #  y=fittedcurve$output, col='deeppink')
}

# Fit exponential models
#fit40 <- fit_exponential(all_data40$Group.1, all_data40$x)
#fit50 <- fit_exponential(all_data50$Group.1, all_data50$x)
#fit60 <- fit_exponential(all_data60$Group.1, all_data60$x)

# Predict fitted values
#y_fit40 <- predict(fit40, list(x = all_data40$Group.1))
#y_fit50 <- predict(fit50, list(x = all_data50$Group.1))
#y_fit60 <- predict(fit60, list(x = all_data60$Group.1))



# Add fitted curves
#lines(all_data40$Group.1, y_fit40, col="black", lty=1, lwd=2)
#lines(all_data50$Group.1, y_fit50, col="black", lty=2, lwd=2)
#lines(all_data60$Group.1, y_fit60, col= "black", lty=3, lwd=2)


#group5 <- c(group1_data,group1_rotated, group1_after, group2_data, 
           # group2_rotated, group2_after)




#50 DEGREES



gridpoints=10
gridfits=10
asymptoteRange=NULL

#there are files within aim60_data that have different col names. so the rbind function wont work
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

exponentialFit50_reach <- function(aim50_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf50_reach <- NULL  
  
  for (file_index in 1:length(aim50_data_fixed)) {
    df <- aim50_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf50_reach)) {
      subdf50_reach <- df
    } else {
      subdf50_reach <- rbind(subdf50_reach, df)
    }
    subdf50_reach <- subdf50_reach[subdf50_reach$reachdeviation_deg >= 0 & subdf50_reach$reachdeviation_deg <= 50, ]
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf50_reach <- aggregate(reachdeviation_deg ~ cutrial_no, data=subdf50_reach, FUN=mean, na.rm=TRUE)
  signal <- agdf50_reach$reachdeviation_deg
  signal <- signal[signal >= 0 & signal <= 50]
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials50_reach <- function() {
  
  exp_par50_reach <- exponentialFit(aim50_data_fixed)
  print(exp_par50_reach)  # Ensure exp_par is properly printed and accessible
  
  N050_reach <- exp_par50_reach['N0']
  lambda50_reach <- exp_par50_reach['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values50_reach <- N050_reach - (N050_reach * (1 - lambda50_reach)^timepoints)
  
  # Main plot setup
  
  
  plot(agdf$cutrial_no, agdf$reachdeviation_deg, type = "n", col = "cornflowerblue", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Reach Deviation",
       xlim = range(agdf$cutrial_no), ylim = c(0,50))
  
  CI <- function(subdf50_reach) {
    aggregate(reachdeviation_deg ~ cutrial_no, 
              data = subdf, 
              FUN = function(x) Reach::getConfidenceInterval(x))
  }
  
  conf_subdf50_reach <- CI(subdf50_reach)
  
  
  lo <- conf_subdf50_reach$reachdeviation_deg[, 1]
  hi <- conf_subdf50_reach$reachdeviation_deg[, 2]
  
  # Plot the polygon representing the confidence intervals
  # polygon(x = c(conf_subdf$cutrial_no, rev(conf_subdf$cutrial_no)),
  #   y = c(lo, rev(hi)),
  #   border = NA,
  #  col = rgb(0.678, 0.847, 0.902, 0.2))
  
  # Calculate confidence intervals for exponential fit
  lambdaCI <- quantile(lambda, c(0.025, 0.975))
  N0CI     <- quantile(N0, c(0.025, 0.975))
  
  # Calculate the fitted curves for both lower and upper CI
  N0_lower <- unname(N0CI[1])
  lambda_lower <- unname(lambdaCI[1])
  par_lower <- c('N0' = N0_lower, 'lambda' = lambda_lower)
  fittedcurve_lower <- exponentialModel(par = par_lower, timepoints = timepoints)
  
  N0_upper <- unname(N0CI[2])
  lambda_upper <- unname(lambdaCI[2])
  par_upper <- c('N0' = N0_upper, 'lambda' = lambda_upper)
  fittedcurve_upper <- exponentialModel(par = par_upper, timepoints = timepoints)
  
  fittedcurve50_reach <- exponentialModel(par=exp_par50_reach,
                                  timepoints=timepoints)
  
  X50_reach <- c(X , rev(timepoints)+1)
  Y50_reach <- c(Y , rev(fittedcurve50_reach$output))
  
  lines(x=timepoints+1,
        y= fitted_values50_reach, col='navy')
  
  polygon( x = X50_reach,
           y = Y50_reach,
           border=NA,
           col = rgb(0.678, 0.847, 0.902, 0.1))
  
  # Add the fitted line on top of the polygon
  #lines(timepoints, fitted_values, col = "black", lwd = 2, lty = 1)
  
}




#aim

exponentialFit50_aim <- function(aim50_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf <- NULL  
  
  for (file_index in 1:length(aim50_data_fixed)) {
    df <- aim60_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf)) {
      subdf <- df
    } else {
      subdf <- rbind(subdf, df)
    }
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf2 <- aggregate(aimdeviation_deg ~ cutrial_no, data=subdf, FUN=mean, na.rm=TRUE)
  signal <- agdf2$aimdeviation_deg
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials50_aim <- function() {
  
  exp_par2 <- exponentialFit2(aim50_data_fixed)
  print(exp_par2)  # Ensure exp_par is properly printed and accessible
  
  N02 <- exp_par2['N0']
  lambda2 <- exp_par2['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf2$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values2 <- N02 - (N02 * (1 - lambda2)^timepoints)
  
  # Main plot setup
  plot(agdf2$cutrial_no, agdf2$aimdeviation_deg, type = "n", col = "deeppink", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Aim Deviation",
       xlim = range(agdf2$cutrial_no), ylim = c(0,50))
  
  
  fittedcurve2 <- exponentialModel(par=exp_par2,
                                   timepoints=timepoints)
  
  X2 <- c(X , rev(timepoints)+1)
  Y2 <- c(Y , rev(fittedcurve2$output))
  
  
  polygon( x = X2,
           y = Y2,
           border=NA,
           col = rgb(255/255, 105/255, 180/255, 0.2))
  
  # Add the fitted line on top of the polygon
  lines(timepoints, fitted_values2, col = "black", lwd = 2, lty = 1)
  #lines(x=timepoints+1,
  #  y=fittedcurve$output, col='deeppink')
}


#40 DEGREE

gridpoints=10
gridfits=10
asymptoteRange=NULL

#there are files within aim60_data that have different col names. so the rbind function wont work
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

exponentialFit40_reach <- function(aim40_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf40_reach <- NULL  
  
  for (file_index in 1:length(aim40_data_fixed)) {
    df <- aim40_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf40_reach)) {
      subdf40_reach <- df
    } else {
      subdf40_reach <- rbind(subdf40_reach, df)
    }
    subdf40_reach <- subdf40_reach[subdf40_reach$reachdeviation_deg >= 0 & subdf40_reach$reachdeviation_deg <= 40, ]
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf40_reach <- aggregate(reachdeviation_deg ~ cutrial_no, data=subdf40_reach, FUN=mean, na.rm=TRUE)
  signal <- agdf40_reach$reachdeviation_deg
  signal <- signal[signal >= 0 & signal <= 40]
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials40_reach <- function() {
  
  exp_par40_reach <- exponentialFit(aim40_data_fixed)
  print(exp_par40_reach)  # Ensure exp_par is properly printed and accessible
  
  N040_reach <- exp_par40_reach['N0']
  lambda40_reach <- exp_par40_reach['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf40_reach$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values40_reach <- N040_reach - (N040_reach * (1 - lambda40_reach)^timepoints)
  
  # Main plot setup
  
  
  plot(agdf40_reach$cutrial_no, agdf40_reach$reachdeviation_deg, type = "n", col = "cornflowerblue", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Reach Deviation",
       xlim = range(agdf40_reach$cutrial_no), ylim = c(0,40))
  
  CI <- function(subdf40_reach) {
    aggregate(reachdeviation_deg ~ cutrial_no, 
              data = subdf40_reach, 
              FUN = function(x) Reach::getConfidenceInterval(x))
  }
  
  conf_subdf40_reach <- CI(subdf40_reach)
  
  
  lo <- conf_subdf$reachdeviation_deg[, 1]
  hi <- conf_subdf$reachdeviation_deg[, 2]
  
  # Plot the polygon representing the confidence intervals
  # polygon(x = c(conf_subdf$cutrial_no, rev(conf_subdf$cutrial_no)),
  #   y = c(lo, rev(hi)),
  #   border = NA,
  #  col = rgb(0.678, 0.847, 0.902, 0.2))
  
  # Calculate confidence intervals for exponential fit
  lambdaCI <- quantile(lambda, c(0.025, 0.975))
  N0CI     <- quantile(N0, c(0.025, 0.975))
  
  # Calculate the fitted curves for both lower and upper CI
  N0_lower <- unname(N0CI[1])
  lambda_lower <- unname(lambdaCI[1])
  par_lower <- c('N0' = N0_lower, 'lambda' = lambda_lower)
  fittedcurve_lower <- exponentialModel(par = par_lower, timepoints = timepoints)
  
  N0_upper <- unname(N0CI[2])
  lambda_upper <- unname(lambdaCI[2])
  par_upper <- c('N0' = N0_upper, 'lambda' = lambda_upper)
  fittedcurve_upper <- exponentialModel(par = par_upper, timepoints = timepoints)
  
  fittedcurve40_reach <- exponentialModel(par=exp_par40_reach,
                                  timepoints=timepoints)
  
  X40_reach <- c(X , rev(timepoints)+1)
  Y40_reach <- c(Y , rev(fittedcurve40_reach$output))
  
  lines(x=timepoints+1,
        y= fitted_values40_reach, col='navy')
  
  polygon( x = X40_reach,
           y = Y40_reach,
           border=NA,
           col = rgb(0.678, 0.847, 0.902, 0.1))
  
  # Add the fitted line on top of the polygon
  #lines(timepoints, fitted_values, col = "black", lwd = 2, lty = 1)
  
}




#aim

exponentialFit50_aim <- function(aim40_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf <- NULL  
  
  for (file_index in 1:length(aim40_data_fixed)) {
    df <- aim40_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf)) {
      subdf <- df
    } else {
      subdf <- rbind(subdf, df)
    }
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf2 <- aggregate(aimdeviation_deg ~ cutrial_no, data=subdf, FUN=mean, na.rm=TRUE)
  signal <- agdf2$aimdeviation_deg
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials40_aim <- function() {
  
  exp_par40_aim <- exponentialFit2(aim40_data_fixed)
  print(exp_par40_aim)  # Ensure exp_par is properly printed and accessible
  
  N02 <- exp_par40_aim['N0']
  lambda2 <- exp_par40_aim['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf40_aim$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values40_aim <- N040_aim - (N040_aim * (1 - lambda40_aim)^timepoints)
  
  # Main plot setup
  plot(agdf40_aim$cutrial_no, agdf40_aim$aimdeviation_deg, type = "n", col = "deeppink", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Aim Deviation",
       xlim = range(agdf2$cutrial_no), ylim = c(0,40))
  
  
  fittedcurve40_aim <- exponentialModel(par=exp_par40_aim,
                                   timepoints=timepoints)
  
  X2 <- c(X , rev(timepoints)+1)
  Y2 <- c(Y , rev(fittedcurve40_aim$output))
  
  
  polygon( x = X2,
           y = Y2,
           border=NA,
           col = rgb(255/255, 105/255, 180/255, 0.2))
  
  # Add the fitted line on top of the polygon
  lines(timepoints, fitted_values40_aim, col = "black", lwd = 2, lty = 1)
  #lines(x=timepoints+1,
  #  y=fittedcurve$output, col='deeppink')
}


####30 DEGREE

gridpoints=10
gridfits=10
asymptoteRange=NULL

#there are files within aim60_data that have different col names. so the rbind function wont work
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

exponentialFit30_reach <- function(aim30_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf30_reach <- NULL  
  
  for (file_index in 1:length(aim30_data_fixed)) {
    df <- aim30_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf30_reach)) {
      subdf30_reach <- df
    } else {
      subdf30_reach <- rbind(subdf30_reach, df)
    }
    subdf30_reach <- subdf30_reach[subdf30_reach$reachdeviation_deg >= 0 & subdf30_reach$reachdeviation_deg <= 30, ]
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf30_reach <- aggregate(reachdeviation_deg ~ cutrial_no, data=subdf30_reach, FUN=mean, na.rm=TRUE)
  signal <- agdf30_reach$reachdeviation_deg
  signal <- signal[signal >= 0 & signal <= 30]
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials30_reach <- function() {
  
  exp_par30_reach <- exponentialFit(aim30_data_fixed)
  print(exp_par30_reach)  # Ensure exp_par is properly printed and accessible
  
  N030_reach <- exp_par30_reach['N0']
  lambda30_reach <- exp_par30_reach['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf30_reach$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values30_reach <- N030_reach - (N030_reach * (1 - lambda30_reach)^timepoints)
  
  # Main plot setup
  
  
  plot(agdf30_reach$cutrial_no, agdf30_reach$reachdeviation_deg, type = "n", col = "cornflowerblue", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Reach Deviation",
       xlim = range(agdf30_reach$cutrial_no), ylim = c(0,30))
  
  CI <- function(subdf30_reach) {
    aggregate(reachdeviation_deg ~ cutrial_no, 
              data = subdf30_reach, 
              FUN = function(x) Reach::getConfidenceInterval(x))
  }
  
  conf_subdf30_reach <- CI(subdf30_reach)
  
  
  lo <- conf_subdf$reachdeviation_deg[, 1]
  hi <- conf_subdf$reachdeviation_deg[, 2]
  
  # Plot the polygon representing the confidence intervals
  # polygon(x = c(conf_subdf$cutrial_no, rev(conf_subdf$cutrial_no)),
  #   y = c(lo, rev(hi)),
  #   border = NA,
  #  col = rgb(0.678, 0.847, 0.902, 0.2))
  
  # Calculate confidence intervals for exponential fit
  lambdaCI <- quantile(lambda, c(0.025, 0.975))
  N0CI     <- quantile(N0, c(0.025, 0.975))
  
  # Calculate the fitted curves for both lower and upper CI
  N0_lower <- unname(N0CI[1])
  lambda_lower <- unname(lambdaCI[1])
  par_lower <- c('N0' = N0_lower, 'lambda' = lambda_lower)
  fittedcurve_lower <- exponentialModel(par = par_lower, timepoints = timepoints)
  
  N0_upper <- unname(N0CI[2])
  lambda_upper <- unname(lambdaCI[2])
  par_upper <- c('N0' = N0_upper, 'lambda' = lambda_upper)
  fittedcurve_upper <- exponentialModel(par = par_upper, timepoints = timepoints)
  
  fittedcurve30_reach <- exponentialModel(par=exp_par30_reach,
                                          timepoints=timepoints)
  
  X30_reach <- c(X , rev(timepoints)+1)
  Y30_reach <- c(Y , rev(fittedcurve30_reach$output))
  
  lines(x=timepoints+1,
        y= fitted_values30_reach, col='navy')
  
  polygon( x = X30_reach,
           y = Y30_reach,
           border=NA,
           col = rgb(0.678, 0.847, 0.902, 0.1))
  
  # Add the fitted line on top of the polygon
  #lines(timepoints, fitted_values, col = "black", lwd = 2, lty = 1)
  
}




#aim

exponentialFit50_aim <- function(aim30_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf30_aim <- NULL  
  
  for (file_index in 1:length(aim30_data_fixed)) {
    df30_aim <- aim30_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf)) {
      subdf30_aim <- df
    } else {
      subdf30_aim <- rbind(subdf30_aim, df)
    }
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf30_aim <- aggregate(aimdeviation_deg ~ cutrial_no, data=subdf30_aim, FUN=mean, na.rm=TRUE)
  signal <- agdf30_aim$aimdeviation_deg
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials30_aim <- function() {
  
  exp_par30_aim <- exponentialFit2(aim30_data_fixed)
  print(exp_par30_aim)  # Ensure exp_par is properly printed and accessible
  
  N030_aim <- exp_par30_aim['N0']
  lambda30_aim <- exp_par30_aim['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf30_aim$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values30_aim <- N02 - (N02 * (1 - lambda2)^timepoints)
  
  # Main plot setup
  plot(agdf30_aim$cutrial_no, agdf30_aim$aimdeviation_deg, type = "n", col = "deeppink", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Aim Deviation",
       xlim = range(agdf2$cutrial_no), ylim = c(0,30))
  
  
  fittedcurve30_aim <- exponentialModel(par=exp_par30_aim,
                                        timepoints=timepoints)
  
  X2 <- c(X , rev(timepoints)+1)
  Y2 <- c(Y , rev(fittedcurve30_aim2$output))
  
  
  polygon( x = X2,
           y = Y2,
           border=NA,
           col = rgb(255/255, 105/255, 180/255, 0.2))
  
  # Add the fitted line on top of the polygon
  lines(timepoints, fitted_values2, col = "black", lwd = 2, lty = 1)
  #lines(x=timepoints+1,
  #  y=fittedcurve$output, col='deeppink')
}




####20 DEGREE

gridpoints=10
gridfits=10
asymptoteRange=NULL

#there are files within aim60_data that have different col names. so the rbind function wont work
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

exponentialFit20_reach <- function(aim20_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf20_reach <- NULL  
  
  for (file_index in 1:length(aim20_data_fixed)) {
    df <- aim20_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf20_reach)) {
      subdf20_reach <- df
    } else {
      subdf20_reach <- rbind(subdf20_reach, df)
    }
    subdf20_reach <- subdf20_reach[subdf20_reach$reachdeviation_deg >= 0 & subdf20_reach$reachdeviation_deg <= 20, ]
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf20_reach <- aggregate(reachdeviation_deg ~ cutrial_no, data=subdf20_reach, FUN=mean, na.rm=TRUE)
  signal <- agdf20_reach$reachdeviation_deg
  signal <- signal[signal >= 0 & signal <= 20]
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials20_reach <- function() {
  
  exp_par20_reach <- exponentialFit(aim20_data_fixed)
  print(exp_par20_reach)  # Ensure exp_par is properly printed and accessible
  
  N020_reach <- exp_par20_reach['N0']
  lambda20_reach <- exp_par20_reach['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf20_reach$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values20_reach <- N020_reach - (N020_reach * (1 - lambda20_reach)^timepoints)
  
  # Main plot setup
  
  
  plot(agdf20_reach$cutrial_no, agdf20_reach$reachdeviation_deg, type = "n", col = "cornflowerblue", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Reach Deviation",
       xlim = range(agdf20_reach$cutrial_no), ylim = c(0,20))
  
  CI <- function(subdf20_reach) {
    aggregate(reachdeviation_deg ~ cutrial_no, 
              data = subdf20_reach, 
              FUN = function(x) Reach::getConfidenceInterval(x))
  }
  
  conf_subdf20_reach <- CI(subdf20_reach)
  
  
  lo <- conf_subdf$reachdeviation_deg[, 1]
  hi <- conf_subdf$reachdeviation_deg[, 2]
  
  # Plot the polygon representing the confidence intervals
  # polygon(x = c(conf_subdf$cutrial_no, rev(conf_subdf$cutrial_no)),
  #   y = c(lo, rev(hi)),
  #   border = NA,
  #  col = rgb(0.678, 0.847, 0.902, 0.2))
  
  # Calculate confidence intervals for exponential fit
  lambdaCI <- quantile(lambda, c(0.025, 0.975))
  N0CI     <- quantile(N0, c(0.025, 0.975))
  
  # Calculate the fitted curves for both lower and upper CI
  N0_lower <- unname(N0CI[1])
  lambda_lower <- unname(lambdaCI[1])
  par_lower <- c('N0' = N0_lower, 'lambda' = lambda_lower)
  fittedcurve_lower <- exponentialModel(par = par_lower, timepoints = timepoints)
  
  N0_upper <- unname(N0CI[2])
  lambda_upper <- unname(lambdaCI[2])
  par_upper <- c('N0' = N0_upper, 'lambda' = lambda_upper)
  fittedcurve_upper <- exponentialModel(par = par_upper, timepoints = timepoints)
  
  fittedcurve20_reach <- exponentialModel(par=exp_par20_reach,
                                          timepoints=timepoints)
  
  X20_reach <- c(X , rev(timepoints)+1)
  Y20_reach <- c(Y , rev(fittedcurve20_reach$output))
  
  lines(x=timepoints+1,
        y= fitted_values20_reach, col='navy')
  
  polygon( x = X20_reach,
           y = Y20_reach,
           border=NA,
           col = rgb(0.678, 0.847, 0.902, 0.1))
  
  # Add the fitted line on top of the polygon
  #lines(timepoints, fitted_values, col = "black", lwd = 2, lty = 1)
  
}




#aim

exponentialFit50_aim <- function(aim20_data_fixed, gridpoints = 10, gridfits = 10, asymptoteRange = NULL) {
  
  
  subdf20_aim <- NULL  
  
  for (file_index in 1:length(aim20_data_fixed)) {
    df20_aim <- aim20_data_fixed[[file_index]]
    df$participant_id <- file_index  # assign participant ID
    
    
    if (is.null(subdf)) {
      subdf20_aim <- df
    } else {
      subdf20_aim <- rbind(subdf20_aim, df)
    }
    
  }
  
  # remove outliers
  
  # Average reach deviation per trial number across participants
  agdf20_aim <- aggregate(aimdeviation_deg ~ cutrial_no, data=subdf20_aim, FUN=mean, na.rm=TRUE)
  signal <- agdf20_aim$aimdeviation_deg
  timepoints = length(signal)
  
  # Set the search grid for exponential fit
  parvals <- seq(1/gridpoints/2, 1-(1/gridpoints/2), 1/gridpoints)
  
  # Define asymptote range if not provided
  if (is.null(asymptoteRange)) {
    asymptoteRange <- c(-1, 2) * max(abs(signal), na.rm=TRUE)
  }
  
  searchgrid <- expand.grid('lambda' = parvals,
                            'N0' = parvals * diff(asymptoteRange) + asymptoteRange[1])
  
  # Evaluate starting positions
  MSE <- apply(searchgrid, MARGIN=1, FUN=exponentialMSE, signal=signal, timepoints=timepoints)
  
  lo <- c(0, asymptoteRange[1])
  hi <- c(1, asymptoteRange[2])
  
  # Run optimx on the best starting positions
  allfits <- do.call("rbind",
                     apply(data.frame(searchgrid[order(MSE)[1:gridfits],]),
                           MARGIN=1,
                           FUN=function(startpar) {
                             optimx::optimx(
                               par = startpar,
                               fn = exponentialMSE,
                               method = 'L-BFGS-B',
                               lower = lo,
                               upper = hi,
                               timepoints = timepoints,
                               signal = signal
                             )
                           }
                     ))
  
  # Pick the best fit
  win <- allfits[order(allfits$value)[1],]
  
  winpar <- unlist(win[1:2])
  winpar <- c('lambda' = win$lambda, 'N0' = win$N0)
  
  # Return the best parameters
  return(winpar)
}

# Main function to call exponentialFit
groupLearningExponentials20_aim <- function() {
  
  exp_par20_aim <- exponentialFit2(aim20_data_fixed)
  print(exp_par20_aim)  # Ensure exp_par is properly printed and accessible
  
  N020_aim <- exp_par20_aim['N0']
  lambda20_aim <- exp_par20_aim['lambda']
  
  # Define the timepoints based on the data
  timepoints <- agdf20_aim$cutrial_no
  
  # Calculate the exponential fitted values directly
  fitted_values20_aim <- N02 - (N02 * (1 - lambda2)^timepoints)
  
  # Main plot setup
  plot(agdf20_aim$cutrial_no, agdf20_aim$aimdeviation_deg, type = "n", col = "deeppink", lwd = 2, 
       xlab = "Trial Number", ylab = "Reach Deviation (degrees)",
       main = "Exponential Fit of Aim Deviation",
       xlim = range(agdf20_aim$cutrial_no), ylim = c(0,20))
  
  
  fittedcurve20_aim <- exponentialModel(par=exp_par20_aim,
                                        timepoints=timepoints)
  
  X2 <- c(X , rev(timepoints)+1)
  Y2 <- c(Y , rev(fittedcurve20_aim2$output))
  
  
  polygon( x = X2,
           y = Y2,
           border=NA,
           col = rgb(255/255, 105/255, 180/255, 0.2))
  
  # Add the fitted line on top of the polygon
  lines(timepoints, fitted_values2, col = "black", lwd = 2, lty = 1)
  #lines(x=timepoints+1,
  #  y=fittedcurve$output, col='deeppink')
}