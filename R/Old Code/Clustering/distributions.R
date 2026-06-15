


plotDistributions <- function(df, columns=NULL) {
  if (is.null(columns)) {
    columns <- names(df)
  }
  
  colrow <- ceiling(sqrt(length(columns)))
  
  layout(matrix(1:(colrow^2), nrow=colrow, ncol=colrow))
  
  for (col in columns) {
    hist(df[[col]], breaks=20, main=col, xlab="", ylab="", )
    
  }

}


normalizeVariables <- function(df, columns=NULL) {
  if (is.null(columns)) {
    columns <- names(df)
  }
  
  # for (col in columns) {
  #   df[[col]] <- df[[col]] - mean(df[[col]], na.rm=TRUE)) / sd(df[[col]], na.rm=TRUE))
  # }
  for (col in columns) {
    df[[col]] <- df[[col]]^normalize(df[[col]])
    df[[col]] <- (df[[col]] - mean(df[[col]], na.rm=TRUE)) / sd(df[[col]], na.rm=TRUE)
  }
  
  return(df)
}


normalize <- function(X) {
  
  optimx::optimx(par=c(1),
                 fn=getW,
                 method="Nelder-Mead",
                 X=X)$p1
  
}

getW <- function(par, X) {
  
  1-shapiro.test(X^par)$statistic
  
}


showNormalization <- function() {
  
  df <- read.csv('~/Desktop/participant_features.csv')
  
  vars <- c(
    "learning_sd", 
    "learning_length", 
    "learning_abs_diff", 
    "num_sign_flips_prop", 
    "jump_ratio",
    "largest_jump_frac",
    "lin_r2", 
    "smoothness",
    "onset_trial",
    "stable_trial"
    )
  
  plot.new()
  
  plotDistributions(df, vars)
  
  dfn <- normalizeVariables(df, vars)
  
  plot.new()
  
  plotDistributions(dfn, vars)
  
}

