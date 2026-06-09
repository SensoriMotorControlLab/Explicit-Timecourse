


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



# 1. Load the data
df <- read.csv('~/Desktop/participant_features.csv')
df2 <- read.csv("~/Desktop/standardized_features.csv")
strategy_data <- read.csv("data/strategy_only_participants.csv")

# 2. Make strategy data unique (1 row per participant)
strategy_unique <- strategy_data %>%
  group_by(participant_id) %>%
  summarize(rotation = first(rotation), .groups = "drop")

df_merged <- df2 %>%
  left_join(strategy_unique, by = "participant_id")

# df_merged <- df_merged %>%
#   mutate(rotation = rotation.y) %>%
#   select(-rotation.x, -rotation.y)

feature_names <- c(
  "learning_sd",
  "learning_length",
  "learning_abs_diff",
  "num_sign_flips_prop",
  "jump_ratio",
  "largest_jump_frac",
  "lin_r2",
  "smoothness"
)

kw_results <- data.frame()

for(f in feature_names){
  
  form <- as.formula(
    paste(f, "~ factor(rotation)")
  )
  
  test <- kruskal.test(form, data = df_merged)
  
  kw_results <- rbind(
    kw_results,
    data.frame(
      feature = f,
      H = unname(test$statistic),
      p = test$p.value
    )
  )
}

kw_results




## plot box plots per rotation ##

plotBox <- function () {
  
  ggplot(df_merged,
         aes(x = factor(rotation),
             y = smoothness )) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.6) +
    theme_classic()
  
  
}

