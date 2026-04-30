
# R helper function data preparation 1
lag_fun <- function(x, lag) {
  
  if (lag >= 1) {
    c(rep(NA, lag), x[-seq(length(x), length(x)-lag+1)])
  } else {
    c(x[-seq(1, -lag)], rep(NA, -lag))
  }
  
}

# function for poos for observed variables and unobserved factor models
POOS_function <- function(date.df, data.01, data.02, h, fre,
                               all.per.sta, all.per.end,
                               est.per.sta, est.per.end,
                               pre.per.sta, pre.per.end,
                               print = FALSE, log.file = NULL) {
  
  # 1) Prepare data ----
  
  # variable names
  data.01.nam <- colnames(data.01)
  data.02.nam <- colnames(data.02)
  
  # collect all data
  data <- cbind(data.01, data.02)
  tmp <- c(data.01.nam, data.02.nam)
  ii <- which(colnames(data) %in% tmp)
  data <- data[,ii]
    
  # 2) Prepare periods and POOS-analysis-index-s (see S&W, 2020, p. 575) ----
  
  # complete period
  all.per <- seq.Date(from = all.per.sta, to = all.per.end, by = fre)
  # estimation period
  est.per <- seq.Date(from = est.per.sta, to = est.per.end, by = fre)
  # prediction period
  pre.per <- seq.Date(from = pre.per.sta, to = pre.per.end, by = fre)
  
  # starting index for estimation/prediction
  est.sta <- which(all.per %in% est.per.sta)
  pre.sta <- which(all.per %in% pre.per.sta)
  
  # starting index for s
  s.ii.00 <- which(all.per %in% pre.per.sta)
  s.ii.00.h <- s.ii.00 - h
  
  # ending index of s
  s.ii.TT <- which(all.per %in% pre.per.end)
  s.ii.TT.h <- s.ii.TT - h
  
  s.ii.seq.h <- seq(s.ii.00.h, s.ii.TT.h)
  
  pre.sta <- pre.sta - 1
  
  # 3) POOS-analysis ----
  
  y.hat <- matrix(NA, nrow = length(s.ii.seq.h))
  y.act <- matrix(NA, nrow = length(s.ii.seq.h))
  u.til <- matrix(NA, nrow = length(s.ii.seq.h))
  
  for (tt in 1:length(s.ii.seq.h)) {
    
    # moving POOS-analysis-index-s
    s.ii <- s.ii.seq.h[tt]
    
    # extract data for estimation
    est.dat.tmp <- data[which(date.df$date %in% seq.Date(from = all.per[est.sta], to = all.per[s.ii], by = fre)),]
    
    # construct formula
    formula <- as.formula(paste(paste0(data.01.nam, " ~ "),
                                paste(data.02.nam, collapse = " + "), " - 1"))
    
    # estimate model
    coef.tmp <- lm(formula = formula, data = est.dat.tmp)$coefficients
    
    # extract data for prediction Y
    y.pre.dat.tmp <- data[which(date.df$date == all.per[pre.sta + tt]), data.01.nam]
    y.act[tt] <- as.numeric(y.pre.dat.tmp)
    
    # extract data for prediction X
    X.pre.dat.tmp <- data[which(date.df$date == all.per[pre.sta + tt]), data.02.nam]
    
    # predict y
    y.hat[tt] <- as.numeric(matrix(coef.tmp, nrow = 1)) %*% as.numeric(matrix(X.pre.dat.tmp, ncol = 1))
    # evaluate prediction
    u.til[tt] <- y.act[tt] - y.hat[tt]
    
    if (print == TRUE) {
      if (!is.null(log.file)) {
        sink(log.file, append = TRUE)
      }
      
      # print for diagnostics
      cat("--------------------------------------------------", "\n")
      cat(paste0("Step ", tt, " from ", length(s.ii.seq.h), "\n"))
      cat(paste0("   Estimation: From ", as.Date(all.per[est.sta]), " to ", as.Date(all.per[s.ii]), "\n"))
      cat(paste0("   Prediction: For ", as.Date(all.per[pre.sta + tt]), "\n"))
      
      cat(paste0("  Y: ", y.pre.dat.tmp, "\n"))
      cat(paste0("  b: ", coef.tmp, "\n"))
      cat(paste0("  b (name): ", names(coef.tmp), "\n"))
      cat(paste0("  X: ", X.pre.dat.tmp, "\n"))
      cat(paste0("  Y (hat): ", y.hat[tt], "\n"))
      cat(paste0("  u (til): ", u.til[tt], "\n"))
      
      if (!is.null(log.file)) {
        sink()  # return output to console
      }
    }    
    
  }
  
  # 4) POOS-analysis-results ----
  
  MSFE.POOS <- 1/length(u.til[-seq(1,h),]) * sum(u.til[-seq(1,h),]^2)
  
  RMSFE.POOS <- sqrt(MSFE.POOS)
  
  # returns
  ret.lis <- list(MSFE.POOS = MSFE.POOS,
                  RMSFE.POOS = RMSFE.POOS,
                  data = data,
                  y.act = y.act,
                  y.hat = y.hat,
                  u.til = u.til)
  
  return(ret.lis)
  
}