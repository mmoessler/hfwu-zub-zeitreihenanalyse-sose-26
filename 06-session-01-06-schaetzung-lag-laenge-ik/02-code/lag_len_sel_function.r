
lag_len_crit_fun <- function(model) {
  
  if (c("lm") %in% class(model)) {
    R2 <- summary(model)$r.squared
    p <- length(model$coef)-1
  } else if (c("summary.lm") %in% class(model)) {
    R2 <- model$r.squared
    p <- nrow(model$coef)-1
  }
  
  ssr <- sum(model$residuals^2)
  t <- length(model$residuals)
  ssr.t <- ssr/t
  log.ssr.t <- log(ssr.t)
  
  p1.ln.t.t <- (p+1)*log(t)/t
  
  BIC <- log(ssr/t) + (p+1) * log(t)/t
  AIC <- log(ssr/t) + (p+1) * 2/t
  
  ret.lis <- list(p = p, t = t,
                  ssr = round(ssr,4),
                  ssr.t = round(ssr.t,4),
                  log.ssr.t = round(log.ssr.t,4),
                  p1.ln.t.t = round(p1.ln.t.t,4),
                  R2 = round(R2,4),
                  BIC = round(BIC,4),
                  AIC = round(AIC,4))
  
  return(ret.lis)
  
}

# Transform to ts variable
# gdp.grw.ts <- ts(gdp.grw.ts.01, frequency = 4, start = c(1960, 2))
gdp.grw.ts <- window(us_macro_qua_ts[,c("GDPGR")], start = c(1960, 2), end = c(2017, 4))

# Estimation using dynlm
ar00.dynlm <- dynlm(gdp.grw.ts ~ 1,
                    start = c(1962, 1), end = c(2017, 3))
ar01.dynlm <- dynlm(gdp.grw.ts ~ L(gdp.grw.ts,1),
                    start = c(1962, 1), end = c(2017, 3))
ar02.dynlm <- dynlm(gdp.grw.ts ~ L(gdp.grw.ts,1) + L(gdp.grw.ts,2),
                    start = c(1962, 1), end = c(2017, 3))
ar03.dynlm <- dynlm(gdp.grw.ts ~ L(gdp.grw.ts,1) + L(gdp.grw.ts,2) + L(gdp.grw.ts,3),
                    start = c(1962, 1), end = c(2017, 3))
ar04.dynlm <- dynlm(gdp.grw.ts ~ L(gdp.grw.ts,1) + L(gdp.grw.ts,2) + L(gdp.grw.ts,3) + L(gdp.grw.ts,4),
                    start = c(1962, 1), end = c(2017, 3))
ar05.dynlm <- dynlm(gdp.grw.ts ~ L(gdp.grw.ts,1) + L(gdp.grw.ts,2) + L(gdp.grw.ts,3) + L(gdp.grw.ts,4) + L(gdp.grw.ts,5),
                    start = c(1962, 1), end = c(2017, 3))
ar06.dynlm <- dynlm(gdp.grw.ts ~ L(gdp.grw.ts,1) + L(gdp.grw.ts,2) + L(gdp.grw.ts,3) + L(gdp.grw.ts,4) + L(gdp.grw.ts,5) + L(gdp.grw.ts,6),
                    start = c(1962, 1), end = c(2017, 3))

# Call function for information criteria
ar00.crit <- lag_len_crit_fun(ar00.dynlm)
ar01.crit <- lag_len_crit_fun(ar01.dynlm)
ar02.crit <- lag_len_crit_fun(ar02.dynlm)
ar03.crit <- lag_len_crit_fun(ar03.dynlm)
ar04.crit <- lag_len_crit_fun(ar04.dynlm)
ar05.crit <- lag_len_crit_fun(ar05.dynlm)
ar06.crit <- lag_len_crit_fun(ar06.dynlm)

# Collect results
res.mat <- rbind(
  cbind(ar00.crit$p,ar00.crit$ssr,ar00.crit$ssr.t,ar00.crit$log.ssr.t,ar00.crit$p1.ln.t.t,ar00.crit$BIC,ar00.crit$AIC,ar00.crit$R2),
  cbind(ar01.crit$p,ar01.crit$ssr,ar01.crit$ssr.t,ar01.crit$log.ssr.t,ar01.crit$p1.ln.t.t,ar01.crit$BIC,ar01.crit$AIC,ar01.crit$R2),
  cbind(ar02.crit$p,ar02.crit$ssr,ar02.crit$ssr.t,ar02.crit$log.ssr.t,ar02.crit$p1.ln.t.t,ar02.crit$BIC,ar02.crit$AIC,ar02.crit$R2),
  cbind(ar03.crit$p,ar03.crit$ssr,ar03.crit$ssr.t,ar03.crit$log.ssr.t,ar03.crit$p1.ln.t.t,ar03.crit$BIC,ar03.crit$AIC,ar03.crit$R2),
  cbind(ar04.crit$p,ar04.crit$ssr,ar04.crit$ssr.t,ar04.crit$log.ssr.t,ar04.crit$p1.ln.t.t,ar04.crit$BIC,ar04.crit$AIC,ar04.crit$R2),
  cbind(ar05.crit$p,ar05.crit$ssr,ar05.crit$ssr.t,ar05.crit$log.ssr.t,ar05.crit$p1.ln.t.t,ar05.crit$BIC,ar05.crit$AIC,ar05.crit$R2),
  cbind(ar06.crit$p,ar06.crit$ssr,ar06.crit$ssr.t,ar06.crit$log.ssr.t,ar06.crit$p1.ln.t.t,ar06.crit$BIC,ar06.crit$AIC,ar06.crit$R2))
colnames(res.mat) <- c("p","SSR(p)","SSR(p)/T","ln(SSR(p)/T)","(p+1)ln(T)/T","BIC(p)","AIC(p)","R2")
# Show results
res.mat

tab <- rbind(
  cbind(ar00.crit$p,ar00.crit$ssr,ar00.crit$ssr.t,ar00.crit$log.ssr.t,ar00.crit$p1.ln.t.t,ar00.crit$BIC,ar00.crit$AIC,ar00.crit$R2),
  cbind(ar01.crit$p,ar01.crit$ssr,ar01.crit$ssr.t,ar01.crit$log.ssr.t,ar01.crit$p1.ln.t.t,ar01.crit$BIC,ar01.crit$AIC,ar01.crit$R2),
  cbind(ar02.crit$p,ar02.crit$ssr,ar02.crit$ssr.t,ar02.crit$log.ssr.t,ar02.crit$p1.ln.t.t,ar02.crit$BIC,ar02.crit$AIC,ar02.crit$R2),
  cbind(ar03.crit$p,ar03.crit$ssr,ar03.crit$ssr.t,ar03.crit$log.ssr.t,ar03.crit$p1.ln.t.t,ar03.crit$BIC,ar03.crit$AIC,ar03.crit$R2),
  cbind(ar04.crit$p,ar04.crit$ssr,ar04.crit$ssr.t,ar04.crit$log.ssr.t,ar04.crit$p1.ln.t.t,ar04.crit$BIC,ar04.crit$AIC,ar04.crit$R2),
  cbind(ar05.crit$p,ar05.crit$ssr,ar05.crit$ssr.t,ar05.crit$log.ssr.t,ar05.crit$p1.ln.t.t,ar05.crit$BIC,ar05.crit$AIC,ar05.crit$R2),
  cbind(ar06.crit$p,ar06.crit$ssr,ar06.crit$ssr.t,ar06.crit$log.ssr.t,ar06.crit$p1.ln.t.t,ar06.crit$BIC,ar06.crit$AIC,ar06.crit$R2))
colnames(tab) <- c("p","SSR(p)","SSR(p)/T","ln(SSR(p)/T)","(p+1)ln(T)/T","BIC(p)","AIC(p)","R2")

# tab %>%
#   kable("html",booktabs=T,escape=F,align=c("l","c","c","c","c","c","c","c"),digits = c(0,2,3,3,3,3,3,3)) %>%
#   #add_header_above(c(" ", " " = 2, "Percentile" = 7)) %>%
#   kable_styling(latex_options="hold_position") %>%
#   row_spec(0,align="c")

# ADL MODELS ----

# AR(1) Model: 1962-Q1 - 2017-Q3
ar_01_dynlm <- dynlm(GDPGR ~ L(GDPGR,1),
                     data = us_macro_qua_ts,
                     start = c(1962, 1), end = c(2017, 3))

ct_ar_01_dynlm <- coeftest(ar_01_dynlm, vcov=vcovHC(ar_01_dynlm, type="HC0"))

# AR(2) Model: 1962-Q1 - 2017-Q3
ar_02_dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2),
                     data = us_macro_qua_ts,
                     start = c(1962, 1), end = c(2017, 3))

ct_ar_02_dynlm <- coeftest(ar_02_dynlm, vcov=vcovHC(ar_02_dynlm, type="HC0"))

# ADL(2,1) Model: 1962-Q1 - 2017-Q3
adl_0201_dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2) + L(TSpread,1),
                        data = us_macro_qua_ts,
                        start = c(1962, 1), end = c(2017, 3))
ct_adl_0201_dynlm <- coeftest(adl_0201_dynlm, vcov=vcovHC(adl_0201_dynlm, type="HC1"))

# ADL(2,2) Model: 1962-Q1 - 2017-Q3
adl_0202_dynlm <- dynlm(GDPGR ~ L(GDPGR,1) + L(GDPGR,2) + L(TSpread,1) + L(TSpread,2),
                        data = us_macro_qua_ts,
                        start = c(1962, 1), end = c(2017, 3))
ct_adl_0202_dynlm <- coeftest(adl_0202_dynlm, vcov=vcovHC(adl_0202_dynlm, type="HC1"))



adl.0201.crit <- lag_len_crit_fun(adl_0201_dynlm)
adl.0202.crit <- lag_len_crit_fun(adl_0202_dynlm)


tab.xxx <- rbind(
  cbind(ar01.crit$p,ar01.crit$ssr,ar01.crit$ssr.t,ar01.crit$log.ssr.t,ar01.crit$p1.ln.t.t,ar01.crit$BIC,ar01.crit$AIC,ar01.crit$R2),
  cbind(ar02.crit$p,ar02.crit$ssr,ar02.crit$ssr.t,ar02.crit$log.ssr.t,ar02.crit$p1.ln.t.t,ar02.crit$BIC,ar02.crit$AIC,ar02.crit$R2),
  cbind(adl.0201.crit$p,adl.0201.crit$ssr,adl.0201.crit$ssr.t,adl.0201.crit$log.ssr.t,adl.0201.crit$p1.ln.t.t,adl.0201.crit$BIC,adl.0201.crit$AIC,adl.0201.crit$R2),
  cbind(adl.0202.crit$p,adl.0202.crit$ssr,adl.0202.crit$ssr.t,adl.0202.crit$log.ssr.t,adl.0202.crit$p1.ln.t.t,adl.0202.crit$BIC,adl.0202.crit$AIC,adl.0202.crit$R2)
  )
colnames(tab.xxx) <- c("K","SSR(K)","SSR(K)/T","ln(SSR(K)/T)","(K+1)ln(T)/T","BIC(K)","AIC(K)","R2")


