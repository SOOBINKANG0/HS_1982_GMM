rm(list=ls())

library(gmm)
library(tidyverse)
library(readxl); library(writexl)
library(data.table)

##----------------------------------
## ND_GMM Table 1
##----------------------------------
real_NDcons_quantity = read_xlsx("Data/ND/DNDGRA3M086SBEA.xlsx",sheet = 2)
real_NDcons_price = read_xlsx("Data/ND/DNDGRG3M086SBEA.xlsx",sheet = 2)
POP = read_xlsx("Data/ND/POPTHM.xlsx",sheet = 2)
ret = read_xlsx("Data/ND/EWR_VWR.xlsx")

##
ret = ret %>% mutate(observation_date = floor_date(date, unit = "month")) %>% dplyr::select(-date)

data <- real_NDcons_quantity %>%
  left_join(
    real_NDcons_price,
    by = "observation_date"
  ) %>%
  left_join(
    POP,
    by = "observation_date"
  ) %>%
  left_join(
    ret,
    by = "observation_date"
  ) %>%
  arrange(observation_date)

rm(list = ls()[!ls() %in% "data"])

data <- data %>%
  mutate(
    
    # Real per-capita consumption
    cons = DNDGRA3M086SBEA / POPTHM,
    
    # Gross consumption growth
    cons_growth = cons / lag(cons),
    
    # Gross inflation
    inflation = DNDGRG3M086SBEA / lag(DNDGRG3M086SBEA),
    
    # Gross real returns
    real_return_ew = (1 + EWR) / inflation,
    real_return_vw = (1 + VWR) / inflation,
    
    one_vec = 1
  )



##
for(i in 1:6){
  
  data <- data %>%
    mutate(
      "NLAG{i}_return_ew" := lag(real_return_ew, i),
      "NLAG{i}_return_vw" := lag(real_return_vw, i),
      "NLAG{i}_growth"    := lag(cons_growth, i)
    )
}

##
res = data.frame()
tempp = c(1,2,4,6)

for(j in tempp){
  nlag <- j
  z_cols <- c("one_vec")
  
  for(i in 1:nlag){
    
    z_cols <- c(
      z_cols,
      paste0("NLAG", i, "_return_ew"),
      paste0("NLAG", i, "_growth")
    )
  }
  ##
  X <- data %>%
    dplyr::select(
      cons_growth,
      real_return_ew,
      all_of(z_cols)
    ) %>%
    drop_na() %>%
    as.data.frame()
  ##
  obj_func <- function(param, data){
    
    gamma <- param[1]
    beta  <- param[2]
    
    r <- data[["real_return_ew"]]
    g <- data[["cons_growth"]]
    
    Z <- as.matrix(data[, z_cols])
    
    u <- beta * r * g^(gamma) - 1
    
    u * Z
  }
  
  Tn <- nrow(X)
  L <- round(Tn^(1/3))
             
  fit <- gmm(
    g = obj_func,
    x = X,
    t0 = c(-1, 0.995),
    type = "iterative",
    wmatrix = "optimal",
    vcov = "HAC",
    kernel = "Bartlett",
    bw = L + 1,
    prewhite = FALSE,
    centeredVcov = FALSE
  )
  
  temp = data.frame(Cons = "ND",Return = "EWR",
                    alpha = round(summary(fit)$coefficients[1, 1],3),
                    alpha_se = round(summary(fit)$coefficients[1, 2],3),
                    beta = round(summary(fit)$coefficients[2, 1],3),
                    beta_se = round(summary(fit)$coefficients[2, 2],3),
                    DF = fit$df, 
                    chi_sq = round(specTest(fit)$test[1, 1], 3),
                    prob = round(
                      pchisq(specTest(fit)$test[1, 1],df = fit$df)
                      ,3))
  res = rbind(res,temp)
}

##
res2 = data.frame()

for(j in tempp){
  nlag <- j
  z_cols <- c("one_vec")
  
  for(i in 1:nlag){
    
    z_cols <- c(
      z_cols,
      paste0("NLAG", i, "_return_vw"),
      paste0("NLAG", i, "_growth")
    )
  }
  
  ##
  X <- data %>%
    dplyr::select(
      cons_growth,
      real_return_vw,
      all_of(z_cols)
    ) %>%
    drop_na() %>%
    as.data.frame()
  ##
  obj_func <- function(param, data){
    
    gamma <- param[1]
    beta  <- param[2]
    
    r <- data[["real_return_vw"]]
    g <- data[["cons_growth"]]
    
    Z <- as.matrix(data[, z_cols])
    
    u <- beta * r * g^(gamma) - 1
    
    u * Z
  }
  
  Tn <- nrow(X)
  L <- round(Tn^(1/3))
  
  fit <- gmm(
    g = obj_func,
    x = X,
    t0 = c(-1, 0.995),
    
    type = "iterative",
    wmatrix = "optimal",
    
    vcov = "HAC",
    kernel = "Bartlett",
    bw = L + 1,
    prewhite = FALSE,
    centeredVcov = FALSE,
    
    crit = 1e-4,
    itermax = 1000,
    
    method = "Nelder-Mead"
  )
  
  
  tempy = data.frame(Cons = "ND",Return = "VWR",  
                     alpha = round(summary(fit)$coefficients[1, 1],3),
                     alpha_se = round(summary(fit)$coefficients[1, 2],3),
                     beta = round(summary(fit)$coefficients[2, 1],3),
                     beta_se = round(summary(fit)$coefficients[2, 2],3),
                     DF = fit$df,
                     chi_sq = round(specTest(fit)$test[1, 1], 3),
                     prob = round(
                       pchisq(specTest(fit)$test[1, 1],df = fit$df)
                       ,3))
  
  res2 = rbind(res2,tempy)
}

final = rbind(res,res2)

write_xlsx(final, "Results/Table1_ND_iter.xlsx")

##----------------------------------
## NDS_GMM Table 1
##----------------------------------

nominal_ND = read_xlsx("Data/NDS/PCEND.xlsx",sheet = 2)
nominal_S = read_xlsx("Data/NDS/PCES.xlsx",sheet = 2)
price = read_xlsx("Data/NDS/PCEPI.xlsx",sheet = 2)

POP = read_xlsx("Data/NDS/POPTHM.xlsx",sheet = 2)
ret = read_xlsx("Data/NDS/EWR_VWR.xlsx")

ret = ret %>% mutate(observation_date = floor_date(date, unit = "month")) %>% dplyr::select(-date)

data = left_join(nominal_ND, nominal_S, by = "observation_date")
data = data %>% left_join(price, by = "observation_date") %>%
  left_join(POP, by = "observation_date") %>% 
  left_join(ret, by = "observation_date") %>% arrange(observation_date) %>% 
  mutate(PCENDS = ((PCEND + PCES)/PCEPI)/POPTHM) %>%
  mutate(inf = PCEPI/lag(PCEPI, 1)) %>% mutate(real_return_ew = (1+EWR)/inf, real_return_vw = (1+VWR)/inf) %>% 
  mutate(cons_growth = PCENDS / lag(PCENDS,1), one_vec = 1)
?gmm
for(i in 1:6){
  
  data <- data %>%
    mutate(
      "NLAG{i}_return_ew" := lag(real_return_ew, i),
      "NLAG{i}_return_vw" := lag(real_return_vw, i),
      "NLAG{i}_growth"    := lag(cons_growth, i)
    )
}


res = data.frame()
tempp = c(1,2,4,6)

for(j in tempp){
  nlag <- j
  z_cols <- c("one_vec")
  
  for(i in 1:nlag){
    
    z_cols <- c(
      z_cols,
      paste0("NLAG", i, "_return_ew"),
      paste0("NLAG", i, "_growth")
    )
  }
  ##
  X <- data %>%
    dplyr::select(
      cons_growth,
      real_return_ew,
      all_of(z_cols)
    ) %>%
    drop_na() %>%
    as.data.frame()
  ##
  obj_func <- function(param, data){
    
    gamma <- param[1]
    beta  <- param[2]
    
    r <- data[["real_return_ew"]]
    g <- data[["cons_growth"]]
    
    Z <- as.matrix(data[, z_cols])
    
    u <- beta * r * g^(gamma) - 1
    
    u * Z
  }
  
  Tn <- nrow(X)
  L <- round(Tn^(1/3))
  
  fit <- gmm(
    g = obj_func,
    x = X,
    t0 = c(-1, 0.995),
    
    type = "iterative",
    wmatrix = "optimal",
    
    vcov = "HAC",
    kernel = "Bartlett",
    bw = L + 1,
    prewhite = FALSE,
    centeredVcov = FALSE,
    
    crit = 1e-4,
    itermax = 1000,
    
    method = "Nelder-Mead"
  )
  
  temp = data.frame(Cons = "NDS",Return = "EWR",
                    alpha = round(summary(fit)$coefficients[1, 1],3),
                    alpha_se = round(summary(fit)$coefficients[1, 2],3),
                    beta = round(summary(fit)$coefficients[2, 1],3),
                    beta_se = round(summary(fit)$coefficients[2, 2],3),
                    DF = fit$df, 
                    chi_sq = round(specTest(fit)$test[1, 1], 3),
                    prob = round(
                      pchisq(specTest(fit)$test[1, 1],df = fit$df)
                      ,3))
  res = rbind(res,temp)
}

##
res2 = data.frame()

for(j in tempp){
  nlag <- j
  z_cols <- c("one_vec")
  
  for(i in 1:nlag){
    
    z_cols <- c(
      z_cols,
      paste0("NLAG", i, "_return_vw"),
      paste0("NLAG", i, "_growth")
    )
  }
  
  ##
  X <- data %>%
    dplyr::select(
      cons_growth,
      real_return_vw,
      all_of(z_cols)
    ) %>%
    drop_na() %>%
    as.data.frame()
  ##
  obj_func <- function(param, data){
    
    gamma <- param[1]
    beta  <- param[2]
    
    r <- data[["real_return_vw"]]
    g <- data[["cons_growth"]]
    
    Z <- as.matrix(data[, z_cols])
    
    u <- beta * r * g^(gamma) - 1
    
    u * Z
  }
  
  fit <- gmm(
    g = obj_func,
    x = X,
    t0 = c(-1, 0.995),
    type = "iterative",
    wmatrix = "optimal",
    vcov = "HAC",
    kernel = "Bartlett",
    prewhite = FALSE,
    centeredVcov = FALSE
  )

  tempy = data.frame(Cons = "NDS",Return = "VWR",  
                     alpha = round(summary(fit)$coefficients[1, 1],3),
                     alpha_se = round(summary(fit)$coefficients[1, 2],3),
                     beta = round(summary(fit)$coefficients[2, 1],3),
                     beta_se = round(summary(fit)$coefficients[2, 2],3),
                     DF = fit$df,
                     chi_sq = round(specTest(fit)$test[1, 1], 3),
                     prob = round(
                       pchisq(specTest(fit)$test[1, 1],df = fit$df)
                       ,3))
  
  res2 = rbind(res2,tempy)
}

final = rbind(res,res2)

write_xlsx(final, "Results/Table1_NDS2.xlsx")
##