##-------------------------------------
## non linear GMM
##-------------------------------------
data = readxl::read_xlsx("Data/GMM_data.xlsx")

nlag <- 2

z_cols <- c("one_vec")

for(i in 1:nlag){
  z_cols <- c(
    z_cols,
    paste0("NLAG", i, "_return"),
    paste0("NLAG", i, "_growth")
  )
}

X <- data %>%
  select(cons_growth, real_return, all_of(z_cols)) %>%
  drop_na() %>%
  as.data.frame()

obj_func <- function(param, data){
  
  gamma <- param[1]
  beta  <- param[2]
  
  r <- data[["real_return"]]
  g <- data[["cons_growth"]]
  
  Z <- as.matrix(data[, z_cols])
  
  u <- beta * r * g^(gamma) - 1
  
  u * Z
}

fit <- gmm(
  g = obj_func,
  x = X,
  t0 = c(1, 0.995),
  type = "twoStep",
  wmatrix = "optimal",
  vcov = "MDS",
  centeredVcov = FALSE
)
summary(fit)
1 - pchisq(11.2481, 11)
##-------------------------------------
## linear GMM (OLS)
##-------------------------------------
rm(list=ls())
data(mtcars)
fit <- lm(mpg ~ wt + cyl, data = mtcars)
summary(fit)$coef

## closed form
gmm_data = mtcars %>% select(mpg, wt, cyl) %>% mutate(one_vec = 1)
X = as.matrix(gmm_data[,c(4,2,3)])
Y = as.matrix(gmm_data[,1])
OLS_res = solve(t(X) %*% X) %*% t(X) %*% Y


## optimize with optim()
dat <- mtcars %>%
  select(mpg, wt, cyl)
y <- as.matrix(dat$mpg)

X <- as.matrix(cbind(
  intercept = 1,
  wt = dat$wt,
  cyl = dat$cyl
))

gbar <- function(beta, y, X){
  
  u <- as.numeric(y - X %*% beta)
  
  g <- X * u
  
  colMeans(g)
  
}

Q <- function(beta, y, X, W){
  
  g <- gbar(beta, y, X)
  
  as.numeric(t(g) %*% W %*% g)
  
}

optim(
  par = c(0,0,0),
  fn = Q,
  y = y,
  X = X,
  W = diag(3),
  method = "BFGS"
)

## gmm packages
gmm_data = mtcars %>% select(mpg, wt, cyl)
obj_func = function(param, data = gmm_data){
  
  ##
  a = param[1]
  b1 = param[2]
  b2 = param[3]
  
  ##
  Y  = data[,1] %>% as.vector()
  X1 = data[,2] %>% as.vector()
  X2 = data[,3] %>% as.vector()
  
  u = Y - b1*X1 - b2*X2 - a
  cbind(u, X1 * u, X2 * u)
}

t0 = c(0,0,0)
res = gmm(g = obj_func, x = gmm_data, t0 = t0, type = "twoStep", wmatrix = "ident", vcov = "iid", method = "BFGS")
summary(res)$coef

##-------------------------------------
## GMM (mean-variance)
##-------------------------------------
rm(list=ls())
set.seed(1000)
dat = rnorm(1000, mean = 5, sd = 2)
mean(dat); sd(dat)

obj_func = function(param, data){
  mu = param[1]
  std = param[2]
  
  u1 = data- mu
  u2 = (data-mu)^2 - std^2
  
  cbind(u1, u2)
}
t0 = c(5,2)
res = gmm(g = obj_func, x = dat, t0 = t0, type = "twoStep",wmatrix = "ident", method = "BFGS")