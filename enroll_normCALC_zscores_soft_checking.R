setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir <- getwd()
options(contrasts = c("contr.sum", "contr.poly"))

# packages
library(dplyr)
library(brms)

# load the data and models
load("data/hd-norm-data.RData")
fit <- readRDS("mods/splines_no_int_mods.rds")
dat[[2]] <- dat[[2]] %>% mutate(`log(trla1)` = log(trla1),
                                `log(trlb1)` = log(trlb1))

# get variables ready
y_names <- c("sdmt", "cft", "scnt", "swrt", "sit", "tmt_a", "tmt_b", "lft")
vars <- c("sdmt1", "verfct5", "scnt1", "swrt1", "sit1", "log(trla1)", "log(trlb1)", "verflt05")

# calculate residual variance for each test
sigma_n_sq <- lapply(1:length(vars), function(i)
  VarCorr(fit[[i]])[["residual__"]][["sd"]][[ , "Estimate"]]^2)
names(sigma_n_sq) <- vars

# calculate predicted values and variance of predictions
y_pred <- list()
sigma_i_sq <- list()

for (i in vars) {
  dum <- fitted(fit[[i]])
  y_pred[[i]] <- dum[ , "Estimate"]
  sigma_i_sq[[i]] <- dum[ , "Est.Error"]^2
}

# calculate z_scores
z <- list()
for (i in vars) {
  y <- na.omit(dat[[2]][[i]])
  z[[i]] <- (y - y_pred[[i]]) / sqrt(sigma_i_sq[[i]] + rep(sigma_n_sq[[i]], length(y_pred[[i]])))
}

# means and standard deviations
z_desc <- list()
for (i in 1:length(vars)) {
  z_desc[[y_names[i]]] <- data.frame(mean = mean(z[[i]]), sd = sd(z[[i]]))
}

View(do.call(rbind.data.frame, z_desc)) # ought to be M = 0, SD = 1
