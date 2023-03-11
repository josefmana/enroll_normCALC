# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# remember current directory
dir <- getwd()

# list packages to be used
pkgs <- c("dplyr", # data wrangling
          "brms", # statistical models
          "openxlsx" # saving outputs in .xlsx
          )

# load or install each of the packages as needed
for ( i in pkgs ) {
  if ( i %in% rownames( installed.packages() ) == F ) install.packages(i) # install if it ain't installed yet
  if ( i %in% names( sessionInfo()$otherPkgs ) == F ) library( i , character.only = T ) # load if it ain't loaded yet
}

# create folders for models, figures and tables
# prints TRUE and creates the folder if it was not present, prints NULL if the folder was already present
sapply( c("mods","tabs","figs"), function(i) if( !dir.exists(i) ) dir.create(i) )



# ---- read the data ----

load("hd-norm-data.RData")

# choose the sample (1 == "full house",
#                    2 == "no depression or comorbidities" the main one,
#                    3 == "no depression or comorbidities + whole battery")
s = 2

# variables getready
# language
for (i in 1:length(dat)) {
  dat[[i]]$language <- recode_factor(dat[[i]]$language,
                                     "US_English" = "1-US_English",
                                     "German" = "2-German",
                                     "Spanish" = "3-Spanish",
                                     "French" = "4-French",
                                     "Canadian French" = "5-Canadian French",
                                     "Dutch" = "6-Dutch",
                                     "Italian" = "7-Italian",
                                     "Spanish_LA" = "8-Spanish_LA",
                                     "Polish" = "9-Polish",
                                     "Danish" = "10-Danish")
}

# age
for (i in 1:length(dat)) {
  dat[[i]]$age <- as.numeric(as.character(dat[[i]]$age))
}

# list of tests
vars <- c("sdmt1", "verfct5", "scnt1", "swrt1", "sit1", "log(trla1)", "log(trlb1)", "verflt05")


# ---- models fitting ----

options(mc.cores = parallel::detectCores()) # execution on a local, multicore CPU with excess RAM
options(contrasts = c("contr.sum", "contr.poly")) # sum coding

# ---- GAMs /w full interaction ----
fit.s_full_int <- list()
for (i in vars) {
  fit.s_full_int[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age, edu, by = sex) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .95), seed = 87542
  )
}

# refit those with divergent transitions
for (i in c("swrt1", "log(trla1)")) {
  fit.s_full_int[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age, edu, by = sex) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.s_full_int, file = "mods/splines_full_int_mods.RDS")


# ---- GAMs w/out interaction by sex ----
fit.s_int_no_sex <- list()
for (i in vars) {
  fit.s_int_no_sex[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age, edu) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .95), seed = 87542
  )
} 

# refit those with divergent transitions
for (i in "swrt1") {
  fit.s_int_no_sex[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age, edu) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.s_int_no_sex, file = "mods/splines_part_int_mods.RDS")

# ---- GAMs w/out interactions ----
fit.s_full <- list()
for (i in vars) {
  fit.s_full[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + t2(edu) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# refit those with divergent transitions
for (i in c("verfct5", "log(trla1)", "log(trlb1)", "verflt05")) {
  fit.s_full[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + t2(edu) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .999), seed = 87542
  )
}

# once more
for (i in c("verfct5", "log(trla1)")) {
  fit.s_full[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + t2(edu) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .9999), seed = 87542
  )
}

# save them
saveRDS(fit.s_full, file = "mods/splines_no_int_mods.RDS")


# ---- GAMS w/out sex ----
fit.s_no_sex <- list()
for (i in vars) {
  fit.s_no_sex[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + t2(edu) + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# refit those with divergent transitions
for (i in c("verfct5", "log(trla1)", "log(trlb1)")) {
  fit.s_no_sex[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + t2(edu) + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .999), seed = 87542
  )
}

# save them
saveRDS(fit.s_no_sex, file = "mods/splines_no_sex_mods.RDS")


# ---- GAMs w/out language ----

fit.s_no_lan <- list()
for (i in vars) {
  fit.s_no_lan[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + t2(edu) + sex"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# refit those with divergent transitions
for (i in c("log(trla1)", "log(trlb1)")) {
  fit.s_no_lan[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + t2(edu) + sex"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .999), seed = 87542
  )
}

# save them
saveRDS(fit.s_no_lan, file = "mod/splines_no_sex_mods.RDS")


# ---- LMRs w/out interactions ----

fit.l_full <- list()
for (i in vars) {
  fit.l_full[[i]] <- brm(
    bf(as.formula(paste0(i, "~ age + edu + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.l_full, file = "mods/linear_no_int_mods.RDS") # no divergent transitions


# ---- LMRs /w interactions ----

fit.l.full_int <- list()
for (i in vars) {
  fit.l.full_int[[i]] <- brm(
    bf(as.formula(paste0(i, "~ age * edu * sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.l.full_int, file = "mods/linear_full_int_mods.RDS") # no divergent transitions


# ---- LMRs w/out sex ----

fit.l_no_sex <- list()
for (i in vars) {
  fit.l_no_sex[[i]] <- brm(
    bf(as.formula(paste0(i, "~ age + edu + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.l_no_sex, file = "mods/linear_no_sex_mods.RDS") # no divergent transitions


# ---- LMRs w/out language ----

fit.l_no_lan <- list()
for (i in vars) {
  fit.l_no_lan[[i]] <- brm(
    bf(as.formula(paste0(i, "~ age + edu + sex"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.l_no_lan, file = "mods/linear_no_language_mods.RDS") # no divergent transitions


# ---- LMRs w/out age ----

fit.l_no_age <- list()
for (i in vars) {
  fit.l_no_age[[i]] <- brm(
    bf(as.formula(paste0(i, "~ edu + sex+ language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.l_no_age, file = "mods/linear_no_age.RDS") # no divergent transitions


# ---- LMRs w/out education ----

fit.l_no_edu <- list()
for (i in vars) {
  fit.l_no_edu[[i]] <- brm(
    bf(as.formula(paste0(i, "~ age + sex+ language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# save the models
saveRDS(fit.l_no_edu, file = "mods/linear_no_edu.RDS") # no divergent transitions


# ---- GAMs w/out age ----

fit.s_no_age <- list()
for (i in vars) {
  fit.s_no_age[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(edu) + sex+ language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# refit those with divergent transitions
for (i in c("sit1", "log(trla1)", "log(trlb1)")) {
  fit.s_no_age[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(edu) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .999), seed = 87542
  )
}

# refit those with divergent transitions
for (i in c("log(trla1)")) {
  fit.s_no_age[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(edu) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99999), seed = 87542
  )
}

# save them
saveRDS(fit.s_no_age, file = "mods/splines_no_age.RDS")


# ---- GAMs w/out education ----

fit.s_no_edu <- list()
for (i in vars) {
  fit.s_no_edu[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + sex+ language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .99), seed = 87542
  )
}

# refit those with divergent transitions
for (i in c("sdmt1", "log(trla1)", "log(trlb1)")) {
  fit.s_no_edu[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .999), seed = 87542
  )
}

# rinse and repeat
for (i in c("log(trla1)", "log(trlb1)")) {
  fit.s_no_edu[[i]] <- brm(
    bf(as.formula(paste0(i, "~ t2(age) + sex + language"))),
    family = "normal", data = dat[[s]],
    inits = 0, control = list(adapt_delta = .9999), seed = 87542
  )
}

# save them
saveRDS(fit.s_no_edu, file = "mods/splines_no_edu.RDS")


# ---- PSIS-LOO ----

# add loo to all models
for(i in names(fit.s_full)) {
  fit.s_full[[i]] <- add_criterion(fit.s_full[[i]], criterion = "loo")
  fit.s_no_sex[[i]] <- add_criterion(fit.s_no_sex[[i]], criterion = "loo")
  fit.s_no_lan[[i]] <- add_criterion(fit.s_no_lan[[i]], criterion = "loo")
  fit.s_no_age[[i]] <- add_criterion(fit.s_no_age[[i]], criterion = "loo")
  fit.s_no_edu[[i]] <- add_criterion(fit.s_no_edu[[i]], criterion = "loo")
  fit.l_full[[i]] <- add_criterion(fit.l_full[[i]], criterion = "loo")
  fit.l_no_sex[[i]] <- add_criterion(fit.l_no_sex[[i]], criterion = "loo")
  fit.l_no_lan[[i]] <- add_criterion(fit.l_no_lan[[i]], criterion = "loo")
  fit.l_no_age[[i]] <- add_criterion(fit.l_no_age[[i]], criterion = "loo")
  fit.l_no_edu[[i]] <- add_criterion(fit.l_no_edu[[i]], criterion = "loo")
}

# save after adding PSIS-LOO
saveRDS(fit.s_full, "mods/splines_no_int_mods.rds")
saveRDS(fit.s_no_sex, "mods/splines_no_sex_mods.rds")
saveRDS(fit.s_no_lan, "mods/splines_no_language_mods.rds")
saveRDS(fit.s_no_age, "mods/splines_no_age.rds")
saveRDS(fit.s_no_edu, "mods/splines_no_edu.rds")
saveRDS(fit.l_full, "mods/linear_no_int_mods.rds")
saveRDS(fit.l_no_sex, "mods/linear_no_sex_mods.rds")
saveRDS(fit.l_no_lan, "mods/linear_no_language_mods.rds")
saveRDS(fit.l_no_age, "mods/linear_no_age.rds")
saveRDS(fit.l_no_edu, "mods/linear_no_edu.rds")

# compare model fit for different specifications
loo <- list()
for (i in vars) {
  loo[[i]] <- loo(fit.s_full[[i]],
                  fit.s_no_sex[[i]], fit.s_no_lan[[i]], fit.s_no_age[[i]], fit.s_no_edu[[i]],
                  fit.l_full[[i]],
                  fit.l_no_sex[[i]], fit.l_no_lan[[i]], fit.l_no_age[[i]], fit.l_no_edu[[i]])
}

# save the comparisons
saveRDS(loo, file = "mods/loo.RDS")

# write tables with loos
loo_diffs <- list()
loo_ic_diffs <- list()
for (i in names(loo)) {
  loo_diffs[[i]] <- loo[[i]]$diffs
  loo_ic_diffs[[i]] <- loo[[i]]$ic_diffs__
}

# write in .xlsx
write.xlsx(loo_diffs, "tabs/loo_diffs.xlsx", rowNames = T)
write.xlsx(loo_ic_diffs, "tabs/loo_ic_diffs.xlsx", rowNames = T)
