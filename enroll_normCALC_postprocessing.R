# set working directory (works only in RStudio)
setwd( dirname(rstudioapi::getSourceEditorContext()$path) )

# remember current directory
dir <- getwd()

# set theme for plotting
theme_set(theme_classic(base_size = 14))

# list packages to be used
pkgs <- c("dplyr", "tidyverse", # data wrangling
          "brms", "bayestestR", "emmeans", "tidybayes", # stats
          "ggplot2", "ggpubr", "RColorBrewer", # plotting
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

load("data/hd-norm-data.RData")

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
var_names <- c("SDMT", "CFT", "SCNT", "SWRT", "SIT", "TMT-A", "TMT-B", "LFT")
var_names2 <- c("SDMT", "CFT", "SCNT", "SWRT", "SIT", "log(TMT-A)", "log(TMT-B)", "LFT")

# ranges
r <- list()
for (i in vars) {
  if (i == "log(trla1)" | i == "log(trlb1)") {
    j <- substr(i, 5, 9)
    r[[j]] <- range(na.omit(dat[[s]][[j]]))
  } else r[[i]] <- range(na.omit(dat[[s]][[i]]))
}
r <- rbind.data.frame(r)


# --- read models ----

options(mc.cores = parallel::detectCores()) # execution on a local, multicore CPU with excess RAM
options(contrasts = c("contr.sum", "contr.poly")) # sum coding
f <- readRDS("mods/splines_no_int_mods.rds") # reading proper


# ---- age tabs ----

age_tab <- data.frame(
  age = seq(20, 90, 10), edu = 14, sex = NA, language = NA, # median edu, global mean sex and lang
  sdmt1 = NA, verfct5 = NA, scnt1 = NA, swrt1 = NA, sit1 = NA, trla1 = NA, trlb1 = NA, verflt05 = NA
  )

for (i in vars) {
  dum <- as.data.frame(fitted(f[[i]], newdata = age_tab))
  if (i == "log(trla1)" | i == "log(trlb1)") {
    k <- substr(i, 5, 9)
    dum <- exp(dum)
  } else {
    k <- i
  }
  age_tab[[k]] <- paste0(
    sprintf("%.0f", round(dum$Estimate, 0)),
    " [", sprintf("%.0f", round(dum$Q2.5, 0)), ", ", sprintf("%.0f", round(dum$Q97.5, 0)), "]" 
  )
}

write.xlsx(age_tab, "tabs/age_tab.xlsx", rowNames = F)


# --- education tabs ----

edu_tab <- data.frame(
  edu = seq(8, 22, 2), age = 48, sex = NA, language = NA, # median age, global mean sex and lang
  sdmt1 = NA, verfct5 = NA, scnt1 = NA, swrt1 = NA, sit1 = NA, trla1 = NA, trlb1 = NA, verflt05 = NA
  )

for (i in vars) {
  dum <- as.data.frame(fitted(f[[i]], newdata = edu_tab))
  if (i == "log(trla1)" | i == "log(trlb1)") {
    k <- substr(i, 5, 9)
    dum <- exp(dum)
  } else {
    k <- i
  }
  edu_tab[[k]] <- paste0(
    sprintf("%.0f", round(dum$Estimate, 0)),
    " [", sprintf("%.0f", round(dum$Q2.5, 0)), ", ", sprintf("%.0f", round(dum$Q97.5, 0)), "]" 
  )
}

write.xlsx(edu_tab, "tabs/edu_tab.xlsx", rowNames = F)

# ---- ROPEs ----

ropes <- data.frame(
  sdmt1 = c(-0.1*sd(na.omit(dat[[2]]$sdmt1)), 0.1*sd(na.omit(dat[[2]]$sdmt1))),
  verfct5 = c(-0.1*sd(na.omit(dat[[2]]$verfct5)), 0.1*sd(na.omit(dat[[2]]$verfct5))),
  scnt1 = c(-0.1*sd(na.omit(dat[[2]]$scnt1)), 0.1*sd(na.omit(dat[[2]]$scnt1))),
  swrt1 = c(-0.1*sd(na.omit(dat[[2]]$swrt1)), 0.1*sd(na.omit(dat[[2]]$swrt1))),
  sit1 = c(-0.1*sd(na.omit(dat[[2]]$sit1)), 0.1*sd(na.omit(dat[[2]]$sit1))),
  `log(trla1)` = c(-0.1*log(sd(na.omit(log(dat[[2]]$trla1)))), 0.1*log(sd(na.omit(log(dat[[2]]$trla1))))),
  `log(trlb1)` = c(-0.1*log(sd(na.omit(log(dat[[2]]$trlb1)))), 0.1*log(sd(na.omit(log(dat[[2]]$trlb1))))),
  verflt05 = c(-0.1*sd(na.omit(dat[[2]]$verflt05)), 0.1*sd(na.omit(dat[[2]]$verflt05))),
  row.names = c("rope_low", "rope_high"))


# ---- sex tabs ----

sex_tab <- list(desc = data.frame(var = vars, female = NA, male = NA),
                difs = data.frame(var = vars, diff = NA, pd = NA, percent_rope = NA))

for (i in names(sex_tab)) {
  sex_tab[[i]] <- sex_tab[[i]] %>% column_to_rownames(var = "var")
}

for (i in vars) {
  dum <- emmeans(f[[i]], ~ sex)
  desc_post <- as.data.frame(describe_posterior(dum, centrality = "mean", ci = .95, rope_ci = 1,
                                                rope_range = c(ropes[[i]][1], ropes[[i]][2])))
  dum <- pairs(dum)
  diff_post <- as.data.frame(describe_posterior(dum, centrality = "mean", ci = .95, rope_ci = 1,
                                                rope_range = c(ropes[[i]][1], ropes[[i]][2])))
  if (i == "log(trla1)" | i == "log(trlb1)") {
    sex_tab$desc[i, ]$female <- paste0(sprintf("%.2f", round(exp(desc_post[1, "Mean"]), 2)), " [",
                                       sprintf("%.2f", round(exp(desc_post[1, "CI_low"]), 2)), ", ",
                                       sprintf("%.2f", round(exp(desc_post[1, "CI_high"]), 2)), "]")
    sex_tab$desc[i, ]$male <- paste0(sprintf("%.2f", round(exp(desc_post[2, "Mean"]), 2)), " [",
                                     sprintf("%.2f", round(exp(desc_post[2, "CI_low"]), 2)), ", ",
                                     sprintf("%.2f", round(exp(desc_post[2, "CI_high"]), 2)), "]")
    sex_tab$difs[i, ]$diff <- paste0(sprintf("%.2f", round(exp(diff_post[1, "Mean"]), 2)), " [",
                                      sprintf("%.2f", round(exp(diff_post[1, "CI_low"]), 2)), ", ",
                                      sprintf("%.2f", round(exp(diff_post[1, "CI_high"]), 2)), "]")
  } else {
    sex_tab$desc[i, ]$female <- paste0(sprintf("%.2f", round(desc_post[1, "Mean"], 2)), " [",
                                       sprintf("%.2f", round(desc_post[1, "CI_low"], 2)), ", ",
                                       sprintf("%.2f", round(desc_post[1, "CI_high"], 2)), "]")
    sex_tab$desc[i, ]$male <- paste0(sprintf("%.2f", round(desc_post[2, "Mean"], 2)), " [",
                                     sprintf("%.2f", round(desc_post[2, "CI_low"], 2)), ", ",
                                     sprintf("%.2f", round(desc_post[2, "CI_high"], 2)), "]")
    sex_tab$difs[i, ]$diff <- paste0(sprintf("%.2f", round(diff_post[1, "Mean"], 2)), " [",
                                     sprintf("%.2f", round(diff_post[1, "CI_low"], 2)), ", ",
                                     sprintf("%.2f", round(diff_post[1, "CI_high"], 2)), "]")
    
  }
  sex_tab$difs[i, ]$pd <- sprintf("%.2f", round(100*diff_post[1, "pd"], 2))
  sex_tab$difs[i, ]$percent_rope <- sprintf("%.2f", round(100*diff_post[1, "ROPE_Percentage"], 2))
  rm(dum, desc_post, diff_post)
}

write.xlsx(sex_tab, "tabs/sex_tab.xlsx", rowNames = T)


# ---- language tabs ----

lan_tab <- list()

for (i in vars) {
  dum <- emmeans(f[[i]], ~ language) %>% pairs()
  lan_tab[[i]] <- describe_posterior(dum, centrality = "mean", ci = .95, rope_ci = 1,
                                     rope_range = c(ropes[[i]][1], ropes[[i]][2]))
}

write.xlsx(lan_tab, "lan_tab.xlsx", rowNames = F)

# lan_tab for text
lan_tab$trla1 <- lan_tab$`log(trla1)`
lan_tab$trlb1 <- lan_tab$`log(trlb1)`
lan_tab2 <- data.frame(sdmt1 = rep(NA, 10), verfct5 = NA, scnt1 = NA, swrt1 = NA,
                       sit1 = NA, trla1 = NA, trlb1 = NA, verflt05 = NA,
                       row.names = levels(dat$`soft-exclusion`$language))
lan_tab2[1, ] <- rep(0, ncol(lan_tab2))

for (i in rownames(lan_tab2)[-1]) {
  for (j in colnames(lan_tab2)) {
    dum <- lan_tab[[j]][1:9, ]
    lan_tab2[i, j] <- paste0(
      sprintf("%.2f", round(-dum[which(grepl(i, dum$Parameter)), "Mean"], 2)), " [",
      sprintf("%.2f", round(-dum[which(grepl(i, dum$Parameter)), "CI_high"], 2)), ", ",
      sprintf("%.2f", round(-dum[which(grepl(i, dum$Parameter)), "CI_low"], 2)), "]"
    )
  }
}

rm(dum)
write.xlsx(lan_tab2, "tabs/lan_tab2.xlsx", rowNames = T)


# ---- posterior predictive checks plot ----

ppc <- list()

for (i in 1:length(vars)) {
  set.seed(87542)
  ppc[[vars[[i]]]] <- pp_check(f[[vars[[i]]]], nsamples = 100) +
    labs(x = var_names2[[i]], y = "Density", color = NULL) +
    scale_color_manual(labels = c("Observed data", "Model predictions"),
                       values = c("black", "slategray2"))
}

ggarrange(ppc$sdmt1, ppc$scnt1, ppc$swrt1, ppc$sit1,
          ppc$verfct5, ppc$verflt05, ppc$`log(trla1)`, ppc$`log(trlb1)`,
          ncol = 2, nrow = 4, common.legend = T, legend = "bottom")

ggsave("figs/ppc_fig.jpeg", height = 11, width = 8, device = "jpeg")


# ---- age plot ----

dum_dat <- data.frame(age = seq(15, 90, 1), edu = 14, sex = NA, language = NA,
                      sdmt1 = NA, verfct1 = NA, scnt1 = NA, swrt1 = NA, sit1 = NA,
                      `log(trla1)` = NA, `log(trlb1)` = NA, verflt05 = NA)

age_fig <- list()

for (i in 1:length(vars)) {
  dd <- dat[[s]]
  dd$test <- dd[[vars[[i]]]]
  if (vars[[i]] == "log(trla1)" | vars[[i]] == "log(trlb1)") {
    j <- substr(vars[[i]], 5, 9)
    dd$test <- dd[[j]]
    age_fig[[vars[[i]]]] <- dum_dat %>%
      add_predicted_draws(f[[vars[[i]]]]) %>%
      mutate(.prediction = exp(.prediction)) %>%
      mean_hdi(.prediction) %>%
      mutate(.lower = ifelse (.lower < 0, 0, .lower),
             .upper = ifelse(.upper > r[[j]][2], r[[j]][2], .upper)) %>%
      ggplot(aes(x = age, y = .prediction)) +
      geom_point(data = dd, aes(x = age, y = test), alpha = .025) +
      geom_smooth(aes(y = .prediction), size = 1.2) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .1) +
      scale_x_continuous(breaks = seq(20, 90, 10), labels = seq(20, 90, 10)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Age (years)", y = var_names[[i]])
  } else {
    age_fig[[vars[[i]]]] <- dum_dat %>%
      add_predicted_draws(f[[vars[[i]]]]) %>%
      mean_hdi(.prediction) %>%
      mutate(.lower = ifelse (.lower < 0, 0, .lower),
             .upper = ifelse(.upper > r[[j]][2], r[[j]][2], .upper)) %>%
      ggplot(aes(x = age, y = .prediction)) +
      geom_point(data = dd, aes(x = age, y = test), alpha = .025) +
      geom_smooth(aes(y = .prediction), size = 1.2) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .1) +
      scale_x_continuous(breaks = seq(20, 90, 10), labels = seq(20, 90, 10)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Age (years)", y = var_names[[i]])
  }
}

age_fig$scnt1 <- age_fig$scnt1 + scale_y_continuous(limits = c(0, 125))
age_fig$`log(trla1)` <- age_fig$`log(trla1)` + scale_y_continuous(limits = c(0, 240),
                                                                  breaks = seq(0, 240, 40),
                                                                  labels = seq(0, 240, 40))
age_fig$`log(trlb1)` <- age_fig$`log(trlb1)` + scale_y_continuous(limits = c(0, 240),
                                                                  breaks = seq(0, 240, 40),
                                                                  labels = seq(0, 240, 40))

ggarrange(age_fig$sdmt1, age_fig$scnt1, age_fig$swrt1, age_fig$sit1,
          age_fig$verfct5, age_fig$verflt05, age_fig$`log(trla1)`, age_fig$`log(trlb1)`,
          ncol = 2, nrow = 4)

ggsave("figs/age_fig.jpeg", height = 11, width = 8, device = "jpeg")


# ---- education plot ----

dum_dat <- data.frame(age = 48, edu = seq(0, 24, 1), sex = NA, language = NA,
                      sdmt1 = NA, verfct1 = NA, scnt1 = NA, swrt1 = NA, sit1 = NA,
                      `log(trla1)` = NA, `log(trlb1)` = NA, verflt05 = NA)

edu_fig <- list()

for (i in 1:length(vars)) {
  dd <- dat[[s]]
  dd$test <- dd[[vars[[i]]]]
  if (vars[[i]] == "log(trla1)" | vars[[i]] == "log(trlb1)") {
    dd$test <- dd[[substr(vars[[i]], 5, 9)]]
    edu_fig[[vars[[i]]]] <- dum_dat %>%
      add_predicted_draws(f[[vars[[i]]]]) %>%
      mutate(.prediction = exp(.prediction)) %>%
      mean_hdi(.prediction) %>%
      mutate(.lower = ifelse (.lower < 0, 0, .lower),
             .upper = ifelse(.upper > r[[j]][2], r[[j]][2], .upper)) %>%
      ggplot(aes(x = edu, y = .prediction)) +
      geom_point(data = dd, aes(x = edu, y = test), alpha = .025) +
      geom_smooth(aes(y = .prediction), size = 1.2) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .1) +
      scale_x_continuous(breaks = seq(0, 24, 2), labels = seq(0, 24, 2)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Education (years)", y = var_names[[i]])
  } else {
    edu_fig[[vars[[i]]]] <- dum_dat %>%
      add_predicted_draws(f[[vars[[i]]]]) %>%
      mean_hdi(.prediction) %>%
      mutate(.lower = ifelse (.lower < 0, 0, .lower),
             .upper = ifelse(.upper > r[[j]][2], r[[j]][2], .upper)) %>%
      ggplot(aes(x = edu, y = .prediction)) +
      geom_point(data = dd, aes(x = edu, y = test), alpha = .025) +
      geom_smooth(aes(y = .prediction), size = 1.2) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .1) +
      scale_x_continuous(breaks = seq(0, 24, 2), labels = seq(0, 24, 2)) +
      scale_y_continuous(limits = c(0, NA)) +
      labs(x = "Education (years)", y = var_names[[i]])
  }
}

edu_fig$scnt1 <- edu_fig$scnt1 + scale_y_continuous(limits = c(0, 125))
edu_fig$`log(trla1)` <- edu_fig$`log(trla1)` + scale_y_continuous(limits = c(0, 240),
                                                                  breaks = seq(0, 240, 40),
                                                                  labels = seq(0, 240, 40))
edu_fig$`log(trlb1)` <- edu_fig$`log(trlb1)` + scale_y_continuous(limits = c(0, 240),
                                                                  breaks = seq(0, 240, 40),
                                                                  labels = seq(0, 240, 40))

ggarrange(edu_fig$sdmt1, edu_fig$scnt1, edu_fig$swrt1, edu_fig$sit1,
          edu_fig$verfct5, edu_fig$verflt05, edu_fig$`log(trla1)`, edu_fig$`log(trlb1)`,
          ncol = 2, nrow = 4)

ggsave("figs/edu_fig.jpeg", height = 11, width = 8, device = "jpeg")


# ---- language plot ----

dum_dat <- data.frame(age = 48, edu = 14, sex = NA,
                      language = as.factor(c("1-US_English", "2-German", "3-Spanish", "7-Italian",
                                             "9-Polish", "5-Canadian French")),
                      sdmt1 = NA, verfct1 = NA, scnt1 = NA, swrt1 = NA, sit1 = NA,
                      `log(trla1)` = NA, `log(trlb1)` = NA, verflt05 = NA)

lan_fig <- list()

for (i in 1:length(vars)) {
  dd <- dat[[s]]
  dd$test <- dd[[vars[[i]]]]
  if (vars[[i]] == "log(trla1)" | vars[[i]] == "log(trlb1)") {
    dd$test <- dd[[substr(vars[[i]], 5, 9)]]
    lan_fig[[vars[[i]]]] <- dum_dat %>%
      add_fitted_draws(f[[vars[[i]]]]) %>%
      mutate(.value = exp(.value)) %>%
      mean_hdi(.value) %>%
      mutate(.lower = ifelse (.lower < 0, 0, .lower),
             .upper = ifelse(.upper > r[[j]][2], r[[j]][2], .upper),
             language = ifelse(language == "1-US_English", "1-US",
                               ifelse(language == "2-German", "2-Ger",
                                      ifelse(language == "3-Spanish", "3-Esp",
                                             ifelse(language == "7-Italian", "4-Ita",
                                                    ifelse(language == "9-Polish", "5-Pol",
                                                           ifelse(language == "5-Canadian French", "6-Can", NA)
                                                    )))))) %>%
      ggplot(aes(x = language, y = .value, ymin = .lower, ymax = .upper)) +
      geom_point(size = 3) +
      geom_errorbar(size = 1, width = .1) +
      scale_y_continuous(name = var_names[[i]], limits = c(0, r[[j]][2])) +
      scale_x_discrete(name = "Language", labels = c("ENG", "GER", "ESP", "ITA", "POL", "CAN"))
  } else {
    lan_fig[[vars[[i]]]] <- dum_dat %>%
      add_fitted_draws(f[[vars[[i]]]]) %>%
      mean_hdi(.value) %>%
      mutate(.lower = ifelse (.lower < 0, 0, .lower),
             .upper = ifelse(.upper > r[[vars[[i]]]][2], r[[vars[[i]]]][2], .upper),
             language = ifelse(language == "1-US_English", "1-US",
                               ifelse(language == "2-German", "2-Ger",
                                      ifelse(language == "3-Spanish", "3-Esp",
                                             ifelse(language == "7-Italian", "4-Ita",
                                                    ifelse(language == "9-Polish", "5-Pol",
                                                           ifelse(language == "5-Canadian French", "6-Can", NA)
                                                    )))))) %>%
      ggplot(aes(x = language, y = .value, ymin = .lower, ymax = .upper)) +
      geom_point(size = 3) +
      geom_errorbar(size = 1, width = .1) +
      scale_y_continuous(name = var_names[[i]], limits = c(0, r[[vars[[i]]]][2])) +
      scale_x_discrete(name = "Language", labels = c("ENG", "GER", "ESP", "ITA", "POL", "CAN"))
  }
}

lan_fig$scnt1 <- lan_fig$scnt1 + scale_y_continuous(name = "SCNT", limits = c(0, 125))
lan_fig$`log(trla1)` <- lan_fig$`log(trla1)` + scale_y_continuous(name = "TMT-A",
                                                                  limits = c(0, 240),
                                                                  breaks = seq(0, 240, 40),
                                                                  labels = seq(0, 240, 40))
lan_fig$`log(trlb1)` <- lan_fig$`log(trlb1)` + scale_y_continuous(name = "TMT-B",
                                                                  limits = c(0, 240),
                                                                  breaks = seq(0, 240, 40),
                                                                  labels = seq(0, 240, 40))

ggarrange(lan_fig$sdmt1, lan_fig$scnt1, lan_fig$swrt1, lan_fig$sit1,
          lan_fig$verfct5, lan_fig$verflt05, lan_fig$`log(trla1)`, lan_fig$`log(trlb1)`,
          ncol = 2, nrow = 4)

ggsave("figs/lan_fig.jpeg", height = 11, width = 8, device = "jpeg")


# ---- language plot with visble whiskers ----

con.lan <- lapply(1:length(f), function(i)
  conditional_effects(f[[i]],
                      effects = "language"))
for (i in c(6, 7)) {
  con.lan[[i]]$language$estimate__ <- exp(con.lan[[i]]$language$estimate__)
  con.lan[[i]]$language$upper__ <- exp(con.lan[[i]]$language$upper__)
  con.lan[[i]]$language$lower__ <- exp(con.lan[[i]]$language$lower__)
}

fig.lan <- list()

for (i in 1:length(con.lan)) {
  if (vars[[i]] == "log(trla1)" | vars[[i]] == "log(trlb1)") {
    dum <- cbind.data.frame(dat[[2]][ , "language"], dat[[2]][ , substr(vars[i], 5, 9)])
  } else {
    dum <- cbind.data.frame(dat[[2]][ , "language"], dat[[2]][ , vars[i]])
  }
  colnames(dum) <- c("x", "test")
  fig.lan[[i]] <- plot(con.lan[[i]], plot = F)[[1]] +
    scale_y_continuous(name = var_names[i],
                       #limits = c(0, NA)
                       ) +
    scale_x_discrete(name = "Language",
                     labels = c("ENG", "GER", "ESP", "FR", "CAN", "DUT", "ITA", "LA", "POL", "DAN")) +
    theme(axis.text.x = element_text(angle = 90))
}

names(fig.lan) <- vars

ggarrange(fig.lan$sdmt1, fig.lan$scnt1, fig.lan$swrt1, fig.lan$sit1,
          fig.lan$verfct5, fig.lan$verflt05, fig.lan$`log(trla1)`, fig.lan$`log(trlb1)`,
          ncol = 2, nrow = 4)

ggsave("figs/lan_fig.jpg", height = 11, width = 8, device = "jpeg")
