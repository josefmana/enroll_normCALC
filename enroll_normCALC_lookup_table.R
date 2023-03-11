setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory to the one in which this
                                                          # script is located
dir <- getwd()

library(brms) # load packages
fit <- readRDS("mods/splines_no_int_mods.rds") # load the models

# function to create the tables and save them as one csv for each test
lookup_tab <- function(var_name, model, data, filename, age, edu) {
  # levels of language
  lang <- c("1-US_English", "2-German", "3-Spanish", "4-French", "5-Canadian French", "6-Dutch", "7-Italian",
            "8-Spanish_LA", "9-Polish", "10-Danish")
  # levels of gender
  sex <- c("female", "male")
  # prepare the dummy data set
  combs <- expand.grid(c(age),       # age
                       c(edu),       # education
                       c(lang, NA),  # language
                       c(sex, NA),   # sex
                       data)         # test values
  colnames(combs) <- c("age", "edu", "language", "sex", var_name)
  
  # create a table with:
  # 1) demographic variables (columns 1:4) == calculator inputs
  # 2) dummy variable to calculate predictions (column 5)
  # 3) test score (column 6) == calculator input (one for each test/csv look-up table)
  # 4) models predictions (columns 7:9) == needed to calculate z-scores
  # 5) z scores (column 10) == calculator output
  tab <- data.frame(combs[ , 1:5],
                    y = combs[ , var_name], # observed test values
                    y_hat = NA, # predicted test values, will be calculated below
                    sigma_i_sq = NA, # individual variance, will be calculated below
                    sigma_n_sq = rep(VarCorr(model)[["residual__"]][["sd"]][[ , "Estimate"]]^2,
                                     nrow(combs)), # model variance, was just calculated
                    z = NA, # z-score == the output
                    perc = NA) # percentile from standard normal == the output
  if (var_name == "log(trla1)" | var_name == "log(trlb1)") {
    tab <- transform(tab, y = log(y)) # logarithmic transformation for trla1 and trlb1
  }
  
  options(contrasts = c("contr.sum", "contr.poly")) # change coding of factor to be the same as in the model
  
  # now calculate the predictions
  dum <- fitted(model, newdata = tab) # predict y_hat and sigma_i_sq for all of combinations of demographic
                                      # variables and test scores specified above (the combs object)
  tab$y_hat <- dum[ , "Estimate"]
  tab$sigma_i_sq <- dum[ , "Est.Error"]^2
  tab <- transform(tab, z = (y - y_hat) / sqrt(sigma_i_sq + sigma_n_sq))
  
  if (var_name == "log(trla1)" | var_name == "log(trlb1)") {
    tab <- transform(tab, z = -z) # for trla1 and trlb1 the z values need to be flipped in order for higher
                                  # values to mean better performance
    tab <- transform(tab, y = exp(y)) # back-transforming log observed values
  }
  
  tab$perc <- 100*pnorm(tab$z, 0, 1) # calculate percentile for each z-score
  
  write.table(tab[, c(1:4, 6, 10, 11)], paste0(filename, ".csv"), sep = ",", row.names = F, col.names = T,
              append = T, quote = F)
  #return(tab)
}

# get ready the ranges for each variable/test
# define possible "measurable" values for each test
# format is 'test = seq (min, max, step)'
data <- list(sdmt1 = seq(0, 150, 1),
             verfct5 = seq(0, 50, 1),
             scnt1 = seq(0, 200, 1),
             swrt1 = seq(0, 200, 1),
             sit1 = seq(0, 200, 1),
             trla1 = seq(5, 240, 1),
             trlb1 = seq(10, 240, 1),
             verflt05 = seq(0, 180, 1)
             )

# create all combinations of age and education
eduage <- expand.grid(c(seq(18, 90, 1)), # age
                      c(seq(0, 25, 1))   # education
                      )

# run for tmt_a
system.time(
  for (i in 1:nrow(eduage)) {
    lookup_tab(var_name = "log(trla1)", model = fit$`log(trla1)`, data = data$trla1, file = "tmt_a",
               age = eduage[i, 1], edu = eduage[i, 2])
  }
)
# run for tmt_b
system.time(
  for (i in 1:nrow(eduage)) {
    lookup_tab(var_name = "log(trlb1)", model = fit$`log(trlb1)`, data = data$trlb1, file = "tmt_b",
               age = eduage[i, 1], edu = eduage[i, 2])
  }
)

# writes a single table for each cognitive test with 6 columns:
# age, edu, language, sex and y need to be provided as the input to the calculator (y is observed test score)
# z is supposed to be the outcome of the calculator, one z for each test
