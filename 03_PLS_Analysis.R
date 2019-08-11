library(here)
library(pls)
library(MASS)
library(caret)

source(here("R", "functions", "03_pls_analysis.R"))
data = readRDS(file = here("Data", "Output", "data_mean.rds"))
rownames(data) = c(1:dim(data)[1])

folds = 10
folded_pls = fold_pls(data, folds, npc = 20)
saveRDS(folded_pls, file = here("Data", "Output", "folded_pls.rds"))

conM = lapply(c(1:length(folded_pls)), function(x) prop.table(folded_pls[[x]]$conf_matrix[[4]]$table,{2}))
Expl_Var = colMeans(t(sapply(c(1:length(folded_pls)), function(x) explvar(folded_pls[[x]]$'fitted model'))))

# mean recall of folds
mean_matrix(conM, mean)

# standard deviation recall of folds
mean_matrix(conM, sd)
