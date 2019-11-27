library(here)

source(here("R", "functions", "02_pls_analysis.R"))
data_mean = readRDS(file = here("Data", "Output", "data_mean.rds"))
#rownames(data) = c(1:dim(data)[1])


folded_pls = fold_pls(data_mean, 100, npc = 20)
saveRDS(folded_pls, file = here("Data", "Output", "folded_pls.rds"))

confusion_matrix_4comp = lapply(c(1:length(folded_pls)), function(x) prop.table(folded_pls[[x]]$conf_matrix[[4]]$table,{2}))
confusion_matrix_10comp = lapply(c(1:length(folded_pls)), function(x) prop.table(folded_pls[[x]]$conf_matrix[[10]]$table,{2}))
Expl_Var = colMeans(t(sapply(c(1:length(folded_pls)), function(x) explvar(folded_pls[[x]]$'fitted model'))))

# 4 Components
# mean recall and standard deviation of folds
mean_matrix(confusion_matrix_4comp, mean)
mean_matrix(confusion_matrix_4comp, sd)

# 10 Components
# mean recall and standard deviation of folds
mean_matrix(confusion_matrix_10comp, mean)
mean_matrix(confusion_matrix_10comp, sd)
