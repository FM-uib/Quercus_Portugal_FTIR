library(here)
library(pls)
library(MASS)
library(caret)

source(here("R", "functions", "03_pls_analysis.R"))
data = readRDS(file = here("Data", "Output", "data_mean.rds"))


folded_pls = fold_pls(data, 10, npc = 20)

conM = lapply(c(1:length(folded_pls)), function(x) prop.table(folded_pls[[x]]$conf_matrix[[4]]$table,{2}))
Expl_Var = colMeans(t(sapply(c(1:length(folded_pls)), function(x) explvar(folded_pls[[x]]$'fitted model'))))

f1 <- function(lst, fun) {
  n <- length(lst)
  rc <- dim(lst[[1]])
  ar1 <- array(unlist(lst), c(rc, n))
  round(apply(ar1, c(1, 2), fun), 2)
}

### Hierarchical
tmp = subset(data, Section == "Ilex")
tmp$Sub_Spec = factor(tmp$Sub_Spec)
tmp$Species.HO <- I(model.matrix(~Sub_Spec-1, tmp))
tmp_pls = cppls(Species.HO ~ FTIR.SG2, npc = 20, data = tmp, center = T, scale = T)

### Environmental Variables

env14 = readRDS(file = here("Data", "Output", "env_WS_kriged_14.rds"))
env30 = readRDS(file = here("Data", "Output", "env_WS_kriged_30.rds"))
data14 <- cbind(data,env14[,-1])
data30 <- cbind(data,env30[,-1])

