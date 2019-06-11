pls_R2 <- function(pls_object, truth, comp = 1) {
  SS_tot = colSums((matrix(rep(colMeans(truth),nrow(truth)),nrow = nrow(truth),byrow = T) - truth)^2)
  SS_res = colSums(pls_object$residuals[,,comp]^2)
  R2 = 1 - SS_res/SS_tot
  return(list(R2,SS_res, SS_tot))
}

p_values <- function(pls_object, truth, comp = 1) {
  SS_res = (pls_object$residuals[,,comp]^2)  
  if(comp == 1){
    SS_tot = ((matrix(rep(colMeans(truth),nrow(truth)),nrow = nrow(truth),byrow = T) - truth)^2)
    res = sapply(c(1:ncol(truth)),function(x) t.test(SS_res[,x], SS_tot[,x])$p.value)
  }else{
    SS_res_2 = (pls_object$residuals[,,comp-1]^2)
    res = sapply(c(1:ncol(truth)),function(x) t.test(SS_res[,x], SS_res_2[,x])$p.value)
  }
  return(res)
}

library(flextable)
l = lapply(c(1:10), function(x) p_values(suber14_ll_plsr[[1]], suber$env ,comp = x))
env_pvalues = as.data.frame(do.call(cbind, l))
colnames(env_pvalues) = sapply(c(1:10), function(x) paste0("PC ",x))
env_pvalues$Variables = colnames(suber$env)
env_pvalues = env_pvalues[,c(11,1:10)]
ft = flextable(env_pvalues)
print(ft, preview = "docx")

l = lapply(c(1:10), function(x) p_values(pls.loo, data$Species.HO ,comp = x))
pls.loo_pvalues = as.data.frame(do.call(cbind, l))
colnames(pls.loo_pvalues) = sapply(c(1:10), function(x) paste0("PC ",x))
pls.loo_pvalues$Species = colnames(data$Species.HO)
pls.loo_pvalues = pls.loo_pvalues[,c(11,1:10)]
ft = flextable(pls.loo_pvalues)
print(ft, preview = "docx")

l = lapply(c(1:10), function(x) pls_R2(pls_object = pls.loo, truth = data$Species.HO, comp = x))
pls_wold_test = as.data.frame(do.call(cbind, l))

mod <- plsr(sensory ~ chemical, data = oliveoil, validation = "LOO", jackknife = TRUE)
comp_1 = pls_R2(mod, oliveoil$sensory)
jack.test(mod, ncomp = 2)
MSEP()
RMSEP()
R2()

comp_1 = pls_R2(suber14_ll_plsr[[1]], suber$env, comp = 1)
comp_2 = pls_R2(suber14_ll_plsr[[1]], suber$env, comp = 2)
comp_3 = pls_R2(suber14_ll_plsr[[1]], suber$env, comp = 3)
comp_4 = pls_R2(suber14_ll_plsr[[1]], suber$env, comp = 4)
comp_5 = pls_R2(suber14_ll_plsr[[1]], suber$env, comp = 5)

unAsIs <- function(X) {
  if("AsIs" %in% class(X)) {
    class(X) <- class(X)[-match("AsIs", class(X))]
  }
  X
}


#Test Trend
suber_xy = suber[,c(6,7)]
suber_rda <- rda(scale(suber$FTIR), suber_xy)
anova(suber_rda)

#Create DBMEM
suber_det = resid(lm(scale(suber$FTIR) ~ . , data = suber_xy))

tmp = dbmem(suber_xy, silent = F)
suber_dbmem = as.data.frame(tmp)
suber_env = (suber[,c("prec", "temp", "srad", "elevation")])


# Forward selection of the environmental variables
suber_env_rda <- vegan::rda(scale(suber$FTIR) ~ . , data = suber_env)
(suber_env_r2a <- RsquareAdj(suber_env_rda)$adj.r.squared)
suber_env_fwd <- 
  forward.sel(scale(suber$FTIR), suber_env,
              adjR2thresh = suber_env_r2a, 
              nperm = 9999)
env_sign <- sort(suber_env_fwd$variables)
env_red <- as.data.frame(suber_env[ ,c(env_sign)])
colnames(env_red) = env_sign

# 3. Test and forward selection of the dbMEM variables
# Run the global dbMEM analysis on the *detrended* mite data
suber_det_dbmem_rda <- rda(suber_det ~., suber_dbmem)
anova(suber_det_dbmem_rda)
# Since the analysis is significant, compute the adjusted R2
# and run a forward selection of the dbMEM variables
(suber_det_dbmem_r2a <- 
    RsquareAdj(suber_det_dbmem_rda)$adj.r.squared)
(suber_det_dbmem_fwd <- 
    forward.sel(suber_det, 
                as.matrix(suber_dbmem), 
                adjR2thresh = suber_det_dbmem_r2a))
# Number of significant dbMEM
(nb_sig_dbmem <- nrow(suber_det_dbmem_fwd))
# Identify the significant dbMEM sorted in increasing order
(dbmem_sign <- sort(suber_det_dbmem_fwd$order))
# Write the significant dbMEM to a new object (reduced set)
dbmem_red <- suber_dbmem[ ,c(dbmem_sign)]

(suber_varpart <- 
    varpart(scale(suber$FTIR), suber_xy, env_red, dbmem_red))
dev.new(
  title = "Suber - environment - dbMEM variation partitioning", 
  width = 12, 
  height = 6, 
  noRStudioGD = TRUE
)
# Show the symbols of the fractions and plot their values
par(mfrow = c(1,2))
showvarparts(3, bg = c("red", "blue", "yellow", "green"))
plot(suber_varpart, 
     digits = 2, 
     bg = c("red", "blue", "yellow", "green")
)





# Create DBMEM
mite.h.det <- resid(lm(as.matrix(mite.h) ~ ., data = mite.xy))

## Step 1. Construct the matrix of dbMEM variables
mite.dbmem.tmp <- dbmem(mite.xy, silent = FALSE)
mite.dbmem <- as.data.frame(mite.dbmem.tmp)

###Variance Partitioning

## Mite - trend - environment - dbMEM variation partitioning

# 1. Test trend
mite.XY.rda <- rda(mite.h, mite.xy)
anova(mite.XY.rda)

# 2. Test and forward selection of the environmental variables
# Recode environmental variables 3 to 5 into dummy binary variables
substrate <- model.matrix( ~ mite.env[ ,3])[ ,-1]
shrubs <- model.matrix( ~ mite.env[ ,4])[ ,-1]
topography <- model.matrix( ~ mite.env[ ,5])[ ,-1]
mite.env2 <- cbind(mite.env[ ,1:2], substrate, shrubs, topography)
colnames(mite.env2) <- 
  c("SubsDens", "WatrCont", "Interface", "Litter", "Sphagn1",
    "Sphagn2", "Sphagn3", "Sphagn4", "Shrubs_Many", "Shrubs_None", 
    "topography")
# Forward selection of the environmental variables
mite.env.rda <- rda(mite.h ~., mite.env2)
(mite.env.R2a <- RsquareAdj(mite.env.rda)$adj.r.squared)
mite.env.fwd <- 
  forward.sel(mite.h, mite.env2,
              adjR2thresh = mite.env.R2a, 
              nperm = 9999)
env.sign <- sort(mite.env.fwd$order)
env.red <- mite.env2[ ,c(env.sign)]
colnames(env.red)

# 3. Test and forward selection of the dbMEM variables
# Run the global dbMEM analysis on the *detrended* mite data
mite.det.dbmem.rda <- rda(mite.h.det ~., mite.dbmem)
anova(mite.det.dbmem.rda)
# Since the analysis is significant, compute the adjusted R2
# and run a forward selection of the dbMEM variables
(mite.det.dbmem.R2a <- 
    RsquareAdj(mite.det.dbmem.rda)$adj.r.squared)
(mite.det.dbmem.fwd <- 
    forward.sel(mite.h.det, 
                as.matrix(mite.dbmem), 
                adjR2thresh = mite.det.dbmem.R2a))
# Number of significant dbMEM
(nb.sig.dbmem <- nrow(mite.det.dbmem.fwd))
# Identify the significant dbMEM sorted in increasing order
(dbmem.sign <- sort(mite.det.dbmem.fwd$order))
# Write the significant dbMEM to a new object (reduced set)
dbmem.red <- mite.dbmem[ ,c(dbmem.sign)]

# 4. Arbitrarily split the significant dbMEM into broad and 
#    fine scale
# Broad scale: dbMEM 1, 3, 4, 6, 7
dbmem.broad <- dbmem.red[ , 1 : 5]
# Fine scale: dbMEM 10, 11, 20
dbmem.fine <- dbmem.red[ , 6 : 8]

## 5. Mite - environment - trend - dbMEM variation partitioning
(mite.varpart <- 
    varpart(mite.h, env.red, mite.xy, dbmem.broad, dbmem.fine))
dev.new(
  title = "Mite - environment - dbMEM variation partitioning", 
  width = 12, 
  height = 6, 
  noRStudioGD = TRUE
)
# Show the symbols of the fractions and plot their values
par(mfrow = c(1,2))
showvarparts(4, bg = c("red", "blue", "yellow", "green"))
plot(mite.varpart, 
     digits = 2, 
     bg = c("red", "blue", "yellow", "green")
) 
# Tests of the unique fractions [a], [b], [c] and [d]
# Fraction [a], pure environmental
anova(
  rda(mite.h, env.red, cbind(mite.xy, dbmem.broad, dbmem.fine))
)
# Fraction [b], pure trend
anova(
  rda(mite.h, mite.xy, cbind(env.red, dbmem.broad, dbmem.fine))
)
# Fraction [c], pure broad scale spatial
anova(rda
      (mite.h, dbmem.broad, cbind(env.red, mite.xy, dbmem.fine))
)
# Fraction [d], pure fine scale spatial
anova(
  rda(mite.h, dbmem.fine, cbind(env.red, mite.xy, dbmem.broad))
)