library(pls)
library(MASS)
library(caret)

fold_pls <- function(data, folds, split = .6, npc = 20, pls = T){
  folds_res = list()
  for(i in 1:folds){
    print(paste0("Training fold ",i," of ", folds))
    #draw new train samples
    sel_train = sample(c(1:nrow(data)),signif(nrow(data)*split,2))
    #assign train variable
    data$train = FALSE
    data$train[sel_train] = TRUE
    print(paste0("No. of test samples: ", sum(!data$train)))
    
    no_sp = length(unique(data[,"Sub_Spec"]))
    
    while(length(unique(data[data$train,"Sub_Spec"])) < no_sp | length(unique(data[!data$train,"Sub_Spec"])) < no_sp){
      sel_train = sample(c(1:nrow(data)),signif(nrow(data)*split,2))
      data$train = FALSE
      data$train[sel_train] = TRUE
      print("Reshuffle")
    }
    
    if(pls){
          #do PLS
      pls_loo <- cppls(Species.HO ~ FTIR.SG2, npc, data = data[data$train,], scale = T, validation = "LOO")
      print("Done")
      pls_loo_eval <- evaluate_pls(npc, pls_loo, data, train = T)
      pls_loo_eval[["fitted model"]] = pls_loo
      folds_res[[paste0("Fold",i)]] = pls_loo_eval
    }else{
      spls_fit = spls(data$FTIR.SG2[data$train,], data$Species.HO[data$train,], eta = .9, K = 9)
      spls_pred = predict(spls_fit, type = "fit", newx = data$FTIR.SG2[!data$train,])
      conM = confusionMatrix(factor(max.col(spls_pred), levels = c(1:no_sp)), factor(max.col(data$Species.HO[!data$train,]), levels = c(1:no_sp)))
      folds_res[[paste0("Fold",i)]] = conM
    }

  }
  return(folds_res)
}

mean_matrix <- function(lst, fun) {
  n <- length(lst)
  rc <- dim(lst[[1]])
  ar1 <- array(unlist(lst), c(rc, n))
  matrix_res = round(apply(ar1, c(1, 2), fun), 2)
  rownames(matrix_res) = c("fag.", "rob", "r. est.", "coc", "rot", "sub")
  colnames(matrix_res) = c("fag.", "rob", "r. est.", "coc", "rot", "sub")
  return(matrix_res)
}