evaluate_pls <- function(npc, trained, data, train = TRUE){
  error <- rep(0, npc)
  con_M <- list()
  if (train){
    tested <- predict(trained, newdata = data[!data$train,], type = "score")
    for (i in 1:npc) {
      fitdata <- data.frame(Species = data$Sub_Spec[data$train], 
                            FTIR.score = I(trained$scores[,1:i, drop = FALSE]))
      testdata <- data.frame(Species = data$Sub_Spec[!data$train],
                             FTIR.score = I(tested[,1:i,drop=FALSE]))
      error[i] <- (nrow(tested) - sum(predict(lda(Species ~ FTIR.score, data = fitdata),
                                              newdata = testdata)$class == testdata$Species))/nrow(tested)
      con_M[[i]] <- I(confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata), 
                                              newdata = testdata)$class, testdata$Species))
    }
    result <- list("error" = error, "conf_matrix" = con_M, "fitdata" = fitdata, "testdata" = testdata)
    return(result)
  }else{
    for (i in 1:npc) {
      fitdata <- data.frame(Species = data$Sub_Spec, 
                            FTIR.score = I(trained$scores[,1:i, drop = FALSE]))}
    result <- list("fitdata" = fitdata)
    return(result)
  }
}

fold_pls <- function(data, folds, split = .6, npc = 20, pls = T){
  require(pls)
  require(MASS)
  require(caret)
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