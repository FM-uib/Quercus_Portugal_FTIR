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

fold_pls <- function(data, folds, split = .6, npc = 20){
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
    
    while(length(unique(data[data$train,"Sub_Spec"])) < 6 | length(unique(data[!data$train,"Sub_Spec"])) < 6){
      sel_train = sample(c(1:nrow(data)),signif(nrow(data)*split,2))
      data$train = FALSE
      data$train[sel_train] = TRUE
      print("Reshuffle")
    }
  
    #do PLS
    pls_loo <- cppls(Species.HO ~ FTIR.SG2, npc, data = data[data$train,], scale = T, validation = "LOO")
    print("Done")
    pls_loo_eval <- evaluate_pls(npc, pls_loo, data, train = T)
    pls_loo_eval[["fitted model"]] = pls_loo
    folds_res[[paste0("Fold",i)]] = pls_loo_eval
  }
  return(folds_res)
}