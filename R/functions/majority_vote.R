df = data.frame(ID = sort(rep(1:6,3)), pred = c(1,1,2,2,2,2,3,4,4,4,4,1,5,3,1,6,6,6))

df %>% 
  count(ID, pred) %>%
  group_by(ID) %>%
  arrange(ID, desc(n)) %>%
  summarise(max_pred = first(pred), n = first(n))

majority_voting = function(prediction, SID){
  #' Determines the majority identity of replicate predictions
  #'
  #' @description This function determins the majority prediction out of multiple replicate predictions.
  #' Prediction with the highest vote count wins. Prediction is vector of predictions, while SID is vector
  #' of corresponding sample identity/number.
  #' 
  #' @param prediction vector. Numeric or character vector of predictions.
  #' @param SID vector. Numeric or character vector of sample identity .
  #' 
  #' @return returns dataframe with sample identity, majority voted prediction and count of winning prediction.
  require(dplyr)
  df = data.frame(SID = SID,
                  pred = prediction)
  result = df %>% 
    count(SID, pred) %>%
    group_by(SID) %>%
    arrange(SID, desc(n)) %>%
    summarise(max_pred = first(pred), n = first(n))
  return(result)
}
