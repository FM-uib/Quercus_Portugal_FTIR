plsr_env <- function(data, form = "cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2", npc = 50, sel = c("prec", "temp", "srad", "elevation", "soil_m_0.07", "Latitude", "Longitude", "Group"), pls_method = "plsr"){
  require(pls)
  if(pls_method == "plsr"){
    pls_loo = plsr(reformulate(form), npc, data = data, scale = T, validation = "LOO")
  }else{
    pls_loo = cppls(reformulate(form), npc, data = data, scale = T, validation = "LOO")
  }
  plot_data = as.data.frame(pls_loo$scores[,])
  colnames(plot_data) = sapply(c(1:npc), function(x) paste0("PC",x))
  plot_data = cbind(plot_data, data[,sel])
  return(list("plsr" = pls_loo, "plot_data" = plot_data))
}

plsr_env_plot <- function(data, data_sub, pls, x, y, lds){
  require(ggplot2)
  require(stringr)
  require(grid)
  require(gridExtra)
  max_prec = ceiling(max(data_sub[,"prec"])/10)*10
  max_temp = ceiling(max(data_sub[,"temp"])/10)*10
  min_temp = floor(min(data_sub[,"temp"]))
  suber.prec.gg <- ggplot(data, aes_string(x, y)) +
    geom_point(size = 3, alpha = .1, show.legend = F) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    geom_point(data = data_sub, inherit.aes = F,  
               aes_string(x, y, shape = "Group", color = "prec", size = 5)) +
    scale_shape_manual(values = c(15,17,19,18))+
    scale_colour_gradient2(low = "#AFDFFF", mid = "#50B0FF",high = "#3347FF" , 
                           midpoint = max_prec/2, limits = c(0,max_prec), name = "Precipitation")+
    guides(size = F, shape = F) + theme_bw() + 
    theme(text = element_text(size = 18), legend.position = "left",
          axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)"))
  
  suber.lalo.gg <- ggplot(data, aes_string(x, y, color = "Latitude", alpha = "Longitude")) +
    geom_point(size = 6) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    theme_bw() + theme(text = element_text(size = 18),
                       axis.title.x=element_blank(),
                       axis.text.x=element_blank(),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank()) +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)")) +
    scale_color_gradient2(low = "red", mid = "green", high = "dark blue", 
                          midpoint = mean(data$Latitude), name = "Latitude") +
    scale_alpha_continuous(range = c(1,.1), breaks = c(-7,-8,-9), name = "Longitude")
  
  suber.temp.gg <-ggplot(data, aes_string(x, y)) +
    geom_point(size = 3, alpha = .1, show.legend = F) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    theme_bw() + theme(text = element_text(size = 18), legend.position = "left") +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)")) +
    geom_point(data = data_sub, inherit.aes = F,  
               aes_string(x, y, shape = "Group", color = "temp", size = 5)) +
    scale_shape_manual(values = c(15,17,19,18))+
    scale_colour_gradient2(low = "#7AFE47", mid = "#FEE547",high = "#FF2903" , 
                           midpoint = max_temp-(max_temp-min_temp)/2, limits = c(min_temp,max_temp), name = "Temperature")+
    guides(size = F, shape = F)
  
  suber.soil.gg <- ggplot(data, aes_string(x, y)) +
    geom_point(size = 3, alpha = .1, show.legend = F) + 
    coord_equal(ratio = 1, xlim = c(min(data[,x])-1,max(data[,x])+1), 
                ylim = c(min(data[,y])+1, max(data[,y])+1)) +
    theme_bw() + theme(text = element_text(size = 18),
                       axis.title.y=element_blank(),
                       axis.text.y=element_blank()) +
    xlab(paste0(x," (",round(explvar(pls)[as.numeric(str_sub(x, -1))],1)," %)")) + 
    ylab(paste0(y," (",round(explvar(pls)[as.numeric(str_sub(y, -1))],1)," %)")) +
    geom_point(data = data_sub, inherit.aes = F,  
               aes_string(x, y, shape = "Group", color = "soil_m_0.07", size = 5)) +
    scale_shape_manual(values = c(15,17,19,18))+
    scale_colour_gradient(low = "#FF9B75",high = "#70BAFA", 
                          limits = c(.2,.4), name = "Soil Moisture")+
    guides(size = F)
  grobz <- lapply(list(suber.prec.gg, suber.lalo.gg, suber.temp.gg, suber.soil.gg), ggplotGrob)
  grobz.plot <- arrangeGrob( grobs = list(rbind(grobz[[1]], grobz[[3]], size = "last"),
                                          rbind(grobz[[2]], grobz[[4]], size = "last")),
                             ncol = 2)
  return(grobz.plot)}

suber14_plsr <- plsr_env(suber, form = "cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2")
suber30_plsr <- plsr_env(suber30, form = "cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2")

suber14_plot <- plsr_env_plot(data = suber14_plsr[[2]], 
                              data_sub = subset(suber14_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                              pls = suber14_plsr[[1]], x = "PC2", y = "PC1", lds = TRUE)
suber30_plot <- plsr_env_plot(data = suber30_plsr[[2]], 
                              data_sub = subset(suber30_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                              pls = suber30_plsr[[1]], x = "PC2", y = "PC1", lds = TRUE)

grid.newpage()
grid.draw(suber14_plot)

grid.newpage()
grid.draw(suber30_plot)

suber$env <- I(scale(as.matrix(suber[,c("prec", "temp", "srad", "elevation", "Latitude", "Longitude")])))
suber30$env <- I(scale(as.matrix(suber30[,c("Latitude", "Longitude")])))#"prec", "temp", "srad", "elevation", 

suber14_ll_plsr <- plsr_env(suber, form = "env ~ FTIR", pls_method = "plsr")
suber30_ll_plsr <- plsr_env(suber30, form = "env ~ FTIR", pls_method = "cppls")

suber14_ll_plot <- plsr_env_plot(data = suber14_ll_plsr[[2]], 
                              data_sub = subset(suber14_ll_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                              pls = suber14_ll_plsr[[1]], x = "PC3", y = "PC1", lds = TRUE)
suber30_ll_plot <- plsr_env_plot(data = suber30_ll_plsr[[2]], 
                              data_sub = subset(suber30_ll_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                              pls = suber30_ll_plsr[[1]], x = "PC2", y = "PC1", lds = TRUE)

grid.newpage()
grid.draw(suber14_ll_plot)

grid.newpage()
grid.draw(suber30_ll_plot)


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
    print(paste0("no of test: ", sum(!data$train)))
    
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

folded_pls = fold_pls(data, 10)

conM = lapply(c(1:length(folded_pls)), function(x) prop.table(folded_pls[[x]]$conf_matrix[[4]]$table,{2}))

f1 <- function(lst, fun) {
  n <- length(lst)
  rc <- dim(lst[[1]])
  ar1 <- array(unlist(lst), c(rc, n))
  round(apply(ar1, c(1, 2), fun), 2)
  }
