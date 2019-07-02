plsr_env <- function(data, form = "cbind(prec, temp, srad, elevation, soil_m_0.07, Latitude, Longitude) ~ FTIR.SG2", npc = 50, sel = c("prec", "temp", "srad", "elevation", "soil_m_0.07", "Latitude", "Longitude", "Group"), pls_method = "plsr"){
  require(pls)
  if(pls_method == "plsr"){
    pls_loo = plsr(reformulate(form), npc, data = data, scale = T, validation = "LOO", jackknife = T)
  }else{
    pls_loo = cppls(reformulate(form), npc, data = data, scale = T, validation = "LOO", jackknife = T)
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
               aes_string(x, y, shape = "Group", color = "elevation", size = 5)) +
    scale_shape_manual(values = c(15,17,19,18))+
    scale_colour_gradient(low = "#70BAFA",high = "#FF9B75", 
                          limits = c(min(data_sub[,"elevation"]),max(data_sub[,"elevation"])), name = "Elevation")+
    guides(size = F)
  grobz <- lapply(list(suber.prec.gg, suber.lalo.gg, suber.temp.gg, suber.soil.gg), ggplotGrob)
  grobz.plot <- arrangeGrob( grobs = list(rbind(grobz[[1]], grobz[[3]], size = "last"),
                                          rbind(grobz[[2]], grobz[[4]], size = "last")),
                             ncol = 2)
  return(grobz.plot)}

data = readRDS(file = here("Data", "Input", "data_meaned.rds"))
env14 = readRDS(file = here("Data", "Output", "env_WS_kriged_14.rds"))
env30 = readRDS(file = here("Data", "Output", "env_WS_kriged_30.rds"))

soil_moist = readRDS(file = here("Data", "Output", "soil_moisture.rds"))


data14 <- cbind(data,dplyr::full_join(env14,soil_moist)[,-1])
data30 <- cbind(data,dplyr::full_join(env30,soil_moist)[,-1])

suber = subset(data14, Species == "suber")
suber30 = subset(data30, Species == "suber")

suber$env <- I(scale(as.matrix(suber[,c("prec", "temp", "srad", "elevation", "Latitude", "Longitude")])))
suber30$env <- I(scale(as.matrix(suber30[,c("prec", "temp", "srad", "elevation", "Latitude", "Longitude")])))

suber14_ll_plsr <- plsr_env(suber, form = "env ~ FTIR", pls_method = "cppls")
suber30_ll_plsr <- plsr_env(suber30, form = "env ~ FTIR", pls_method = "cppls")

suber14_ll_plot <- plsr_env_plot(data = suber14_ll_plsr[[2]], 
                              data_sub = subset(suber14_ll_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                              pls = suber14_ll_plsr[[1]], x = "PC1", y = "PC2", lds = TRUE)
suber14_ll_plot_2 <- plsr_env_plot(data = suber14_ll_plsr[[2]], 
                                 data_sub = subset(suber14_ll_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                                 pls = suber14_ll_plsr[[1]], x = "PC3", y = "PC4", lds = TRUE)
suber30_ll_plot <- plsr_env_plot(data = suber30_ll_plsr[[2]], 
                              data_sub = subset(suber30_ll_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                              pls = suber30_ll_plsr[[1]], x = "PC1", y = "PC2", lds = TRUE)
suber30_ll_plot_2 <- plsr_env_plot(data = suber30_ll_plsr[[2]], 
                                 data_sub = subset(suber30_ll_plsr[[2]], Group == "Arrabida" | Group == "Alijo" | Group == "Porto"| Group == "Coimbra"),
                                 pls = suber30_ll_plsr[[1]], x = "PC3", y = "PC4", lds = TRUE)

grid.newpage()
env_14 = grid.draw(suber14_ll_plot)
ggsave("env_14.png", plot = grid.draw(suber14_ll_plot), device = "png", path = here("R", "figures"), width = 40, height = 30, units = "cm", dpi = 600)
ggsave("env_14_2.png", plot = grid.draw(suber14_ll_plot_2), device = "png", path = here("R", "figures"), width = 40, height = 30, units = "cm", dpi = 600)

grid.newpage()
grid.draw(suber30_ll_plot)

ggsave("env_30.png", plot = grid.draw(suber30_ll_plot), device = "png", path = here("R", "figures"), width = 40, height = 30, units = "cm", dpi = 600)
ggsave("env_30_2.png", plot = grid.draw(suber30_ll_plot_2), device = "png", path = here("R", "figures"), width = 40, height = 30, units = "cm", dpi = 600)

suber14_ll_sm = plot_env_supp(suber14_ll_plsr[[1]],suber$env)
suber30_ll_sm = plot_env_supp(suber30_ll_plsr[[1]],suber30$env)

env_expl_var = grid.draw(rbind(ggplotGrob(suber14_ll_sm[[2]]),ggplotGrob(suber30_ll_sm[[2]])))
ggsave("env_expl_var.png", plot = grid.draw(rbind(ggplotGrob(suber14_ll_sm[[2]]),ggplotGrob(suber30_ll_sm[[2]]))), device = "png", path = here("R", "figures"), width = 10, height = 20, units = "cm", dpi = 600)

suber14_ll_sm = plot_env_supp(suber14_ll_plsr[[1]],suber$env, comp = 6)
ggsave("env_obs_pre.png", plot = suber14_ll_sm[[1]], device = "png", path = here("R", "figures"), width = 15, height = 10, units = "cm", dpi = 600)


plot_env_supp <- function(fitted_pls, measured, comp = 4){
  require(pls)
  require(ggplot2)
  require(reshape2)
  df = data.frame(PC = c(1:ncol(fitted_pls$scores)),
                  expl_var = explvar(fitted_pls))
  Expl_Var_Cum = ggplot(data = df, aes(PC, cumsum(expl_var))) + geom_line() +
    theme_bw() + theme(text = element_text(size = 18)) +
    xlab("No. of Components") + ylab("Explained Variance in %")
  df2 = data.frame(ID = rep("fitted", 69), suber14_ll_plsr[[1]]$fitted.values[,,comp])
  df3 = data.frame(ID = rep("measured", 69), unclass(measured))
  df2 = melt(df2, id.vars = "ID")
  colnames(df2)[3] = "Fitted"
  df3 = melt(df3, id.vars = "ID")
  df2 = cbind(df2[,-c(1)],df3[,-c(1,2)])
  colnames(df2)[3] = "Measured"
  df2$variable <- factor(df2$variable)
  levels(df2$variable) = c("Precipitation", "Temperature", "Solar Radiation", "Elevation", "Latitude", "Longitude")
  tmp = ggplot(df2, aes(Measured, Fitted)) + 
    geom_abline(slope = 1, intercept = 0, alpha = .5) + geom_point() + facet_wrap(~variable) +
    theme_bw() + theme(legend.position = "none")
  return(list(tmp,Expl_Var_Cum))
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
Expl_Var = colMeans(t(sapply(c(1:length(folded_pls)), function(x) explvar(folded_pls[[x]]$'fitted model'))))

f1 <- function(lst, fun) {
  n <- length(lst)
  rc <- dim(lst[[1]])
  ar1 <- array(unlist(lst), c(rc, n))
  round(apply(ar1, c(1, 2), fun), 2)
  }


tmp = data.frame(Comp = c(rep("PC1",5), rep("PC2",5)),Variable = rep(c("temp", "prec","srad","lat", "lon"),2), value = c(rnorm(5),rnorm(5,2)))


loadings_plot <- function(pls_object, sel = c(1:4), yload=T){
  library(reshape2)
  
  peaks = data.frame(peaks_wn = as.numeric (c("1745", "1462", "721",
                                    "1651", "1641", "1551", "1535",
                                    "1107", "1076", "1055", "1028", "995",
                                    "1605", "1516", "1171", "852", "833", "816")),
                     peaks_c = as.character(c(rep("L",3), rep("P", 4), rep("C",5), rep("S",6))))
  peaks = peaks[order(peaks$peaks_wn, decreasing = T),]
  x_load = as.data.frame(pls_object$loadings[,])
  colnames(x_load) = sapply(c(1:ncol(x_load)), function(x) paste0("PC",x))
  x_load = x_load[,sel]
  x_load = x_load[round(as.numeric(rownames(x_load))) %in% peaks$peaks_wn, ]
  x_load$ID = paste(peaks$peaks_c,peaks$peaks_wn)
  #x_load$col = as.factor(peaks$peaks_c)
  x_load = melt(x_load, id.vars = "ID")
  x_load_gg = ggplot(x_load, aes(ID, value, fill = rep(peaks$peaks_c,4))) + geom_col() + 
    facet_wrap(~variable, ncol = 1) +
    xlab("Wavenumbers") + ylab("Loadings") +
    theme_bw() + theme(text = element_text(size = 18),
                       axis.text.x = element_text(angle = 90, hjust = 1),
                       legend.position = "top",
                       legend.direction = "horizontal") +
    scale_fill_discrete(name = "Compounds", labels = c("Carbohydrates", "Lipids", "Protein", "Sporopollenin"))
  if(yload){
    y_load = as.data.frame(pls_object$Yloadings[,])
    colnames(y_load) = sapply(c(1:ncol(y_load)), function(x) paste0("PC",x))
    y_load = y_load[,sel]
    y_load$ID = rownames(y_load)
    y_load = melt(y_load, id.vars = "ID")
    y_load_gg = ggplot(y_load, aes(ID, value)) + geom_col() + facet_wrap(~variable, ncol = 1) +
      xlab("Environmental Variables") + ylab("Loadings") +
      scale_x_discrete(limits = c("prec","temp","srad","elevation","Latitude","Longitude")) +
      theme_bw() + theme(text = element_text(size = 18))
    return(list(x_load_gg, y_load_gg))
  }else{
    return(x_load_gg)
  }
}

suber14_load = loadings_plot(suber14_ll_plsr[[1]])
ggsave("env_14_xload.png", plot = (suber14_load[[1]]), device = "png", path = here("R", "figures"), width = 20, height = 30, units = "cm", dpi = 600)
ggsave("env_14_yload.png", plot = (suber14_load[[2]]), device = "png", path = here("R", "figures"), width = 20, height = 30, units = "cm", dpi = 600)
