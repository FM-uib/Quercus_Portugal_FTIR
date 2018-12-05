setwd("O:/PhD/Data/Portugal 2018/paper")
library(vegan)
library(ggplot2)

load(file = "data.rda")
load(file = "pls.rda")
load(file = "pca_ord_surf.rda")

ord.surf <- function(x, y){
  ord.surf <- ordisurf(x = x, y = y, plot = FALSE)
  grid <- ord.surf$grid
  ordi <- expand.grid(x = grid$x, y = grid$y)
  ordi$z <- as.vector(grid$z)
  ordi <- data.frame(na.omit(ordi))
  return(ordi)
}

ord.surf.test <- ord.surf(x = pca.scores, y = subset$mean_precip)

pls.scores <- scores(pls.train, choices = c(1,2))
ord.surf.pls.train <- ord.surf(x = pls.scores, y = subset$mean_precip[subset$train])
ggplot(ord.surf.pls.train, aes(x,y, colour = z))+geom_point()