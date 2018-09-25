library(plyr)
library(baseline)
library(prospectr)
library(ggplot2)
library(ggbiplot)
library(reshape2)
library(EMSC)


read.files<-function(file){
  f <- readLines(file)
  Q <- read.csv(text = f[c(-2,-3)], 
               header = T, 
               nrows = length(f)-2)
  Q2<-as.matrix(Q[,1:3318])
  row.names(Q2)<-Q$Sample.Name
  return(Q2)
}

pca_plot <- function(pca_data, groups){
  g <- ggbiplot(pca_data, obs.scale = 1, var.scale = 1, 
                groups = groups, ellipse = TRUE, 
                circle = TRUE,
                var.axes = F, choices = 1:2)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
  png(filename = paste(deparse(substitute(pca_data)),"_PC_1+2.png", sep=""), width = 1000, height = 1000, res = 150)
  print(g)
  dev.off()
  
  g <- ggbiplot(pca_data, obs.scale = 1, var.scale = 1, 
                groups = groups, ellipse = TRUE, 
                circle = TRUE,
                var.axes = F, choices = 3:4)
  png(filename = paste(deparse(substitute(pca_data)),"_PC_3+4.png", sep=""), width = 1000, height = 1000, res = 150)
  print(g)
  dev.off()
  
  g <- ggbiplot(pca_data, obs.scale = 1, var.scale = 1, 
                groups = groups, ellipse = TRUE, 
                circle = TRUE,
                var.axes = F, choices = 5:6)
  png(filename = paste(deparse(substitute(pca_data)),"_PC_5+6.png", sep=""), width = 1000, height = 1000, res = 150)
  print(g)
  dev.off()  
}
setwd("O:/PhD/Data/Portugal 2018")
load("Samples.rda") #Load Frame with sample info
load("Samples.df.rda")

setwd("./FTIR") #Change to FTIR Folder


files <- list.files(pattern = "*.csv")

Querc <- lapply(files, function(x) read.files(x)) #read files as matrix
Querc <- do.call(rbind, Querc) #merge matrices
colnames(Querc)<-seq(7000,600,-1.928873)
save(Querc, file = "Quercus_red_set.rda") #save data for faster loading


load("Quercus_red_set.rda")

Querc.df<-melt(as.data.frame(Querc))
Querc.df$ID<-row.names(Querc)
colnames(Querc.df)<-c("Wavelength","Absorbance", "ID")
Querc.df$Wavelength<-as.numeric(as.character(Querc.df$Wavelength))

df2<-join(df,Portugal,by="ID",type="inner") #full for full join inner only joins matching records
df1<-df2[,c(6,4,1:3,7:12,16,17,13:15,5)]
df1<-rename(df1,c("rows"="Sample.Name"))
samples.df<-df1


save(Querc.df, file = "Querc_red_set_df.rda")

#example plot
ggplot_spectra<-function(spectra, pattern, wavel, rows=row.names(Querc)){
  df<-melt(as.data.frame(spectra))
  df$ID<-rows
  colnames(df)<-c("Wavelength","Absorbance", "ID")
  df$Wavelength<-as.numeric(as.character(df$Wavelength))
  d<-subset(df, grepl(pattern,df$ID) & Wavelength < wavel)
  ggplot(data = d, aes(Wavelength, Absorbance))+#, colour = ID))+
    geom_line()
}

d<-subset(Querc.df, grepl("ACo_Qcoc_01",Querc.df$ID) & Wavelength < 6000)
ggplot(data = d, aes(Wavelength, Absorbance, colour = ID))+
  #xlim(4000,7000)+
  #ylim(0,0.2)+
  geom_line()
ggplot(data = d, aes(wavelength, d, color = "mine"))+geom_line()+
  geom_line(data = e, aes(V2,V1, color = "boris"))

baseline_correction<-function(data){
  base<-baseline(data, method = "lowpass")#[,600:3318]
  data.corr<-base@corrected
  df<-melt(as.data.frame(data.corr))
  df$ID<-row.names(data.corr)
  colnames(df)<-c("Wavelength","Absorbance", "ID")
  df$Wavelength<-as.numeric(as.character(df$Wavelength))
  return(df)
}

#Subset data
selection <- !grepl("Qfagx",samples.df$Sample.Name)&!grepl("Qlst",samples.df$Sample.Name) #removing faginea hybrids and lusitanica
Querc <- samples.df$FTIR[selection,]
colnames(Querc)<-round(as.numeric(colnames(Querc)))
save(Querc,file="Querc.rda")


load("Querc.no_fagx_lst.rda") #set without faginea X and lusitanica
#PCA over reduced species set org spectra and 1st deriv
Querc.EMSC<-EMSC(Querc[,2695:3276]) #Spectra from 700 (3276) 750 (3240) 1800 (2695) to 2000 (2600) 4000 (1550)
pca.EMSC.data<-Querc.EMSC$corrected
samples<-row.names(pca.EMSC.data)
row.names(pca.EMSC.data)<-c(1:nrow(pca.EMSC.data))

pca.EMSC.SG.1d.data<-savitzkyGolay(pca.EMSC.data,1,3,11)
pca.EMSC.SG.2d.data<-savitzkyGolay(pca.EMSC.data,2,3,11)

pca.EMSC<-prcomp(pca.EMSC.data,
                 center = T,
                 scale = T)
loadings.EMSC<-pca.EMSC$rotation
plot(row.names(loadings.EMSC),loadings.EMSC[,1],type = "l")

pca.EMSC.SG.1d<-prcomp(pca.EMSC.SG.1d.data,
                       center = T,
                       scale = T)
loadings.EMSC.SG.1d<-pca.EMSC.SG.1d$rotation
plot(row.names(loadings.EMSC.SG.1d),loadings.EMSC.SG.1d[,1],type = "l")

pca.EMSC.SG.2d<-prcomp(pca.EMSC.SG.2d.data,
                       center = T,
                       scale = T)
loadings.EMSC.SG.2d<-pca.EMSC.SG.2d$rotation
plot(row.names(loadings.EMSC.SG.2d),loadings.EMSC.SG.2d[,1],type = "l")

pca_plot(pca.EMSC, str_sub(samples,start = 9,end =-11)) #by Species EMCS

pca_plot(pca.EMSC.SG.1d, str_sub(samples,start = 9,end =-11)) #by Species 1st deriv

pca_plot(pca.EMSC.SG.2d, str_sub(rownames(pca.EMSC.SG.2d.data),start = 9,end =-11))

#PCA LDA
fitdata.pca <- data.frame(Species = PLSR$Species[PLSR$train],
                       FTIR.score = I(pca.EMSC.SG.2d$x[PLSR$train,1:i,drop=FALSE]))
testdata.pca <- data.frame(Species = PLSR$Species[!PLSR$train],
                        FTIR.score = I(pca.EMSC.SG.2d$x[!PLSR$train,1:i,drop=FALSE]))

conv.Pollen.pca<- confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata.pca),newdata = testdata.pca)$class, 
                                   testdata.pca$Species)


#PCA on only Sporopollenin peaks
S.only <- pca.EMSC.SG.2d.data[,c("1605", "1516", "1171", "833")]
pca.S.only<-prcomp(S.only, center = T, scale = T)

S.regions <- pca.EMSC.SG.2d.data[,c(98:110,145:155,322:335,497:510)]
pca.S.regions<-prcomp(S.regions, center = T, scale = T)

pca.s.regions<-prcomp(~as.numeric(Species),
                      data = s.regions[PLSR$train,], 
                      newdata = s.regions[!PLSR$train,], 
                      center = T, scale = T)

g <- ggbiplot(pca.S.regions, obs.scale = 1, var.scale = 1, 
              groups = str_sub(rownames(S.regions),start = 9,end =-11), ellipse = TRUE, 
              circle = TRUE,
              var.axes = F, choices = 1:2)+
  coord_fixed()

pca.all.peaks
pca.S.L
dfr<-as.data.frame(pca.S.regions$x)
dfr$Species<-PLSR$Species

ggplot(dfr,aes(PC1, PC2, colour = Species))+
  geom_point()+stat_ellipse()+
  theme(aspect.ratio = 1, text = element_text(size = 30))+
  theme_bw()+geom_vline(xintercept = 0, linetype = "dashed")+ geom_hline(yintercept = 0,linetype = "dashed")+
  xlab("PC1 49.0%")+ylab("PC2 16.9%")

dfr<-as.data.frame(pca.EMSC.data[1:3,])
dfr2<-dfr
dfr2[, -c(98:110,145:155,322:335,497:510)]<-0
dfr2<-rbind(dfr,dfr2)
dfr2$ID<-c(rep("org",3), rep("regions",3))

dfr2<-melt(as.data.frame(dfr2),id="ID")
colnames(dfr2)<-c("ID","Wavenumber","Absorbance")
dfr2$Wavenumber<-as.numeric(as.character(dfr2$Wavenumber))

ggplot(data = dfr2, aes(Wavenumber, Absorbance, color = factor(ID)))+
  geom_line()+theme_bw()+theme(text = element_text(size = 18))+sporopollenin+
  scale_x_reverse()

ggplot(data = subset(dfr2, ID=="org"), aes(Wavenumber, Absorbance, color = factor(ID)))+
  geom_line()+theme_bw()+theme(text = element_text(size = 18))+sporopollenin+lipids+carbs+proteins


IDS<-str_sub(samples,start = 0,end =15)
Location<-str_sub(samples,start = 0,end =3)
Species<-str_sub(samples,start = 9,end =-11)

#create Color shades
sapply()

nspec<-levels(factor(Species))
Sp_colors<-sapply(c(1:length(nspec)),function(x) col2rgb(x))

Gro_Sp<-factor(paste(str_sub(rows,start = 0,end =3),str_sub(rows,start = 9,end =-11)))


grepl("Qrob",levels(Gro_Sp))

colors.df<-data.frame(Sp = (factor(paste(str_sub(samples.df$Sample.Name, start = 9, end = -11)))),
                      Gro = factor(paste(str_sub(samples.df$Sample.Name, start = 0, end = 3))))

#plot(c(1:3), c(0,0,0),col = shading(col2rgb(6), 4), pch=15, cex = 10) #samples plot some colors

colors.df$Gro_Sp <- factor(paste(str_sub(samples.df$Sample.Name, start = 9, end = -11),
                                          str_sub(samples.df$Sample.Name, start = 0, end = 3)))

colors<-sapply(levels(colors.df$Sp),function(x) sum(grepl(x, levels(colors.df$Gro_Sp))))

counts<-colors.df %>% 
  group_by(Sp) %>%
  summarise(no_rows = length(Sp))

shading<-function(color,n_shade){
  f_shade<-.50/n_shade
  shades<-sapply(c(1:n_shade), function(x) color*(1-f_shade*x)/255)
  shades<-sapply(c(1:n_shade), function(x) rgb(shades[1,x],shades[2,x],shades[3,x]))
  return(shades)
}

colours<-sapply(c(2:7),function(x) shading(col2rgb(x),colors[x-1]))
colours<-unlist(colours)
plot(c(1:43), rep(0,43),col = colours, pch=15, cex = 10)


levels(factor(Gro_Sp))

#SampleCodes
rows<-row.names(Querc)
samples.df$rows<-rows
samples.df$Species<-str_sub(samples.df$rows,start = 9,end =-11)
samples.df$Group<-paste(str_sub(samples.df$rows,start = 0,end =3))
samples.df$Location<-paste(str_sub(samples.df$rows,start = 5,end =7))
samples.df$SRep<-paste(str_sub(samples.df$rows,start = -8,end =-8))
samples.df$MRep<-paste(str_sub(samples.df$rows,start = -2,end =-1))
samples.df$SID<-paste(samples.df$Species,samples.df$Group,samples.df$Location,samples.df$SRep,samples.df$MRep,sep="_")

levels(samples.df$Location)<-c("A","B","C","A","A","B","B","C","A","D","A","A","A","A","C","D","E","F","B","B","H","C","B","D","A","E","O","N","Z","E","C","B","D","E","B","F","C","C","F","S","B")
levels(samples.df$Group)<-c("A","B","C","D","E","F","H","L","M","O","N","R","P","S","T")

old.levels<-list(Species=levels(samples.df$Species),
                 Group=levels(samples.df$Group),
                 Location=levels(samples.df$Location))

ggbiplot(pca.EMSC, obs.scale = 1, var.scale = 1,
         var.axes = F, choices = 1:2, alpha = 0)+
  geom_point(aes(colour = colors.df$Gro_Sp),size = 3)+
  scale_shape_manual(values = c(3,4,15:18))+
  scale_colour_manual(values = setNames(colours, levels(colors.df$Gro_Sp)))
  theme(legend.direction = 'horizontal', 
        legend.position = 'top')


Querc.pca.data<-Querc[,2600:3240]
row.names(Querc.pca.data)<-c(1:959)
Querc.SG <- savitzkyGolay(Querc.pca.data,2,3,11)
Querc.red.pca <- prcomp(Querc.pca.data,
                   center = T,
                   scale = T)
pca_plot(Querc.red.pca, str_sub(rows,start = 9,end =-11))

SG_2d<-as.data.frame(pca.EMSC.SG.2d$x)
SG_2d<-SG_2d[,-7:-536]
SG_2d$ID<-colors.df$Gro_Sp
SG_2d$Gro<-colors.df$Gro
SG_2d$Sp<-colors.df$Sp

data<-subset(SG_2d, Sp == "Qfag")
ggplot(data,aes(PC3, PC4, colour = Gro))+geom_point()+stat_ellipse()
ggplot(data,aes(PC1, PC2))+geom_point(aes(colour = longitude))+scale_color_gradientn(colours = c("#CCE5FF","#66B2FF","#000099"))

#loadings Plot
ggplot(loadings.EMSC.df, aes(as.numeric(rownames(loadings.EMSC.df)),PC2))+geom_line()

sporopollenin <- geom_vline(xintercept = c(1605,1515,1171,833), colour = c("black"), size = 1, alpha = .5)
lipids <- geom_vline(xintercept = c(1745,1462,722), colour = c("red"), size = 1, alpha = .6)
carbs <- geom_vline(xintercept = c(1107,1055,1028,1076,995), colour = c("blue"), size = 1, alpha = .4)
proteins <- geom_vline(xintercept = c(1654,1550),colour = c("green"), size = 1, alpha = .6)

save(sporopollenin, lipids, carbs, proteins, file = "vert.lines.rda")

#PLSR

plsr.samples <- plsr(FTIR[,2600:3200] ~ as.numeric(Species) + Latitude + Longitude , 6 ,data = samples.df,validation = "CV")

Z<-cbind(as.matrix(as.numeric(s.df$Species)),as.matrix(s.df[,10:11]),as.matrix(samples.df$mean_precip))
colnames(Z)<-c("Species","Latitude", "Longitude", "mean_precip")

Z<-Z[selection,]

plsr.SG.2d<-plsr(Z~pca.EMSC.SG.2d.data, 20, validation= "CV")

plot(RMSEP(plsr.SG.2d))
plot(plsr.SG.2d, ncomp = 15, asp = 1, line = TRUE)
plot(plsr.SG.2d, "loadings", comps = 1:2, legendpos = "topleft", xlab = "nm")



#Full Sett of all spectra and relevant variables
plsr.data<-as.data.frame(samples.shp@coords)
plsr.data$SID<-samples.shp@data$SID
plsr.data$Sp<-factor(substr(samples.shp@data$SID,0,2))
plsr.data$precip<-samples.shp@data$mean_precip
plsr.data$elevation<-samples.shp@data$elevation
plsr.data$temp<-samples.shp@data$temp
plsr.data$FTIR<-samples.shp@data$FTIR

#select for red set
plsr.data.red <- plsr.data[selection,]
plsr.data.red$Sp<-factor(plsr.data.red$Sp)


#PLSR matrices
dat<-plsr.data.red[sample(c(1:920),10),c(-3,4,-8)]
cbind(dat[-3], model.matrix( ~ 0 + Sp, dat))
dat<-plsr.data.red[,c(-3,-8)]
Y<-cbind(dat[-3], model.matrix( ~ 0 + Sp, dat))
Y<-as.matrix(Y)
colnames(Y)<-c("lon", "lat", "pre", "ele", "tem", "co", "fa", "rb", "re", "ro", "sb")
Y<-replace(Y,Y==0,-1)
Z<-pca.EMSC.SG.2d.data

plsr.SG.2d<-plsr(Y~Z, ncomp = 20, validation = "LOO")
plot(RMSEP(plsr.SG.2d))
plot(plsr.SG.2d, ncomp = 15, asp = 1, line = TRUE)
plot(plsr.SG.2d, "loadings", comps = 1:2, legendpos = "topleft",labels = "numbers", xlab = "wavenumbers")
abline(v=c(1605, 1515, 1171, 833)) #Sporopollenin
abline(v=c(1745,1462,722), col="red") #Lipids
abline(v=c(1107, 1055, 1028,1076, 995), col="blue") #Carbohydrates
abline(v=c(1650,1550), col="green") #Proteins
plot(plsr.SG.2d, plottype = "scores", comps = 1:10)


#After Example PLSR LDA

PLSR <- data.frame(Species = plsr.data.red$Sp)
PLSR$dummy <- 
  I(model.matrix(~y-1, data.frame(y = factor(PLSR$Species))))
PLSR$FTIR.SG2<-I(pca.EMSC.SG.2d.data)
colnames(PLSR$FTIR.SG2)<-wavenumbers
PLSR$env<-I(as.matrix(plsr.data.red[,c(1:2,5:7)]))
PLSR$train<-rep(TRUE,920)
PLSR[sample(c(1:920),250),5]<-FALSE #randomize test-train set
PLSR$Sp<-as.numeric(PLSR$Species)

# Predict CPLS scores for test data
npc=30
Pollen.cpls <- cppls(dummy ~ FTIR.SG2, npc, data = PLSR, subset = train, scale=T)
Pollen.test <- predict(Pollen.cpls, newdata = PLSR[!PLSR$train,], type = "score")

# Predict CPLS scores for test data (experimental used design as additional Y information)
#Pollen.cpls.yadd <- cppls(dummy ~ FTIR, npc, data = PLSR, subset = train,Y.add = env, scale=T) #removed Yadd

Pollen.cpls.yadd <- cppls(cbind(dummy,env2) ~ FTIR.SG2 ,npc, data = PLSR, subset = train, scale=T)#,Y.add = env2 )
Pollen.test.yadd <- predict(Pollen.cpls.yadd, newdata = PLSR[!PLSR$train,], type = "score")

Pollen.PLSR <- plsr(cbind(dummy,env[,c(-1,-2)]) ~ FTIR.SG2, npc, data = PLSR, subset = train, scale=T)
Pollen.PLSR.test <- predict(Pollen.PLSR, newdata = PLSR[!PLSR$train,], type = "score")

error <- matrix(ncol = npc, nrow = 3)
dimnames(error) <- list(Model = c('CPLS', 'CPLS (Y.add)', 'PLSR'), ncomp = 1:npc)

#Pollen.cpls.comb <- cppls(cbind(dummy,env)~FTIR.SG2,20,data=PLSR)


for (i in 1:npc) {
  fitdata1 <- data.frame(Species = PLSR$Species[PLSR$train],
                         FTIR.score = I(Pollen.cpls$scores[,1:i,drop=FALSE]))
  testdata1 <- data.frame(Species = PLSR$Species[!PLSR$train],
                          FTIR.score = I(Pollen.test[,1:i,drop=FALSE]))
  error[1,i] <-
    (250 - sum(predict(lda(Species ~ FTIR.score, data = fitdata1),
                      newdata = testdata1)$class == testdata1$Species)) / 250
  
  fitdata2 <- data.frame(Species = PLSR$Species[PLSR$train],
                         FTIR.score = I(Pollen.cpls.yadd$scores[,1:i,drop=FALSE]))
  testdata2 <- data.frame(Species = PLSR$Species[!PLSR$train],
                          FTIR.score = I(Pollen.test.yadd[,1:i,drop=FALSE]))
  error[2,i] <-
    (250 - sum(predict(lda(Species ~ FTIR.score, data = fitdata2),
                      newdata = testdata2)$class == testdata2$Species)) / 250
  
  fitdata3 <- data.frame(Species = PLSR$Species[PLSR$train],
                         FTIR.score = I(Pollen.PLSR$scores[,1:i,drop=FALSE]))
  testdata3 <- data.frame(Species = PLSR$Species[!PLSR$train],
                          FTIR.score = I(Pollen.PLSR.test[,1:i,drop=FALSE]))
  error[3,i] <-
    (250 - sum(predict(lda(Species ~ FTIR.score, data = fitdata3),
                       newdata = testdata3)$class == testdata3$Species)) / 250
}
round(error,2)

plot(RMSEP(Pollen.cpls.yadd))

for (i in 1:10){
  fitdata1 <- data.frame(Species = PLSR$Species[PLSR$train],
                         FTIR.score = I(Pollen.cpls$scores[,1:i,drop=FALSE]))
  testdata1 <- data.frame(Species = PLSR$Species[!PLSR$train],
                          FTIR.score = I(Pollen.test[,1:i,drop=FALSE]))
  fitdata2 <- data.frame(Species = PLSR$Species[PLSR$train],
                         FTIR.score = I(Pollen.cpls.yadd$scores[,1:i,drop=FALSE]))
  testdata2 <- data.frame(Species = PLSR$Species[!PLSR$train],
                          FTIR.score = I(Pollen.test.yadd[,1:i,drop=FALSE]))
  fitdata3 <- data.frame(Species = PLSR$Species[PLSR$train],
                         FTIR.score = I(Pollen.PLSR$scores[,1:i,drop=FALSE]))
  testdata3 <- data.frame(Species = PLSR$Species[!PLSR$train],
                          FTIR.score = I(Pollen.PLSR.test[,1:i,drop=FALSE]))
  
  conv.Pollen.cpls<- confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata1),newdata = testdata1)$class, 
                                     testdata1$Species)
  conv.Pollen.cpls.yadd<- confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata2),newdata = testdata2)$class, 
                                          testdata2$Species)
  conv.Pollen.PLSR<-confusionMatrix(predict(lda(Species ~ FTIR.score, data = fitdata3),newdata = testdata3)$class, 
                                    testdata3$Species)
}




error.df<-data.frame(pc = c(1:30),CPLS = error[1,], CPLS.env = error[2,], PLSR = error[3,])
error.df<-melt(error.df,id="pc")
colnames(error.df)<-c("Components", "Model", "RMSEP")
ggplot(error.df, aes(Components, RMSEP , colour=Model))+
  geom_line()+theme_bw()+theme(text = element_text(size = 18))

PLS.plot.data<- data.frame(Species = factor(fitdata2$Species),
                           PC1 = fitdata2$FTIR.score[,1],
                           PC2 = fitdata2$FTIR.score[,2],
                           PC3 = fitdata2$FTIR.score[,3],
                           PC4 = fitdata2$FTIR.score[,4],
                           PC5 = fitdata2$FTIR.score[,5])
ggplot(PLS.plot.data,aes(PC4,PC5, color = Species))+
  geom_point()+
  scale_color_discrete()+stat_ellipse()+
  theme_bw()+theme(text = element_text(size = 18))


#RDA
#RDA with FTIR Data
Pollen.pca<-rda(PLSR$FTIR.SG2, scale = T)
#Pollen.pca<-rda(pca.EMSC.SG.2d.data)
biplot(Pollen.pca, scaling = 2)

env<-data.frame(Longitude = PLSR$env[,1],
                Latitude = PLSR$env[,2],
                Precip = PLSR$env[,3],
                Elevation = PLSR$env[,4],
                Temp = PLSR$env[,5])

Pollen.ev <- envfit(Pollen.pca ~ Precip + Elevation + Temp, data = env)
#plot(Pollen.ev)

Pollen.sf <- ordisurf(Pollen.pca ~ Elevation, data = env, plot = FALSE)
#biplot(Pollen.pca, scaling = 2)
#plot(Pollen.ev)
#plot(Pollen.sf, col = "forestgreen", add = TRUE)

#ordisurf:
ordi.grid <- Pollen.sf$grid #extracts the ordisurf object
ordi <- expand.grid(x = ordi.grid$x, y = ordi.grid$y) #get x and ys
ordi$z <- as.vector(ordi.grid$z) #unravel the matrix for the z scores
ordi.mite.na <- data.frame(na.omit(ordi))

smry <- summary(Pollen.pca)
PCs <- data.frame(smry$sites[,1:2])
PCs$Species<-PLSR$Species
ldngs <- data.frame(smry$species[,1:2])
ldngs.peaks <- ldngs[c("1605", "1516", "1171", "833", "1746","1462","722","1107", "1055", "1028","1076", "995", "1651","1551"),]
ldngs.peaks$Comp<-c("S 1605", "S 1515", "S 1171", "S 833", "L 1745","L 1462","L 722","C 1107", "C 1055", "C 1028","C 1076", "C 995", "P 1650","P 1550")
ldngs.peaks$Comp<-c(rep("S",4), rep("L", 3), rep("C",5), rep("P",2))

ldngs.df<-ldngs
ldngs.df$Wavenumber<-as.numeric(row.names(ldngs))
ldngs.df<-melt(ldngs.df, id="Wavenumber")
colnames(ldngs.df)<-c("Wavenumber", "PC", "SG2")

tmp<-subset(ldngs.df, PC = "PC1")
ggplot(data = subset(ldngs.df, PC == "PC1"), aes(Wavenumber, SG2))+
  geom_line(alpha=0.5)+theme_bw()+theme(text = element_text(size = 18))+
  scale_x_reverse()+ylab("PC 1")+
  geom_hline(yintercept = 0, linetype="dashed")+
  coord_fixed(ratio = 170)+
  sporopollenin+lipids+carbs+proteins


env.vectors<-as.data.frame(Pollen.ev$vectors$arrows)

rda.plot <- ggplot(PCs, aes(x=PC1, y=PC2, color = Species)) + 
  geom_point(alpha = .1) +
  theme_bw()+
  geom_hline(yintercept=0, linetype="dotted") +
  geom_vline(xintercept=0, linetype="dotted") +
  stat_ellipse()+
  theme(text = element_text(size = 18))+
  coord_fixed()
rda.plot

rda.biplot <- rda.plot +
  geom_segment(data=ldngs.peaks, aes(x=0, xend=PC1*2, y=0, yend=PC2*2), #color = "red"
               color = "red", arrow=arrow(length=unit(0.05,"npc"))) +
  geom_text(data=ldngs.peaks, 
            aes(x=PC1*2,y=PC2*2,label=ldngs.peaks$Comp,
                hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
            color = "black", size=6) #color="red"
rda.biplot

rda.env<- rda.plot +
  geom_segment(data=env.vectors[3,], aes(x=0, xend=PC1, y=0, yend=PC2), 
               color = "blue", arrow=arrow(length=unit(0.05,"npc"))) +
  geom_text(data=env.vectors[3,], 
            aes(x=PC1,y=PC2,label=rownames(env.vectors[3,]),
                hjust=0.5*(1-sign(PC1)),vjust=0.5*(1-sign(PC2))), 
            color = "black", size=6, check_overlap = F)
rda.env

rda.c <-rda.env+stat_contour(data = ordi.mite.na, 
                          aes(x = x, y = y, z = z),
                          binwidth = .2, size = 1, linetype = "dashed", colour = "grey50")
rda.c

c <- ggplot(data = ordi.mite.na, aes(x = x, y = y, color = z)) +
  geom_point(alpha = .1)+
  scale_colour_gradient(high = "darkgreen", low = "darkolivegreen1")+
  stat_contour(data = ordi.mite.na, aes(x = x, y = y, z = z, color = ..level..),
                      binwidth = 10, size = 0.5, linetype = "dashed")



#RDA on sb and fa
tmp<-subset(PLSR, Species == "sb")
sb.pca<-rda(tmp$FTIR.SG2, scale = T)
sb.ev <- envfit(sb.pca ~ Precip + Elevation + Temp + Latitude + Longitude, data = env[rownames(tmp),])
sb.sf <- ordisurf(sb.pca ~ Temp, data = env[rownames(tmp),], plot = FALSE, scaling = 2)
grid <- sb.sf$grid #extracts the ordisurf object
ord <- expand.grid(x = grid$x, y = grid$y) #get x and ys
ord$z <- as.vector(grid$z) #unravel the matrix for the z scores
ord.sb <- data.frame(na.omit(ord))

rda.c <-rda.env+stat_contour(data = ord.sb, 
                             aes(x = x, y = y, z = z),
                             binwidth = .1, size = 1, linetype = "dashed", colour = "grey50") #binwidth = 3
rda.c
smry <- summary(sb.pca)
PCs <- data.frame(smry$sites[,1:2])
PCs$Species<-"sb"
ldngs <- data.frame(smry$species[,1:2])
ldngs.peaks <- ldngs[c("1605", "1516", "1171", "833", "1746","1462","722","1107", "1055", "1028","1076", "995", "1651","1551"),]
ldngs.peaks$Comp<-c(rep("S",4), rep("L", 3), rep("C",5), rep("P",2))
env.vectors<-as.data.frame(sb.ev$vectors$arrows)


tmp.pca<-subset(PLSR, Species == "fa")
fa.pca<-rda(tmp$FTIR.SG2, scale = T)



#CPPLS example
data(mayonnaise)
# Create dummy response
mayonnaise$dummy <-
  I(model.matrix(~y-1, data.frame(y = factor(mayonnaise$oil.type))))

# Predict CPLS scores for test data
may.cpls <- cppls(dummy ~ NIR, 20, data = mayonnaise, subset = train)
may.test <- predict(may.cpls, newdata = mayonnaise[!mayonnaise$train,], type = "score")

# Predict CPLS scores for test data (experimental used design as additional Y information)
may.cpls.yadd <- cppls(dummy ~ NIR, 20, data = mayonnaise, subset = train, Y.add=design)
may.test.yadd <- predict(may.cpls.yadd, newdata = mayonnaise[!mayonnaise$train,], type = "score")

# Classification by linear discriminant analysis (LDA)
library(MASS)
error <- matrix(ncol = 20, nrow = 2)
dimnames(error) <- list(Model = c('CPLS', 'CPLS (Y.add)'), ncomp = 1:20)
for (i in 1:20) {
  fitdata1 <- data.frame(oil.type = mayonnaise$oil.type[mayonnaise$train],
                         NIR.score = I(may.cpls$scores[,1:i,drop=FALSE]))
  testdata1 <- data.frame(oil.type = mayonnaise$oil.type[!mayonnaise$train],
                          NIR.score = I(may.test[,1:i,drop=FALSE]))
  error[1,i] <-
    (42 - sum(predict(lda(oil.type ~ NIR.score, data = fitdata1),
                      newdata = testdata1)$class == testdata1$oil.type)) / 42
  fitdata2 <- data.frame(oil.type = mayonnaise$oil.type[mayonnaise$train],
                         NIR.score = I(may.cpls.yadd$scores[,1:i,drop=FALSE]))
  testdata2 <- data.frame(oil.type = mayonnaise$oil.type[!mayonnaise$train],
                          NIR.score = I(may.test.yadd[,1:i,drop=FALSE]))
  error[2,i] <-
    (42 - sum(predict(lda(oil.type ~ NIR.score, data = fitdata2),
                      newdata = testdata2)$class == testdata2$oil.type)) / 42
}
round(error,2)


ggplot(fitdata2,aes(fitdata2$NIR.score[,1],fitdata2$NIR.score[,2], color = factor(oil.type)))+
  geom_point()+
  scale_color_discrete()+
  stat_ellipse()
