library(here)

# PCC

source(here("R", "functions", "PCC.R"))

tech_rep = PCC_variability(data$ftir,id = data$ID)
Section_PCC = PCC_variability(data$ftir,id = data$Section)
Sp_PCC = PCC_variability(data$ftir,id = data$Species)
Reg_PCC = PCC_variability(data$ftir,id = data$Group)
Sp_Reg_PCC = PCC_variability(data$ftir,id = paste0(data$Sub_Spec,"_",as.character(data$Group)))

PCC = data.frame(ID = "tech_rep", n = nrow(tech_rep), mean_PCC = mean(tech_rep$mean_PCC), sd_PCC = mean(tech_rep$sd_PCC))
PCC = rbind(PCC, Section_PCC, Sp_PCC)


plot(1:20,cumsum(Expl_Var), main="Cumulative explained variance of CPPLS model",
     xlab="No. of Components", ylab="Expl. variance in %", type ="l")
