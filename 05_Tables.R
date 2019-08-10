library(dplyr)
library(here)
data = readRDS(file = here("Data", "Output", "data_mean.rds"))

# Summary Table over trees per location
table = data[1:9] %>% group_by(Group, Species, Sub_Spec) %>% summarise(n = n())


library(here)
library(dplyr)
library(knitr)
library(kableExtra)

data = readRDS(file = here("Data", "Output", "data_mean.rds"))
env14 = readRDS(file = here("Data", "Output", "env_WS_kriged_14.rds"))
data14 <- cbind(data[,c("Group", "Latitude", "Longitude")],env14[,-1])
colnames(data14)[1] = "Group"

df = data14%>%
  group_by(Group)%>%
  summarise_all(list(~min(.), ~max(.), ~mean(.)))
df = df[,c("Group", "prec_mean", "prec_min", "prec_max",
           "temp_mean", "temp_min", "temp_max",
           "srad_mean", "srad_min", "srad_max",
           "Latitude_mean", "Longitude_mean")]
colnames(df)[2:12] = c(rep(c("mean", "min", "max"),3), "Latitude", "Longitude")

digits = c(0,rep(10,6),rep(1,3),rep(1000,2))
df[,-1] = as.data.frame(sapply(c(2:12), function(x) round(df[,x]*digits[x])/digits[x]))

kable(df, "latex", booktabs = T) %>%
  #kable_styling(bootstrap_options = c("striped", "hover")) %>%
  add_header_above(c(" " = 1, "Precipitation" = 3, "Temperature" = 3, "Solar Radiation" = 3, " " = 2)) %>%
  as_image(file = here("R","tables","df.html"))
save_kable(file = here("R","tables","df.html"))