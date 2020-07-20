# This is code to assess the death trends and excess deaths due to COVID-19 in Peru
# Code developed by Renzo JC Calderon-Anyosa
# Last Revised: July 20, 2020 

# Required packages
library(tidyverse)


#SINADEF total mortality data
db <- read.csv(url("https://www.datosabiertos.gob.pe/node/6450/download"),
               strip.white = TRUE,
               sep = ';', 
               header = FALSE,
               stringsAsFactors = FALSE)


#COVID-19 deaths
dbcov <- read.csv(url("https://cloud.minsa.gob.pe/s/Md37cjXmjT9qYSa/download"),
                  strip.white = TRUE,
                  sep = ',', 
                  header = TRUE,
                  stringsAsFactors = FALSE)


# SINADEF formating
# Data starts in column 4, with names in column 3
# Renaming and arrenging columns 

colnames(db) <- tolower(gsub("[[:space:]]", ".", as.vector(t(db)[,3]), perl=T))
db <- db[-c(1:3), ]

# Droping last 4 empty columns 
db <- db[-c(32:35)]


# Geneate age ####
# Missings
db$edad[db$edad=="SIN REGISTRO"] <- NA
# age from chr to numb
db$edad <- as.numeric(db$edad)
# Common age in days
db$aget <- ifelse(db$tiempo.edad=="DIAS",1,NA)
db$aget <- ifelse(db$tiempo.edad=="MESES",30.5,db$aget)
db$aget <- ifelse(db$tiempo.edad=="AÑOS",365.5,db$aget)
db$aget <- ifelse(db$tiempo.edad=="HORAS",1/24,db$aget)
db$aget <- ifelse(db$tiempo.edad=="MINUTOS",1/(24*60),db$aget)
db$aget <- ifelse(db$tiempo.edad=="SEGUNDOS",1/(24*60*60),db$aget)
db$aget <- ifelse(db$tiempo.edad=="SIN REGISTRO",NA,db$aget)
# gen age
db$age <- db$edad*db$aget

# Fecha = date
db$fecha <- as.Date(db$fecha)

# Adult DB 
db.adult <- db %>% filter(db$age>=365.5*18)

# Total no external deaths
db.adult$non.violentdeath <- ifelse(db.adult$muerte.violenta=="SIN REGISTRO",1,NA)

# Sum number of deaths by days and region
db.deaths.dep.day <- db.adult %>% group_by(departamento.domicilio, año, mes, fecha) %>%  summarise(su.non.violentdeath = sum(non.violentdeath, na.rm = T))

#Only Data february - July
db.deaths.dep.day$mes <- as.numeric(db.deaths.dep.day$mes)
db.deaths.dep.day.f <-  db.deaths.dep.day %>% filter(mes>=2 & mes<8)

#Gen date as numbers
db.deaths.dep.day.f$fecha2 <- as.numeric(db.deaths.dep.day.f$fecha)

#Droping Feb 29 2020
db.deaths.dep.day.f = db.deaths.dep.day.f[db.deaths.dep.day.f$fecha2!= 18321, ] 

#day number correlative by year
db.deaths.dep.day.f$fecha3 <- ifelse(db.deaths.dep.day.f$año==2018,db.deaths.dep.day.f$fecha2-365*1, db.deaths.dep.day.f$fecha2)
db.deaths.dep.day.f$fecha3 <- ifelse(db.deaths.dep.day.f$año==2019,db.deaths.dep.day.f$fecha2-365*2, db.deaths.dep.day.f$fecha3)
db.deaths.dep.day.f$fecha3 <- ifelse(db.deaths.dep.day.f$año==2020,db.deaths.dep.day.f$fecha2-365*3, db.deaths.dep.day.f$fecha3)
db.deaths.dep.day.f$fecha3 <- ifelse(db.deaths.dep.day.f$fecha2>18321,db.deaths.dep.day.f$fecha3-1, db.deaths.dep.day.f$fecha3)

#Regions
db.deaths.dep.day.f.nolima <- db.deaths.dep.day.f %>%  filter(departamento.domicilio!="LIMA" & departamento.domicilio!="SIN REGISTRO" & departamento.domicilio!="EXTRANJERO")

#Adult population in each region
dbpoblacion <- data.frame("departamento.domicilio" = c("AMAZONAS","ANCASH","APURIMAC","AREQUIPA","AYACUCHO","CAJAMARCA","CALLAO",
                                                       "CUSCO","HUANCAVELICA","HUANUCO","ICA","JUNIN","LA LIBERTAD", 
                                                       "LAMBAYEQUE","LIMA","LORETO","MADRE DE DIOS","MOQUEGUA","PASCO","PIURA",
                                                       "PUNO","SAN MARTIN","TACNA","TUMBES","UCAYALI"),
                          "poblacion"=c(235657,736289,266194,991969,403809,872747,706004,
                                        809150,219406,463043,580269,828244,1195702,
                                        811573,6932443,510979,91491,126342,169985,1206440,
                                        817428,516331,237769,148193,300098))

#Merging with sinadef deaths
db.regions.per.pob <- merge(db.deaths.dep.day.f.nolima,dbpoblacion,by="departamento.domicilio")

#Creating deaths per 10K
db.regions.per.pob$death.by.10k <- (db.regions.per.pob$su.non.violentdeath/db.regions.per.pob$poblacion)*10000

#Deaths Covid
dbcov$date <- as.numeric(as.Date(dbcov$FECHA_FALLECIMIENTO, "%d/%m/%y"))

#same date to merge with sinadef data
dbcov$fecha3 <- (dbcov$date-365*3) - 1

#keeping adult deaths
dbcov.adult <- dbcov %>% filter(EDAD_DECLARADA>=18)

#renaiming to merge with sinadef data
dbcov.adult$departamento.domicilio <- dbcov.adult$DEPARTAMENTO
dbcov.adult$año <- 2020
dbcov.adult$death<- 1
dbcov.deaths.dep.day <- dbcov.adult %>% group_by(departamento.domicilio, año, fecha3) %>%  summarise(su.covdeath = sum(death, na.rm = T))

#merging with sinadef data
db.cov.and.total <- merge(db.regions.per.pob,dbcov.deaths.dep.day,by=c("departamento.domicilio","año","fecha3"), all.x = T)

#Covid deaths per 10K
db.cov.and.total$cov.per10k <- (db.cov.and.total$su.covdeath/db.cov.and.total$poblacion)*10000

#Mean daily death in the previous month 
meanbydis <- db.cov.and.total %>% filter(año==2020 & fecha2<18336) %>%  group_by(departamento.domicilio) %>%  summarise(mean.previosmonths = mean(death.by.10k, na.rm = T))

#merging 
db.cov.and.total.with.means <- merge(db.cov.and.total,meanbydis,by=c("departamento.domicilio"), all.x = T)

#Exces covid deaths (mean+reported covid)
db.cov.and.total.with.means$cov.excess <- db.cov.and.total.with.means$mean.previosmonths+db.cov.and.total.with.means$cov.per10k

#Droping last date (because not updated)
db.cov.and.total.with.means.2 <-db.cov.and.total.with.means[db.cov.and.total.with.means$fecha2!=max(db.cov.and.total.with.means$fecha2),]

#Graph
#English by Regions
ggplot(data = db.cov.and.total.with.means.2 , mapping = aes(x = fecha3, y = death.by.10k , color = año)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=	17241, xmax=17347, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means.2, departamento.domicilio=="AREQUIPA"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means.2, departamento.domicilio=="ICA"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means.2, departamento.domicilio=="JUNIN"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means.2, departamento.domicilio=="HUANUCO"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means.2, departamento.domicilio=="SAN MARTIN"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means.2, departamento.domicilio=="MADRE DE DIOS"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means.2, departamento.domicilio=="ANCASH"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "Lockdown", guide = "none") +
  scale_x_continuous(name="Days", breaks=c(17198,17241,17347), labels=c("Feb 01","March 16","June 30"),limits = c(17198,17378)) +
  scale_y_continuous(name="Non Violent Deaths per 10,000 Adult Population", limits=c(0,1.5))+
  geom_vline(xintercept = 17241, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(17198,17347), colour="grey") +
  theme(text=element_text(size=12,  family="sans")) +
  facet_wrap(~departamento.domicilio) +
  theme(strip.background =element_rect(fill="grey"))+
  theme(strip.text = element_text(colour = 'black'))+
  scale_color_manual(values=c("#E0E0E0", "#C0C0C0", "#A0A0A0", "black")) +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  geom_line(data = subset(db.cov.and.total.with.means.2,año==2020), aes(y=mean.previosmonths, x=fecha3), colour = "#287AB2", size = 1.5, alpha = 0.7) +
  geom_point(data = subset(db.cov.and.total.with.means.2,año==2020), aes(y=cov.excess, x=fecha3), colour = "#1C732E", size = 1.5, alpha = 0.7) +
  stat_smooth(data = subset(db.cov.and.total.with.means.2,año==2020  & fecha2>=18320) , mapping = aes(x = fecha3 , y =cov.excess), method = "loess", formula = y~x, span = 0.3, color = "#49B65F", fill = NA, alpha = 0.7, size = 1.5) +
  stat_smooth(data = subset(db.cov.and.total.with.means.2,año==2020  & fecha2>=18320+15) , mapping = aes(x = fecha3, y =death.by.10k), method = "loess", formula = y~x, span = 0.3, color = "#F34D1B", fill = NA, alpha = 0.7, size = 1.5) +
  ggtitle("Non-Violent Deaths in Adults by Region, Peru")



#Spanish
ggplot(data = db.cov.and.total.with.means , mapping = aes(x = fecha3, y = death.by.10k , color = año)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=	17241, xmax=17347, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means, departamento.domicilio=="AREQUIPA"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means, departamento.domicilio=="ICA"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means, departamento.domicilio=="JUNIN"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means, departamento.domicilio=="HUANUCO"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means, departamento.domicilio=="SAN MARTIN"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means, departamento.domicilio=="MADRE DE DIOS"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_rect(data=filter(db.cov.and.total.with.means, departamento.domicilio=="ANCASH"), aes(ymin=-Inf, ymax=Inf, xmin=	17241, xmax=Inf, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "Lockdown", guide = "none") +
  scale_x_continuous(name="Días", breaks=c(17198,17241,17347), labels=c("Feb 01","Mar 16","Jun 30"),limits = c(17198,17378)) +
  scale_y_continuous(name="Muertes No Violentas por 10,000 Adultos", limits=c(0,1.5))+
  geom_vline(xintercept = 17241, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(17198,17347), colour="grey") +
  theme(text=element_text(size=12,  family="sans")) +
  facet_wrap(~departamento.domicilio) +
  theme(strip.background =element_rect(fill="grey"))+
  theme(strip.text = element_text(colour = 'black'))+
  scale_color_manual(values=c("#E0E0E0", "#C0C0C0", "#A0A0A0", "black")) +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  geom_line(data = subset(db.cov.and.total.with.means,año==2020), aes(y=mean.previosmonths, x=fecha3), colour = "#287AB2", size = 1.5, alpha = 0.7) +
  geom_point(data = subset(db.cov.and.total.with.means,año==2020), aes(y=cov.excess, x=fecha3), colour = "#1C732E", size = 1.5, alpha = 0.7) +
  stat_smooth(data = subset(db.cov.and.total.with.means,año==2020  & fecha2>=18320) , mapping = aes(x = fecha3 , y =cov.excess), method = "loess", formula = y~x, span = 0.3, color = "#49B65F", fill = NA, alpha = 0.7, size = 1.5) +
  stat_smooth(data = subset(db.cov.and.total.with.means,año==2020  & fecha2>=18320+15) , mapping = aes(x = fecha3, y =death.by.10k), method = "loess", formula = y~x, span = 0.3, color = "#F34D1B", fill = NA, alpha = 0.7, size = 1.5) +
  ggtitle("Muertes No Violentas por Region, Perú")


#Lima
db.deaths.dep.day.f.lima <- db.deaths.dep.day.f %>%  filter(departamento.domicilio=="LIMA")

#Merging with sinadef deaths
db.lima.per.pob <- merge(db.deaths.dep.day.f.lima,dbpoblacion,by="departamento.domicilio")

#Creating deaths per 10K
db.lima.per.pob$death.by.10k <- (db.lima.per.pob$su.non.violentdeath/db.lima.per.pob$poblacion)*10000

#Deaths Covid
#merging with sinadef data
db.cov.and.total.lima <- merge(db.lima.per.pob,dbcov.deaths.dep.day,by=c("departamento.domicilio","año","fecha3"), all.x = T)

#Covid deaths per 10K
db.cov.and.total.lima$cov.per10k <- (db.cov.and.total.lima$su.covdeath/db.cov.and.total.lima$poblacion)*10000

#Mean daily death in the previous month 
meanbydis.lima <- db.cov.and.total.lima %>% filter(año==2020 & fecha2<18336) %>%  group_by(departamento.domicilio) %>%  summarise(mean.previosmonths = mean(death.by.10k, na.rm = T))

#merging 
db.cov.and.total.with.means.lima <- merge(db.cov.and.total.lima,meanbydis.lima,by=c("departamento.domicilio"), all.x = T)

#Exces covid deaths (mean+reported covid)
db.cov.and.total.with.means.lima$cov.excess <- db.cov.and.total.with.means.lima$mean.previosmonths+db.cov.and.total.with.means.lima$cov.per10k

db.cov.and.total.with.means.lima.2 <- db.cov.and.total.with.means.lima[db.cov.and.total.with.means.lima$fecha2!=max(db.cov.and.total.with.means$fecha2),]

#Graph
#English
ggplot(data = db.cov.and.total.with.means.lima.2 , mapping = aes(x = fecha3, y = death.by.10k , color = año)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=	17241, xmax=17347, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "Lockdown", guide = "none") +
  scale_x_continuous(name="Days", breaks=c(17198,17241,17347), labels=c("Feb 01","March 16","June 30"),limits = c(17198,17378)) +
  scale_y_continuous(name="Non Violent Deaths per 10,000 Adult Population", limits=c(0,1))+
  geom_vline(xintercept = 17241, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(17198,17347), colour="grey") +
  theme(text=element_text(size=12,  family="sans")) +
  facet_wrap(~departamento.domicilio) +
  theme(strip.background =element_rect(fill="grey"))+
  theme(strip.text = element_text(colour = 'black'))+
  scale_color_manual(values=c("#E0E0E0", "#C0C0C0", "#A0A0A0", "black")) +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  geom_line(data = subset(db.cov.and.total.with.means.lima.2,año==2020), aes(y=mean.previosmonths, x=fecha3), colour = "#287AB2", size = 1.5, alpha = 0.7) +
  geom_point(data = subset(db.cov.and.total.with.means.lima.2,año==2020), aes(y=cov.excess, x=fecha3), colour = "#1C732E", size = 1.5, alpha = 0.7) +
  stat_smooth(data = subset(db.cov.and.total.with.means.lima.2,año==2020  & fecha3>=17241) , mapping = aes(x = fecha3 , y =cov.excess), method = "loess", formula = y~x, span = 0.3, color = "#49B65F", fill = NA, alpha = 0.7, size = 1.5) +
  stat_smooth(data = subset(db.cov.and.total.with.means.lima.2,año==2020  & fecha3>=17241) , mapping = aes(x = fecha3, y =death.by.10k), method = "loess", formula = y~x, span = 0.3, color = "#F34D1B", fill = NA, alpha = 0.7, size = 1.5) +
  ggtitle("Non-Violent Deaths in Adults in Lima, Peru")


#Spanish
ggplot(data = db.cov.and.total.with.means.lima.2 , mapping = aes(x = fecha3, y = death.by.10k , color = año)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=	17241, xmax=17347, fill="Lockdown"), fill="#E9E9E9", color = NA, alpha=0.25) +
  geom_point() + 
  scale_fill_identity(name = "Lockdown", guide = "none") +
  scale_x_continuous(name="Días", breaks=c(17198,17241,17347), labels=c("Feb 01","Mar 16","Jun 30"),limits = c(17198,17378)) +
  scale_y_continuous(name="Muertes No Violentas por 10,000 Adultos", limits=c(0,1))+
  geom_vline(xintercept = 17241, linetype="longdash") +
  theme(
    plot.background = element_rect(fill = "white"),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    panel.grid.major = element_line(color = "grey"),
    panel.ontop = T,
    panel.background = element_rect(fill = NA),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept=c(17198,17347), colour="grey") +
  theme(text=element_text(size=12,  family="sans")) +
  facet_wrap(~departamento.domicilio) +
  theme(strip.background =element_rect(fill="grey"))+
  theme(strip.text = element_text(colour = 'black'))+
  scale_color_manual(values=c("#E0E0E0", "#C0C0C0", "#A0A0A0", "black")) +
  theme(legend.position = "") +
  theme(axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  geom_line(data = subset(db.cov.and.total.with.means.lima.2,año==2020), aes(y=mean.previosmonths, x=fecha3), colour = "#287AB2", size = 1.5, alpha = 0.7) +
  geom_point(data = subset(db.cov.and.total.with.means.lima.2,año==2020), aes(y=cov.excess, x=fecha3), colour = "#1C732E", size = 1.5, alpha = 0.7) +
  stat_smooth(data = subset(db.cov.and.total.with.means.lima.2,año==2020  & fecha2>=18320) , mapping = aes(x = fecha3 , y =cov.excess), method = "loess", formula = y~x, span = 0.3, color = "#49B65F", fill = NA, alpha = 0.7, size = 1.5) +
  stat_smooth(data = subset(db.cov.and.total.with.means.lima.2,año==2020  & fecha2>=18320+15) , mapping = aes(x = fecha3, y =death.by.10k), method = "loess", formula = y~x, span = 0.3, color = "#F34D1B", fill = NA, alpha = 0.7, size = 1.5) +
  ggtitle("Muertes No Violentas en Lima, Perú")

