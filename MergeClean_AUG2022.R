# Load library
library('sf') # For the shape file
library(readstata13)
library(tidyverse)
# Raw firm level data

#datS1 <- read.dta13("C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Data/Stata/Cleaned.dta",fromEncoding="macroman") #Need this for distance
#datS2 <- read.dta13("C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Data/Stata/Cleaned.dta") # Read stata
datS1 <- read.dta13("C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Data/Stata/Cleaned_AUG2022.dta",fromEncoding="macroman") #Need this for distance
datS2 <- read.dta13("C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Data/Stata/Cleaned_AUG2022.dta") # Read stata
datS1$city <- as.character(datS1$city)

datS1$city <- str_replace(datS1$city,"Combarbal<e1><e1>","Combarbalá")
datS1$city <- str_replace(datS1$city,"Combarbal·","Combarbalá")
datS1$city <- str_replace(datS1$city,"Conc<f3>n","Concón")
datS1$city <- str_replace(datS1$city,"ConcÛn","Concón")
datS1$city <- str_replace(datS1$city,"Copiap<f3>","Copiapó")
datS1$city <- str_replace(datS1$city,"CopiapÛ","Copiapó")
datS1$city <- str_replace(datS1$city,"Puchuncav<ed>","Puchuncaví")
datS1$city <- str_replace(datS1$city,"PuchuncavÌ","Puchuncaví")

datS1$city <- str_replace(datS1$city,"ValparaÌso","Valparaíso")
datS1$city <- str_replace(datS1$city,"MarÌa Elena","María Elena")
datS1$city <- str_replace(datS1$city,"LongavÌ","Longaví")
datS1$city <- str_replace(datS1$city,"RÌo Negro","Río Negro")
datS1$city <- str_replace(datS1$city,"MaullÌn","Maullín")
datS1$city <- str_replace(datS1$city,"Santa MarÌa","Santa María")
datS1$city <- str_replace(datS1$city,"RÌo Claro","Río Claro")

datS1$city <- str_replace(datS1$city,"CopiapÛ","Copiapó")
datS1$city <- str_replace(datS1$city,"CuricÛ","Curicó")
datS1$city <- str_replace(datS1$city,"QuellÛn","Quellón")
datS1$city <- str_replace(datS1$city,"ConcepciÛn","Concepción")
datS1$city <- str_replace(datS1$city,"CochamÛ","Cochamó")
datS1$city <- str_replace(datS1$city,"ConstituciÛn","Constitución")
datS1$city <- str_replace(datS1$city,"PuqueldÛn","Puqueldón")

datS1$city <- str_replace(datS1$city,"Santa B·rbara","Santa Bárbara")
datS1$city <- str_replace(datS1$city,"Juan Fern·ndez","Juan Fernández")

datS1$city <- str_replace(datS1$city,"TomÈ","Tomé")
datS1$city <- str_replace(datS1$city,"VichuquÈn","Vichuquén")
datS1$city <- str_replace(datS1$city,"OlmuÈ","Olmué")
datS1$city <- str_replace(datS1$city,"MulchÈn","Mulchén")
datS1$city <- str_replace(datS1$city,"HualpÈn","Hualpén")
datS1$city <- str_replace(datS1$city,"QueilÈn","Queilén")
datS1$city <- str_replace(datS1$city,"QuilpuÈ","Quilpué")
datS1$city <- str_replace(datS1$city,"ChaitÈn","Chaitén")
datS1$city <- str_replace(datS1$city,"Curaco de VÈlez","Curaco de Vélez")
datS1$city <- str_replace(datS1$city,"LicantÈn","Licantén")

datS1$city <- str_replace(datS1$city,"Los ¡lamos","Los Álamos")
datS1$city <- str_replace(datS1$city,"Los ¡ngeles","Los Ángeles")
datS1$city <- str_replace(datS1$city,"ViÒa del Mar","Viña del Mar")
datS1$city <- str_replace(datS1$city,"ChaÒaral","Chañaral")
datS1$city <- str_replace(datS1$city,"VicuÒa","Vicuña")
datS1$city <- str_replace(datS1$city,"CaÒete","Cañete")
datS1$city <- str_replace(datS1$city,"HualaÒÈ","Hualañé")
datS1$city <- str_replace(datS1$city,"Colb˙n","Colbún")
datS1$city <- str_replace(datS1$city,"Tir˙a","Tirúa")
datS1$city <- str_replace(datS1$city,"Futaleuf˙","Futaleufú")


#dowload shapefile from: https://www.bcn.cl/siit/mapas_vectoriales/index_html

# Load shapefile
shapename <- read_sf('C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Data/Comunas/comunas.shp')

geo_df <- st_as_sf(shapename)

shapename1 <- geo_df %>% mutate(lon = st_coordinates(st_centroid(geo_df$geometry))[,1],
                                lat = st_coordinates(st_centroid(geo_df$geometry))[,2])

shapename_short <- shapename1 %>% select(Region:lat)
write.csv(shapename_short,file='C:/WBG/LocalITUtilities/ARG/geo.csv',row.names = FALSE)

rm(shapename1,geo_df,shapename)

#change the names so they match
#"La Calera"   "Los Álamos"  "Los Ángeles"

shapename_short$Comuna[shapename_short$Comuna=="Calera"]<-"La Calera"
shapename_short$Comuna[shapename_short$Comuna=="Los Alamos"]<-"Los Álamos"
shapename_short$Comuna[shapename_short$Comuna=="Los Angeles"]<-"Los Ángeles"

df_geo = merge(datS1, shapename_short, by.x=c("city"), by.y=c("Comuna"), all.x = TRUE)

##drop <- c("geometry")
##df_geo_small = df_geo[,!(names(df_geo) %in% drop)]

df_geo_small = df_geo

# ===================================================
# SUMMARIZE THE NUMBER OF POWER UNITS
# ===================================================

df_geo_small$punits <- 0

df_geo_small$punits[df_geo_small$city=="Antofagasta"] <- 1 # Antofagasta = 1
df_geo_small$punits[df_geo_small$city=="Cabrero"] <- 4 # Cabrero = 4
df_geo_small$punits[df_geo_small$city=="Combarbalá"] <- 1 # Combarbal<e1> = 1 # Combarbalá
df_geo_small$punits[df_geo_small$city=="Concón"] <- 1 # Conc<f3>n = 1 # Concón
df_geo_small$punits[df_geo_small$city=="Copiapó"] <- 2 # Copiap<f3> = 2 # Copiapó
df_geo_small$punits[df_geo_small$city=="Coquimbo"] <- 1 # Coquimbo = 1
df_geo_small$punits[df_geo_small$city=="Diego de Almagro"] <- 2 # Diego de Almagro = 2
df_geo_small$punits[df_geo_small$city=="Huasco"] <- 1 # Huasco = 1
df_geo_small$punits[df_geo_small$city=="Llaillay"] <- 1 # Llaillay = 1
df_geo_small$punits[df_geo_small$city=="Los Vilos"] <- 2 # Los Vilos = 2
df_geo_small$punits[df_geo_small$city=="Mejillones"] <- 5 # Mejillones = 5
df_geo_small$punits[df_geo_small$city=="Puchuncaví"] <- 4 # Puchuncav<ed> = 4 # Puchuncaví
df_geo_small$punits[df_geo_small$city=="Puerto Montt"] <- 1 # Puerto Montt = 1
df_geo_small$punits[df_geo_small$city=="Quintero"] <- 1 # Quintero = 1
df_geo_small$punits[df_geo_small$city=="Tocopilla"] <- 2 # Tocopilla = 2
df_geo_small$punits[df_geo_small$city=="Vallenar"] <- 1 # Vallenar = 1


# ===================================================
# CREATING DISTANCE METRIC
# ===================================================

# Calculate distance; https://stackoverflow.com/questions/27928/calculate-distance-between-two-latitude-longitude-points-haversine-formula

fdistance <- function(lat1,lat2,lon1,lon2){
  
  dlon = lon2 - lon1 
  dlat = lat2 - lat1 
  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
  c = 2 * atan2( sqrt(a), sqrt(1-a) ) 
  distance1 = 3961 * c
  distance = distance1*pi/180
  return(distance)
}


# Create indices

#tt2 <- subset(df_geo_small, city == "Antofagasta", select = c(prov,year,id,city,yl,punits,lon,lat))
#tt3 <- datS1[datS1$city == "Antofagasta",c(40,1,39,55,36)]

citynames <- c("Antofagasta","Cabrero","Combarbalá","Concón","Copiapó","Coquimbo","Diego de Almagro","Huasco","Llaillay","Los Vilos","Mejillones",
               "Puchuncaví","Puerto Montt","Quintero","Tocopilla","Vallenar")

short <- df_geo_small %>% filter(year=="2010") %>% select(city,lon,lat) %>% distinct()
#short$ANTO <- fdistance(short$lat,short$lat[which(short$city=="Antofagasta")],short$lon,short$lon[which(short$city=="Antofagasta")])

#i <- 0
for(z in citynames){
  #short$z <- fdistance(short$lat,short$lat[which(short$city==z)],short$lon,short$lon[which(short$city==z)])
  a1<- print(paste("short$`",z,"`<- fdistance(short$lat,short$lat[which(short$city=='",z,"')],short$lon,short$lon[which(short$city=='",z,"')])",sep=""))
  eval(parse(text=a1))
  rm(a1)
}


short$Min <- apply(short[, 4:length(short)], 1, min, na.rm = TRUE)
short$DistDum <- ifelse(short$Min<1.6*10,1,0)

datS1 = merge(datS1, short[,c("city","Min","DistDum")], by="city", all.x = TRUE)

datS2$min <-  datS1$min #= merge(datS1, short[,c("city","Min")], by="city", all.x = TRUE) # This is the final dataset
datS2$DistDum <-  datS1$DistDum

rm(datS1,df_geo,short,shapename_short)

saveRDS(datS2, file = "C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Data/Stata/Forrobust.rds")
# Restore the object
readRDS(file = "Forrobust.rds")

# ===================================================
# SUMMARY STATISTICS
# ===================================================

datS2 %>% summarize(minPol = min(min_pm25),
                    maxPol = max(max_pm25),
                    meanPol = mean(mean_pm25),
                    minSales = min(VentasanualesenUF), # Sales (annual)
                    maxSales = max(VentasanualesenUF),
                    meanSales = mean(VentasanualesenUF),
                    minWorkers = min(Trabajadoresponderadospormese), # Employees (monthly weighted averages)
                    maxWorkers = max(Trabajadoresponderadospormese),
                    meanWorkers = mean(Trabajadoresponderadospormese),
                    minWage = min(Rentanetainformada), # Wages
                    maxWage = max(Rentanetainformada),
                    meanWage = mean(Rentanetainformada),
                    Numbers = n())

datS2 %>% filter(year=="2016") %>% summarize(sum2=length(unique(city[mean_pm25>5])))

dan<- datS2 %>% filter(year=="2016") %>% select(city,mean_pm25)
dan1 <- unique(dan$city)
NROW(unique(dan[dan$mean_pm25>5,]))
  
NROW(unique(datS2$Númerodeempresas)) # Number of firm
NROW(unique(datS2$city)) # Number of cities

datS2$lnRWage <- log(datS2$Rentanetainformada/datS2$CPI) # Real wage
datS2$lnEMP <- log(datS2$Trabajadoresponderadospormese)
datS2$lnsales <- log(datS2$sales)

datS2 <- datS2 %>% group_by(year) %>% mutate(dum_dm = dummy-mean(dummy))


#datS3 <- transform(datS1,ID2 = as.numeric(factor(id)))

# https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html#3_Instrumental_variables
library(fixest)

# Vanilla case
est_panel = feols(yl~lpm, datS2, panel.id = ~id + year)
summary(est_panel, "newey_west")

# Vanilla case
est_panel2 = feols(yl~1|year| lpm~dummy, datS2)
summary(est_panel2, stage = 1)
etable(summary(est_panel2, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)


# Vanilla case for IV
est_iv = feols(yl~1 |id+year+prov+sector4| lpm~dummy, datS2) # Region
est_iv
summary(est_iv, stage = 1)
etable(summary(est_iv, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

# Varying slopes case for IV
est_iv1 = feols(yl~1 |id+year+sector4+prov[year]| lpm~dummy, datS2)
est_iv1
summary(est_iv1, stage = 1)
etable(summary(est_iv1, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

# Varying slopes case for IV
est_iv2 = feols(yl~1 |id+sector4[year]+prov[year]| lpm~dummy, datS2)
est_iv2
summary(est_iv2, stage = 1)
etable(summary(est_iv2, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

#########################

# City and time fixed effects
est_panel = feols(d_ly_mb~d_lpm_mb|city+year, datS2, panel.id = ~id+year)
summary(est_panel, "newey_west")

# Time fixed effects
est_panel = feols(d_ly_mb~d_lpm_mb|year, datS2, panel.id = ~id+year)
summary(est_panel, "newey_west")

# Subsector, province, time fixed effects
est_panel = feols(d_ly_mb~d_lpm_mb|year+sector4+prov, datS2, panel.id = ~id+year)
summary(est_panel, "newey_west")

# province, time , subs*time, prov*time fixed effects
est_panel = feols(d_ly_mb~d_lpm_mb|year+prov+prov[year]+sector4[year], datS2, panel.id = ~id+year)
summary(est_panel, "newey_west")

# province, time , subs*time, prov*time fixed effects
est_panel = feols(d_ly_mb~1|prov[year]+sector4[year]| d_lpm_mb~dummy, datS2, panel.id = ~id+year)
est_panel = feols(d_ly_mb~1|year+prov+prov[year]+sector4[year]| d_lpm_mb~dummy, datS2, panel.id = ~id+year)
etable(summary(est_panel, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

# city, time , subs*time, city*time fixed effects: If there are significant differences – means that there are spill-overs across cities in the same province
est_panel = feols(d_ly_mb~1|year+city+city[year]+sector4[year]| d_lpm_mb~dummy, datS2, panel.id = ~id+year)
etable(summary(est_panel, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

# province fixed effects: Checking whether there are spill-overs across sectors
est_panel = feols(d_ly_mb~1|year+prov[year]| d_lpm_mb~dummy, datS2, panel.id = ~id+year)
etable(summary(est_panel, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

#########################

# Paper

# Vanilla case
est_panel = feols(yl~lpm|year+prov+sector4[year], datS2, panel.id = ~id + year)
summary(est_panel, "newey_west")

# Vanilla case
est_ivPap = feols(yl~1|year+prov+sector4[year]| lpm~dummy, datS2)
etable(summary(est_ivPap, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)
summary(est_ivPap, DK ~ sector)
summary(est_ivPap, vcov = "twoway")
summary(est_ivPap, vcov=~prov)


est_ivPapMain = feols(yl~1|year+prov+prov[year]+sector4[year]| lpm~dummy, datS2)
etable(summary(est_ivPapMain, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

summary(est_ivPapMain, DK ~ sector)
summary(est_ivPapMain, vcov = "twoway")
summary(est_ivPapMain, vcov=~prov)

# Simple test of instrument: LPM on dummy OLS without constant
#model <- lm(lpm ~ 0 + dummy, data = datS2)



library(plm)
model11 <- plm(lpm ~ 0 + dummy, data = datS2, model = "pooling")
summary(model11)
fitstat(model11, "f", simplify = TRUE)

model112 = feols(lpm ~ 0 + dummy, datS2,)
summary(model112, vcov = ~sector) # Clustered standard errors around province
summary(model112, "NW")
fitstat(model112, "f.stat", simplify = TRUE)

model2 = feols(lpm~dummy|year+prov+sector4[year], datS2, panel.id = ~id + year)
summary(model2, vcov = "twoway")
summary(model2, "newey_west")
summary(model2, DK ~ sector)
summary(model2, "DK")
summary(model2, vcov = ~sector+year) # Clustered standard errors around province
fitstat(model2, "f.stat", simplify = TRUE)

# Wages
summary(lm(yl~lpm,data=datS2))
est_panelW = feols(lnEMP~lpm|year+prov+sector4[year], datS2, panel.id = ~id + year)
summary(est_panelW, "newey_west")
#lnRWage
#lnEMP
#lnsales

# ===============================================================================
### Run sector specific effects [AGRICULTURE]
datS2$sector[datS2$sector=="A - Agricultura, ganader<ed>a, silvicultura y pesca"] <- "Agriculture"
datAGR<-datS2[datS2$sector %in% "Agriculture", ] # Keep only variables with data

summary(datAGR$min)
datAGR$DistDum <- ifelse(datAGR$min<2,1,0)
datAGR$DistDum <- case_when(datAGR$min <= 1 ~ 1,
                            datAGR$min <= 2 & datAGR$min > 1 ~ 2,
                            datAGR$min <= 3 & datAGR$min > 2 ~ 3,
                            datAGR$min <= 10 & datAGR$min > 3 ~ 4,
                            datAGR$min > 10 ~ 0)


# Vanilla case
est_panelA = feols(yl~lpm|year+sector4[year]+prov[year], datAGR, panel.id = ~id + year)
summary(est_panelA, "newey_west")

# Vanilla case
est_ivPapA = feols(yl~1|year+sector4[year]+prov[year]| lpm~dummy, datAGR)
etable(summary(est_ivPapA, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

est_ivPapA = feols(yl~1|year+sector4[year]+prov[year]| lpm~i(dummy,DistDum), datAGR)
etable(summary(est_ivPapA, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)


est_cj = feols(yl~i(DistDum,lpm)|prov+sector4+year, datAGR)
summary(est_cj)
iplot(est_cj)

est_cj$coeftable[,1] # Coefficients
est_cj$coeftable[,2] # Standard errors

agrBeta <- est_cj$coeftable[,1]
agrSD <- est_cj$coeftable[,2]
range<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")
range_order<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")

agrplot<-data.frame(agrBeta,agrSD,range)
agrplot$lower <- agrplot$agrBeta-1.96*agrplot$agrSD
agrplot$upper <- agrplot$agrBeta+1.96*agrplot$agrSD

plot_coeffs <- function(mlr_model,nameCJ) {
  coeffs <- mlr_model$coeftable[,1]
  mp <- barplot(coeffs, col="black", xaxt='n', main="Pollution impact on productivity conditional on distance from power station")
  lablist <- nameCJ
  grid()
  text(mp, par("usr")[3], labels = lablist, srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=0.6)
}
plot_coeffs(est_cj,year)


pAGR <- ggplot(agrplot, aes(x=range, y=agrBeta))+geom_point(aes(x=factor(range,level=range_order), y=agrBeta),shape=15, position = position_dodge(width = 0.25))+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=.25, position = position_dodge(width = 0.25),size=.65)+
  geom_hline(yintercept = 0, color='firebrick')+theme_bw()+labs(title="Pollution on agriculture productivity",y= "Estimate with 95% CI", x="")



# ===============================================================================
### Run sector specific effects [Manufacturing]
datS2$sector[datS2$sector=="C - Industria manufacturera"] <- "Manufacturing"
datMAN<-datS2[datS2$sector %in% "Manufacturing", ] # Keep only variables with data

datMAN$DistDum <- case_when(datMAN$min <= 1 ~ 1,
                            datMAN$min <= 2 & datMAN$min > 1 ~ 2,
                            datMAN$min <= 3 & datMAN$min > 2 ~ 3,
                            datMAN$min <= 10 & datMAN$min > 3 ~ 4,
                            datMAN$min > 10 ~ 0)

# Vanilla case
est_ivPapM = feols(yl~1|year+sector4[year]+prov| lpm~dummy, datMAN)
etable(summary(est_ivPapM, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

est_ivPapM = feols(yl~1|year+sector4[year]+prov[year]| lpm~dummy+min+dummy*min, datMAN)
etable(summary(est_ivPapM, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)



est_cjM = feols(yl~i(DistDum,lpm)|prov+sector4+year, datMAN)
summary(est_cjM)
iplot(est_cjM)


manBeta <- est_cjM$coeftable[,1] # Coefficients
manSD <- est_cjM$coeftable[,2] # Standard errors
range<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")
range_order<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")

manplot<-data.frame(manBeta,manSD,range)
manplot$lower <- manplot$manBeta-1.96*manplot$manSD
manplot$upper <- manplot$manBeta+1.96*manplot$manSD

pMAN <- ggplot(manplot, aes(x=range, y=manBeta))+geom_point(aes(x=factor(range,level=range_order), y=manBeta),shape=15, position = position_dodge(width = 0.25))+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=.25, position = position_dodge(width = 0.25),size=.65)+
  geom_hline(yintercept = 0, color='firebrick')+theme_bw()+labs(title="Pollution on manufacturing productivity",y= "Estimate with 95% CI", x="")

# ===============================================================================
### Run sector specific effects [Construction]
datS2$sector[datS2$sector=="F - Construcci<f3>n"] <- "Construction"
datCON<-datS2[datS2$sector %in% "Construction", ] # Keep only variables with data

datCON$DistDum <- case_when(datCON$min <= 1 ~ 1,
                            datCON$min <= 2 & datCON$min > 1 ~ 2,
                            datCON$min <= 3 & datCON$min > 2 ~ 3,
                            datCON$min <= 10 & datCON$min > 3 ~ 4,
                            datCON$min > 10 ~ 0)

# Vanilla case
est_ivPapC = feols(yl~1|year+sector4[year]+prov| lpm~dummy, datCON)
etable(summary(est_ivPapC, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

est_ivPapC= feols(yl~1|year+sector4[year]+prov[year]| lpm~dummy+min+dummy*min, datCON)
etable(summary(est_ivPapC, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)


est_cjC = feols(yl~i(DistDum,lpm)|prov+sector4+year, datCON)
summary(est_cjC)
iplot(est_cjC)


cnsBeta <- est_cjC$coeftable[,1] # Coefficients
cnsSD <- est_cjC$coeftable[,2] # Standard errors
range<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")
range_order<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")

cnsplot<-data.frame(cnsBeta,cnsSD,range)
cnsplot$lower <- cnsplot$cnsBeta-1.96*cnsplot$cnsSD
cnsplot$upper <- cnsplot$cnsBeta+1.96*cnsplot$cnsSD

pCNS <-ggplot(cnsplot, aes(x=range, y=cnsBeta))+geom_point(aes(x=factor(range,level=range_order), y=cnsBeta),shape=15, position = position_dodge(width = 0.25))+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=.25, position = position_dodge(width = 0.25),size=.65)+
  geom_hline(yintercept = 0, color='firebrick')+theme_bw()+labs(title="Pollution on construction productivity",y= "Estimate with 95% CI", x="")

# ===============================================================================
### Run sector specific effects [Transport]
datS2$sector[datS2$sector=="H - Transporte y almacenamiento"] <- "Transport"
datTRN<-datS2[datS2$sector %in% "Transport", ] # Keep only variables with data

datTRN$DistDum <- case_when(datTRN$min <= 1 ~ 1,
                            datTRN$min <= 2 & datTRN$min > 1 ~ 2,
                            datTRN$min <= 3 & datTRN$min > 2 ~ 3,
                            datTRN$min <= 10 & datTRN$min > 3 ~ 4,
                            datTRN$min > 10 ~ 0)

# Vanilla case
est_ivPapT = feols(yl~1|year+sector4[year]+prov| lpm~dummy, datTRN)
etable(summary(est_ivPapT, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

est_ivPapT = feols(yl~1|year+sector4[year]+prov[year]| lpm~dummy+min+dummy*min, datTRN)
etable(summary(est_ivPapT, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)


est_cjT = feols(yl~i(DistDum,lpm)|prov+sector4+year, datTRN)
summary(est_cjT)
iplot(est_cjT)


TRNBeta <- est_cjT$coeftable[,1] # Coefficients
TRNSD <- est_cjT$coeftable[,2] # Standard errors
range<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")
range_order<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")

TRNplot<-data.frame(TRNBeta,TRNSD,range)
TRNplot$lower <- TRNplot$TRNBeta-1.96*TRNplot$TRNSD
TRNplot$upper <- TRNplot$TRNBeta+1.96*TRNplot$TRNSD

pTRN <-ggplot(TRNplot, aes(x=range, y=TRNBeta))+geom_point(aes(x=factor(range,level=range_order), y=TRNBeta),shape=15, position = position_dodge(width = 0.25))+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=.25, position = position_dodge(width = 0.25),size=.65)+
  geom_hline(yintercept = 0, color='firebrick')+theme_bw()+labs(title="Pollution on transport productivity",y= "Estimate with 95% CI", x="")

# ===============================================================================
### Run sector specific effects [Wholesale]
datS2$sector[datS2$sector=="G - Comercio al por mayor y al por menor; reparaci<f3>n de veh<ed>culos automotores y motocicletas"] <- "Wholesale"
datWHS<-datS2[datS2$sector %in% "Wholesale", ] # Keep only variables with data

datWHS$DistDum <- case_when(datWHS$min <= 1 ~ 1,
                            datWHS$min <= 2 & datWHS$min > 1 ~ 2,
                            datWHS$min <= 3 & datWHS$min > 2 ~ 3,
                            datWHS$min <= 10 & datWHS$min > 3 ~ 4,
                            datWHS$min > 10 ~ 0)

# Vanilla case
est_ivPapW = feols(yl~1|year+sector4[year]+prov| lpm~dummy, datWHS)
etable(summary(est_ivPapW, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)

est_ivPapW = feols(yl~1|year+sector4[year]+prov[year]| lpm~dummy+min+dummy*min, datWHS)
etable(summary(est_ivPapW, stage = 1:2), fitstat = ~ . + ivfall + ivwaldall.p)


est_cjW = feols(yl~i(DistDum,lpm)|prov+sector4+year, datWHS)
summary(est_cjW)
iplot(est_cjW)


WHSBeta <- est_cjW$coeftable[,1] # Coefficients
WHSSD <- est_cjW$coeftable[,2] # Standard errors
range<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")
range_order<-c("1 KM","1-2KM","2-3KM","3-10KM",">10KM")

WHSplot<-data.frame(WHSBeta,WHSSD,range)
WHSplot$lower <- WHSplot$WHSBeta-1.96*WHSplot$WHSSD
WHSplot$upper <- WHSplot$WHSBeta+1.96*WHSplot$WHSSD

pWHS <-ggplot(WHSplot, aes(x=range, y=WHSBeta))+geom_point(aes(x=factor(range,level=range_order), y=WHSBeta),shape=15, position = position_dodge(width = 0.25))+
  geom_errorbar(aes(ymin=lower, ymax=upper),width=.25, position = position_dodge(width = 0.25),size=.65)+
  geom_hline(yintercept = 0, color='firebrick')+theme_bw()+labs(title="Pollution on wholesale productivity",y= "Estimate with 95% CI", x="")

# ==================================
# SOME CHARTS


jpg(filename = "C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Paper/jpg", 
    pointsize =12, width = 800, bg = "white", res = NA, restoreConsole = TRUE) # quality = 200,

pdf(file = "C:/Users/wb481167/OneDrive - WBG/Charldocs/Research/Pollution/Paper/AllPOL.pdf",width = 12)
multiplot(pAGR,pMAN,pCNS,pTRN,pWHS, cols=3)
dev.off()


theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_blank(),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         legend.position="bottom",
                         plot.title = element_text(size=16)))

#shapename2 <- geo_df %>% mutate(lon = unlist(map(geo_df$geometry,1)),
#                                  lat = unlist(map(geo_df$geometry,2)))

#shapename2 <- geo_df %>%
#  mutate(long = unlist(map(geo_df$geometry,1)),
#         lat = unlist(map(geo_df$geometry,2)))

# ggplot()+geom_sf(data = shapename, size = 3, color = "black")+ggtitle("Chile Boundaries")# + coord_sf()

ggplot(data = shapename1)+geom_sf()+ggtitle("Chile Boundaries")+coord_sf(xlim = c(-7559648,-12172891),ylim=c(-7559648,-7367328), expand = FALSE)
ggplot(shapename1) +geom_sf(aes(geometry = shapename1$geometry), color = "grey40", size = 0.5)# +coord_sf(xlim = c(90,60),ylim=c(60,20), expand = FALSE)


# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

punits <- df_geo_small %>% select(city,punits,lon,lat)
punits <- punits %>% distinct()
punits <- punits %>% drop_na()
punits <- punits %>% filter(punits > 0)

#ggplot(data = shapename)+geom_sf()+geom_point(data = punits, mapping = aes(x = lon, y = lat,fill=punits))+
#  ggtitle("Chile Boundaries")+scale_fill_viridis_c(option = "plasma", trans = "sqrt")+theme_bw()#+coord_sf(xlim = c(-80,-60), expand = FALSE) #theme_opts# +coord_sf(xlim = c(60,80), ylim = c(10,60))


ggplot() +
  geom_sf(data = shapename) +
  geom_point(data = punits, aes(x = lon, y = lat,col = "red", size =punits)) + # col = "red", size = 3
  labs(x = "Longitude", y = "Latitude", fill = "") +
  theme_bw() +
  theme(panel.grid = element_line(colour = "transparent"),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12))



#::::::::::::::::::::::::::::::::::::::::
### FUNCTIONS ###

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
