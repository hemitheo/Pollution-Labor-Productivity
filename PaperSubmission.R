# ===================================================
# LOAD LIBRARIES
# ===================================================
library('sf')
library(readstata13)
library(tidyverse)

# ===================================================
# STEP 1: READ DATA
# ===================================================
datS1 <- read.dta13("C:/Users/Data/Stata/Cleaned_AUG2022.dta", fromEncoding="macroman")
datS2 <- read.dta13("C:/Users//Data/Stata/Cleaned_AUG2022.dta")
datS1$city <- as.character(datS1$city)
datS2$city <- as.character(datS2$city)

# ===================================================
# STEP 2: ENCODING FIXES (applied to BOTH datS1 and datS2)
# ===================================================
fix_encoding <- function(x) {
  x <- str_replace(x,"Combarbal<e1><e1>","Combarbalá")
  x <- str_replace(x,"Combarbal·","Combarbalá")
  x <- str_replace(x,"Combarbal<e1>","Combarbalá")
  x <- str_replace(x,"Conc<f3>n","Concón")
  x <- str_replace(x,"ConcÛn","Concón")
  x <- str_replace(x,"Concepci<f3>n","Concepción")
  x <- str_replace(x,"ConcepciÛn","Concepción")
  x <- str_replace(x,"Copiap<f3>","Copiapó")
  x <- str_replace(x,"CopiapÛ","Copiapó")
  x <- str_replace(x,"Puchuncav<ed>","Puchuncaví")
  x <- str_replace(x,"PuchuncavÌ","Puchuncaví")
  x <- str_replace(x,"Valpara<ed>so","Valparaíso")
  x <- str_replace(x,"ValparaÌso","Valparaíso")
  x <- str_replace(x,"Mar<ed>a Elena","María Elena")
  x <- str_replace(x,"MarÌa Elena","María Elena")
  x <- str_replace(x,"Longav<ed>","Longaví")
  x <- str_replace(x,"LongavÌ","Longaví")
  x <- str_replace(x,"R<ed>o Negro","Río Negro")
  x <- str_replace(x,"RÌo Negro","Río Negro")
  x <- str_replace(x,"Maull<ed>n","Maullín")
  x <- str_replace(x,"MaullÌn","Maullín")
  x <- str_replace(x,"Santa Mar<ed>a","Santa María")
  x <- str_replace(x,"Santa MarÌa","Santa María")
  x <- str_replace(x,"R<ed>o Claro","Río Claro")
  x <- str_replace(x,"RÌo Claro","Río Claro")
  x <- str_replace(x,"Curic<f3>","Curicó")
  x <- str_replace(x,"CuricÛ","Curicó")
  x <- str_replace(x,"Quell<f3>n","Quellón")
  x <- str_replace(x,"QuellÛn","Quellón")
  x <- str_replace(x,"Cocham<f3>","Cochamó")
  x <- str_replace(x,"CochamÛ","Cochamó")
  x <- str_replace(x,"Constituci<f3>n","Constitución")
  x <- str_replace(x,"ConstituciÛn","Constitución")
  x <- str_replace(x,"Puqueld<f3>n","Puqueldón")
  x <- str_replace(x,"PuqueldÛn","Puqueldón")
  x <- str_replace(x,"Santa B<e1>rbara","Santa Bárbara")
  x <- str_replace(x,"Santa B·rbara","Santa Bárbara")
  x <- str_replace(x,"Juan Fern<e1>ndez","Juan Fernández")
  x <- str_replace(x,"Juan Fern·ndez","Juan Fernández")
  x <- str_replace(x,"Tom<e9>","Tomé")
  x <- str_replace(x,"TomÈ","Tomé")
  x <- str_replace(x,"Vichuqu<e9>n","Vichuquén")
  x <- str_replace(x,"VichuquÈn","Vichuquén")
  x <- str_replace(x,"Olmu<e9>","Olmué")
  x <- str_replace(x,"OlmuÈ","Olmué")
  x <- str_replace(x,"Mulch<e9>n","Mulchén")
  x <- str_replace(x,"MulchÈn","Mulchén")
  x <- str_replace(x,"Hualp<e9>n","Hualpén")
  x <- str_replace(x,"HualpÈn","Hualpén")
  x <- str_replace(x,"Queil<e9>n","Queilén")
  x <- str_replace(x,"QueilÈn","Queilén")
  x <- str_replace(x,"Quilpu<e9>","Quilpué")
  x <- str_replace(x,"QuilpuÈ","Quilpué")
  x <- str_replace(x,"Chait<e9>n","Chaitén")
  x <- str_replace(x,"ChaitÈn","Chaitén")
  x <- str_replace(x,"Curaco de V<e9>lez","Curaco de Vélez")
  x <- str_replace(x,"Curaco de VÈlez","Curaco de Vélez")
  x <- str_replace(x,"Licant<e9>n","Licantén")
  x <- str_replace(x,"LicantÈn","Licantén")
  x <- str_replace(x,"Los <c1>ngeles","Los Ángeles")
  x <- str_replace(x,"Los ¡ngeles","Los Ángeles")
  x <- str_replace(x,"Los <c1>lamos","Los Álamos")
  x <- str_replace(x,"Los ¡lamos","Los Álamos")
  x <- str_replace(x,"Vi<f1>a del Mar","Viña del Mar")
  x <- str_replace(x,"ViÒa del Mar","Viña del Mar")
  x <- str_replace(x,"Cha<f1>aral","Chañaral")
  x <- str_replace(x,"ChaÒaral","Chañaral")
  x <- str_replace(x,"Vicu<f1>a","Vicuña")
  x <- str_replace(x,"VicuÒa","Vicuña")
  x <- str_replace(x,"Ca<f1>ete","Cañete")
  x <- str_replace(x,"CaÒete","Cañete")
  x <- str_replace(x,"Huala<f1><e9>","Hualañé")
  x <- str_replace(x,"HualaÒÈ","Hualañé")
  x <- str_replace(x,"Colb<fa>n","Colbún")
  x <- str_replace(x,"Colb˙n","Colbún")
  x <- str_replace(x,"Tir<fa>a","Tirúa")
  x <- str_replace(x,"Tir˙a","Tirúa")
  x <- str_replace(x,"Futaleuf<fa>","Futaleufú")
  x <- str_replace(x,"Futaleuf˙","Futaleufú")
  x <- str_replace(x,"Hualaihu<e9>","Hualaiué")
  x <- str_replace(x,"Alto Biob<ed>o","Alto Biobío")
  return(x)
}

datS1$city <- fix_encoding(datS1$city)
datS2$city <- fix_encoding(datS2$city)

# ===================================================
# STEP 3: LOAD SHAPEFILE AND BUILD df_geo_small
# ===================================================
shapename <- read_sf('C:/Users/Data/Comunas/comunas.shp')
geo_df <- st_as_sf(shapename)
shapename1 <- geo_df %>% mutate(
  lon = st_coordinates(st_centroid(geo_df$geometry))[,1],
  lat = st_coordinates(st_centroid(geo_df$geometry))[,2]
)
shapename_short <- shapename1 %>% select(Region:lat)
write.csv(shapename_short, file='C:/Users/geo.csv', row.names=FALSE)
rm(shapename1, geo_df, shapename)

# Fix shapefile city name mismatches
shapename_short$Comuna[shapename_short$Comuna=="Calera"]     <- "La Calera"
shapename_short$Comuna[shapename_short$Comuna=="Los Alamos"] <- "Los Álamos"
shapename_short$Comuna[shapename_short$Comuna=="Los Angeles"]<- "Los Ángeles"

df_geo      <- merge(datS1, shapename_short, by.x="city", by.y="Comuna", all.x=TRUE)
df_geo_small <- df_geo

# ===================================================
# STEP 4: ASSIGN POWER UNITS
# ===================================================
df_geo_small$punits <- 0
df_geo_small$punits[df_geo_small$city=="Antofagasta"]    <- 1
df_geo_small$punits[df_geo_small$city=="Cabrero"]        <- 4
df_geo_small$punits[df_geo_small$city=="Combarbalá"]     <- 1
df_geo_small$punits[df_geo_small$city=="Concón"]         <- 1
df_geo_small$punits[df_geo_small$city=="Copiapó"]        <- 2
df_geo_small$punits[df_geo_small$city=="Coquimbo"]       <- 1
df_geo_small$punits[df_geo_small$city=="Diego de Almagro"]<- 2
df_geo_small$punits[df_geo_small$city=="Huasco"]         <- 1
df_geo_small$punits[df_geo_small$city=="Llaillay"]       <- 1
df_geo_small$punits[df_geo_small$city=="Los Vilos"]      <- 2
df_geo_small$punits[df_geo_small$city=="Mejillones"]     <- 5
df_geo_small$punits[df_geo_small$city=="Puchuncaví"]     <- 4
df_geo_small$punits[df_geo_small$city=="Puerto Montt"]   <- 1
df_geo_small$punits[df_geo_small$city=="Quintero"]       <- 1
df_geo_small$punits[df_geo_small$city=="Tocopilla"]      <- 2
df_geo_small$punits[df_geo_small$city=="Vallenar"]       <- 1

# ===================================================
# STEP 5: COMPUTE DISTANCES
# ===================================================
fdistance <- function(lat1, lat2, lon1, lon2){
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  distance <- 3961 * c * pi/180
  return(distance)
}

citynames <- c("Antofagasta","Cabrero","Combarbalá","Concón","Copiapó","Coquimbo",
               "Diego de Almagro","Huasco","Llaillay","Los Vilos","Mejillones",
               "Puchuncaví","Puerto Montt","Quintero","Tocopilla","Vallenar")

# Build short with ONE row per city, dropping missing coordinates
short <- df_geo_small %>%
  select(city, lon, lat) %>%
  filter(!is.na(lon) & !is.na(lat)) %>%   # KEY FIX: drop cities with no shapefile match
  group_by(city) %>%
  summarise(
    lon = mean(lon, na.rm=TRUE),
    lat = mean(lat, na.rm=TRUE),
    .groups = "drop"
  )

# Check all power station cities are present
missing_ps <- citynames[!citynames %in% short$city]
if(length(missing_ps) > 0){
  cat("WARNING - these power station cities are missing from short:\n")
  print(missing_ps)
} else {
  cat("All power station cities found in short. OK\n")
}

# Compute distances
for(z in citynames){
  short[[z]] <- fdistance(
    short$lat,
    short$lat[which(short$city == z)],
    short$lon,
    short$lon[which(short$city == z)]
  )
}

# Take minimum distance across all power station cities
short$Min    <- apply(short[, citynames], 1, min, na.rm=TRUE)
short$DistDum <- ifelse(short$Min < 1.6*10, 1, 0)

cat("Summary of Min distances:\n")
print(summary(short$Min))

# ===================================================
# STEP 6: MERGE DISTANCE INTO datS2 AND CLEAN UP NAs
# ===================================================
datS2 <- merge(datS2, short[, c("city","Min","DistDum")], by="city", all.x=TRUE)
names(datS2)[names(datS2)=="Min"] <- "min"

# Assign large distance to unmatched cities (e.g. Hualaiué - 30 rows)
datS2$min[is.na(datS2$min)]       <- 999
datS2$DistDum[is.na(datS2$DistDum)] <- 0

# Final checks
cat("Remaining NAs in min:", sum(is.na(datS2$min)), "\n")
cat("Remaining NAs in DistDum:", sum(is.na(datS2$DistDum)), "\n")
cat("Summary of min in datS2:\n")
print(summary(datS2$min))

# Confirm time-invariant (one unique min per city)
check <- datS2 %>%
  group_by(city) %>%
  summarise(n_unique_min = n_distinct(min, na.rm=TRUE), .groups="drop") %>%
  summarise(any_variation = any(n_unique_min > 1))
cat("Any within-city variation in min (should be FALSE):", check$any_variation, "\n")

# ===================================================
# STEP 7: CLEAN UP AND SAVE
# ===================================================
rm(datS1, df_geo, df_geo_small, short, shapename_short)

saveRDS(datS2, file="C:/Users/Data/Stata/Forrobust2.rds")
cat("Saved successfully.\n")# Restore the object
readRDS(file = "Forrobust2.rds")

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

# Function for plotting
# Robust plotting function that works regardless of how many DistDum categories exist
plot_sector_coefs <- function(est_cj, title) {
  
  coef_table <- est_cj$coeftable
  coef_names <- rownames(coef_table)
  
  # Extract number between "::" and ":lpm"
  distdum_vals <- gsub("DistDum::(\\d+):lpm", "\\1", coef_names)
  
  label_map <- c(
    "0" = ">10 KM",
    "1" = "1 KM",
    "2" = "1-2 KM",
    "3" = "2-3 KM",
    "4" = "3-10 KM"
  )
  range_order <- c("1 KM","1-2 KM","2-3 KM","3-10 KM",">10 KM")
  
  plotdf <- data.frame(
    Beta  = coef_table[,1],
    SD    = coef_table[,2],
    range = label_map[distdum_vals]
  )
  plotdf$lower <- plotdf$Beta - 1.96*plotdf$SD
  plotdf$upper <- plotdf$Beta + 1.96*plotdf$SD
  
  ggplot(plotdf, aes(x=factor(range, level=range_order), y=Beta)) +
    geom_point(shape=15, position=position_dodge(width=0.25)) +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.25,
                  position=position_dodge(width=0.25), linewidth=.65) +
    geom_hline(yintercept=0, color='firebrick') +
    theme_bw() +
    labs(title=title, y="Estimate with 95% CI", x="Distance to power station")
}


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
pAGR <- plot_sector_coefs(est_cj,  "Pollution on agriculture productivity")
summary(est_cj)
iplot(est_cj)

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
pMAN <- plot_sector_coefs(est_cjM, "Pollution on manufacturing productivity")

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
pCNS <- plot_sector_coefs(est_cjC,  "Pollution on construction productivity")
summary(est_cjC)
iplot(est_cjC)

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
pTRN <- plot_sector_coefs(est_cjT,  "Pollution on transport productivity")
summary(est_cjT)
iplot(est_cjT)

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
pWHS <- plot_sector_coefs(est_cjW,  "Pollution on wholesale productivity")
summary(est_cjW)
iplot(est_cjW)

library(patchwork)

# Combine all sector plots into one figure
combined_plot <- (pAGR | pMAN | pCNS) / (pTRN | pWHS) +
  plot_annotation(
    title    = "Effect of Pollution on Productivity by Sector",
    subtitle = "IV estimates with 95% CI, distance buckets from nearest coal power station",
    theme    = theme(
      plot.title    = element_text(size=14, face="bold", hjust=0.5),
      plot.subtitle = element_text(size=10, hjust=0.5)
    )
  )

combined_plot


# ===================================================
# CITY-SECTOR PRODUCTIVITY GRADIENT BY DISTANCE
# ===================================================

# ===================================================
# 1. CHART - CLEANED SECTOR LABELS ONLY
# ===================================================

range_order <- c("1 KM","1-2 KM","2-3 KM","3-10 KM",">10 KM")

# Use only the relabelled sectors
clean_sectors <- c("Agriculture","Manufacturing","Construction","Transport","Wholesale")

city_sector_summary <- datS2 %>%
  filter(!is.na(min) & min != 999) %>%
  filter(sector %in% clean_sectors) %>%
  mutate(DistBucket = case_when(
    min <= 1             ~ "1 KM",
    min <= 2 & min > 1  ~ "1-2 KM",
    min <= 3 & min > 2  ~ "2-3 KM",
    min <= 10 & min > 3 ~ "3-10 KM",
    min > 10            ~ ">10 KM"
  )) %>%
  group_by(sector, DistBucket) %>%
  summarise(
    mean_yl  = mean(yl, na.rm=TRUE),
    sd_yl    = sd(yl, na.rm=TRUE),
    n_cells  = n(),
    se_yl    = sd_yl / sqrt(n_cells),
    lower    = mean_yl - 1.96*se_yl,
    upper    = mean_yl + 1.96*se_yl,
    .groups  = "drop"
  )

p_gradient <- ggplot(city_sector_summary,
       aes(x=factor(DistBucket, level=range_order), y=mean_yl, group=sector)) +
  geom_point(shape=15) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.25, linewidth=.65) +
  geom_line(linetype="dashed", alpha=0.5) +
  geom_hline(yintercept=0, color='firebrick') +
  facet_wrap(~sector, ncol=3) +
  theme_bw() +
  labs(
    title    = "Average productivity by distance to power station and sector",
    subtitle = "Mean log productivity with 95% CI across municipality-sector cells",
    y        = "Mean log productivity (yl)",
    x        = "Distance to nearest power station"
  ) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

p_gradient

ggsave("C:/Users/Pollution/gradient_by_sector.png",
       p_gradient, width=14, height=8, dpi=300)

# ===================================================
# 2. EXCEL OUTPUT - CITY-SECTOR DISTANCES
# ===================================================
library(writexl)

# Table 1: City-sector summary with distance buckets (collapsed across years)
city_sector_export <- datS2 %>%
  filter(!is.na(min) & min != 999) %>%
  filter(sector %in% clean_sectors) %>%
  mutate(DistBucket = case_when(
    min <= 1             ~ "1 KM",
    min <= 2 & min > 1  ~ "1-2 KM",
    min <= 3 & min > 2  ~ "2-3 KM",
    min <= 10 & min > 3 ~ "3-10 KM",
    min > 10            ~ ">10 KM"
  )) %>%
  select(city, prov, sector, min, DistBucket) %>%
  distinct() %>%
  rename(
    `Municipality`              = city,
    `Province`                  = prov,
    `Sector`                    = sector,
    `Distance to Power Station (miles)` = min,
    `Distance Bucket`           = DistBucket
  ) %>%
  arrange(Sector, `Distance to Power Station (miles)`)

# Table 2: Summary statistics by sector and distance bucket
city_sector_stats <- city_sector_summary %>%
  rename(
    `Sector`          = sector,
    `Distance Bucket` = DistBucket,
    `Mean Log Productivity` = mean_yl,
    `SD`              = sd_yl,
    `N Cells`         = n_cells,
    `SE`              = se_yl,
    `Lower 95% CI`    = lower,
    `Upper 95% CI`    = upper
  )

write_xlsx(
  list(
    "City-Sector Distances" = city_sector_export,
    "Productivity by Distance" = city_sector_stats
  ),
  path = "C:/Users/Pollution/city_sector_distances.xlsx"
)

cat("Excel file written.\n")


