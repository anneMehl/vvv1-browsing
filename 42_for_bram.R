setwd("P:/12894100_stipendiat_anne_mehlhoop_anvendt_okologi_og_natu/01_Browsing_pressure/analysis/Bram_Anne/")
#install.packages("glmmTMB")
library(glmmTMB)

## Load HOUSE models data----

#### Browsing pressure 
load("output/browsing_2021_02_02/browse_h_bin1.rda")
zeromod1 <- browse_h_bin1
summary(zeromod1)

load("output/browsing_2021_02_02/browse_h_beta2.rda")
percmod1 <- browse_h_beta2
summary(percmod1)

load("processdata/browse_house_2021_02_01.rda")
str(browse_house) # Check if variables are scaled, else scale below
# browse_house <- browse_house[ ,-c(4,16:20,22)] #8,

# Scale
browse_house$tretetthet9 <- scale(browse_house$tretetthet9)
browse_house$helling <- scale(browse_house$helling)
browse_house$HOH <- scale(browse_house$HOH)
# browse_house$moose_density <- scale(browse_house$moose_density)
browse_house$disthus_200 <- scale(browse_house$disthus_200)
str(browse_house)

dat1 <- browse_house
head(dat1)


#### Recruitment
load("output/recruitment_2021_02_02/rec_h_bin2_600_int.rda")
zeromod2 <- rec_h_bin2_600_int
summary(zeromod2)


load("output/recruitment_2021_02_02/rec_h_beta3_TMB_int.rda")
percmod2 <- rec_h_beta3_TMB_int
summary(percmod2)

load("processdata/recruits_house_2021_02_02.rda")
str(recruits_house)
# recruits_house <- recruits_house[ ,-c(7)]
recruits_house$recruitment_bin <- ifelse(recruits_house$recruitment == 0, 0, 1)
# scale
# recruits_house$beitetrykk9 <- scale(recruits_house$beitetrykk9)
recruits_house$helling <- scale(recruits_house$helling)
recruits_house$HOH <- scale(recruits_house$HOH)
# recruits_house$moose_density <- scale(recruits_house$moose_density)
recruits_house$disthus_600 <- scale(recruits_house$disthus_600)
# recruits_house$disthus_500 <- scale(recruits_house$disthus_500)

is.integer(recruits_house$recruitment)
recruits_house$recruitment <- as.integer(recruits_house$recruitment)
is.integer(recruits_house$recruitment)

str(recruits_house)

dat2 <- recruits_house
head(dat2)



source("function_resp_curve_08_12_2020_rev_BVM.R")


#### Select focal variable
(focal = c( "disthus", "distvei", "skogkategori", "treartgruppe9", "kant", 
            "helling", "HOH", "tretetthet9", "beitetrykk9", "moose_density")[1])
## REMEMBER to load the road datasets and models after disthus (house model)!!!!!



#### Run function for the different models/ focals and SAVE them
# hurdle1_only = browsing pressure hurdle
# hurdle2_only = recruitment hurdle (indirect effect)
# total = total effect

resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
           which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=20, plotit=T, nb_btstrp=9)

resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2,
           which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=20, plotit=T, nb_btstrp=9)

resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
           which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=20, plotit=T, nb_btstrp=9)


## Load ROAD models and data ----


#### Browsing pressure
load("output/browsing_2021_02_02/browse_r_bin1.rda")
zeromod1 <- browse_r_bin1
summary(zeromod1)

load("output/browsing_2021_02_02/browse_r_beta2.rda")
percmod1 <- browse_r_beta2 
summary(percmod1)

load("processdata/browse_roads_2021_02_02.rda")
str(browse_roads)
browse_roads$beite_bin <- ifelse(browse_roads$beitetrykk9 == 0, 0, 1)
# Scale
browse_roads$tretetthet9 <- scale(browse_roads$tretetthet9)
browse_roads$helling <- scale(browse_roads$helling)
browse_roads$HOH <- scale(browse_roads$HOH)
# browse_roads$moose_density <- scale(browse_roads$moose_density)
str(browse_roads)

dat1 <- browse_roads
head(dat1)


#### Recruitment
load("output/recruitment_2021_02_02/rec_r_bin3.rda")
zeromod2 <- rec_r_bin3
summary(zeromod2)

load("output/recruitment_2021_02_02/rec_r_beta2_TMB.rda")
percmod2 <- rec_r_beta2_TMB
summary(percmod2)

load("processdata/recruits_roads_2021_02_03.rda")
str(recruits_roads)
recruits_roads$recruitment_bin <- ifelse(recruits_roads$recruitment == 0, 0, 1)
is.integer(recruits_roads$recruitment)
recruits_roads$recruitment <- as.integer(recruits_roads$recruitment)
is.integer(recruits_roads$recruitment)
# Scale
# recruits_roads$beitetrykk9 <- scale(recruits_roads$beitetrykk9)
recruits_roads$helling <- scale(recruits_roads$helling)
recruits_roads$HOH <- scale(recruits_roads$HOH)
# recruits_roads$moose_density <- scale(recruits_roads$moose_density)
recruits_roads$tretetthet9 <- scale(recruits_roads$tretetthet9) # this is not in the recruitment model, but is used when the total effect gets calculated

dat2 <- recruits_roads
head(dat2)






## Load and run function ----

source("function_resp_curve_08_12_2020_rev_BVM.R")


#### Select focal variable
(focal = c( "disthus", "distvei", "skogkategori", "treartgruppe9", "kant", 
            "helling", "HOH", "tretetthet9", "beitetrykk9", "moose_density")[1])
## REMEMBER to load the road datasets and models after disthus (house model)!!!!!



#### Run function for the different models/ focals and SAVE them
# hurdle1_only = browsing pressure hurdle
# hurdle2_only = recruitment hurdle (indirect effect)
# total = total effect

bp_disthus <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=20, plotit=F, nb_btstrp=9)

recruit_disthus <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2,
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=20, plotit=F, nb_btstrp=9)

total_disthus <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                                  which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=9)


### Browse
# attributes(browse_house$disthus_200) # $`scaled:center` 192.2636 $`scaled:scale` 27.71453
bp_disthus$var <- bp_disthus$var * 27.71453 + 192.2636

### Recruitment
# attributes(recruits_house$disthus_600) # $`scaled:center` 499.182 $`scaled:scale` 162.9665
recruit_disthus$var <- recruit_disthus$var * 162.9665 + 499.182

### Total
total_disthus$var <- total_disthus$var * 162.9665 + 499.182
head(total_disthus)

plot(point~var, data=total_disthus, type="l")




## Re-scale for plotting ----
# If you need values to re-scale something


#### House --

### Browse
# attributes(browse_house$disthus_200) # $`scaled:center` 192.2636 $`scaled:scale` 27.71453
bp_disthus$var <- bp_disthus$var * 27.71453 + 192.2636

### Recruitment
# attributes(recruits_house$disthus_600) # $`scaled:center` 499.182 $`scaled:scale` 162.9665
recruit_disthus$var <- recruit_disthus$var * 162.9665 + 499.182

### Total
total_disthus$var <- total_disthus$var * 162.9665 + 499.182


#### Roads --

### Browse
# attributes(browse_roads$helling) # $`scaled:center` 19.83571 $`scaled:scale` 20.63004
# attributes(browse_roads$HOH) # $`scaled:center` 378.861 $`scaled:scale` 243.1602
# attributes(browse_roads$moose_density) # $`scaled:center` 184.0303 $`scaled:scale` 196.1478
# attributes(browse_roads$tretetthet9) # $`scaled:center` 993.2908 $`scaled:scale` 1152.076
bp_helling$var <- bp_helling$var * 20.63004 + 19.83571
bp_HOH$var <- bp_HOH$var * 243.1602 + 378.861
# bp_moose_density$var <- bp_moose_density$var * 196.1478 + 184.0303
bp_tretett$var <- bp_tretett$var * 1152.076 + 993.2908

bp_distvei$var <- exp(bp_distvei$var)-1

### Recruitment
# attributes(recruits_roads$beitetrykk9) # $`scaled:center` 26.01752 $`scaled:scale` 30.81325
# attributes(recruits_roads$helling) # $`scaled:center` 19.10349 $`scaled:scale` 19.60225
# attributes(recruits_roads$HOH) # $`scaled:center` 365.9363 $`scaled:scale` 245.9447
# attributes(recruits_roads$moose_density) # $`scaled:center` 200.6176 $`scaled:scale` 203.1264
recruit_bp$var <- recruit_bp$var * 30.81325 + 26.01752
recruit_helling$var <- recruit_helling$var * 19.60225 + 19.10349
recruit_HOH$var <- recruit_HOH$var * 245.9447 + 365.9363
# recruit_moose_density$var <- recruit_moose_density$var * 203.1264 + 200.6176

recruit_distvei$var <- exp(recruit_distvei$var)-1

### Total 
# attributes(recruits_roads$tretetthet9) # $`scaled:center` 984.6252 $`scaled:scale` 1193.3
total_helling$var <- total_helling$var * 19.60225 + 19.10349
total_tretett$var <- total_tretett$var * 1193.3 + 984.6252
total_bp$var <- total_bp$var * 30.81325 + 26.01752

total_distvei$var <- exp(total_distvei$var)-1