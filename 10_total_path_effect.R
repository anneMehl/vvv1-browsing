setwd("R:/Prosjekter/12649303_beitetrykk_og_forstyrrelser_sis_wp_3/Fra Anne")

#### BP
## houses
load(paste0(getwd(), "/browsing_pressure_data_models/house_data_models/m1kbin2.rda"))
zeromod1 <- m1kbin2

load(paste0(getwd(), "/browsing_pressure_data_models/house_data_models/m1ibeta2.rda"))
percmod1 <- m1ibeta2

load(paste0(getwd(), "/browsing_pressure_data_models/house_data_models/browse9.4_house.rda"))
dat1 <- browse9.4_house


## roads
load(paste0(getwd(), "/browsing_pressure_data_models/m4abin2.rda"))
zeromod1 <- m4abin2
# summary(zeromod1)

load(paste0(getwd(), "/browsing_pressure_data_models/m4abeta2.rda"))
percmod1 <- m4abeta2 
# summary(percmod1)

load(paste0(getwd(), "/browsing_pressure_data_models/browse9.4.rda"))
# head(browse9.4)
dat1 <- browse9.4




#### Trees
## houses
load(paste0(getwd(), "/prop_large_trees_data_models/house_data_models/browse9.5msc.rda"))
dat2 <- browse9.5msc

load(paste0(getwd(), "/prop_large_trees_data_models/house_data_models/mhusabin.rda"))
zeromod2 <- mhusabin

load(paste0(getwd(), "/prop_large_trees_data_models/house_data_models/m16abeta.rda"))
percmod2 <- m16abeta


## roads
load(paste0(getwd(), "/prop_large_trees_data_models/m16cbin.rda"))
zeromod2 <- m16cbin
# summary(zeromod2)

load(paste0(getwd(), "/prop_large_trees_data_models/m16abeta.rda"))
percmod2 <- m16abeta
# summary(percmod2)

load(paste0(getwd(), "/prop_large_trees_data_models/browse9.6sc.rda"))
# head(browse9.6sc)
dat2 <- browse9.6sc



source("function_resp_curve.R")


(focal = c("distvei", "skogkategori", "treartgruppe", "kant", 
           "helling", "HOH", "tretetthet", "browspres_merged", "disthus")[9])
(focal = c("distvei", "skogkategori", "treartgruppe", "kant", 
           "helling", "HOH", "tretetthet", "browspres_merged", "disthus")[9])
par(mfrow=c(2,2))
resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
           which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=T, nb_btstrp=99)
resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
           which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=T, nb_btstrp=99)
resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
           which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=T, nb_btstrp=99)
#resp_curveV1("helling", isfactor=F, dat=dat1, treedat=dat2, zeromod=zeromod1, percmod=percmod1, 
# treemod=zeromod1, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)


#for debugging only
#nvals=10
#nb_btstrp=4
  



h1_disthus$var <- h1_disthus$var * 27.71453 + 192.2636
h1_distvei$var <- exp(h1_distvei$var)-1
h1_hoh$var <- h1_hoh$var * 244.7748 + 361.8423


h2_distvei$var <- exp(h2_distvei$var)-1



library(ggplot2)

ggplot(data = h1_disthus, aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nDistance to house (m)") + 
  ylab("Browsing pressure (%)\n") 


ggplot(data = h1_distvei, aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nDistance to road (m)") +
  ylab("Browsing pressure (%)\n")
# 
# 
# ggplot(data = h1_skogkat,aes(var, point)) +
#   geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
#   theme_light() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
#   xlab("\nForest category") + 
#   ylab("Browsing pressure (%)\n") +
#   scale_x_discrete(labels = c("Highproductive \nold forest", "Highproductive \nyoung forest", 
#                               "Lowproductive \nold forest", "Lowproductive \nyoung forest", 
#                               "Unproductive \nforest"))
# 
# 
# ggplot(data = h1_treart,aes(var, point)) +
#   geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
#   theme_light() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
#   xlab("\nTree species groups") + 
#   ylab("Browsing pressure (%)\n") +
#   scale_x_discrete(labels = c("Pine", "Other deciduous \ntrees", "Rowan, aspen, \nwillow"))
# 
# 
# ggplot(data = h1_kant, aes(var, point)) +
#   geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
#   theme_light() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
#   xlab("\nEdge effect") + 
#   ylab("Browsing pressure (%)\n") +
#   scale_x_discrete(labels = c("Edge <10m", "Edge >20m", "Edge 10-10m", "Edge information\nmissing"))
# 
# 
ggplot(data = h1_hell,aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nSlope (%)") +
  ylab("Browsing pressure (%)\n")
# 
# 
# ggplot(data = h1_hoh,aes(var, point)) +
#   geom_line(colour = "#009292", linetype = 1, size = 1)+
#   geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
#   theme_light() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
#   xlab("\nAltitude (m)") + 
#   ylab("Browsing pressure (%)\n")
# 
# 
# ggplot(data = h1_tretetthet,aes(var, point)) +
#   geom_line(colour = "#009292", linetype = 1, size = 1)+
#   geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
#   theme_light() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
#   xlab("\nTree density") + 
#   ylab("Browsing pressure (%)\n")




ggplot(data = h2_distvei, aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nDistance to road (m)") + 
  ylab("Proportion large trees\n")


# ggplot(data = h2_skogkat,aes(var, point)) +
#   geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
#   theme_light() +
#   theme(axis.text = element_text(size = 10),
#         axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
#   xlab("\nForest category") + 
#   ylab("Proportion large trees\n") +
#   scale_x_discrete(labels = c("Highproductive \nold forest", "Highproductive \nyoung forest", 
#                               "Lowproductive \nold forest", "Lowproductive \nyoung forest", 
#                               "Unproductive \nforest"))
