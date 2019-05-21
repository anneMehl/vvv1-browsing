setwd("R:/Prosjekter/12649303_beitetrykk_og_forstyrrelser_sis_wp_3/Fra Anne")



# BP
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




# Trees
## houses
load(paste0(getwd(), "/prop_large_trees_data_models/house_data_models/browse9.5msc.rda"))
dat2 <- browse9.5msc

load(paste0(getwd(), "/prop_large_trees_data_models/house_data_models/mhusbin.rda"))
zeromod2 <- mhusbin

load(paste0(getwd(), "/prop_large_trees_data_models/house_data_models/m16beta.rda"))
percmod2 <- m16beta


## roads
load(paste0(getwd(), "/prop_large_trees_data_models/m16bin.rda"))
zeromod2 <- m16bin
# summary(zeromod2)

load(paste0(getwd(), "/prop_large_trees_data_models/m16beta.rda"))
percmod2 <- m16beta
# summary(percmod2)

load(paste0(getwd(), "/prop_large_trees_data_models/browse9.6sc.rda"))
# head(browse9.6sc)
dat2 <- browse9.6sc




source("function_resp_curve.R")

(focal = c("disthus", "distvei", "skogkategori", "treartgruppe", "kant", 
           "helling", "HOH", "tretetthet", "browspres_merged")[3])




h1_disthus <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
           which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)

h1_distvei <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)

h1_skogkat <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)

h1_treart <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)

h1_kant <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)

h1_hell <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)

h1_hoh <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)

h1_tretetthet <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[1], nvals=100, plotit=F, nb_btstrp=99)




h2_disthus <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)

h2_distvei <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)

h2_skogkat <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)

h2_treart <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                        which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)

h2_hell <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                      which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)

h2_hoh <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                     which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)

h2_browse <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                            which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)




tt_disthus <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_distvei <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_skogkat <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                         which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_treart <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                        which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_hell <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                      which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_hoh <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                     which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_browse <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                        which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_tretetthet <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                        which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=F, nb_btstrp=99)

tt_kant <- resp_curve(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, 
                        which_effect=c("hurdle1_only", "hurdle2_only", "total")[2], nvals=100, plotit=F, nb_btstrp=99)



  


## rescale/ transform
d$s.x * attr(d$s.x, 'scaled:scale') + attr(d$s.x, 'scaled:center')
# BP
h1_disthus$var <- h1_disthus$var * 27.71453 + 192.2636
h1_distvei$var <- exp(h1_distvei$var)-1
h1_hoh$var <- h1_hoh$var * 244.7748 + 361.8423


# Trees
h2_disthus$var <- h2_disthus$var * 88.5 + 359
h2_distvei$var <- exp(h2_distvei$var)-1
h2_hell$var <- h2_hell$var * 19.4 + 18.7
h2_hoh$var <- h2_hoh$var * 243 + 407

# Total
tt_distvei$var <- exp(tt_distvei$var)-1
tt_hell$var <- tt_hell$var * 19.4 + 18.7
tt_hoh$var <- tt_hoh$var * 243 + 407
tt_disthus$var <- tt_disthus$var * 88.5 + 359




library(ggplot2)

## BP hurdle
par(mfrow=c(3,3))
bp_disthus <- ggplot(data = h1_disthus, aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nDistance to house (m)") + 
  ylab("Browsing pressure (%)\n") 


bp_distvei <- ggplot(data = h1_distvei, aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nDistance to road (m)") +
  ylab("Browsing pressure (%)\n")+
  scale_x_continuous(limits = c(0, 500))


bp_skogkat <- ggplot(data = h1_skogkat,aes(var, point)) +
  geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nForest category") +
  ylab("Browsing pressure (%)\n") +
  scale_x_discrete(labels = c("Highproductive \nold forest", "Highproductive \nyoung forest",
                              "Lowproductive \nold forest", "Lowproductive \nyoung forest",
                              "Unproductive \nforest"))


bp_treart <- ggplot(data = h1_treart,aes(var, point)) +
  geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nTree species groups") +
  ylab("Browsing pressure (%)\n") +
  scale_x_discrete(labels = c("Pine", "Other deciduous \ntrees", "Rowan, aspen, \nwillow"))


bp_kant <- ggplot(data = h1_kant, aes(var, point)) +
  geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nEdge effect") +
  ylab("Browsing pressure (%)\n") +
  scale_x_discrete(labels = c("Edge <10m", "Edge >20m", "Edge 10-10m", "Edge information\nmissing"))


bp_hell <- ggplot(data = h1_hell,aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nSlope (%)") +
  ylab("Browsing pressure (%)\n")


bp_hoh <- ggplot(data = h1_hoh,aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nAltitude (m)") +
  ylab("Browsing pressure (%)\n")


bp_tretetthet <- ggplot(data = h1_tretetthet,aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nTree density") +
  ylab("Browsing pressure (%)\n")



## Trees hurdle
t_disthus <- ggplot(data = h2_disthus, aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nDistance to house (m)") + 
  ylab("Large trees (%)\n") 


t_distvei <- ggplot(data = h2_distvei, aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nDistance to road (m)") + 
  ylab("Large trees (%)\n")+
  scale_x_continuous(limits = c(0, 500))


t_skogkat <- ggplot(data = h2_skogkat,aes(var, point)) +
  geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nForest category") +
  ylab("Large trees (%)\n") +
  scale_x_discrete(labels = c("Highproductive \nold forest", "Highproductive \nyoung forest",
                              "Lowproductive \nold forest", "Lowproductive \nyoung forest",
                              "Unproductive \nforest"))


t_treart <- ggplot(data = h2_treart,aes(var, point)) +
  geom_pointrange(aes(ymin = CI025, ymax = CI975), colour = "#009292", size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nTree species groups") +
  ylab("Large trees (%)\n") +
  scale_x_discrete(labels = c("Pine", "Other deciduous \ntrees", "Rowan, aspen, \nwillow"))


t_hell <- ggplot(data = h2_hell,aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nSlope (%)") +
  ylab("Large trees (%)\n")


t_hoh <- ggplot(data = h2_hoh,aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nAltitude (m)") +
  ylab("Large trees (%)\n")


t_browse <- ggplot(data = h2_browse,aes(var, point)) +
  geom_line(colour = "#009292", linetype = 1, size = 1)+
  geom_ribbon(aes(ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nBrowsing pressure") +
  ylab("Large trees (%)\n")+
  scale_x_continuous(limits = c(0, 40))




## Direct (tree hurdle) and total effect on trees
td_disthus <- ggplot() + 
  geom_line(data = tt_disthus, aes(var, point, color = "total"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = tt_disthus, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  geom_line(data = h2_disthus, aes(var, point, color = "direct"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = h2_disthus, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  xlab("\nDistance to road (m)") + 
  ylab("Large trees (%)\n") +
  theme_light()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949"))


td_distvei <- ggplot() + 
  geom_line(data = tt_distvei, aes(var, point, color = "total"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = tt_distvei, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  geom_line(data = h2_distvei, aes(var, point, color = "direct"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = h2_distvei, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  xlab("\nDistance to road (m)") + 
  ylab("Large trees (%)\n") +
  theme_light()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949"))+
  scale_x_continuous(limits = c(0, 500))


td_skogkat <- ggplot() +
  geom_pointrange(data = tt_skogkat, aes(x = var, y = mean, ymin = CI025, ymax = CI975, colour = "total"), size = 1) +
  geom_pointrange(data = h2_skogkat, aes(x = var, y = mean, ymin = CI025, ymax = CI975, colour = "direct"), size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nForest category") + 
  ylab("Large trees (%)\n") +
  scale_x_discrete(labels = c("Highproductive \nold forest", "Highproductive \nyoung forest", "Lowproductive \nold forest", "Lowproductive \nyoung forest", "Unproductive \nforest"))+
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949"))+
  scale_y_continuous(limits = c(-0.2, 3))


td_treart <- ggplot() +
  geom_pointrange(data = tt_treart, aes(x = var, y = point, ymin = CI025, ymax = CI975, colour = "total"), size = 1) +
  geom_pointrange(data = h2_treart, aes(x = var, y = point, ymin = CI025, ymax = CI975, colour = "direct"), size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nTree species group") + 
  ylab("Large trees (%)\n") +
  scale_x_discrete(labels = c("Pine", "Other deciduous \ntrees", "Rowan, aspen, \nwillow")) +
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949"))


td_kant <- ggplot() +
  geom_pointrange(data = tt_kant, aes(x = var, y = point, ymin = CI025, ymax = CI975, colour = "total"), size = 1) +
  theme_light() +
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  xlab("\nEdge effect") +
  ylab("Large trees (%)\n") +
  scale_x_discrete(labels = c("Edge <10m", "Edge >20m", "Edge 10-10m", "Edge information\nmissing")) +
  scale_color_manual(name = "Effect on large \ntree %",
                     values = c("total" = "#009292", "direct" = "#004949"))


td_hell <- ggplot() + 
  geom_line(data = tt_hell, aes(var, point, color = "total"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = tt_hell, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  geom_line(data = h2_hell, aes(var, point, color = "direct"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = h2_hell, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  xlab("\nSlope (%)") + 
  ylab("Large trees (%)\n") +
  theme_light()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949")) 


td_hoh <- ggplot() + 
  geom_line(data = tt_hoh, aes(var, point, color = "total"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = tt_hoh, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  geom_line(data = h2_hoh, aes(var, point, color = "direct"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = h2_hoh, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  xlab("\nAltitude (%)") + 
  ylab("Large trees (%)\n") +
  theme_light()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949"))


td_tretetthet <- ggplot() + 
  geom_line(data = tt_tretetthet, aes(var, point, color = "total"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = tt_tretetthet, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  xlab("\nTree density (%)") + 
  ylab("Large trees (%)\n") +
  theme_light()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949"))


td_browse <- ggplot() + 
  geom_line(data = tt_browse, aes(var, point, color = "total"), linetype = 1, size = 1, show.legend = T) +
  geom_ribbon(data = tt_browse, aes(x = var, ymin = CI025, ymax = CI975), colour = "#004949", linetype = 2, alpha = 0.2) +
  xlab("\nBrowsing pressure (%)") + 
  ylab("Large trees (%)\n") +
  theme_light()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(family = "sans", size = 14, color = "grey30")) +
  scale_color_manual(name = "Effect on large \ntree %", 
                     values = c("total" = "#009292", "direct" = "#004949"))


library(gridExtra)
grid.arrange(h1_tretetthet, h1_disthus, h1_distvei, h1_hell, h1_hoh, h1_kant, h1_skogkat, 
             h1_treart,
             nrow = 3, ncol = 3)

grid.arrange(td_disthus, td_distvei, td_hell, td_kant, td_skogkat, td_treart, td_tretetthet,
             nrow = 3, ncol = 3)
