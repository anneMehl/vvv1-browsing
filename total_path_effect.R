setwd("R:/Prosjekter/12649303_beitetrykk_og_forstyrrelser_sis_wp_3/Fra Anne")
load("browse9.2_dist.rda")
head(browse9.2_dist)

load("m4bin.rda")
zeromod <- m4abin

load("m4beta.rda")
percmod <- m4abeta

load("m13.rda")
treemod <- m13

dat <- browse9.2_dist
focal <- "HOH_scale"
isfactor=F

load("browse9.3_dist.rda")
treedat <- browse9.3_dist

#focal: a numeric variable or a factor 
resp_curve <- function(focal, isfactor=FALSE, dat, treedat, zeromod, percmod, treemod, which_effect=c("hurdle_only", "total", "direct")[2], nvals=100, plot=T, nb_btstrp=999){
  treefocal <- focal
  if (focal=="distvei2_trunc") treefocal <- "log_distvei2_trunc"
  if (focal=="helling_scale") treefocal <- "helling"
  
  if (!isfactor) {
    rng2 <- rng <- quantile(dat[,focal], probs=c(0.005, 0.995))
    dat[c(1:nvals),focal] <- seq(rng[1], rng[2], length.out=nvals)
    if (focal!=treefocal) rng2 <- quantile(treedat[,treefocal], probs=c(0.005, 0.995))
    treedat[c(1:nvals),treefocal] <- seq(rng2[1], rng2[2], length.out=nvals)
  }

## first, hurdle model
  #model matrix for zeromod
  mm1 <- model.matrix(terms(zeromod), data=dat)
  #set to mean value, or set factors to intercept
  colnms <- colnames(mm1)
  modframe <- zeromod@frame
  modframe$"factor(beitetrykkr > 0)" <- NULL
  fcts <- names(modframe)[sapply(modframe, class)=="factor"]
  nofcts <- names(modframe)[sapply(modframe, class)!="factor"]
  if (!isfactor) nofcts <- nofcts[!grepl(focal, nofcts)]
  for (i in nofcts){
    if (!is.na(match(i, colnames(mm1)))){
      mm1[c(1:nvals),i] <- rep(median(mm1[,i]), each=nvals)
    }
  }
  for (i in fcts){
      mm1[c(1:nvals),grep(i, colnms)] <- 0
  }
  #set focal variable
  if (!isfactor) mm1 <- mm1[c(1:nvals),] 
  if (isfactor) {
    mm1 <- mm1[c(1:(1+length(grep(focal, colnms)))),] 
    j=1
    for (i in grep(focal, colnms)){
      j=j+1
      mm1[j,i] <- 1
    }
  }

  #model matrix for percmod
  mm2 <- model.matrix(terms(percmod), data=dat)
  #set to mean value, or set factors to intercept
  colnms <- colnames(mm2)
  modframe <- percmod$frame
  modframe$bt100 <- NULL
  fcts <- names(modframe)[sapply(modframe, class)=="factor"]
  nofcts <- names(modframe)[sapply(modframe, class)!="factor"]
  if (!isfactor) nofcts <- nofcts[!grepl(focal, nofcts)]
  for (i in nofcts){
    if (!is.na(match(i, colnames(mm2)))){
      mm2[c(1:nvals),i] <- rep(median(mm2[,i]), each=nvals)
    }
  }
  for (i in fcts){
    mm2[c(1:nvals),grep(i, colnms)] <- 0
  }
  #set focal variable
  if (!isfactor) mm2 <- mm2[c(1:nvals),] 
  if (isfactor) {
    mm2 <- mm2[c(1:(1+length(grep(focal, colnms)))),] 
    j=1
    for (i in grep(focal, colnms)){
      j=j+1
      mm2[j,i] <- 1
    }
  }
  

  #bootstrap focal coefficent
  require(nlme)
  zerocoef <- fixef(zeromod)
  zeromat <- matrix(zerocoef, nrow=length(zerocoef), ncol=nb_btstrp+1, byrow=F)
  ses <- sqrt(diag(as.matrix(vcov(zeromod))))
  if (!isfactor){
    ses <- ses[grep(focal, names(ses))]
    smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=ses))
    zeromat[1,] <- zeromat[1,]+smpl
  } 
  if (isfactor){
    sess <- c(ses[1], ses[grep(focal, names(ses))])
    for (i in c(1:j)){
      if (i==1) smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=sess[i]))
      if (i>1) smpl <- rbind(smpl, c(0, rnorm(nb_btstrp, mean=0, sd=sess[i])))
    }
    zeromat[c(1, grep(focal, names(ses))),] <- zeromat[c(1, grep(focal, names(ses))),]+smpl
  } 
  
  require(boot)
  predzero_smpl <- inv.logit(mm1 %*% zeromat) 
  
  perccoef <- fixef(percmod)[[1]]
  percmat <- matrix(perccoef, nrow=length(perccoef), ncol=nb_btstrp+1, byrow=F)
  ses <- sqrt(diag(as.matrix(vcov(percmod))[[1]]))
  if (!isfactor){
    ses <- ses[grep(focal, names(ses))]
    smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=ses))
    percmat[1,] <- percmat[1,]+smpl
  }
  if (isfactor){
    sess <- c(ses[1], ses[grep(focal, names(ses))])
    for (i in c(1:j)){
      if (i==1) smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=sess[i]))
      if (i>1) smpl <- rbind(smpl, c(0, rnorm(nb_btstrp, mean=0, sd=sess[i])))
    }
    percmat[c(1, grep(focal, names(ses))),] <- percmat[c(1, grep(focal, names(ses))),]+smpl
  } 
  
  predperc_smpl <- inv.logit(mm2 %*% percmat)*100 
  
  pred_smpl <- predzero_smpl*predperc_smpl
  
  if (which_effect=="hurdle_only"){
    if (!isfactor){
      res <- data.frame(var=seq(rng[1], rng[2], length.out=nvals), point=pred_smpl[,1], mean=apply(pred_smpl, 1, mean), sd=apply(pred_smpl, 1, sd), 
                      CI025=apply(pred_smpl, 1, quantile, probs=0.025), CI975=apply(pred_smpl, 1, quantile, probs=0.975))
      if (plot) {
        plot(res[,"var"], res$point, type="l", lwd=2, xlab=focal, ylab="browsing pressure")
        lines(res[,"var"], res$CI975, col="dark grey")
        lines(res[,"var"], res$CI025, col="dark grey")
        lines(res[,"var"], res$point, col="black")
      }
    }
    
    if (isfactor){
      res <- data.frame(var=levels(dat[, grep(focal, names(dat))]), point=pred_smpl[,1], mean=apply(pred_smpl, 1, mean), sd=apply(pred_smpl, 1, sd), 
                        CI025=apply(pred_smpl, 1, quantile, probs=0.025), CI975=apply(pred_smpl, 1, quantile, probs=0.975))
      if (plot) {
        plot(c(1:nrow(res)), res$point, type="p", ylim=c(min(res$CI025), max(res$CI975)), xlab=focal, ylab="browsing pressure", xaxt = 'n')
        axis(1, at=c(1:nrow(res)), labels=res$var)
        segments(c(1:nrow(res)), res$CI975, c(1:nrow(res)), res$CI025)
      }
    }
  }
  
  if (which_effect!="hurdle_only"){
    #model matrix for treemod
    mm3 <- model.matrix(terms(treemod), data=treedat)
    #set to mean value, or set factors to intercept
    colnms <- colnames(mm3)
    modframe <- treemod@frame
    modframe$"log(tretetthet9_trunc + 1)" <- NULL
    fcts <- names(modframe)[sapply(modframe, class)=="factor"]
    nofcts <- names(modframe)[sapply(modframe, class)!="factor"]
    if (!isfactor) nofcts <- nofcts[!grepl(treefocal, nofcts)]
    for (i in nofcts){
      if (!is.na(match(i, colnames(mm3)))){
        mm3[c(1:nvals),i] <- rep(median(mm3[,i]), each=nvals)
      }
    }
    for (i in fcts){
      mm3[c(1:nvals),grep(i, colnms)] <- 0
    }
    #set focal variable
    if (!isfactor) mm3 <- mm3[c(1:nvals),] 
    if (isfactor) {
      mm3 <- mm3[c(1:(1+length(grep(treefocal, colnms)))),] 
      j=1
      for (i in grep(treefocal, colnms)){
        j=j+1
        mm3[j,i] <- 1
      }
    }
    
    toto <- function(x, mm3, pred_smpl, which_effect=c("hurdle_only", "total", "direct")){
      tmp <- mm3
      if (which_effect=="total") tmp[,which(colnames(tmp)=="beitetrykkr")] <- pred_smpl[,x]
      if (which_effect=="direct") tmp[,which(colnames(tmp)=="beitetrykkr")] <- pred_smpl[1,1]
      return(tmp)
    }
    lmm3 <- lapply(c(1:ncol(pred_smpl)), toto, mm3=mm3, pred_smpl=pred_smpl, which_effect=which_effect)
    
    #bootstrap focal coefficent
    treecoef <- fixef(treemod)
    if (focal!="HOH_scale"){
      treemat <- matrix(treecoef, nrow=length(treecoef), ncol=nb_btstrp+1, byrow=F)
      ses <- sqrt(diag(as.matrix(vcov(treemod))))
      if (!isfactor){
        ses <- ses[grep(treefocal, names(ses))]
        smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=ses))
        treemat[1,] <- treemat[1,]+smpl
      }
      if (isfactor){
        sess <- c(ses[1], ses[grep(treefocal, names(ses))])
        for (i in c(1:j)){
          if (i==1) smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=sess[i]))
          if (i>1) smpl <- rbind(smpl, c(0, rnorm(nb_btstrp, mean=0, sd=sess[i])))
        }
        treemat[c(1, grep(treefocal, names(ses))),] <- treemat[c(1, grep(treefocal, names(ses))),]+smpl
      } 
    }
    if (focal=="HOH_scale") treemat <- matrix(treecoef, nrow=length(treecoef), ncol=1, byrow=F)
    
    lpred_smpl <- lapply(lmm3, function(x, treemat){x %*% treemat}, treemat=treemat)
    pred_smpl <- do.call("cbind", lpred_smpl)
    
    if (!isfactor){
      res <- data.frame(var=seq(rng[1], rng[2], length.out=nvals), point=pred_smpl[,1], mean=apply(pred_smpl, 1, mean), sd=apply(pred_smpl, 1, sd), 
                        CI025=apply(pred_smpl, 1, quantile, probs=0.025), CI975=apply(pred_smpl, 1, quantile, probs=0.975))
      if (plot) {
        plot(res[,"var"], res$point, type="l", lwd=2, xlab=focal, ylab="log(treedensity)")
        lines(res[,"var"], res$CI975, col="dark grey")
        lines(res[,"var"], res$CI025, col="dark grey")
        lines(res[,"var"], res$point, col="black")
      }
    }
    
    if (isfactor){
      res <- data.frame(var=levels(dat[, grep(focal, names(dat))]), point=pred_smpl[,1], mean=apply(pred_smpl, 1, mean), sd=apply(pred_smpl, 1, sd), 
                        CI025=apply(pred_smpl, 1, quantile, probs=0.025), CI975=apply(pred_smpl, 1, quantile, probs=0.975))
      if (plot) {
        plot(c(1:nrow(res)), res$point, type="p", ylim=c(min(res$CI025), max(res$CI975)), xlab=focal, ylab="log(treedensity)", xaxt = 'n')
        axis(1, at=c(1:nrow(res)), labels=res$var)
        segments(c(1:nrow(res)), res$CI975, c(1:nrow(res)), res$CI025)
      }
    }
  }
  
  if (!plot) return(res)
}


setwd("R:/Prosjekter/12649303_beitetrykk_og_forstyrrelser_sis_wp_3/Fra Anne")
load("browse9.2_dist.rda")
browse9.2_dist[,"treartgruppe9"] <- NULL

load("browse9.3_dist.rda")

load("m4bin.rda")

load("m4beta.rda")

load("m13.rda")

par(mfrow=c(2,3))
resp_curve("distvei2_trunc", isfactor=F, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="total", nvals=100, plot=T, nb_btstrp=999)
resp_curve("skogkategori", isfactor=T, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="total", nvals=100, plot=T, nb_btstrp=999)
resp_curve("treartgruppe", isfactor=T, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="total", nvals=100, plot=T, nb_btstrp=999)
resp_curve("kant", isfactor=T, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="total", nvals=100, plot=T, nb_btstrp=999)
resp_curve("helling_scale", dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="total", nvals=100, plot=T, nb_btstrp=999)
resp_curve("HOH_scale", dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="total", nvals=100, plot=T, nb_btstrp=999)

par(mfrow=c(3,3))
resp_curve("distvei2_trunc", isfactor=F, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)
resp_curve("skogkategori", isfactor=T, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)
resp_curve("treartgruppe", isfactor=T, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)
resp_curve("kant", isfactor=T, dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)
resp_curve("helling_scale", dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)
resp_curve("HOH_scale", dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)
resp_curve("tretetthet9_trunc_scale", dat=browse9.2_dist, treedat=browse9.3_dist, zeromod=m4abin, percmod=m4abeta, treemod=m13, which_effect="hurdle_only", nvals=100, plot=T, nb_btstrp=999)



resp_curve <- function(focal, isfactor=FALSE, dat, treedat, zeromod, percmod, treemod, which_effect=c("hurdle_only", "total")[2], nvals=100, plot=T, nb_btstrp=999){
  

