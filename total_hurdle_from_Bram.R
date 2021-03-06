setwd("R:/Prosjekter/12649303_beitetrykk_og_forstyrrelser_sis_wp_3/Fra Anne")
load("browse9.2_dist.rda")
head(browse9.2_dist)

load("m4bin.rda")
zeromod <- m4abin

load("m4beta.rda")
percmod <- m4abeta

dat <- browse9.2_dist
focal <- "distvei2"

#focal: a numeric variable, doesn't work for factors, yet 
resp_curve <- function(focal, isfactor=FALSE, dat, zeromod, percmod, nvals=100, plot=T, nb_btstrp=999){
  if (!isfactor) rng <- quantile(dat[,focal], probs=c(0.01, 0.99))
  if (!isfactor) dat[c(1:nvals),focal] <- seq(rng[1], rng[2], length.out=nvals)
  #model matrix for zeromod
  mm1 <- model.matrix(terms(zeromod), data=dat)

  #set to mean value, or set factors to intercept
  colnms <- colnames(mm1)
  fcts <- names(zeromod@frame)[sapply(zeromod@frame, class)=="factor"]
  if (!isfactor) mm1[c(1:nvals),-grep(focal, colnms)] <- rep(apply(mm1[,-grep(focal, colnms)], 2, median), each=nvals)
  if (!isfactor) mm1 <- mm1[c(1:nvals),] 
  if (isfactor) mm1 <- mm1[c(1:(1+length(grep(focal, colnms)))),] 
  for (i in c(fcts)){
    if (length(grep(i, colnms))>0) mm1[,grep(i, colnms)] <- 0
  }
  if (isfactor) {
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
  fcts <- names(percmod$frame)[sapply(percmod$frame, class)=="factor"]
  if (!isfactor) mm2[c(1:nvals),-grep(focal, colnms)] <- rep(apply(mm2[,-grep(focal, colnms)], 2, median), each=nvals)
  if (!isfactor) mm2 <- mm2[c(1:nvals),] 
  if (isfactor) mm2 <- mm2[c(1:(1+length(grep(focal, colnms)))),] 
  for (i in c(fcts)){
    if (length(grep(i, colnms))>0) mm2[,grep(i, colnms)] <- 0
  }
  if (isfactor) {
    j=1
    for (i in grep(focal, colnms)){
      j=j+1
      mm2[j,i] <- 1
    }
  }
  
  #bootstrap focal coefficent
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
  
  if (!plot) return(res)
}

load("browse9.2_dist.rda")
load("m4bin.rda")
load("m4beta.rda")

browse9.2_dist[,"treartgruppe9"] <- NULL

par(mfrow=c(3,3))
resp_curve("distvei2_trunc", dat=browse9.2_dist, zeromod=m4abin, percmod=m4abeta, nvals=100, plot=T, nb_btstrp=999)
resp_curve("skogkategori", isfactor=T, dat=browse9.2_dist, zeromod=m4abin, percmod=m4abeta, nvals=100, plot=T, nb_btstrp=999)
resp_curve("treartgruppe", isfactor=T, dat=browse9.2_dist, zeromod=m4abin, percmod=m4abeta, nvals=100, plot=T, nb_btstrp=999)
resp_curve("kant", isfactor=T, dat=browse9.2_dist, zeromod=m4abin, percmod=m4abeta, nvals=100, plot=T, nb_btstrp=999)
resp_curve("helling_scale", dat=browse9.2_dist, zeromod=m4abin, percmod=m4abeta, nvals=100, plot=T, nb_btstrp=999)
resp_curve("HOH_scale", dat=browse9.2_dist, zeromod=m4abin, percmod=m4abeta, nvals=100, plot=T, nb_btstrp=999)
resp_curve("tretetthet9_trunc_scale", dat=browse9.2_dist, zeromod=m4abin, percmod=m4abeta, nvals=100, plot=T, nb_btstrp=999)


