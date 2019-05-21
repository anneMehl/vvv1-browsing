make_modelmatrix <- function(mod, dat, focal, isfactor, nvals, whurdle=c("BP", "trees")[1], type=c("zero", "perc")[1]){
  #model matrix
  mm <- model.matrix(terms(mod), data=dat)
  #set to mean value, or set factors to intercept
  colnms <- colnames(mm)
  
  if(type=="perc"){
    modframe <- mod$frame
    }else{modframe <- mod@frame}

  modframe <- modframe[,-1]
  fcts <- names(modframe)[sapply(modframe, class)=="factor"]
  nofcts <- names(modframe)[sapply(modframe, class)!="factor"]
  focal <- ifelse((focal=="helling" & whurdle=="trees" & type=="perc"), NA, focal)
  focal <- ifelse((focal=="disthus_400" & whurdle=="trees" & type=="perc"), NA, focal)
  isfactor <- ifelse(is.na(focal), F, isfactor)
  if (!isfactor & !is.na(focal)) nofcts <- nofcts[!grepl(focal, nofcts)]
  for (i in nofcts){
    if (!is.na(match(i, colnames(mm)))){
      mm[c(1:nvals),i] <- rep(median(mm[,i]), each=nvals)
    }
  }
  for (i in fcts){
    mm[c(1:nvals),grep(i, colnms)] <- 0
  }
  #set focal variable
  if (!isfactor) mm <- mm[c(1:nvals),] 
  if (isfactor) {
    mm <- mm[c(1:(1+length(grep(focal, colnms)))),] 
    j=1
    for (i in grep(focal, colnms)){
      j=j+1
      mm[j,i] <- 1
    }
  }
  return(mm)
}  

bootstrap_focal <- function(mod, focal, isfactor, nb_btstrp, whurdle=c("BP", "trees")[1], type=c("zero", "perc")[1]){
  if (grepl("distvei", focal)) focal <- paste0("log(", focal, " + 1)")
  coef <- fixef(mod)
  if (type=="perc") coef <- coef[[1]] 
  mat <- matrix(coef, nrow=length(coef), ncol=nb_btstrp+1, byrow=F)
  focal <- ifelse((focal=="helling" & whurdle=="trees" & type=="perc"), NA, focal)
  focal <- ifelse((focal=="disthus_400" & whurdle=="trees" & type=="perc"), NA, focal)
  if (!is.na(focal)){
    if (type=="perc"){ 
      ses <- sqrt(diag(as.matrix(vcov(mod))[[1]]))
    }else{ses <- sqrt(diag(as.matrix(vcov(mod))))}
    
    if (!isfactor){
      wvar <- which(focal==names(ses))
      sess <- ses[wvar]
      smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=sess))
      mat[wvar,] <- mat[wvar,]+smpl
      if (focal %in% c("HOH_scale", "HOH")){
        sess <- ses[wvar+1]
        smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=sess))
        mat[(wvar+1),] <- mat[(wvar+1),]+smpl  
      }
    } 
    if (isfactor){
      sess <- c(ses[1], ses[grepl(focal, names(ses))])
      for (i in c(1:length(sess))){
        if (i==1) smpl <- c(0, rnorm(nb_btstrp, mean=0, sd=sess[i]))
        if (i>1) smpl <- rbind(smpl, c(0, rnorm(nb_btstrp, mean=0, sd=sess[i])))
      }
      mat[c(1, grep(focal, names(ses))),] <- mat[c(1, grep(focal, names(ses))),]+smpl
    }
  } 
  return(mat)
}

output_function <- function(pred_smpl, isfactor, xvals, plotit, ylab){
  if (!isfactor){
    res <- data.frame(var=xvals, point=pred_smpl[,1], mean=apply(pred_smpl, 1, mean), sd=apply(pred_smpl, 1, sd), 
                      CI025=apply(pred_smpl, 1, quantile, probs=0.025), CI975=apply(pred_smpl, 1, quantile, probs=0.975))
    if (plotit) {
      #res[,"var"] <- exp(res[,"var"])-1
      plot(res[,"var"], res$point, type="l", lwd=2, xlab=focal, ylab=ylab)
      lines(res[,"var"], res$CI975, col="dark grey")
      lines(res[,"var"], res$CI025, col="dark grey")
      lines(res[,"var"], res$point, col="black")
    }
  }
  
  if (isfactor){
    res <- data.frame(var=xvals, point=pred_smpl[,1], mean=apply(pred_smpl, 1, mean), sd=apply(pred_smpl, 1, sd), 
                      CI025=apply(pred_smpl, 1, quantile, probs=0.025), CI975=apply(pred_smpl, 1, quantile, probs=0.975))
    if (plotit) {
      plot(c(1:nrow(res)), res$point, type="p", ylim=c(min(res$CI025), max(res$CI975)), xlab=focal, ylab=ylab, xaxt = 'n')
      axis(1, at=c(1:nrow(res)), labels=res$var)
      segments(c(1:nrow(res)), res$CI975, c(1:nrow(res)), res$CI025)
    }
  }
  return(res)
}

#focal: a numeric variable or a factor 
resp_curve <- function(focal, dat1, dat2, zeromod1, percmod1, zeromod2, percmod2, which_effect=c("hurdle1_only", "hurdle2_only", "total")[3], nvals=100, plotit=T, nb_btstrp=999){
  road_upper <- 500 #meter
  require(boot)
  require(nlme)
  
  focal1 <- c("distvei2_trunc",   "skogkategori", "treartgruppe", "kant", 
              "helling", "HOH_scale", "tretetthet9_trunc", "disthus_trunc_cut200_scale")
  focal1 <- focal1[grepl(focal, focal1)]
  focal1 <- ifelse(length(focal1)==0, NA, focal1)
  focal2 <- c("distvei2",   "skogkategori", "treartgruppe", "helling", "HOH", 
              "browspres_merged", "disthus_400")
  focal2 <- focal2[grepl(focal, focal2)]
  focal2 <- ifelse(length(focal2)==0, NA, focal2)
    
  isfactor <- ifelse(is.na(focal1), focal2, focal1) %in% c("skogkategori", "treartgruppe", "kant")

  if ((!isfactor) | focal=="disthus") {
    if (!is.na(focal1)){
      rng2 <- rng1 <- c(min(dat1[,focal1], na.rm=T), max(dat1[,focal1], na.rm=T))
      dat1[c(1:nvals),focal1] <- seq(rng1[1], rng1[2], length.out=nvals)
      if (!is.na(focal2)){
        if (focal1!=focal2) rng2 <- c(min(dat2[,focal2], na.rm=T), max(dat2[,focal2], na.rm=T))
        dat2[c(1:nvals),focal2] <- seq(rng2[1], rng2[2], length.out=nvals)
      }
    }else {
      rng2 <- rng1 <- c(min(dat2[,focal2], na.rm=T), max(dat2[,focal2], na.rm=T))
      dat2[c(1:nvals),focal2] <- seq(rng2[1], rng2[2], length.out=nvals)
    }
  }

  if (focal=="disthus") {
    xvals1 <- seq(-6.714585, 11.10379, length.out=nvals)
    dat1[c(1:nvals),focal1] <- ifelse(xvals1>0.2791458, 0.2791458, xvals1)
    xvals2 <- seq(-3.980824, 1.59322, length.out=nvals)
    dat2[c(1:nvals),focal2] <- ifelse(xvals2>0.2791458, 0.2791458, xvals2)
  }

  if (focal=="distvei") {
    rng2 <- rng1 <- c(0, road_upper)
    dat1[c(1:nvals),focal1] <- seq(rng1[1], rng1[2], length.out=nvals)
    dat2[c(1:nvals),focal2] <- seq(rng2[1], rng2[2], length.out=nvals)
  }
  
  if (focal=="browspres_merged"){ #we get this over with first...
    mm3 <- make_modelmatrix(mod=zeromod2, dat=dat2, focal=focal2, isfactor=isfactor, nvals=nvals, whurdle="trees", type="zero")
    mm4 <- make_modelmatrix(mod=percmod2, dat=dat2, focal=focal2, isfactor=isfactor, nvals=nvals, whurdle="trees", type="perc")
    
    #bootstrap focal coefficent
    zeromat <- bootstrap_focal(mod=zeromod2, focal=focal2, isfactor=isfactor, nb_btstrp=nb_btstrp, whurdle="trees", type="zero")
    predzero_smpl <- inv.logit(mm3 %*% zeromat) 
    
    percmat <- bootstrap_focal(mod=percmod2, focal=focal2, isfactor=isfactor, nb_btstrp=nb_btstrp, whurdle="trees", type="perc")
    predperc_smpl <- inv.logit(mm4 %*% percmat)*100 
    
    pred_smpl <- predzero_smpl*predperc_smpl
    
    xvals <- mm3[, grep(focal2, colnames(mm3))]
    res <- output_function(pred_smpl, isfactor, xvals, plotit, ylab="perc. large trees")
  } #done with hurlde2 only
  
  
  if (focal!="browspres_merged"){
    mm1 <- make_modelmatrix(mod=zeromod1, dat=dat1, focal=focal1, isfactor=isfactor, nvals=nvals, whurdle="BP", type="zero")
    mm2 <- make_modelmatrix(mod=percmod1, dat=dat1, focal=focal1, isfactor=isfactor, nvals=nvals, whurdle="BP", type="perc")
    
    #bootstrap focal coefficent
    zeromat <- bootstrap_focal(mod=zeromod1, focal=focal1, isfactor=isfactor, nb_btstrp=nb_btstrp, whurdle="BP", type="zero")
    predzero_smpl <- inv.logit(mm1 %*% zeromat) 

    percmat <- bootstrap_focal(mod=percmod1, focal=focal1, isfactor=isfactor, nb_btstrp=nb_btstrp, whurdle="BP", type="perc")
    predperc_smpl <- inv.logit(mm2 %*% percmat)*100 
  
    pred_smpl <- predzero_smpl*predperc_smpl
  
    if (which_effect=="hurdle1_only"){
      if (!isfactor){
        xvals <- mm2[, grep(focal1, colnames(mm2))]
        if (!class(xvals)=="numeric") xvals <- xvals[,1]
      }
      if (isfactor) xvals <- levels(dat1[, grep(focal1, names(dat1))])
      if (focal=="disthus") xvals <- xvals1
      res <- output_function(pred_smpl, isfactor, xvals, plotit, ylab="browsing pressure")
    }
    #end of the first hurdle
  
    if (which_effect!="hurdle1_only"){
      #merge ROS and LAUV
      if (!is.na(focal2)) {
        if (focal2=="treartgruppe") {
        pred_smpl[1,] <- pred_smpl[1,] + pred_smpl[3,]
        pred_smpl <- pred_smpl[c(1:2),]
      }}

      mm3 <- make_modelmatrix(mod=zeromod2, dat=dat2, focal=focal2, isfactor=isfactor, nvals=nvals, whurdle="trees", type="zero")
      mm4 <- make_modelmatrix(mod=percmod2, dat=dat2, focal=focal2, isfactor=isfactor, nvals=nvals, whurdle="trees", type="perc")
  
      #include first hurdle results...
      toto <- function(x, mm, pred_smpl){
        mm[,which(colnames(mm)=="browspres_merged")] <- pred_smpl[,x]
        if (which_effect=="hurdle2_only"){
          mm[,which(colnames(mm)=="browspres_merged")] <- mean(pred_smpl[,1])
        }
        return(mm)
      }
      if (focal1=="kant"){
        mm3 <- mm3[c(1:4),]
        mm4 <- mm4[c(1:4),]
      }
      lmm3 <- lapply(c(1:ncol(pred_smpl)), toto, mm=mm3, pred_smpl=pred_smpl)
      lmm4 <- lapply(c(1:ncol(pred_smpl)), toto, mm=mm4, pred_smpl=pred_smpl)
      
      #bootstrap focal coefficent
      zeromat2 <- bootstrap_focal(mod=zeromod2, focal=focal2, isfactor=isfactor, nb_btstrp=nb_btstrp, whurdle="trees", type="zero")
  
      lpredzero_smpl <- lapply(lmm3, function(x, mat){inv.logit(x %*% mat)}, mat=zeromat2)
      predzero_smpl <- do.call("cbind", lpredzero_smpl)
      
      percmat2 <- bootstrap_focal(mod=percmod2, focal=focal2, isfactor=isfactor, nb_btstrp=nb_btstrp, whurdle="trees", type="perc")
      lpredperc_smpl <- lapply(lmm4, function(x, mat){inv.logit(x %*% mat)*100}, mat=percmat2)
      predperc_smpl <- do.call("cbind", lpredperc_smpl)
      
      pred_smpl <- predzero_smpl*predperc_smpl
      
      if (!isfactor){
        xvals <- mm1[, grep(focal1, colnames(mm1))]
        if (!class(xvals)=="numeric") xvals <- xvals[,1]
      }
      if (isfactor) xvals <- levels(dat1[, grep(focal1, names(dat1))])
      if (focal1=="treartgruppe") xvals <- levels(dat2[, grep(focal2, names(dat2))])
      if (focal=="disthus") xvals <- xvals2
      res <- output_function(pred_smpl, isfactor, xvals, plotit, ylab="perc. large trees")
    }
  }
  if (!plotit) return(res)
}

