####################
## Auxiliary functions
####################
require (Hmisc)
# Example:
#  paneldata.lags(X, "person", "year", c("v1","v2"), lags=1:4)
#  X is the data.frame
paneldata.lags <- function(X, unitvar, timevar, lagvars, lags=1) {
  stopifnot(length(lagvars)>=1)
  X <- X[order(X[,timevar]),]           # just in case it's not sorted.
  
  innertask <- function(Y, lagvars, lags) {
    E <- labels <- NULL
    for (v in lagvars) {
      for (i in lags) {
        E <- cbind(E, Lag(Y[,v], i))
      }
      labels <- c(labels, paste(v, ".l", lags, sep=""))
    }
    colnames(E) <- labels
    cbind(Y, E)
  }
  
  J <- do.call("rbind", by(X, X[,unitvar], innertask, lagvars, lags))
  return (J[,(ncol(J)+1-length(lagvars)*length(lags)):ncol(J)])
}






Lead <- function (x, shift = 1) 
{
  if (shift == 0) 
    return(x)
  xLen <- length(x)
  ret <- as.vector(character(xLen), mode = storage.mode(x))
  attrib <- attributes(x)
  if (!is.null(attrib$label)) 
    atr$label <- paste(attrib$label, "lead", shift, "observations")
  if (xLen > shift) {
    for (i in 1:(xLen-shift))
    {
      ret[i] <- x[i+shift]
    }
  }
  attributes(ret) <- attrib
  return(ret)
}

paneldata.leads <- function(X, unitvar, timevar, leadvars, leads=1) {
  stopifnot(length(leadvars)>=1)
  X <- X[order(X[,timevar]),]           # just in case it's not sorted.
  
  innertask <- function(Y, leadvars, leads) {
    E <- labels <- NULL
    for (v in leadvars) {
      for (i in leads) {
        E <- cbind(E, Lead(Y[,v], i))
      }
      labels <- c(labels, paste(v, ".lead", leads, sep=""))
    }
    colnames(E) <- labels
    cbind(Y, E)
  }
  
  J <- do.call("rbind", by(X, X[,unitvar], innertask, leadvars, leads))
  return (J[,(ncol(J)+1-length(leadvars)*length(leads)):ncol(J)])
}
gbm.boot <- function(data,indices,var,params){
  new.data <- data[indices,]
  params$data <- new.data
  temp.gbm <- do.call(gbm,params)
  res <- plot(temp.gbm
              ,i.var=var                   
              ,return.grid=TRUE
              ,continuous.resolution=11)
  res$y
}

cond_fx_boot <- function(data,params,model,vars){
  cond <- boot(data
               ,statistic=gbm.boot
               ,R=500
               ,parallel="multicore"
               ,ncpus=parallel::detectCores()
               ,var=vars
               ,params=params)
  cond_c <- adply(cond$t,2,quantile,probs=c(0.25,0.5,0.75))
  names(cond_c)[c(2:4)] <- c("LB","Median","UB")
  levs <- model$var.levels[vars]
  cond_c$Var1 <- rep(levs[[1]],rep=length(levs[[2]]))
  cond_c$Var2 <- rep(levs[[2]],each=length(levs[[1]]))
  cond_c
}

full_est <- function(formula,data,params,var_names){
  cv_model <- cross.val(data
                        ,data
                        ,formula
                        ,.model="gbm"
                        ,.params=params)
  model_par <- cv_model$mod.res[-which(names(cv_model$mod.res)=="fold")]
  model_par$formula <- formula
  model_par$data <- data
  model_par$distribution <- "gaussian"
  
  full_model <- do.call(gbm,model_par)
  
  model_boot <- llply(1:length(full_model$var.names)
                      ,function(x)boot(data=data
                                       ,statistic = gbm.boot
                                       ,R = 500
                                       ,parallel="multicore"
                                       ,ncpus=parallel::detectCores()
                                       ,var=x
                                       ,params=model_par)
  )
  model_boot_c <- ldply(model_boot, function(x)adply(x$t,2,quantile,probs=c(0.25,0.5,0.75)))
  names(model_boot_c)[c(2:4)] <- c("LB","Median","UB")
  
  model_boot_c$vals <- do.call(c,full_model$var.levels)
  
  model_boot_c$vars <- rep(var_names,laply(full_model$var.levels,length))
  
  return(list(pars=model_par
              ,influence=relative.influence(full_model,model_par$n.trees,TRUE,TRUE)
              ,model_res=full_model
              ,full_fx=model_boot_c
  ))
}
