BayesGRM <- "model {
  for (i in 1:N){ # index for country-year observations
    for (j in 1:J){ # index for policy item
      Y[i,j] ~ dcat(prob[i,j,1:K[j]]) # each policy item has K categories; K can be item-specific
      for (k in 1:(K[j]-1)){
        logit(P[i,j,k]) <- kappa[j,k] - alpha[j]*theta[i]
      }
      P[i,j,K[j]] <- 1
    }
    for (j in 1:J){ # Recovering probabilities
      prob[i,j,1] <- P[i,j,1]
      for (k in 2:K[j]) {
        prob[i,j,k] <- P[i,j,k] - P[i,j,k-1]
      }
    }
    theta[i] ~ dnorm(0,1)
  }
  for (j in 1:J){
    alpha[j] ~ dnorm(m.alpha, pr.alpha)T(0,) # We constrain this parameter to be POSITIVE
    for (k in 1:(K[j]-1)){
      kappa.star[j,k] ~ dnorm(m.kappa, pr.kappa)
    }
    kappa[j, 1:(K[j]-1)] <- sort(kappa.star[j, 1:(K[j]-1)])
  }
  pr.alpha <- pow(s.alpha,-2) # s.alpha is prior standard deviation on discrimination parameter
  pr.kappa <- pow(s.kappa,-2)
}"

# "For example, if the numbers of categories for a 5 item test are 2,3,4,4,and 4,
# then a 5 × 3 matrix of the following format should be passed to JAGS for the matrix kappa"

#NA  0  0
#NA NA  0
#NA NA NA
#NA NA NA
#NA NA NA



