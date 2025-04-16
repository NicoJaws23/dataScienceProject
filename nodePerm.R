#Node based permutation function
library(tidyverse)
library(rptR)
library(lme4)
library(mosaic)

nodePerm <- function(df, shuffler, formula, n, type = c("rpt", "rptPoisson", "GLMM", "GLMMpoisson"), grname, na.action, returnVal){
  Obs_est <- NA
  permN <- numeric(n)
    
  if(type == "rpt"){
  m <- rptR::rpt(data = df, formula = formula, grname = grname, datatype = "Gaussian", na.action = na.action)
  Obs_est <- m$R[[returnVal]]
  permN <- mosaic::do(n) * {
    permd <- df
    permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
    p <- rptR::rpt(data = permd, formula = formula, grname = grname, datatype ="Gaussian", na.action = na.action)
    p$R[[returnVal]]
    }
  }
    
    
  else if(type == "rptPoisson"){
  m <- rptR::rptPoisson(data = df, formula = formula, grname, na.action = na.action)
  Obs_est <- m$R[[returnVal]]
  permN <- mosaic::do(n) * {
    permd <- df
    permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
    p <- rptR::rptPoisson(data = permd, formula = formula, grname, na.action = na.action)
    p$R[[returnVal]]
    }
  }
  
  else if(type == "GLMM"){
  m <- lme4::lmer(data = df, formula = formula, na.action = na.action)
  Obs_est <- lme4::fixef(m)[[returnVal]]
  permN <- mosaic::do(n) * {
    permd <- df
    permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
    p <- lme4::lmer(data = permd, formula = formula, na.action = na.action)
    lme4::fixef(p)[[returnVal]]
  }
  }
  
  else if(type == "GLMMpoisson"){
  m <- lme4::glmer(data = df, formula = formula, family = poisson, na.action = na.action)
  Obs_est <- lme4::fixef(m)[[returnVal]]
  permN <- mosaic::do(n) * {
    permd <- df
    permd[[shuffler]] = mosaic::sample(permd[[shuffler]])
    p <- lme4::glmer(data = permd, formula = formula, family = poisson, na.action = na.action)
    lme4::fixef(p)[[returnVal]]
  }
  }
  
  return(list(observed = Obs_est, permuted = permN))
}
  


c <- nodePerm(df = df, shuffler = "Sex", formula = In.Degree ~ (1|ID) + Sex, n = 1000, type = "GLMM", returnVal = "SexM", na.action = na.exclude)

#network with focals and humans

#create graph function for complex networks, with node specific attributes
