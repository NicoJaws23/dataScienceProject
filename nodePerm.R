#Node based permutation function
library(tidyverse)
library(rptR)
library(lme4)
library(mosaic)

nodePerm <- function(df, shuffler, formula, n, type = c("rpt", "rptPoisson", "GLMM", "GLMMpoisson"), grname, na.action, returnVal){
  permN <- do(n) * {
    permd <- df
    permd[[shuffler]] = sample(permd[[shuffler]]) 
    
    perm_est <- NA
    
    if(type == "rpt"){
    m <- rpt(data = permd, formula = formula, grname = grname, datatype = "Gaussian")
    perm_est <- m$R[[returnVal]]
    }
    else if(type == "rptPoisson"){
    m <- rpt(data = permd, formula = formula, grname)
    perm_est <- m$R[[returnVal]]
    }
    else if(type == "GLMM"){
    m <- lmer(data = permd, formula = formula, na.action = na.action)
    perm_est <- fixef(m)[[returnVal]]
    }
    else if(type == "GLMMpoisson"){
    m <- glmer(data = permd, formula = formula, family = poisson)
    perm_est <- fixef(m)[[returnVal]]
    }
    data.frame(perm_est = perm_est)
  }
  return(permN)
}



c <- nodePerm(df = df, shuffler = "Sex", formula = In.Degree ~ (1|ID) + Sex, n = 1000, type = "GLMM", returnVal = "SexM", na.action = na.exclude)






