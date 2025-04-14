#Node based permutation function
library(tidyverse)
library(rptR)
library(lme4)
library(mosaic)

nodePerm <- function(df, shuffler, formula, n, type = c("rpt", "rptPoisson", "GLMM", "GLMMpoisson"), grname, na.action, returnVal){
  permN <- do(n) * {
    permd <- df
    permd[[shuffler]] = sample(permd[[shuffler]]) 
    if(type == "rpt"){
    m <- rpt(data = permd, formula = formula, grname = grname, datatype = "Gaussian")
    data.frame(perm_est = returnVal)
    }
    if(type == "rptPoisson"){
    m <- rpt(data = permd, formula = formula, grname)
    data.frame(perm_est = returnVal)
    }
    if(type == "GLMM"){
    m <- lmer(data = permd, formula = formula, na.action = na.action)
    data.frame(perm_est = returnVal)
    }
    if(type == "GLMMpoisson"){
    m <- glmer(data = permd, formula = formula, family = poisson)
    data.frame(perm_est = returnVal)
    }
  }
  return(permN)
}

coef_sex <- fixef(m)["SexM"]


c <- nodePerm(df = df, shuffler = "Sex", formula = In.Degree ~ (1|ID) + Sex, n = 1000, type = "GLMM", returnVal = fixef(m)["SexM"])






