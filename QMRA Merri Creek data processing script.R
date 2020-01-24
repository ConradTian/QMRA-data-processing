## Set working directory & read in concentration data 
Initialization <- function(){
  setwd("C:/Users/10510/OneDrive - Monash University/2019_01_23/Curriculum/Summer research/study/R language/Framework and Sourcecode for QMRA")
  concent.all <<- read.csv("Concentration template.csv",header = TRUE)
  print(colnames(concent.all))
  print("Extract concentration by function concentration(patho.name = ) from picking a name above")
}

## Extract concentration of specified pathogen
concentration <- function(patho.name){
  concentration <- (parse(text = paste("concent.all$",patho.name,sep = "")))
  concent <<- eval(concentration)
  print("input ingestion Type as Primary or Secondary or Both")
  return(concent)
  patho.name <<- patho.name
}


## Generate ingestion volume for 100 people
Ingestion <- function(vol.type){
  if(vol.type == 'Primary'){
    volp <<- rexp(n = 100, rate = 0.0372)
    hist(volp)
  }
  else if(vol.type == 'Secondary'){
    vols <<- rlnorm(n = 100, meanlog = 0.7, sdlog = 1.3)
    hist(vols)
  }
  else if(vol.type == 'Both'){
    rando <- abs(floor(runif(n = 1)*100))
    volp <- rexp(n = rando, rate = 0.0372)
    vols <- rlnorm(n = 100-rando, meanlog = 0.7, sdlog = 1.3)
    volb <<-append(x = volp, values = vols, after = length(volp))
    hist(volb)
  }
}


## Produce dose
dosing <- function(iter.dose, vol.type){
  concent.sample <- matrix(nrow = iter.dose, ncol = 1)
  vol.type.sample <- matrix(nrow = iter.dose, ncol = 1)
  dose <<- matrix(nrow = iter.dose, ncol = 1)
for(i in 1:iter.dose)
  {
    concent.sample[i] <- runif(n = 1, min(concent), max(concent))
    vol.type.sample[i] <- runif(n = 1, min(vol.type), max(vol.type))
    dose[i] <<- concent.sample[i]* vol.type.sample[i]
}
  hist(dose)
  return(i)
  print("In the next step (i.e. response, enter {'BP' for Beta-Possion}, {'Hyp' for Hypergeometric}, {'Exp' for Exponential})")
}


## Generate dose and response relationship
responsing <- function(iter.response, dist.type, dist.para1, dist.para2 = NULL, dist.para3 = NULL, dist.para4 = NULL){
  dose.sample <- matrix(nrow = iter.response, ncol = 1)
  response <<- matrix(nrow = iter.response, ncol = 1)
  if(dist.type == 'BP'){
    beta <- (dist.para2)/(2^(1/dist.para1)-1)
  for(i in 1:iter.response)
    {
    dose.sample[i] <- runif(n = 1, min(dose), max(dose))
    response[i] <<- 1-((1+dose.sample[i]/beta)^(-dist.para1))
    }
  }
  else if(dist.type == 'Exp'){
  for(i in 1:iter.response)
    {
      dose.sample[i] <- runif(n = 1, min(dose), max(dose))
      response[i] <<- 1-exp(-dose.sample[i]*dist.para1)
    }
  }
    x11();par(mfrow = c(3,1));plot(dose.sample,response, main = 'Risk Plot', xlab = 'Dose', ylab = 'Risk of infection');hist(response, main = "Risk of illness", xlab = "Risk");hist(dose.sample, main = "Simulated doses", xlab = "Dose")
    }

  #else if(dist.type == 'Hyp'){
#  for(i in 1:iter.response)
#  {
#    dose.sample[i] <- runif(n = 1, min(dose), max(dose))
#    response[i] <<- 
#  }
#}
