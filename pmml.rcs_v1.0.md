# pmml.coxph.rcs
# XML output for a Cox proportional hazards regression model with restricted cubic splines


### Create dataset
set.seed(1000)
age <- rnorm(100, 70.3, 5.7)
sex <- rbinom(100, 1, 0.596)
edu <- sample(0:2, 100, replace = TRUE)
rural <- rbinom(100, 1, 0.4)
marstat <- sample(0:3, 100, replace = TRUE)
livewith <- sample(0:3, 100, replace = TRUE)
caregiver <- rbinom(100, 1, 0.258)
diabetes <- rbinom(100, 1, 0.131)
stroke <- rbinom(100, 1, 0.082)
chf <- rbinom(100, 1, 0.164)
chd <- rbinom(100, 1, 0.117)
hypertension <- rbinom(100, 1, 0.517)
pvd <- rbinom(100, 1, 0.117)
dementia <- rbinom(100, 1, 0.05)
ms <- rbinom(100, 1, 0.05)
parkinson <- rbinom(100, 1, 0.05)
cancer <- rbinom(100, 1, 0.145)
copd <- rbinom(100, 1, 0.089)
renal <- rbinom(100, 1, 0.03)
afib <- rbinom(100, 1, 0.05)
psychosis <- rbinom(100, 1, 0.05)
num_diseases <- diabetes+stroke+chf+chd+hypertension+pvd+dementia+ms+parkinson+cancer+copd+renal+afib+psychosis
iadl_diff <- rbinom(100, 6, 0.2)
adl_hier <- rbinom(100, 6, 0.05)
adl_decl <- rbinom(100, 1, 0.03)
cps <- rbinom(100, 6, 0.05)
chess <- rbinom(100, 6, 0.15)
painDaily <- rbinom(100, 1, 0.555)
painDisrupt <- rbinom(100, 1, 0.5)
inpatient <- rbinom(100, 9, 0.054)
emerg <- rbinom(100, 9, 0.074)
outpatient <- rbinom(100, 9, 0.254)
meds <- rbinom(100, 9, 0.55)
respirator <- rbinom(100, 1, 0.01)
chemo <- rbinom(100, 1, 0.1)
dialysis <- rbinom(100, 1, 0.01)
assessType <- rbinom(100, 2, 0.15)
assessYear <- sample(0:7, 100, replace = TRUE)
time <- rnorm(100, 90, 15)
censor <- rbinom(100, 1, 0.15)

data <- data.frame(cbind(age, sex, edu, rural, marstat, livewith, caregiver, diabetes, stroke, chf, chd, hypertension, pvd, dementia, ms, parkinson, cancer, copd, renal, afib, psychosis, num_diseases, iadl_diff, adl_hier, adl_decl, cps, chess, painDaily, painDisrupt, inpatient, emerg, outpatient, meds, respirator, chemo, dialysis, assessType, assessYear, time, censor))


### Data transformation
data$sex <- factor(data$sex, levels=c(0,1), labels=c("Male","Female"))
data$edu <-  factor(data$edu, levels=c(0,1,2), labels=c("Highschool","College","University"))
data$rural <- factor(data$rural, levels=c(0,1), labels=c("urban","rural"))
data$marstat <- factor(data$marstat, levels=c(0,1,2,3), labels=c("NeverMarried","Married","Widowed","DivSep"))
data$livewith <- factor(data$livewith, levels=c(0,1,2,3), labels=c("Alone","Spouse","Child","Group"))
data$caregiver <- factor(data$caregiver, levels=c(0,1), labels=c("No","Yes"))
data$diabetes <- factor(data$diabetes, levels=c(0,1), labels=c("No","Yes"))
data$stroke <- factor(data$stroke, levels=c(0,1), labels=c("No","Yes"))
data$chf <- factor(data$chf, levels=c(0,1), labels=c("No","Yes"))
data$chd <- factor(data$chd, levels=c(0,1), labels=c("No","Yes"))
data$hypertension <- factor(data$hypertension, levels=c(0,1), labels=c("No","Yes"))
data$pvd <- factor(data$pvd, levels=c(0,1), labels=c("No","Yes"))
data$dementia <- factor(data$dementia, levels=c(0,1), labels=c("No","Yes"))
data$ms <- factor(data$ms, levels=c(0,1), labels=c("No","Yes"))
data$parkinson <- factor(data$parkinson, levels=c(0,1), labels=c("No","Yes"))
data$cancer <- factor(data$cancer, levels=c(0,1), labels=c("No","Yes"))
data$copd <- factor(data$copd, levels=c(0,1), labels=c("No","Yes"))
data$renal <- factor(data$renal, levels=c(0,1), labels=c("No","Yes"))
data$afib <- factor(data$afib, levels=c(0,1), labels=c("No","Yes"))
data$psychosis <- factor(data$psychosis, levels=c(0,1), labels=c("No","Yes"))
data$iadl_diff <- factor(data$iadl_diff, levels=c(0,1,2,3,4,5,6))
data$adl_hier <- factor(data$adl_hier, levels=c(0,1,2,3,4,5,6))
data$adl_decl <- factor(data$adl_decl, levels=c(0,1), labels=c("No","Yes"))
data$cps <- factor(data$cps, levels=c(0,1,2,3,4,5,6))
data$chess <- factor(data$chess, levels=c(0,1,2,3,4,5,6))
data$painDaily <- factor(data$painDaily, levels=c(0,1), labels=c("No","Yes")) 
data$painDisrupt <- factor(data$painDisrupt, levels=c(0,1), labels=c("No","Yes"))
data$respirator <- factor(data$respirator, levels=c(0,1), labels=c("No","Yes"))
data$chemo <- factor(data$chemo, levels=c(0,1), labels=c("No","Yes"))
data$dialysis <- factor(data$dialysis, levels=c(0,1), labels=c("No","Yes"))
data$assessType <- factor(data$assessType, levels=c(0,1,2), labels=c("Initial","FollowUp","ChangeStat"))
data$assessYear <- factor(data$assessYear, levels=c(0,1,2,3,4,5,6,7), labels=c("2007","2008","2009","2010","2011","2012","2013","2014"))


### Variable specification
data$fem <- data$sex=="Female"
data$m_wid <- data$marstat=="Widowed"
data$m_sep <- data$marstat=="DivSep"
data$m_nvr <- data$marstat=="NeverMarried"
data$lv_sp <- data$livewith=="Spouse"
data$lv_ch <- data$livewith=="Child"
data$lv_gp <- data$livewith=="Group"
data$careY <- data$caregiver=="Yes"
data$edu_c <- data$edu=="College"
data$edu_u <- data$edu=="University"
data$pxINF <- data$painDisrupt=="Yes"


### coxph model with rcs()
#installpackages
library(survival)
library(rms)
library(pmml)
library(XML)

(model <- coxph(Surv(time, censor) ~ rcs(age, 3) + fem + edu_c + edu_u + rural + m_wid + m_sep + m_nvr + lv_sp + lv_ch + lv_gp + careY + diabetes + stroke + chf + chd + hypertension + pvd + dementia + ms + parkinson + cancer + copd + renal + afib + psychosis + num_diseases + adl_decl + painDaily + pxINF + inpatient + emerg + outpatient + meds + respirator + chemo + dialysis, data=data, robust=TRUE))


### Generate XML output
match.test <- try(length(names(coefficients(model)))==length(attributes(model$terms)$dataClasses))
if (inherits(match.test, "try-error")) {
	stop()
	} else {
output <- xmlOutputDOM(tag="AmyXMLmarkup")
ddNode <- xmlNode("DataDictionary")
    dnum <- 1
    dataType <- attributes(model$terms)$dataClasses
    for (i in 1:length(names(coefficients(model)))) {
    	dname <- paste("p", dnum)
        dname <- gsub(" ", "", dname)
        dnum <- dnum + 1
        dnode <- xmlNode("DataField", attrs = c(name = dname, label = names(coefficients(model))[i], opType=dataType[[i]]))
        if (grepl("nmatrix", xmlAttrs(dnode)[["opType"]], fixed=FALSE)){
        	dnode <- xmlNode("DataField", attrs = c(name = dname, label = names(coefficients(model))[i], opType="rcs"))
        }
        else {
        	dnode <- xmlNode("DataField", attrs = c(name = dname, label = names(coefficients(model))[i], opType=dataType[[i]]))
        	}
        ddNode <- append.XMLNode(ddNode, dnode)
   }
output$addNode(ddNode)
plNode <- xmlNode("ParameterList")
    pnum <- 1
    for (i in 1:length(names(coefficients(model)))) {
        pname <- paste("p", pnum)
        pname <- gsub(" ", "", pname)
        pnum <- pnum + 1
        pnode <- xmlNode("Parameter", attrs = c(name = pname, label = names(coefficients(model))[i], referencePoint=model$means[[i]]))
        plNode <- append.XMLNode(plNode, pnode)
    }
output$addNode(plNode)
cat(saveXML(output, prefix='<Amy PMML-mod>\n'))
}
