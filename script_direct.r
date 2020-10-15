#######################################
##### TD SVM
#######################################

###############
###Partie 1
###############
setwd("D:/Master statistique/cours M2 SSV/Analyse données 2/TD/TD_SVM")
data<-read.table("data_cart.csv",header = T,sep = ";",dec=".")
dim(data)
library(kernlab)

##Q3 : nu-classification avec paramÃ¨tres par dÃ©faut
?ksvm
ressvm<-ksvm(regime~.,data=data[,-121],type="nu-svc")
## OU ressvm<-ksvm(x=as.matrix(data[,1:120]),y=data[,122],type="nu-svc")

##Q4 : le noyau
?kernels
slotNames(ressvm)
ressvm@kernelf
# Gaussian Radial Basis kernel function. 
 # Hyperparameter : sigma =  0.00675073035696293 

## Q5 : nu
# nu = 0.2 (cf. documentation)

## Q6 : erreur d'apprentissage
ressvm@error
## 0

## Q7 : erreur en CV
enrerr4FCV=NULL
for (i in 1:30)
	{
		ressvmloc=ksvm(regime~.,data=data[,-121],type="nu-svc",cross=4)
		enrerr4FCV=c(enrerr4FCV,ressvmloc@cross)
	}
mean(enrerr4FCV)
## 0.4908

enrerr2FCV=NULL
for (i in 1:30)
	{
		ressvmloc=ksvm(regime~.,data=data[,-121],type="nu-svc",cross=2)
		enrerr2FCV=c(enrerr2FCV,ressvmloc@cross)
	}
mean(enrerr2FCV)
## 0.5792


###############
###Partie 2
###############

## Q2 : 
seqnu=seq(0.1,0.5,by=0.1)
seqsig=10^((-2):2)

enrres=matrix(NA,length(seqnu)*length(seqsig),3)
colnames(enrres)=c("nu","sigma","errmoy")
compt=1
for (i in seqnu)
	{
print(i)
		for (j in seqsig)
			{
print(j)
				enrloc=NULL
				for (k in 1:10)
					{
						ressvmloc=ksvm(regime~.,data=data[,-121],type="nu-svc",cross=4,nu=i,kpar = list(sigma = j))
						enrloc=c(enrloc,ressvmloc@cross)
					}
				enrres[compt,]=c(i,j,mean(enrloc))
				compt=compt+1
			}
	}

enrres

> enrres
nu sigma errmoy
#[1,] 0.1 1e-02 0.5400
#[2,] 0.1 1e-01 0.9400
#[3,] 0.1 1e+00 0.8950
#[4,] 0.1 1e+01 0.9050
#[5,] 0.1 1e+02 0.9400
#[6,] 0.2 1e-02 0.5300
#[7,] 0.2 1e-01 0.9350
#[8,] 0.2 1e+00 0.8900
#[9,] 0.2 1e+01 0.9475
#[10,] 0.2 1e+02 0.9375
#[11,] 0.3 1e-02 0.5475
#[12,] 0.3 1e-01 0.9250
#[13,] 0.3 1e+00 0.9000
#[14,] 0.3 1e+01 0.9125
#[15,] 0.3 1e+02 0.9500
#[16,] 0.1 1e-02 0.5725
#[17,] 0.1 1e-01 0.9100
#[18,] 0.1 1e+00 0.8900
#[19,] 0.1 1e+01 0.9275
#[20,] 0.1 1e+02 0.9200
#[21,] 0.2 1e-02 0.5375
#[22,] 0.2 1e-01 0.9100
#[23,] 0.2 1e+00 0.8825
#[24,] 0.2 1e+01 0.9175
#[25,] 0.2 1e+02 0.9225


####### le nouveau modèle avec les nouveaux paramètres

ressvm1=ksvm(regime~.,data=data[,-121],type="nu-svc",cross=4,nu=0.2,kpar = list(sigma = 10^-02))

enrerr2FCV=NULL
for (i in 1:30)
{
  ressvm1=ksvm(regime~.,data=data[,-121],type="nu-svc",cross=4,nu=0.2,kpar = list(sigma = 10^-02))
  enrerr2FCV=c(enrerr2FCV,ressvmloc@cross)
}
mean(enrerr2FCV)













