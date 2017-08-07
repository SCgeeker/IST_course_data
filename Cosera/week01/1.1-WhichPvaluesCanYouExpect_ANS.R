#Load pwr package to easily calculate the statistical power
if(!require(pwr)){install.packages('pwr')}
library(pwr)
#Disable scientific notation (1.05e10)
options(scipen=999)
#Set number of simulations
nSims <- 100000 #number of simulated experiments



M<-106 #Mean IQ score in the sample (will be compared with 100 in a one-sample t-test)
n<-26 #set sample size
SD<-15 #SD of the simulated data
#With a mean difference of 6, and SD of 15, and a sample size of 26, the test has 50% power)
  
p <-numeric(nSims) #set up empty variable to store all simulated p-values
bars<-20
#Run simulation
for(i in 1:nSims){ #for each simulated experiment
  x<-rnorm(n = n, mean = M, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
  z<-t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
  p[i]<-z$p.value #get the p-value and store it
}

#Check power by summing significant p-values and dividing by number of simulations
(sum(p < 0.05)/nSims) #power
### This is the answer of Q1

#Q8_1 <- length(p[p > .04 & p < .05])/(nSims/bars)

#Calculate power formally by power analysis
power<-pwr.t.test(d=(M-100)/SD, n=n,sig.level=0.05,type="one.sample",alternative="two.sided")$power #determines M when power > 0. When power = 0, will set  M = 100.

print(power)

#Plot figure
#png(file="P-valueDist.png",width=4000,height=3000, , units = "px", res = 500)
op <- par(mar = c(5,7,4,4)) #change white-space around graph
hist(p, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
     main=paste("P-value Distribution with",round(power*100, digits=1),"% Power"),
     col="grey", xlim=c(0,1),  ylim=c(0, nSims))
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
abline(h=nSims/bars, col = "red", lty=3)
#dev.off()

n_Q2 <- 51
p <-numeric(nSims) #set up empty variable to store all simulated p-values
for(i in 1:nSims){ #for each simulated experiment
        x<-rnorm(n = n_Q2, mean = M, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
        z<-t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
        p[i]<-z$p.value #get the p-value and store it
}
(sum(p < 0.05)/nSims) #power
### This is the answer of Q2
#Q8_2 <- length(p[p > .04 & p < .05])/(nSims/bars)


###Plot figure of Q3; steeper than 50% power
power<-pwr.t.test(d=(M-100)/SD, n=n_Q2,sig.level=0.05,type="one.sample",alternative="two.sided")$power 
op <- par(mar = c(5,7,4,4)) #change white-space around graph
hist(p, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
     main=paste("P-value Distribution with",round(power*100, digits=1),"% Power"),
     col="grey", xlim=c(0,1),  ylim=c(0, nSims))
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
abline(h=nSims/bars, col = "red", lty=3)


### Q4 & Q5
M_Q4 <- 100

p <-numeric(nSims) #set up empty variable to store all 
#Run simulation
for(i in 1:nSims){ #for each simulated experiment
        x<-rnorm(n = n, mean = M_Q4, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
        z<-t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
        p[i]<-z$p.value #get the p-value and store it
}
(sum(p < 0.05)/nSims) #power
### This is the answer of Q4

#Q8_3 <- length(p[p > .04 & p < .05])/(nSims/bars)

###Plot figure of Q5; It is type 1 error(alpha) in this case.
power<-pwr.t.test(d=(M_Q4-100)/SD, n=n_Q2,sig.level=0.05,type="one.sample",alternative="two.sided")$power 
op <- par(mar = c(5,7,4,4)) #change white-space around graph
hist(p, breaks=bars, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
     main=paste("P-value Distribution with",round(power*100, digits=1),"% Power"),
     col="grey", xlim=c(0,1),  ylim=c(0, nSims))
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
abline(h=nSims/bars, col = "red", lty=3)

### Q6
M_Q6 <- 107
bars_Q6 <- 100

p <-numeric(nSims) #set up empty variable to store all 
#Run simulation
for(i in 1:nSims){ #for each simulated experiment
        x<-rnorm(n = n_Q2, mean = M_Q6, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
        z<-t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
        p[i]<-z$p.value #get the p-value and store it
}

#Q8_4 <- length(p[p > .04 & p < .05])/(nSims/bars)


###Plot figure of Q6; 
power<-pwr.t.test(d=(M_Q6-100)/SD, n=n_Q2,sig.level=0.05,type="one.sample",alternative="two.sided")$power 
op <- par(mar = c(5,7,4,4)) #change white-space around graph
hist(p, breaks=bars_Q6, xlab="P-values", ylab="number of p-values\n", axes=FALSE,
     main=paste("P-value Distribution with",round(power*100, digits=1),"% Power"),
     col="grey", xlim=c(0,0.05),  ylim=c(0, nSims))
axis(side=1, at=seq(0,1, 0.1), labels=seq(0,1,0.1))
axis(side=2, at=seq(0,nSims, nSims/4), labels=seq(0,nSims, nSims/4), las=2)
abline(h=nSims/bars_Q6, col = "red", lty=3)

### When the alpha is 0.01, we have the power
pwr.t.test(d=(M_Q6-100)/SD, n=n_Q2,sig.level=0.01,type="one.sample",alternative="two.sided")$power
### answer of Q6

### Plot of Q8
P_play <- rep(1,11)
t <- 1
for(M_Play in 110:100){
        p <-numeric(nSims) #set up empty variable to store all simulated p-values
        bars<-100
        #Run simulation
        for(i in 1:nSims){ #for each simulated experiment
                x<-rnorm(n = n, mean = M_Play, sd = SD) #Simulate data with specified mean, standard deviation, and sample size
                z<-t.test(x, mu=100) #perform the t-test against mu (set to value you want to test against)
                p[i]<-z$p.value #get the p-value and store it
        }
        P_play[t] <- length(p[p > .04 & p < .05])
        t = t+1
}

P_play/1000
### at best it is 4 times...

#? Daniel Lakens, 2016. 
# This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License. https://creativecommons.org/licenses/by-nc-sa/4.0/
