library(lattice)
setwd('/home/michel/Publications/2016/Boosting/SeinMinn')
m <- read.table('finalresult_real_multipertub.txt')
head(m)
summary(m)

m$Algorithms <- factor(m$Algorithms, c("MaxDiff", "MinRSS", "ALSC", "NB", "RT", "BRT", "CT", "BCT", "J48"))
#trellis.par.get()

algo.col <- c(1,1,1,2:6,'chocolate')
trellis.par.set(superpose.symbol=list(col=algo.col, pch=1:9,cex=1.2), title="Algorithm",lines=T,points=T)
mypanel <- function(x,y,...) {
    panel.grid(h=-1,v=0,lwd=2)
    panel.xyplot(x,y,...)
}
xyplot(qlogis(pert) ~ Perturbation|QM, m, type='b', group=m$Algorithms, auto.key=list(space='right'), subset=m$Perturbation<11, panel=mypanel,pch=1:9, col=algo.col, lwd=2,cex=1.2,main='pert')

xyplot(qlogis(Fscore) ~ Perturbation|QM, m, type=c('p','l'), group=m$Algorithms, auto.key=list(space='right'), subset=m$Perturbation<11, panel=mypanel,pch=1:9, col=algo.col, lwd=2,cex=1.2,main='F-Score')

xyplot(qlogis(nonpert) ~ Perturbation|QM, m, type=c('p','l'), group=m$Algorithms, auto.key=list(space='right'), subset=m$Perturbation<11, panel=mypanel,pch=1:9, col=algo.col, lwd=2,cex=1.2,main='nonpert')

#############################################################################
## Precision function
#############################################################################

##' .. content for \description{} (no empty lines) ..
##'
##' The intercept with 0.5 precision will occur at X, the number of perturbations
##' @title Precision 
##' @param x Number of FP
##' @param X Number of perturbations
##' @param N Total number of cells
##' @param s Delta: slope factor to adjust the rate of penalty on precision as a function of the number of FP
##' @return precision
##' @author michel
f <- function(x,X,N,s) 1 - plogis((x/X-1)/x/(1-x/(N-X)), scale=1/s)

pdf('precision-measure.pdf',width=6,height=6)

delta=2;pert1=1;pert5=5;N=33
plot(function(x) f(x,pert1,N,delta),xlim=c(0,N-pert),ylab='Precision',xlab='FP',col='chocolate1',lwd=2,lty=1);grid()
plot(function(x) f(x,pert1,N,delta*2),xlim=c(0,N-pert),ylab='Precision',xlab='FP',col='chocolate2',lwd=2,lty=2,add=T)
plot(function(x) f(x,pert1,N,delta/2),xlim=c(0,N-pert),ylab='Precision',xlab='FP',col='chocolate3',lwd=2,lty=3,add=T)
plot(function(x) f(x,pert5,N,delta),xlim=c(0,N-pert5),ylab='Precision',xlab='FP',col='steelblue1',lwd=2,lty=1,add=T)
plot(function(x) f(x,pert5,N,delta*2),xlim=c(0,N-pert5),ylab='Precision',xlab='FP',col='steelblue2',lwd=2,lty=2,add=T)
plot(function(x) f(x,pert5,N,delta/2),xlim=c(0,N-pert5),ylab='Precision',xlab='FP',col='steelblue3',lwd=2,lty=3,add=T)
legend('topright', c(expression(paste(x==1, ', ', delta==2)),
               expression(paste(x==1, ', ', delta==4)),
               expression(paste(x==1, ', ', delta==1)),
               expression(paste(x==5, ', ', delta==2)),
               expression(paste(x==5, ', ', delta==4)),
               expression(paste(x==5, ', ', delta==1))
               ),
       lty=c(1,2,3,1,2,3), lwd=2, col=c('chocolate1','chocolate2','chocolate3','steelblue1','steelblue2','steelblue3'), bg='gray90')

dev.off()
