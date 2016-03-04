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
##' @param i Offset
##' @return precision
##' @author michel
f <- function(x,X,N,s,i=0) 1 - plogis((x/X-1+i)/x/(1-x/(N-X)), scale=1/s)

pdf('precision-measure.pdf',width=6,height=6)

delta=2;pert1=3;pert5=4;N=33
plot(function(x) f(x,pert1,N,delta),xlim=c(0,N-pert1),ylab='Precision',xlab='FP',col='chocolate1',lwd=2,lty=1);grid()
plot(function(x) f(x,pert1,N,delta*2),xlim=c(0,N-pert1),ylab='Precision',xlab='FP',col='chocolate2',lwd=2,lty=2,add=T)
plot(function(x) f(x,pert1,N,delta/2),xlim=c(0,N-pert1),ylab='Precision',xlab='FP',col='chocolate3',lwd=2,lty=3,add=T)
plot(function(x) f(x,pert5,N,delta),xlim=c(0,N-pert5),ylab='Precision',xlab='FP',col='steelblue1',lwd=2,lty=1,add=T)
plot(function(x) f(x,pert5,N,delta*2),xlim=c(0,N-pert5),ylab='Precision',xlab='FP',col='steelblue2',lwd=2,lty=2,add=T)
plot(function(x) f(x,pert5,N,delta/2),xlim=c(0,N-pert5),ylab='Precision',xlab='FP',col='steelblue3',lwd=2,lty=3,add=T)
legend('topright', c(expression(paste(x==1, ', ', delta==delta)),
               expression(paste(x==1, ', ', delta==delta*2)),
               expression(paste(x==1, ', ', delta==delta/2)),
               expression(paste(x==5, ', ', delta==delta)),
               expression(paste(x==5, ', ', delta==delta*2)),
               expression(paste(x==5, ', ', delta==delta/2))
               ),
       lty=c(1,2,3,1,2,3), lwd=2, col=c('chocolate1','chocolate2','chocolate3','steelblue1','steelblue2','steelblue3'), bg='gray90')

dev.off()


#############################################################################
## Actual values version
#############################################################################

pdf('precision-measure.pdf',width=6,height=6)

N=33
m <- data.frame(pert=c(1:5,10),delta=c(1,2,3,4,4,4),col='steelblue',grid=c(T,F,F,F,F,F),lty=c(1,2,3,4,4,4))
foo <- apply(m, 1, function(i) {
    if(grepl('T',(i['grid']))) {
        plot(function(x) f(x,as.numeric(i['pert']),N,as.numeric(i['delta'])),ylim=c(0.2,1),xlim=c(0,10),ylab='Precision',xlab='FP',col=i['col'],lwd=2,lty=as.numeric(i['lty']))
        grid()
        abline(h=0.5,col='grey80',lwd=2)
    } else {
        plot(function(x) f(x,as.numeric(i['pert']),N,as.numeric(i['delta'])),xlim=c(0,N-as.numeric(i['pert'])),ylab='Precision',xlab='FP',col=i['col'],lwd=2,lty=as.numeric(i['lty']),add=T)
    }
})
legend('topright', c(expression(delta==1,delta==2,delta==3,delta==4)),
       lty=1:4, lwd=2, col='steelblue', bg='gray90')
text(.25,0.52,'0.5',col='gray70')
text(7,0.6,'p=10',col='steelblue')
text(7,0.46,'p=5',col='steelblue')
text(7,0.39,'p=4',col='steelblue')
text(7,0.34,'p=3',col='steelblue')
text(7,0.3,'p=2',col='steelblue')
text(7,0.26,'p=1',col='steelblue')

dev.off()


#############################################################################
## Precision function revised
#############################################################################

pdf('precision-measure.pdf',width=6,height=6)

N=33
m <- data.frame(pert=c(1:5,10),delta=c(1,2,3,4,4,4),col='steelblue',grid=c(T,F,F,F,F,F),lty=c(1,2,3,4,4,4))
foo <- apply(m[1:4,], 1, function(i) {
    if(grepl('T',(i['grid']))) {
        plot(function(x) f(x,as.numeric(i['pert']),N,as.numeric(i['delta'])),ylim=c(0.2,1),xlim=c(0,10),ylab='Precision',xlab='FP',col=i['col'],lwd=2,lty=as.numeric(i['lty']))
        grid()
        abline(h=0.5,col='grey80',lwd=2)
    } else {
        plot(function(x) f(x,as.numeric(i['pert']),N,as.numeric(i['delta'])),xlim=c(0,10),ylab='Precision',xlab='FP',col=i['col'],lwd=2,lty=as.numeric(i['lty']),add=T)
    }
})
legend('topright', c(expression(delta==1,delta==2,delta==3,delta==4)),
       lty=1:4, lwd=2, col='steelblue', bg='gray90')
text(.25,0.52,'0.5',col='gray70')
i=4;text(5,f(5,i,33,i)+.025,paste0('p=',i),col='steelblue')
i=3;text(5,f(5,i,33,i)+.025,paste0('p=',i),col='steelblue')
i=2;text(5,f(5,i,33,i)+.025,paste0('p=',i),col='steelblue')
i=1;text(5,f(5,i,33,i)+.025,paste0('p=',i),col='steelblue')

dev.off()
