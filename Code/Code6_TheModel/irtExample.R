#Producing figure irtExample

ruler <- seq(-4,4,length=50)
curve1 <- pnorm((ruler-1)/1)
curve2 <- pnorm((ruler-(-1))/1)
curve3 <- pnorm((ruler-1)/3)

pdf ("Graphs/irtExample.pdf", h=7, w=12)
par (mar=c(2,7,0,0), las=0)
plot (ruler, curve1, type="l", lwd=3, ylim=c(0,1), xlim=c(-4,4), axes=F, xlab="", ylab="")
points (xy.coords(ruler, curve2), type="l", col="black", lwd=1)
points (xy.coords(ruler, curve3), type="l", col="black", lwd=1, lty=2)
axis (1, at=c(-4,4), labels=FALSE, tick=TRUE)
#axis (2, at=c(0,1), labels=FALSE, tick=TRUE)
mtext (expression (paste ("State-market disposition (", zeta, ")")), side=1, line=1)
mtext ("Probability of choosing pro-market response", side=2, line=5)
par (las=1)
segments (x0=-4, x1=-4, y0=0, y1=1)
text (xy.coords(0.1,0.15), labels="high difficulty, high discrimination", adj=c(0,NA))
text (xy.coords(0.1,0.10), labels=expression(alpha[1]== 1), adj=c(0,NA))
text (xy.coords(0.1,0.05), labels=expression(lambda[1]==1), adj=c(0,NA))
text (xy.coords(-2,0.95), labels="low difficulty, high discrimination", adj=c(0,NA))
text (xy.coords(-2,0.90), labels=expression(alpha[0]==-1), adj=c(0,NA))
text (xy.coords(-2,0.85), labels=expression(lambda[0]==1), adj=c(0,NA))
text (xy.coords(1.9,0.60), labels="high difficulty, low discrimination", adj=c(0,NA))
text (xy.coords(1.9,0.55), labels=expression(alpha[2]==1), adj=c(0,NA))
text (xy.coords(1.9,0.50), labels=expression(lambda[2]==3), adj=c(0,NA))
text (xy.coords(1.7,0), labels=expression(paste(zeta[2], " (rightist)")), adj=c(-0.1,NA))
text (xy.coords(-0.8,0), labels=expression(paste(zeta[1], " (centrist)")), adj=c(-0.1,NA))

segments (x0=1.7, x1=1.7, y0=0, y1=pnorm((1.7+1)/1), lty=3)
segments (x0=-4, x1=1.7, y0=pnorm((1.7-1)/1), y1=pnorm((1.7-1)/1), lty=3)
segments (x0=-4, x1=1.7, y0=pnorm((1.7+1)/1), y1=pnorm((1.7+1)/1), lty=3)

segments (x0=-0.8, x1=-0.8, y0=0, y1=pnorm((-0.8+1)/1), lty=3)
segments (x0=-4, x1=-0.8, y0=pnorm((-0.8+1)/1), y1=pnorm((-0.8+1)/1), lty=3)
segments (x0=-4, x1=-0.8, y0=pnorm((-0.8-1)/1), y1=pnorm((-0.8-1)/1), lty=3)

segments (x0=-4, x1=-0.8, y0=pnorm((-0.8-1)/3), y1=pnorm((-0.8-1)/3), lty=3)


mtext (at=pnorm((1.7+1)/1), side=2, text=expression(paste (Phi, bgroup("(", frac(zeta[2]-alpha[0],lambda[0]), ")"))), line=-1)
mtext (at=pnorm((1.7-1)/1), side=2, text=expression(paste (Phi, bgroup("(", frac(zeta[2]-alpha[1],lambda[1]), ")"))), line=-1)
mtext (at=pnorm((-0.8+1)/1), side=2, text=expression(paste (Phi, bgroup("(", frac(zeta[1]-alpha[0],lambda[0]), ")"))), line=-1)
mtext (at=pnorm((-0.8-1)/1), side=2, text=expression(paste (Phi, bgroup("(", frac(zeta[1]-alpha[1],lambda[1]), ")"))), line=-1)
mtext (at=pnorm((-0.8-1)/3), side=2, text=expression(paste (Phi, bgroup("(", frac(zeta[1]-alpha[2],lambda[2]), ")"))), line=-1)
dev.off()