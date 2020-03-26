
# choose the name and location for your .tex file
# it should be the same directory as your latex document
graphPath <- c('Graphs/')

# plots a normal distribution curve
pdf (paste0(graphPath, "cutpointConstraints.pdf"), h=2, w=7)
par (mar=c(2,2,1,1))
curve(dnorm(x,0,1),xlim=c(-3,3),main="",
      xlab = "", ylab = "",
      frame.plot = FALSE, axes = FALSE)
Axis(side=1, at=c(-3,3), labels=FALSE)
mtext (side=1, line=0, expression(theta))
mtext (side=1, line=-1.5, at=-0.25, expression(kappa[2]))
# mtext (side=1, line=-1.5, at= 0.25, expression(kappa[3]))
mtext (side=1, line=-3, at=-0.9, expression(kappa[1]==hat(mu)[0.1]))
mtext (side=1, line=-3, at= 0.7, expression(kappa[3]==hat(mu)[0.9]))
arrows (x0=-0.9, x1=-0.9, y0=0.11, y1=0, length=0.1, angle=15)
arrows (x0=0.7, x1=0.7, y0=0.11, y1=0, length=0.1, angle=15)
# must turn device off to complete .tex file
dev.off()
