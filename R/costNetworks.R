# ## Loading Google fonts (http://www.google.com/fonts)
# font_add_google("Arya", "arya")
# ## Automatically use showtext to render text for future devices
# showtext_auto()
#

library(graphicsutils)

## ------- compute some costs
input <- function(x, eff=0.1) (1-x)/(eff) + x/(eff^2)
seqx <- seq(0, 1, 0.01)
val <- input(seqx)
val2 <- input(seqx, eff=.05)

cords <- data.frame(
  x = rep(c(.2, .5, .8), each=3) + c(0, -.075, .075, 0, -0.05, .05, 0, 0, 0),
  y = rep(c(0, 1, 1), 3),
  num = rep(c(5,4,2), 3)
)

cords$y[6] <- 1.5
cords$y[9] <- 2

lks <- data.frame(
  from = c(1, 1, 4, 4, 5, 7, 8),
  to = c(2, 3, 5, 6, 6, 8, 9)
  )



### Figure
figCostMotif <- function(filename="img/motifcost", part=1, wi = 8, hg = 7,
  res = 300, colg1 = "#CCCCCC", colg2 = '#545454', colW = '#c7254e') {

  filename <- paste0(filename, part, ".png")
  png(filename, width=wi, height=hg, unit="in", res=300)
  graphics::layout(matrix(1:2,2,1), heights=c(1,2))
  ##
  par(
    cex.lab = 1.2,
    cex.axis = 1.2,
    las = 1,
    bg = "transparent",
    fg= colg1,
    col.axis = colg1,
    lwd = 2,
    family = "arya",
    mar = c(1, 4, 1, 1),
    mgp = c(2.4, 1, 0)
  )

    cexT = 2
    cexP = 5

    nod = 1:9
    edg = 1:7

    if (part == 1){
      nod = 4:6
      edg = 3:5
    }

    plot0(c(0,1), c(-0.25,2.25))
    ##-- edges
    for (i in edg){
      lines(c(cords[lks[i,1], 1], cords[lks[i,2], 1]),
      c(cords[lks[i,1], 2], cords[lks[i,2], 2]), col = colg2, lwd=2.4)
    }
    ##
    for (i in nod) {
      points(cords$x[i], cords$y[i], pch=21, bg=colg2, col=colg1, cex=cexP, lwd=2)
      text(cords$x[i], cords$y[i], labels=cords$num[i], col=colg1, cex=cexT)
    }
    ##
    text(.42, .5, expression(alpha), cex=2, col=colg1)
    text(.58, .52, expression(1-alpha), cex=2, col=colg1)

  if (part>2) {
    par(mar=c(5, 6, 1, 1))
    plot0(c(0,1), range(c(val,val2)))
    lines(seqx, val, lwd=3)
    lines(seqx, val2, lty=2, lwd=3)
    ##
    axis(1, at = c(0,.5,1), labels = c(0,.5,1))
    mtext(1, line = 3.5, text=expression(alpha), cex=2.6, col=colg1)
    axis(2, at = c(min(val), max(val), max(val2)), labels = c("Emin", "Emax1", "Emax2"))
    ##
    legend("topleft", legend = c(expression(phi==0.1), expression(phi==0.08)), lty = c(1,2), bty="n",   cex=1.5, lwd=2)
    box2(1:2, lwd=2, col=colg2)
  }


  dev.off()
}


figCostMotif(part = 1)
figCostMotif(part = 2)
figCostMotif(part = 3)
figCostMotif(part = 4)
