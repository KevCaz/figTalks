figNetEnerg <- function(filename="img/netenerg", part=1, wi = 5, hg = 7,
  res = 300, colg1 = "#CCCCCC", colg2 = '#545454', colt= colg2, colW = '#c7254e'){

  filename <- paste0(filename, part, ".png")
  ##
  cexT = 3
  cexP = 5
  ##
  png(file=filename, res=res, width=wi, height=hg, unit="in")
  ##
  par(
    cex.lab = 4,
    cex.axis = 4,
    las = 1,
    bg = "transparent",
    fg= colg2,
    col.axis = colg2,
    lwd = 2,
    family = "arya",
    mar = c(0, 0, 0, 0),
    mgp = c(2.4, 1, 0)
  )

  ##-- Metaweb
  plot0(c(0,4),c(-2,5))
  seqx <- c(2,1,3,2,1,3)
  seqy <- c(4,3,3,2,1,1)
  ##
  edges <- matrix(c(1,2,1,3,2,4,2,5,4,5,3,6), ncol=2, byrow=TRUE)
  ##
  labs <- 1:length(seqx)
  ##
  if (part>4) {
    labs <- labs[-1]
    seqx <- seqx[-1L]
    seqy <- seqy[-1L]
    edges <- edges[-2,]
    edges <- edges-1
  }
  ##
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=4, col=colg2)
  ## Energy edeges
  if (part>1) {
    lines(c(1,2), c(1,-1), lwd=4, col=colg2)
    lines(c(3,2), c(1,-1), lwd=4, col=colg2)
  }
  ##
  points(seqx, seqy, pch=21, bg=colg2, col=colg1, cex=8, lwd=2)
  text(seqx, seqy, labels=labs, col=colg1, cex=cexT)
  ## Energy
  if (part>1){
    points(2, -1, pch=22, bg=colg2, col=colg1, cex=cexP*2, lwd=2*2)
    text(2, -1, labels='E', col=colg1, cex=cexT*1.2)
  }
  ##-- Annotations
  cexA <- 3
  if (part == 3) {
    text(2, 4.7, labels=expression(P[1] > MVP), cex=cexA, col=colg1)
  }
  if (part == 5) {
    text(2, 4.7, labels=expression(P[1] < MVP), cex=cexA, col=colg1)
  }
  ##--
  if (part == 4) {
      for (i in 1:nrow(edges)) {
        text(mean(seqx[edges[i,]]), mean(seqy[edges[i,]]),  labels = expression(phi), cex=cexA, col=colg1)
      }
  }
  ##
  if (part>5) {
    circle(x=4/3, y=2, radi=1.4, border=colW, lwd=4)
  }
  ##
  dev.off()
}

figNetEnerg(part=1)
figNetEnerg(part=2)
figNetEnerg(part=3)
figNetEnerg(part=4)
figNetEnerg(part=5)
figNetEnerg(part=6)
