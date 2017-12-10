## ---- Figure Post Ecography
library(graphicsutils)
library(showtext)


## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Arya", "arya")
## Automatically use showtext to render text for future devices
showtext_auto()

figEcogr <- function(filename="img/figeco", part=1, wi = 8.5, hg = 5,
  res = 300, colg1 = "#CCCCCC", colg2 = '#545454', colt= colg2, colW = '#c7254e'){

  filename <- paste0(filename,part,".png")
  mypal <-colorRampPalette(c(colg1, "#3fb3b2", colg2))(100)
  mypal2 <- mypal[c(50, 20, 30, 60, 15, 80)]
  ##
  cexT = 8
  cexP = 5
  ##
  png(file=filename, res=res, width=wi, height=hg, unit="in")
  ##
  layout(matrix(c(1,2,2,2,3,4,5,6,0,7,7,7,9,8,8,8,9,10,10,10),4), heights=c(0.25,1), widths=c(1,1,0.1,0.16,0.7))

  par(
    cex.lab = 4,
    cex.axis = 4,
    las = 1,
    bg = "transparent",
    fg= colg2,
    col.axis = colg2,
    lwd = 2,
    family = "arya",
    mar = c(2.5, 1.5, 2, 0),
    mgp = c(2.4, 1, 0)
  )
  par(mar=c(0,0,0,0))
  ## Title 1
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(.2,.5, labels="Regional pool", cex=cexT, pos=4, col=colg1)

  ## Plot 1 -- metaweb
  par(mar=c(0,1,0,0))
  plot0(c(0.5,3.5),c(0,5))
  seqx <- c(2,1,3,2,1,3)
  seqy <- c(4,3,3,2,1,1)
  edges <- matrix(c(1,2,1,3,2,4,2,5,4,5,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=4, col=colg2)
  points(seqx, seqy, pch=21, bg=colg2, col=colg1, cex=8, lwd=2)
  text(seqx, seqy, labels=1:6, col=colg1, cex=cexT)
  ##
  if (part>1){
  ## Title 2
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(1,0.5, labels="Local communities", cex=cexT, pos=2, col=colg1)


  ## Plot 2-A First communities
  par(mar=c(.5, 4, .5, 1))
  plot0(c(-1,3.5), c(0,5), fill=mypal[80])
  id_sp1 <- c(1,3,6)
  edges <- matrix(c(1,3,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
    points(seqx[id_sp1], seqy[id_sp1], pch=21, col=colg1, bg=colg2, cex=cexP, lwd=1)
    text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=cexP, col=colg1)
  }

  if (part>2){
  ## Plot 2-B
  plot0(c(-1,3.5),c(0,5), fill=mypal[40])
  id_sp1 <- c(1,2,3,4,6)
  edges <- matrix(c(1,2,1,3,2,4,3,6), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, col=colg1, bg=colg2, cex=cexP, lwd=1)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=cexP, col=colg1)
  }

  if (part>2){
  ## Plot 2-C
  plot0(c(0.5,8),c(0,5), fill=mypal[10L])
  id_sp1 <- c(1,2,4,5)
  edges <- matrix(c(1,2,2,4,2,5,4,5), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  points(seqx[id_sp1], seqy[id_sp1], pch=21, col=colg1, bg=colg2, cex=cexP, lwd=1)
  text(seqx[id_sp1], seqy[id_sp1], labels=id_sp1, cex=cexP, col=colg1)
  sz_sg <- 0.4
  if (part>3)segments(x0=2-c(sz_sg,sz_sg),y0=2+1.6*c(sz_sg,-sz_sg),x1=2+c(sz_sg,sz_sg),
    y1=2-1.6*c(sz_sg,-sz_sg), lwd=3, col=colW)
  }
  #
  if (part>4){
  seqx <- 6.5+c(1,1,1)
  seqy <- c(.8, 2.5, 4.4)
  edges <- matrix(c(1,2,2,3), ncol=2, byrow=TRUE)
  for (i in 1:nrow(edges)) lines(seqx[edges[i,]],seqy[edges[i,]], lwd=3, col=colg2)
  arrows2(x0=2.8, y0=2.5, x1=6.6, cex.arr=2.8, lwd=1.6, col=colt, border=NA, prophead=FALSE)
  points(seqx, seqy, pch=21, col=colg1, bg=colg2, cex=cexP, lwd=2)
  text(seqx, seqy, labels=c(5,2,1), cex=cexP, col=colg1)
  text(4.5, 3.5, "Extinction", cex=.85*cexT)
  }

  if (part>5){
  #### Plot Gradient
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0.5,0.5, labels="Environmental gradient", srt=270, cex=cexT, col = colg1)
  ####
  par(mar=c(1.5, .9, 1.5, .9), font=2)
  image(matrix(1:100, nrow=1), col=mypal, axes=FALSE, ann=FALSE)

  ## Title 3
  par(mar=c(0,0,0,0))
  plot(c(0,1),c(0,1), type="n", axes=FALSE, ann=FALSE)
  text(0.5,.5, labels="Colonisation rate", col=colg1, cex=cexT)
  #### Plot3
  par(mar=c(.5,0,.5,.5), xaxs="i",yaxs="i")
  seqt <- seq(-10,10, by=0.1)
  plot0(c(0,0.5), range(seqt))
  box2(2:3, col2fill=NULL, col=colg2)
  moy <- -c(0,2,-3,4,7.5,-6)
  ect <- c(4,1.5,1.5,1.2,1.5,1.5)
  wei <- c(3.2,1.4,0.7,1,1.4,1.2)
  #
  seqs <- 1
  if (part>6) seqs <- c(1,6,5)
  if (part>7) seqs <- 1:6
  for (i in seqs) lines(wei[i]*dnorm(seqt,moy[i],ect[i]),seqt, lwd=3.2, col=mypal2[i])
  abline(v=0,h=10,lwd=4)
  seqx <- c(0.38,0.44,0.25,0.4, 0.44, 0.38)
  seqy <- moy
  points(seqx[seqs], seqy[seqs],  pch=21, col=colg1, bg=colg2, cex=cexP, lwd=1)
  text(seqx[seqs], seqy[seqs], labels=seqs, cex=cexP, col=colg1)
  }
  ####
  if (part>1){
    if (part<=5) {
      par(mar=c(0,0,0,0))
      plot0()
      plot0()
      plot0()
    }
    par(new=TRUE, fig=c(0,1,0,1))
    plot0(c(0,1),c(0,1))
    text(percX(30), percY(45), labels="Colonization", cex=cexT, col=colg1)
    arrows2(x0=percX(29), y0=percY(40), x1=percX(42), lwd=4, col=colt, border=NA)
  }

  ##
  dev.off()
}

figEcogr(part=3)
figEcogr(part=5)
figEcogr(part=6)
figEcogr(part=7)

# for (i in 1:1) figEcogr(part=i)
