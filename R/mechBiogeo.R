##---
myspec <-  function(nb, cx, cy, ry, ...){
  theta <- runif(nb)*2*pi
  rayon <- runif(nb)*ry
  points(cx + rayon*cos(theta), cy + rayon*sin(theta), ...)
}

##---
"#3fb3b2";
"#ffdd55";
"#c7254e";
"#1b95e0";
"#8555b4";
"#8ddd75";
"#787878";
"#CCCCCC";


##---
figAll <- function(filename="img/figall", part=1, wi=8.5, hg=5, colg="#CCCCCC",
  col1="#ffdd55", col2="#8ddd75", col3="#c7254e", col4="#1b95e0"){

  filename <- paste0(filename, part, ".png")

  png(file=filename, res=300, width=wi, height=hg, unit="in")
  par(
    cex=1.5,
    cex.lab = 4,
    cex.axis = 4,
    las = 1,
    lwd = 2,
    bg = "transparent",
    fg= colg,
    col.axis = colg,
    family="arya",
    mar = c(1.5,0.5,1,0),
    mgp=c(2.2,1,0)
  )
  layout(rbind(1,2,3,c(4,5)), heights=c(.2, .06, 1, .24), widths=c(.2,1))

  par(mar=c(0.2,0.1,0.1,0.1))
  ##
  plot0(c(0,10), c(0,.5))
  seqx <- seq(0, 10, .1)
  if (part<3) text(5,.25, labels="e.g. temperature", cex=6, col=colg)
  if (part>3 & part<8) lines(seqx, dnorm(seqx, 2, .78), col=col1, lwd=1.8)
  if (part>4 & part<8) {
    lines(seqx, dnorm(seqx, 5.4, .78), col=col2, lwd=1.8)
    lines(seqx, dnorm(seqx, 8.2, .78), col=col4, lwd=1.8)
  }
  if (part>5) lines(seqx, dnorm(seqx, 5, 1.1), col=col3, lwd=1.8)

  if (part>7){
    lines(seqx, dnorm(seqx, 7.8, .92), col=col2, lwd=1.8)
    lines(seqx, dnorm(seqx, 2, .88), col=col1, lwd=1.8)
  }

  par(mar=c(0.1, 2, 0.1, 2))
  image(matrix(1:100, ncol=1), col=colorRampPalette(c('#CCCCCC', '#232323'))(512), axes=FALSE, ann=FALSE)

  ##
  par(mar=c(0.2,0.1,0.1,0.1), yaxs="i")
  plot0(c(0,10), c(0,5))


  myspec(2, 1, 1, .8, pch=19, cex=1.2, col=col1, lwd=.8)
  if (part==1) circle(x = 1, y = 1, radi =.9, lwd=1)

  if (part>1){
    myspec(60, 1, 1, .8, pch=19, cex=1.2, col=col1, lwd=.8)
    if (part>2){
      myspec(60, 2, 4, .8, pch=19, cex=1.2, col=col1, lwd=.8)
      myspec(60, 3, 2, .8, pch=19, cex=1.2, col=col1, lwd=.8)
    }
  }

  if (part>4 & part<8){
    myspec(60, 5, 3.8, 1.2, pch=19, cex=1.2, col=col2, lwd=.8)
    myspec(40, 4.5, 1, .6, pch=19, cex=1.2, col=col2, lwd=.8)
    myspec(45, 6.8, 1.6, 1, pch=19, cex=1.2, col=col2, lwd=.8)
    #
    myspec(60, 7.5, 4, .8, pch=19, cex=1.2, col=col4, lwd=.8)
    myspec(80, 9, 1.6, 1.4, pch=19, cex=1.2, col=col4, lwd=.8)
  }
  if (part>7){
    myspec(60, 5, 3.8, 1.2, pch=19, cex=1.2, col=col2, lwd=.8)
    myspec(20, 5, 3.8, 1.2, pch=19, cex=1.2, col=col1, lwd=.8)
    myspec(30, 4.5, 1, .6, pch=19, cex=1.2, col=col1, lwd=.8)
    myspec(45, 6.8, 1.6, 1, pch=19, cex=1.2, col=col2, lwd=.8)
    #
    myspec(60, 7.5, 4, .8, pch=19, cex=1.2, col=col2, lwd=.8)
    myspec(80, 9, 1.6, 1.4, pch=19, cex=1.2, col=col2, lwd=.8)
  }
  if (part>5){
    myspec(30, 5, 3.8, 1.2, pch=19, cex=1.2, col=col3, lwd=.8)
    myspec(20, 7.5, 4, .8, pch=19, cex=1.2, col=col3, lwd=.8)
    myspec(15, 3, 2, .8, pch=19, cex=1.2, col=col3, lwd=.8)
  }

  if (part>5){
    par(mar=c(1,0,0,0))
    plot0(c(0,10),c(0,10))
    lines(c(2, 5), c(1.5, 8.5), lwd=2)
    lines(c(5, 5), c(1.5, 8.5), lwd=2)
    if (part<8) lines(c(8, 5), c(1.5, 8.5), lwd=2)
    points(c(2,5,5),c(1.5,1.5,8.5), pch=19, col=c(col1, col2, col3), cex=3.4)
    if (part<8) points(8, 1.5, pch=19, col=col4, cex=3.4)
  }


  if (part>6){
    par(mar=c(1,2,0,1), lend=2)
    plot0(c(0,1,0,1))
    cold <- col4
    ##
    if (part>7) {
      xe <- .9
      xe2 <- .65
      cold <- NA
    } else xe <- xe2 <- .65
    lwhc <- 1.8
    lines(c(.1,xe), c(.1,.1), lwd=lwhc)
    lines(c(.1,.4), c(.65,.65), lwd=lwhc)
    lines(c(.4,xe), c(.5,.5), lwd=lwhc)
    lines(c(.4,xe2), c(.9,.9), lwd=lwhc)
    lines(c(.1,.1), c(.1,.65), lwd=lwhc)
    lines(c(.4,.4), c(.5,.9), lwd=lwhc)
    lines(c(0,.1), c(.375,.375), lwd=lwhc)
    points(c(xe,xe,xe2), c(.1, .5, .9), col=c(col1, col2, cold), pch=19, cex=2.2)
  }

  dev.off()
}

figAll(part = 1)
figAll(part = 2)
figAll(part = 3)
figAll(part = 4)
figAll(part = 5)
figAll(part = 6)
figAll(part = 7)
figAll(part = 8)
