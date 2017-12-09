library(graphicsutils)
library(showtext)


## ---- Code to generate a figure to illustrate the classic TIB
## ---- figMW (MacArthur Wilson)
n = 100
A = 0:n
B = matrix(rep(0,3*(n+1)),101)
C = matrix(rep(0,3*(n+1)),101)
for (k in 0:n) {
  c=0.002
  e=0.002
  B[k+1,1]=1-(1-c)^(n-k)
  C[k+1,1]=1-(1-e)^k
  c=0.001
  e=0.005
  B[k+1,2]=1-(1-c)^(n-k)
  C[k+1,2]=1-(1-e)^k
  c=0.005
  e=0.001
  B[k+1,3]=1-(1-c)^(n-k)
  C[k+1,3]=1-(1-e)^k
}


## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Arya", "arya")
## Automatically use showtext to render text for future devices
showtext_auto()



figMW <- function(filename = "img/figMW", part = 1, hg = 5, wi = 8.5, res = 300,
  col1 = '#787878', col2 = "#1b95e0", col3 = '#232323'){
  ##
  cex.l <- 6
  filename <- paste0(filename, part, ".png")
  ##
  png(filename, height = hg, width = wi, unit="in", res = res)

    layout(matrix(c(1,2), ncol=2), width = c(1,0.24))

    par(
      cex.lab = 4,
      cex.axis = 4,
      las = 1,
      lwd = 2,
      bg = "transparent",
      fg= col1,
      col.axis = col1,
      family = "arya",
      mar = c(2.5, 1.5, 2, 0),
      mgp = c(2.4, 1, 0)
    )

    ##--- Main plot region
    plot0(c(0,100), c(0,0.44))
    if (part>3){
      if (part>6) lines(A, B[,2L], lwd=3, col=col1)
	    if (part>6) lines(A, C[,2L], lwd=3, col=col1, lty=4)
	    if (part>4) lines(A, B[,3L], lwd=3, col=col2)
	    if (part>5) lines(A, C[,3L], lwd=3, col=col2, lty=4)
      mtext(1, text="Specific richness", cex=cex.l, line=.5)
      mtext(3, at=-0.5, text="Rates", cex=cex.l, line=.25)
      box(bty="l")

      if (part>7) {
      par(mgp=c(2, .4, 0))
       points(c(16.53,82.92), c(0.08,0.08), col=c(col1, col2), pch=19)
       axis(1,at=c(16.53,82.92), lwd=0, lwd.ticks=2, labels=c("-","+"), col=col1, tck=0, cex=2)
      }

      if (part>4){
        if (part<6){
          legend(0, 0.53, c("Colonisation"), lty=1, ncol=2, bty="n",
          seg.len=1.2, cex=4, lwd=2)
        } else {
          legend(0, 0.53, c("Colonisation", "Extinction"), lty=c(1,4), ncol=2, bty="n",
          seg.len=1.2, cex=4, lwd=2)
        }
      }
    }
    ##
    par(mar=c(.5,0,.25,0.5))
    plot0(c(0,10),c(0,10))
    abline(h=8, lwd=2, col=col1)
    rect(-1,8,11,11, col=col1, border=NA)
    text(5, 9.25, "Continent", cex=cex.l, col=col3)

    if (part>1){
    rect(.5, 6.5, 6, 5.25, col=col2, border=NA)
    if (part>2) arrows2(x0=mean(c(.5,6)), y0=8, y1=6.5, col=col1, border=NA, prophead=FALSE)
    text(3.25, 5.875, "Island", cex=cex.l*.9, col=col3)
    }

    if (part>6) {
      arrows2(x0=mean(c(7.5,9.5)), y0=8, y1=2, col=col1, border=NA, prophead=FALSE)
      rect(7.5, 2, 9.5, 1, col=col1, border=NA)
    }
  dev.off()
}


figMW(part=5)
figMW(part=6)
figMW(part=8)
