bingo <- function(x, n=30, file='bingo.pdf') {
	pdf(file, width=11, height=8.5)
	par(mfrow=c(1,2))
	par(mar=c(1,1,3.1,1),xaxs='i',yaxs='i')
	seen <- list()
	for(i in 1:n) {
		repeat{
			cur <- sample(x, 24)
			tmp <- sapply( seen, function(x) length(setdiff(x,cur)) )
			if( all( tmp > 0 ) ) break
		}
		seen[[i]] <- cur
		cur <- append(cur, 'FREE', after=12)
		plot.new()
		mtext("Sewing Bingo", cex=2, line=1, font=4)
		abline(v=seq(0,1,by=0.2))
		abline(h=seq(0,1,by=0.2))
		tmp <- expand.grid(x=seq(0.1,0.9, by=0.2),y=seq(0.1,0.9, by=0.2))
		cx <- 0.18/strwidth(cur)
		text(tmp$x,tmp$y, cur, cex=cx)
	}
	for( i in 0:(length(x) %/% 25) ) {
		plot.new()
		abline(v=seq(0,1,by=0.2), lty=3)
		abline(h=seq(0,1,by=0.2), lty=3)
		tmp <- expand.grid(x=seq(0.1,0.9, by=0.2), y=seq(0.1,0.9, by=0.2))
		cur <- x[ (i*25 + 1):(i*25+25) ]
		cx <- 0.18/strwidth(cur)
		text(tmp$x,tmp$y, cur, cex=cx)
	}
	dev.off()
}


words <- c("Tape Measure", "Seam Guage", "Pinking Shears", "Rotary Cutter", 
"Pincushion", "Seam Ripper", "Iron", "Thread", "Bobbin", "Serger", 
"Needles", "Gingham", "Shank Button", "Snap", "Welt Pocket", 
"Patch Pocket", "Batting", "Denim", "Pintuck", "Cutting Table", 
"Casing", "Knit", "Muslin", "Calico", "Applique", 
"Stripe", "Smocking", "Log Cabin", "Seersucker", "Placket", "Ruffle", 
"Elastic", "Brocade", "Velvet", "Eyelets", "Shantung", "Pins", "Batik",
"Interfacing", "Binding", "Bias", "Zipper", "Buttonhole", "Scissors" )


words2 <- gsub(" +", "\n", words)

bingo(words2,n=40)

