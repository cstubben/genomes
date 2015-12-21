plotby <- function (x, name="Organism_Name", date="Release_Date", groupby = "Status", top = 5, labels = FALSE,
    curdate=TRUE, abbrev = TRUE, flip = NA, legend = "topleft", lbty = "o", 
    lcol = 1, ltitle = NULL, lcex = 1, lsort = TRUE, cex = 1, inset=0,
    ylim = NA, las = 1, lwd = 1, log = "", xlab = "Release Date", ylab = "Genomes", type = "l",
    col = c("blue", "red", "green3", "magenta", "yellow"), lty = 1:top, pch = c(15:18, 1:3), 
    ...)
{
    # check column names (or numbers) - 
   if(!is.numeric(name) && !name %in% names(x) ) stop("No column matching ", name)
   if(!is.numeric(date) &&  !date %in% names(x) ) stop("No column matching ", date)
    if(!is.numeric(groupby) &&  !groupby %in% names(x) ) stop("No column matching ", groupby)
 
  
    x <- data.frame(date = x[, date], name = x[, name], groupby = x[, groupby], 
        stringsAsFactors = FALSE)
    x <- x[!is.na(x$date), ]
    if (nrow(x) == 0) {
        stop("No rows to plot")
    }
   ## get top groups (total lines in plot)
    y <- table2(x$groupby, n = top)
    y <- head(rownames(y), top)
    x <- x[x$groupby %in% y, ]
    ## IN case groupby is factor, convert to character so empty factors do not show up in legend
    if (is.factor(x$groupby)) {
        x$groupby <- as.character(x$groupby)
    }
    if (length(pch) < top) {
        pch <- rep(pch, length = top)
    }
    if (length(col) < top) {
        col <- rep(col, length = top)
    }
    if (length(lty) < top) {
        lty <- rep(lty, length = top)      
    }

    #--------------------------------------------------------------------------#
   ## PLOT groups using one line with different symbols and labeled points
    if (labels) {
        x <- x[order(x$date, x$groupby, x$name), ]
       # rownames(x) <- 1:nrow(x)
        x$y<- 1:nrow(x)
        if(curdate) {
      
          x <- rbind( x,  data.frame( released=Sys.Date(), name="", groupby="", y=nrow(x) ))
        }
 
        if (abbrev) {
            x$name <- abbrev(x$name)
        }
          # flip labels on left or right at midway point between dates by default
         # OR just increase margins and set xpd=TRUE (write outside margins)       
        if (is.na(flip)) {
           x2 <- mean(as.numeric(range(x$date)))
            class(x2) <- "Date"
            flip <- nrow(x[x$date < x2, ])
        }
        n <- flip
        # July 2012 added missing ylim option to plot
        if (any(is.na(ylim))) {
            ylim <- c(1, max(x$y) )
        }

        plot(x$date, x$y, type = type, col = "gray70", ylim = ylim, 
            lty = lty, xlab = xlab, ylab = ylab, las = las, lwd = lwd, 
            ...)
        ## order top groups alphabetically ???
        lnames <- sort(y)
        for (i in 1: max(x$y) ) {
          ## if groupby is blank, then grep matches everthing - use [1]
            j <- grep(x$groupby[i], lnames)[1]
            points(x$date[i], i, pch = pch[j], col = col[j])
            text(x$date[i], i, x$name[i], pos = ifelse(i > 
                n, 2, 4), cex = cex)
        }
        legend(legend[1], legend[2], lnames, col = col, pch = pch, 
            bty = lbty, ncol = lcol, title = ltitle, inset = inset, cex = lcex)
    }
    #--------------------------------------------------------------------------#
   ## plot GROUPs using multiple lines
    else {
        genomes <- table(x$date, x$groupby)
        # add today's date
        if(curdate) {
           genomes <- rbind(genomes, 0)
           rownames(genomes)[nrow(genomes)]<- as.character(Sys.Date() )
        }
        genomes <- apply(genomes, 2, cumsum)
        ystart <- 0
        #start log scale at 1
        if (!log == "") {
            ystart <- 1
        }
        if (any(is.na(ylim))) {
            ylim <- c(ystart, max(genomes))
        }
        plot(as.Date(rownames(genomes)), 1:nrow(genomes), type = "n", 
            ylim = ylim, xlab = xlab, ylab = ylab, las = las, 
            log = log, ...)
        ## order legend by decreasing order of max (or mean?) number in column?
        if (lsort) {
            n <- sort(apply(genomes, 2, max, na.rm = TRUE), index.return = TRUE, 
                decreasing = TRUE)$ix
        }else {
            n <- 1:ncol(genomes)
        }
        ## sort groups high to low
        y <- sort(apply(genomes, 2, max), index.return = TRUE, 
            decreasing = TRUE)
         ## Loop through groups
        for (i in 1:ncol(genomes)) {
            lines(as.Date(rownames(genomes)), genomes[, n[i]], 
                lty = lty[i], lwd = lwd, col = col[i], type = type, 
                ...)
        }
        legend(legend[1], legend[2], colnames(genomes)[n], lty = lty, 
            col = col, bty = lbty, ncol = lcol, title = ltitle, inset = inset,
            cex = lcex, lwd = lwd)
    }
}




  
