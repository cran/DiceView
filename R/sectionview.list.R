sectionview.list <- function(model,
                             center = NULL,
                             npoints = 100,
                             col_points = "red",
                             col_surf = "blue",
                             bg_blend = 5,
                             mfrow = NULL,
                             Xname = NULL,
                             yname = NULL,
                             Xscale = 1,
                             yscale = 1,
                             ylim = NULL,
                             title = NULL,
                             ...) {

  D <- length(model$data$X)
  
  if (is.null(center)) {
    if (D != 1) stop("Section center in 'section' required for >1-D model.")
  }

  if (is.null(mfrow)) {
    nc <- round(sqrt(D))
    nl <- ceiling(D/nc)
    mfrow <- c(nc,nl)
  }
  
  if (mfrow[1] != 1 || mfrow[2] != 1)
    par(mfrow=mfrow)

  # apply scaling factor
  X_doe <- Xscale * model$data$X
  n <- dim(X_doe)[1]
  y_doe <- yscale * model$data$Y
  
  ## find limits: 'rx' is matrix with min in row 1 and max in row 2
  rx <- apply(X_doe, 2, range)
  rownames(rx) <- c("min", "max") 
  drx <- rx["max", ] - rx["min", ]
  
  if (is.null(ylim)) {
    ymin <- min(y_doe)
    ymax <- max(y_doe)
    ylim <- c(ymin, ymax)
  }
  
  # define X & y labels
  if (is.null(yname)) yname <- names(y_doe)
  if (is.null(yname)) yname <-  "y"
  if (is.null(Xname)) Xname <- names(X_doe)
  if (is.null(Xname)) Xname <- paste(sep = "", "X", 1:D)

  ## try to find a good formatted value 'fcenter' for 'center'
  fcenter <- tryFormat(x = center, drx = drx)
  
  for (d in 1:D) {

    xdmin <- rx["min", d]
    xdmax <- rx["max", d]
    xd <- seq(from = xdmin, to = xdmax, length.out = npoints)
    x <- data.frame(t(matrix(as.numeric(center), nrow = D , ncol = npoints)))
    names(x) <- names(center)
    x[ , d] <- xd
    y_mean <- array(0,npoints)
    
    for (i in 1:npoints) {
      y <- modelPredict(model, newdata = as.array(x[i,]))
      y_mean[i] <- yscale * y
    }

    if (is.null(title)){
      if (D>1) {
        title_d <- paste(collapse = ", ", paste(Xname[-d], '=', fcenter[-d]))
      } else {
        title_d <- paste(collapse = "~", yname, Xname[d])
      }
    } else {
      title_d <- title
    }

    plot(xd, y_mean,
         xlab = Xname[d], ylab = yname,
         ylim = ylim, main = title_d,
         type = "l",
         col = col_surf,
         ...)
       
    n <- dim(X_doe)[1]
    
    if (D>1) {

      xrel <- scale(x = as.matrix(X_doe),
                    center = center,
                    scale = rx["max", ] - rx["min", ])
      
      alpha <- apply(X = xrel[ , -d, drop = FALSE],
                     MARGIN = 1,
                     FUN = function(x) (1 - (sqrt(sum(x^2)/D)))^bg_blend)
      
      ##   for (i in 1:n) {
      ##         xrel = data.frame(((X_doe[i,] - center) / (xmax - xmin)))
      ##         xrel[d] <- NULL
      ##         alpha[i] = (1 - sqrt(sum(xrel^2)/(D))) ^ bg_blend
      ##       }
    } else {
      alpha <- rep(1, n)
    }

    ## modif YD : col
    col1 <- fade(color = col_points, alpha = alpha)
    points(X_doe[ , d], y_doe,
           col = col1,
           ## col = rgb(1, 1-alpha, 1-alpha, alpha),
           pch = 20)

  }

}
