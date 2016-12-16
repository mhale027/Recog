extract.patch <- function(feat, size, id, im.train = im.train) {
      
      coord_x <- paste(feat, "x", sep="_")
      coord_y <- paste(feat, "y", sep="_")
      
      im  <- matrix(data = im.train[id,], nrow=96, ncol=96)
      
      x   <- d.train[id, coord_x]
      y   <- d.train[id, coord_y]
      x1  <- (x-size)
      x2  <- (x+size)
      y1  <- (y-size)
      y2  <- (y+size)
      if ( (!is.na(x)) && (!is.na(y)) && (x1>=1) && (x2<=96) && (y1>=1) && (y2<=96) ){
            mat <- as.vector(im[x1:x2, y1:y2])
            # eval(image(1:nrow(mat), 1:ncol(mat), mat, col=gray((0:255)/255)), parent.frame())
            mat
      } else { NULL }
}
