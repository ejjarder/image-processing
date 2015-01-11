require(jpeg)

rotate.image <- function(input.file, output.file, angle) {
    image.data <- readJPEG(input.file)
    
    height <- nrow(image.data)
    width <- ncol(image.data)
    
    half.height <- height / 2
    half.width <- width / 2
    
    image.copy <- array(0, c(height, width, 3))
    
    rad.angle <- angle * pi / 180
    
    # image data is an row x column x 3 array
    for (row in -half.height:half.height) {
        for (col in -half.width:half.width) {
            source.row <- row + half.height
            source.col <- col + half.width
            
            if (source.row >= 1 && source.row <= height &&
                    source.col >= 1 && source.col <= width) {
                new.row <- floor(-col * sin(rad.angle) + row * cos(rad.angle) +
                                     half.height)
                new.col <- floor(col * cos(rad.angle) + row * sin(rad.angle) +
                                     half.width)
                
                if (new.row >= 1 && new.row <= height &&
                    new.col >= 1 && new.col <= width) {
                    image.copy[new.row, new.col, ] <-
                        image.data[source.row, source.col , ]
                }
            }
        }
    }
    
    writeJPEG(image.copy, output.file)
}

if (length(args) == 3) {
    rotate.image(args[1], args[2], args[3])
} else {
    print(paste('Usage: RScript.exe rotate.image.R',
                '<input image> <output image> <angle>'))
}
