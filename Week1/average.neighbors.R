require(jpeg)

average.neighbors <- function(input.file, output.file) {
    image.data <- readJPEG(input.file)
    
    height <- nrow(image.data)
    width <- ncol(image.data)
    
    image.copy <- array(0, c(height, width, 3))
    
    # image data is an row x column x 3 array
    for (row in 1:height) {
        for (col in 1:width) {
            elements.summed <- 0
            
            for (row.off in -1:1) {
                neighbor.row <- row + row.off
                if (neighbor.row >= 1 && neighbor.row <= height) {
                    for (col.off in -1:1) {
                        neighbor.col <- col + col.off
                        if (neighbor.col >= 1 && neighbor.col <= width) {
                            image.copy[row, col, ] <- image.copy[row, col, ] +
                                image.data[neighbor.row, neighbor.col, ]
                            elements.summed <- elements.summed + 1
                        }
                    }
                }
            }
            
            image.copy[row, col, ] <- image.copy[row, col, ] / elements.summed
        }
    }
    
    writeJPEG(image.copy, output.file)
}

if (length(args) == 2) {
    average.neighbors(args[1], args[2])
} else {
    print(paste('Usage: RScript.exe average.neighbors.R',
                '<input image> <output image>'))
}
