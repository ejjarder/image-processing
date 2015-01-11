require(jpeg)

reduce.image <- function(input.file, output.file, block.size) {
    image.data <- readJPEG(input.file)
    
    height <- nrow(image.data)
    width <- ncol(image.data)
    
    start.row <- ceiling(block.size / 2)
    copy.rows <- floor(height / block.size)
    row.indices <- seq(start.row, start.row + copy.rows * block.size,
                       block.size)
    
    start.col <- ceiling(block.size / 2)
    copy.cols <- floor(width / block.size)
    col.indices <- seq(start.col, start.col + copy.cols * block.size,
                       block.size)
    
    image.copy <- array(0, c(copy.rows, copy.cols, 3))
    
    half.block <- floor(block.size / 2)
    
    pix.count <- block.size * block.size
    
    for (row in 1:copy.rows) {
        for (col in 1:copy.cols) {
            row.index <- row.indices[row]
            col.index <- col.indices[col]
            
            for (row.off in -half.block:half.block) {
                for (col.off in -half.block:half.block) {
                    image.copy[row, col, ] <- image.copy[row, col, ] +
                        image.data[row.index + row.off,
                                   col.index + col.off, ]
                }
            }
            
            image.copy[row, col, ] <- image.copy[row, col, ] / pix.count
        }
    }
    
    writeJPEG(image.copy, output.file)
}

if (length(args) == 3) {
    reduce.image(args[1], args[2], args[3])
} else {
    print(paste('Usage: RScript.exe reduce.image.R',
                '<input image> <output image> <block size>'))
}