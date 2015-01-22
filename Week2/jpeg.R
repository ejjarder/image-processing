require(bmp)
require(dtt)

block.size <- 8

q.base <- matrix(c(
    16, 11, 10, 16, 24, 40, 51, 61, 
    12, 12, 14, 19, 26, 58, 60, 55, 
    14, 13, 16, 24, 40, 57, 69, 56, 
    14, 17, 22, 29, 51, 87, 80, 62, 
    18, 22, 37, 56, 68, 109, 103, 77, 
    24, 35, 55, 64, 81, 104, 113, 92, 
    49, 64, 78, 87, 103, 121, 120, 101,
    72, 92, 95, 98, 112, 100, 103, 99),
    ncol = 8, byrow = TRUE)

get.blocks <- function(image.data, blocks.height, blocks.width) {
    blocks <- array(0, c(blocks.height, blocks.width, block.size, block.size))
    
    for (block.row in 0:(blocks.height - 1)) {
        for (block.col in 0:(blocks.width - 1)) {
            rows <- seq(block.row * block.size + 1,
                        length.out = block.size)
            cols <- seq(block.col * block.size + 1,
                        length.out = block.size)
            blocks[block.row + 1, block.col + 1,,] <- image.data[rows, cols]
        }
    }
    
    blocks
}

blocks.to.image <- function(blocks, blocks.height, blocks.width) {
    image.height <- blocks.height * block.size
    image.width <- blocks.width * block.size
    
    image.data <- array(0, c(image.height, image.width))
    
    for (block.row in 0:(blocks.height - 1)) {
        for (block.col in 0:(blocks.width - 1)) {
            rows <- seq(block.row * block.size + 1,
                        length.out = block.size)
            cols <- seq(block.col * block.size + 1,
                        length.out = block.size)
            image.data[rows, cols] <- blocks[block.row + 1, block.col + 1, , ]
        }
    }
    
    image.data
}

convert.to.jpeg <- function(image.file.name, q.levels = 1) {
    image.data <- read.bmp(image.file.name)[,,1] # use only one channel
    
    image.height <- nrow(image.data)
    image.width <- ncol(image.data)
    
    blocks.height <- ceiling(image.height / block.size)
    blocks.width <- ceiling(image.width / block.size)
    
    blocks <- get.blocks(image.data, blocks.height, blocks.width)
    
    q.matrix <- q.base * q.levels
    
    for (block.row in 1:blocks.height) {
        for (block.col in 1:blocks.width) {
            dtt.results <- dtt(blocks[block.row, block.col, , ], type = 'dct')
            dtt.results <- floor(dtt.results / q.matrix) * q.matrix
            blocks[block.row, block.col, , ] <- dtt.results
        }
    }
    
    blocks
}

convert.from.jpeg <- function(jpeg.data) {
    blocks.dim <- dim(jpeg.data)
    
    blocks.height <- blocks.dim[1]
    blocks.width <- blocks.dim[2]
    
    for (block.row in 1:blocks.height) {
        for (block.col in 1:blocks.width) {
            dtt.results <- dtt(jpeg.data[block.row, block.col, , ],
                               type = 'dct', inverted = TRUE)
            jpeg.data[block.row, block.col, , ] <- dtt.results
        }
    }
    
    blocks.to.image(jpeg.data, blocks.height, blocks.width)
}

test.blocks <- function(blocks, image.data) {
    blocks.dim <- dim(blocks)
    
    blocks.height <- blocks.dim[1]
    blocks.width <- blocks.dim[2]
    
    valid <- TRUE
    
    for (block.row in 0:(blocks.height - 1)) {
        for (block.col in 0:(blocks.width - 1)) {
            rows <- seq(block.row * block.size + 1,
                        length.out = block.size)
            cols <- seq(block.col * block.size + 1,
                        length.out = block.size)
            equal.values <- sum(image.data[rows, cols] ==
                                    blocks[block.row + 1, block.col + 1,,])
            if (equal.values != block.size * block.size) {
                valid <- FALSE
                break
            }
        }
        if (!valid) {
            break
        }
    }
    
    valid
}

draw.bmp <- function(bmp.data) {
    image(t(bmp.data)[,nrow(bmp.data):1])
}

draw.from.blocks <- function(blocks) {
    blocks.dim <- dim(blocks)
    blocks.height <- blocks.dim[1]
    blocks.width <- blocks.dim[2]
    
    draw.bmp(blocks.to.image(blocks, blocks.height, blocks.width))
}