require(jpeg)

change.intensity <- function(input.file, output.file, levels) {
    # check if power of 2
    if (bitwAnd(levels, levels - 1) == 0 && levels >= 2) {
        factor <- 1 / (levels - 1)
        
        image.data <- readJPEG(input.file)
        image.data <- floor(image.data / factor) * factor
        writeJPEG(image.data, output.file)
    } else {
        print(sprintf('Levels is not a valid power of 2! Level input: %d',
                      levels))
    }
}

if (length(args) == 3) {
    change.intensity(args[1], args[2], args[3])
} else {
    print(paste('Usage: RScript.exe intensity.changer.R',
                        '<input image> <output image> <levels>'))
}
