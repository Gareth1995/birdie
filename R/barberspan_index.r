# calculating biodiversity index for the barberspan region

library(devtools)
library(rlpi)

# Install from main ZSL repository online
#install_github("Zoological-Society-of-London/rlpi", dependencies=TRUE)

# Get example data from package
# Copy zipped data to local directory 
file.copy(from=system.file("extdata", "example_data.zip", package = "rlpi"), to=getwd())

# Extract data, this will create a directory of terrestrial LPI data to construct a terrestrial index from.
unzip("example_data.zip")

# Make a Nearctic LPI 

# Default gives 100 bootstraps (this takes a couple of minutes to run on a 2014 MacBook)
Nearc_lpi <- LPIMain("example_data/terrestrial_class_nearctic_infile.txt", use_weightings = 1, VERBOSE=FALSE, show_progress=FALSE)



