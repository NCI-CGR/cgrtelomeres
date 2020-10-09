## cgrtelomeres: automated variant of CGR qPCR telomere analysis

### Overview

This is an R package designed to emulate the current series of tools
and manual steps used by lab personnel to analyze their telomere qPCR
data. This project was undertaken at the request of Casey Dagnall.

There are currently two structured milestones for this project: 
the first, a minimal working state, will replicate the data output
from the current analysis protocol (minus some Microsoft Office-specific
things that aren't going to be in the final product). This is primarily
to test that I have a complete understanding of the calculations
and manipulations being applied, especially some apparent manual steps
being applied to an excel template. The project is currently coming 
up on this milestone, effective 09 October 2020.

The second milestone, which is likely the final product, will take
the computed data and use some form of Rmd processing to make everything
into a pretty and informative report.

### Installation and Execution

R theoretically has the capacity to install packages directly from
gitlab. However, due to what appear to be API version issues, this 
is not currently working for me from CGR's gitlab. Hopefully
migration to another host will resolve this issue in the coming months.

For the moment, the following should work (from the NIH network):

`git clone --branch default http://10.133.130.114/palmercd/cgrtelomeres.git`

This will clone the project into a subdirectory of your current
directory called `cgrtelomeres`. You can then install this package
from within R with the following commands:

`# depending on your system: install.packages("devtools")
require(devtools)
install("cgrtelomeres")
require(cgrtelomeres)`

The primary entry point for the software is `cgrtelomeres::process.experiment`.
You can get somewhat useful help documentation for this function
in the usual R manner: `?cgrtelomeres::process.experiment`. An example
run command might be:

`cgrtelomeres::process.experiment("/path/to/Examples for Bioinformatics", "output_dir", "GP0317-TL7")`

I have not yet worked out how I want to deal with distribution of test
data, so for the moment applicable parties can use the test dataset from
the original email from Casey Dagnall. The input directory is not written to,
so it's safe to use in place; just make sure `"output_dir"` doesn't point
to anything important with existing workflow output.

### Version History

 * 09 October 2020 (current version, not frozen): it minimally produces the output
 of Data/Analysis but it's not pretty. Plots and testing not implemented.
