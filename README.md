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

R has the capacity to install packages directly from
GitHub. In R:

`# the following step is only required if you don't have the 'devtools' package installed yet`

`install.packages("devtools")`

`# the following steps are always required when launching R`

`require(devtools)`

`devtools::install_github("NCI-CGR/cgrtelomeres")`

`require(cgrtelomeres)`

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
 * 03 February 2021: migrated to NCI-CGR GitHub from GitLab.

 * 09 October 2020: it minimally produces the output
 of Data/Analysis but it's not pretty. Plots and testing not implemented.
