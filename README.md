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

### Installation Instructions

R theoretically has the capacity to install packages directly from
gitlab. However, due to what appear to be API version issues, this 
is not currently working for me from CGR's gitlab. Hopefully
migration to another host will resolve this issue in the coming months.
