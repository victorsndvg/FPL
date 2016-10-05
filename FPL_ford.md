Project: FPL
Author: Víctor Sande Veiga
Author_description: Scientific software developer
github: https://github.com/victorsndvg
website: https://github.com/victorsndvg
email: victorsv@gmail.com
Summary: A modern Fortran dictionary
project_github: https://github.com/victorsndvg/FPL
project_download: https://github.com/victorsndvg/FPL/releases
Date: October 5, 2016
blank-value: 
docmark: <
search: true
preprocess: true
source: true
graph: true
print_creation_date: true
fpp_extensions: f90
                i90
macro: DEBUG
       memcheck
       ENABLE_MPI
       ENABLE_HDF5
src_dir: ./src
output_dir: ./docs
exclude: sort.f90
exclude_dir: ./src/include
md_extensions: markdown.extensions.toc(anchorlink=True)
               markdown.extensions.extra
               markdown.extensions.footnotes


{!README.md!}
