# *Gifa* v4 program - an effort to revive the program.

This is a repository of the historic NMR processing program *Gifa v4* as published in:


Jean-Luc Pons, Thérèse E. Malliavin, Marc A. Delsuc Gifa V. 4: A complete package for NMR data set processing. 
*Journal of Biomolecular NMR* (1996), Volume **8**, Issue 4, pp 445–452 
doi: [10.1007/BF00228146](https://doi.org/10.1007/BF00228146)
 
**Open Access version:** http://www.delsuc.net/pdf/2007-Journal%20of%20Magnetic%20Resonance-Tramesel.pdf

It is also feature an [article](article/article.pdf) submitted as a contribution to the ["Ten Years Reproducibility Challenge"](https://github.com/ReScience/ten-years) run by [**ReScience C**](https://rescience.github.io/).


## Contents of this repository

The directory [`article`](article) contains the LaTeX source code and accessory files of the article submitted to **ReScience C**.

The directory [`code`](code) contains the code 

- [`com`](code/com) - various scripts used by the program while running
- [`com_devel`](code/com_devel) - various scripts used to build the program or the documentation
- [`doc`](code/doc) - the interactive documentation
- [`help`](code/help) - help files
- [`linux_obj`](code/linux_obj) - binary is built there
- [`macro`](code/macro) - the set of macros used by the program while running
- [`source`](code/source) - the complete source of the program
- [`test`](code/test) - the test suite
- [`util`](code/util) - utility programs - not used in this release
- [`Original_Distrib_Files`](code/Original_Distrib_Files) - Old files unused for this release

The directory [`data`](data) contain the test data used in the publication

The first commits are as follows:
- 1/ initial:  source files, macros, and tests of the 4.31 version - dated 1st Aug 2000 - the older source I could find (nearly 20 years later !)
- 2/ same set-up version 4.41 1 Jan 2002
- 3/ same set-up version 4.5  Mars 2004


# How to compile Gifa on your system
This is the way I did it.

## prepare the system
I created a virtual machine, (using Virtual Box) with a 10GB disk, 4GB memory and 16MB graphic memory. (that was a big machine back in 1996 !) and installed a fully updated Ubuntu 16.04 32bits on it.
Be careful not to use a blank or an extended ASCII char in the user name - Gifa cannot handle this.

Then with `apt-get` I install the following packages:

`sudo apt-get install xxx` with `xxx` being successively
`git`
`f2c`
`libmotif-dev`
`libreadline6-dev`
`libgdbm-dev`

## install and build Gifa
get the source
```
git clone https://github.com/delsuc/Gifa.git
```
go to the source folder
```
cd Gifa/code/source
```
create the depend_list, used by `Makefile` to compile everything
```
make depend
```
build the binary
```
make gifa
```
then install copies stuff in `/usr/local` and `/usr/local/bin`
```
make install
```

`make test` does not work because of some problem with dealing with the standard input 

but...
```
cd ../test
gifa
```
... works.

## duplicating the examples
The two *Gifa* macros `fig2a.g` and `figure3.g`, located in the [`article`](article) directory reproduce the Figure 2 and 3 from the original article.
( Note that in `fig2a.g` the action button produces an error because the `dispcont_doit` is not present. )

Then the timing presented in Table 1 are obtained as follows:

- for the 2D processing, launch *Gifa* goto the [`code/test`](code/test) directory, and type `benchmark`.
This executes the standard benchmarking macro, which has been present since the very beginning of the development,
and contains timing for all machines on which it has been tested.

- for the 3D, launch *Gifa* goto the [`article`](article) directory, and type
```
timer 1
bench3D.g
```

*et Voilà!*

[<img src="https://i.creativecommons.org/l/by/4.0/88x31.png">](http://creativecommons.org/licenses/by/4.0/)  
This work is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/)
