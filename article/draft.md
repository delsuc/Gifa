---
title: Reproducing "*Gifa V.4:* A complete package for NMR data set processing"
author: M-A.Delsuc
templa: https://github.com/rescience/template
refe: https://github.com/ReScience/ten-years/issues/1#issuecomment-543788418
---

# Introduction
I present here the reproduction of the 1996 article "*Gifa V.4*: A complete package for NMR data set processing" [@Pons_1996].

The *Gifa* project is a carrier long effort.
It was started in 1987 as a way to gather various codes and ideas I had developed to handled and process  NMR datasets in the early days of 2D and 3D NMR[^1].
Since the beginning, *Gifa* has been built over a careful memory management, a tuned data organisation, and a large set of elementary functions meant to be pipelined. 
The version 4 published in 1996 was a major improvement over the previous published version (1988) as it brought a complete scripting language, a complete graphic user interface based on the Motif library, and a database management system.
This program was not open-source at that time, but nevertheless profited from a large number of users, and some are still using it nowadays, despite the difficulty to maintain it.
This was an additional motivation to try to renew this program and to make it freely available on a code forge.

[^1]: I started doing NMR as early as 1979, for my French DEA project.
2D NMR was in its infancy (the initial paper from R.Ernst group is dated 1976).
NMR spectrometers, computers, and programs were badly prepared for this new approach, where complex pulse sequences had to be prepared and launched in automation, generated large datasets, which had to be transformed, visualized and quantified while not fitting in the 64kB central memory, and even barely on the removable 14 inch disk pack that stored 2.5MB of data !

When I entered the *ten years challenge*, I first thought of reproducing a latter avatar of the same project, a 2007 program implementation called at that time NPK [@Tramesel_2007]. 
The NPK version was an effort to move away from the monolithic environment of *Gifa* which was nearly impossible to port to MacOs or Windows.
This had lead us to move the graphic from X/Motif to Java/Swing and the scripting language to python 2.1 (thanks to then excellent `jython` package recently released then).
However the mathematical core was unchanged and still written in F[ortran]{.smallcaps}77 with strong 32 bit limitations. 
Because of this limitation, NPK was abandoned, and a much extended program has been developed in python since then [@SPIKE].

After more consideration, I realized that reproducing a program written as a mix of F[ortran]{.smallcaps}77 , Java, and Python 2.1 was not worth the effort, in particular considering my poor knowledge of Java and the fact that nobody was using this program anymore.
This is why I decided to fall back on the 1996 version of the same project. 

*Gifa* was started in the eighties, when the first 32\ bits architectures (VMS) were coexisting with older 16\ bits ones (PDP-11).
It was also a time were 64MB of memory was plenty, and the challenge was to fit the 2D NMR datasets in memory, in contrast with other programs at that time which were doing lengthy processes by swapping the data on disk.
*Gifa* is thus based on a carefully carved memory organisation, built on many F[ortran]{.smallcaps}77 `COMMON` and `EQUIVALENCE` structures, which allowed to use part of the memory for certain actions, but also to use the whole memory when needed.
Due to F[ortran]{.smallcaps}77 limitations, this allocation is purely static, and set at compile time - for this reason, several version of *Gifa* were distributed, ranging from a very small buffer (total 1MB), to the largest one (256MB).

*Gifa* is a data analysis program meant for NMR.
In its 1996 avatar, it presents a complete set of fundamental mathematical operations (Fourier transforms, data-set arithmetics, model fitting, ...) and some advanced tools (Linear Prediction tools, Maximum Entropy optimisation, DOSY processing by Inverse Laplace transform...).
It is able to process 1D, 2D and in a less optimized manner 3D data-sets.
It comes with a complete recursive script language with variable scoping, recursion, arrays, dictionaries, file I/O, and some database capabilities.
This macro language, complemented with a set of graphic primitives is used to build a complete dynamic GUI, which covers basic and advanced operations, an assignment tool, and a relaxation module.
As mentioned above, the memory model is very static and monolithic to  allow an optimal use of the resources. 
This organisation insures little dependencies with respect to external tools, and a good control of the development. 

# Recovering the software
The source files of the historical *Gifa* was still stored somehow on my current hard drive and has been through two laboratory relocations (from Montpellier to Lyon and then to Strasbourg), many computer upgrades (with migration of the accounts), and one OS change (from MacOsX to Linux Ubuntu).

No version control system was used at that time, however I could find several versions of the source in in tar archives from version 4.31  dated Aug 2000, to version 4.5 dated Mars 2004.
In the absence of the article version dated 1996, I decided to use these versions, and moved them to [`github.com`](https://github.com/delsuc/Gifa) as successive commits.
The code is about 54.000 lines of F[ortran]{.smallcaps}77 , 18.000 lines of C and 27.000 lines of the macro language.

The second step is to remember the file organisation and how to build the program.
As the program was not open-source, this was not described in the official documentation.
A central `make` program controls the building process - the `Makefile` contains some documentation, and guided me in the process.

## Steps by steps
All the development work was performed in a virtual machine running Ubuntu 16.04 with 4GB of memory hosted by a MacBookPro with a Intel Core i5-7360U processor at 2.30GHz, running Ubuntu 19.10.

At the beginning of this project, I had three main concerns: i) the 32 bit organisation of the program; ii) the maintainability of the production pipeline; and iii) external libraries, in particular the X/Motif library used for display.

### a 32 bits program
As a consequence of the memory management, the 32 bit organisation is intrinsic to the program.
Not sure on how to handle this, I decided to work in a Virtual Machine (VM) based on the last Ubuntu version which proposes a 32bit version: 16.04 LTS.
Despite this environment being more than 20 years posterior to the one in which *Gifa* was developed, everything was built without any particular problems.

### The production pipeline

Thanks to the already long development history, the program had been ported to VMS and many Unix flavours (Linux, SunOs, HP-UX, IRIX, IRIX64, AIX).
This was possible through the use of a home-built production pipeline able to adapt the variations of each architecture,
using a mixture of `make` and `perl` scripts and the `f2c` F[ortran]{.smallcaps}77 to C translator [@f2c].
Fortran source code first goes through a home-made F[ortran]{.smallcaps}77 code preprocessor which permits to include files, conditional code, and allows to tune and adapt the produced code.
Fortran code is then translated to C by `f2c` and finally compiled with `gcc`.
This scheme simplifies the choice of the F[ortran]{.smallcaps}77 compiler and
 allows additional features, such as recursivity (not allowed in F[ortran]{.smallcaps}77 ) and simple link with C code.

This set-up, which was optimized to allow a simple porting to a new architecture, has helped a lot in the reproduction work.
Only a few small modifications was needed to adapt the code to a recent version of Linux, running in a Virtual environment,
as the basic languages (`make`, `perl`, and `C`) on which everything is based have not significantly changed.

### External  libraries
The program uses very few libraries apart for X/Motif graphic user interface, and the `ndbm` database engine. 
Most mathematical algorithms (Fourier transform, Brent or Levenberg-Marquardt optimisers, ...) are natively coded, inspired from the excellent *Numerical Recipes* book [REF].
Linear algebra operations, uses the LINPACK [@linpack] routines, which sources were integrated in the code itself.
Basic vector operations are constructed from a home-made library ( `vect.for` ) implementing 60+ basic operations, in an atomic manner, easy to optimize for the compiler.

The X/Motif library had been a major hurdle during the *Gifa* early development in particular the porting of various versions available, and I was fearing it would still be a problem.
It turned out to be extremely simple, just an installation from the repository.


### Actual changes
**libraries:**
I had to install  X/Modif from the repository  `libmotif-dev` and the `libxext-dev`; readline from `libreadline6-dev`.
The `ndbm` database engine is not distributed, but the GNU foundation proposes `gdbm` which has a `ndbm` compatibility mode, 
activated with a flag at link time `-lgdbm_compat`.

**Linux and environment:**
A few constructs were obsolete or not adapted to a modern system and had to be adapted.

- At that time, `perl` was in version 4 (`perl` 5 was released in 1994 and not widely adopted then).
The Ubuntu chosen release used `perl` 5.22 and a few simple syntaxic changes were needed to have a working pipeline.
- the timing code was using the `CLK_TCK` macro which seems to be obsolete, this was modified to use `sysconf (_SC_CLK_TCK) `;
- temporary files were created on `/usr/tmp`.
 I simply made a link to `/tmp` to solve this problem which no modification in the code;
- I struggled a little with the link line - the simpler appeared to be the better !
- The HTML documentation is created with a `perl` script, 
HTML comments were using `<!-- comments >` which is not understood any-more; had to be changed to `<!-- comments -->`
- HTML display was using Netscape or Mozilla, this was changed to Firefox
<!--
- Xm => apt-get install libmotif-dev
- -lXext => apt-get install libxext-dev
- ndbm.h is not found => apt-get install libgdbm-dev
- apt-get install libreadline6-dev
-->


**licensing:**
At the time of the publication, the program was not open-source, and had to be activated with a licence key distributed upon request.
A C code was checking the validty of the key at launch time,
this code has been deactivated, and a CeCILL 2.1 license was asspciated to the code.

# Reproduction
After performing the modifications above, the program runs directly with no particular problem,
and the internal test suit is validated with no error.
The display system is up and the graphic User Interface is correctly functioning.
Figures 1 and 2a in the original study, which present the macro language are not reproduced here.
Listing 1 reproduce Figure 2b, and Figures 1 and 2 reproduce Figures  2c and 3 from the original study.
The data-set which is shown in the original work could not be recovered.
In  consequence, I chose to present a TOCSY spectrum of Artemisine, obtained at 700MHz in a recent study [ARTE]

```{.bash}
Listing 1

    ; This macro creates a form box which permits to modify the
    ; parameters of the contour-mode display window.
    ; The user can - open / close the contour-mode display window
    ;              - choose to display positive / negative / both sides
    ;              - change the number and spacing of contour levels.
    ;Preset parameters:
    if ($CDISP2D==1) then     ;$cdisp2d describes graphic states.
        set cdef = on
    else
        set cdef = off
    endif
    if ($SIGN==1) then        ;Describes which levels are displayed
        set pndef = positive  ; (positive, negative, both)
    eisif ($SlGN == 0) then
        set pndef = both
    else
        set pndef = negative
    endif
    ;Build the form
    formbox "Display control" \
        "dispcont_doit $cont $posneg $lev Slog" \   ;Callback
        "Contour Display (cdisp2d)" \
            enum "on,off'' cont $cdef \             ;Define fields
        "Display mode (sign)" \
            enum "positive,negative,both" posneg $pndef \
        "Number of levels (level)" int lev $LEVEL \
        "Level Algo : (loga)" real log $LOGA \
        "(x=1 : equidistant; x>1 : by power of x)" message \  
        *             ; * is end of list
    exit
```

![The form resulting from the execution of the macro in Listing 1 is a static graphic box with an action button 'Apply', with which the macro command `dispcontdoit` (*not show*) is associated.](figure2c.png){width=50%}

Slight differences can be observed between the original figures and the current ones. 
Most of the differences come to simple improvements in the program after the initial release, for instance the button 'Cancel' changed to 'Close', or the reorganisation, and extension of the user menus.


![The default graphic environment as invoked during start-up of Gifa. All the basic commands can be run from this graphic environment.
The control box called `Zoom Box` permits interaction with the graphic window. The data are currently displayed in a contour plot 2D window
and in a 1D window. The small vignette seen in the `Zoom Box` is a view of the complete data set held into the 2D buffer, the square in it indicates
the current zoom window.](figure3.png)

The benchmark programs proposed in the original publication were run, the results are shown in Table 1. 

Processing  Time (s)    SUN   IBM   SGI    HP    This study
---------------------  ----- ----- ----- ------ ------------
2D                      17.5  12.6  10.5  7.5      0.05
3D                      1064  329   193   172      1.10

Table: Different processing times observed using the *Gifa* program[^table]

[^table]: The following machines were used: SUN: Sparc 10-30 with the SunOS
4.1.3 operating system; IBM: RS6000-560 with the AIX V3 operating
system; SGI: Silicon Graphics R4000 at 175 MHz, with the IRIX 5.3
operating system; HP: Hewlett-Packard 735 at 99 MHz, with the HP-
UX 9.01 operating system; this study: a MacBookPro with a Intel Core i5 processor at 2.30GHz. 
Two processing protocols were used for
these benchmarks. 2D is an in-memory processing of a $512 \times 2048$ 2D
data set, consisting of an exponential apodisation in $t_2$, followed by
real Fourier transform, phase correction, reduction to real type and
five-point spline baseline correction, and a sine-bell apodisation in $t_1$,
followed by one level of zero-filling, real Fourier transform and phase
correction. 3D is a $256 \times 64 \times 512$ 3D NMR data processing with sine-bell apodisation, zero-filling, complex Fourier transform, phasing and
reduction to real type in all three dimensions, a linear baseline correction being applied on the F3 axis.

<!-- [^bench]:
on a 
to be compared with:
1'30" on a SUN Sparc 330,
47" on a SiliconGraphics 4D/25 (circa 1988);
34" DEC Station 5000-200 (circa 1990);
17" HP 725/50 (circa 1991);
16" on a SiliconGraphics Indigo R4000 (circa 1991);
12.6" IBM Risc6000 560 (circa 1992);
3.2" SUN/Entreprise CPU UltraSparcII 250MHz (circa 1997);
2.49" on a  SiliconGraphics Octane R10000 at 250MHz (circa 1998);
2.5" PowerBook G3 400MHz running MacOsX (circa 1999);
1.31" Pentium III 800MHz Linux RH 6.2 (circa 1999);
0.940" on a Pentium IV at 1.5GHz runnning RedHat 7.1 (circa 2000);
 -->

At this stage, the reproduction is considered as successful, even though a complete check of all functionalities has not been fully performed.

# Conclusion
The *Gifa* program was successfully reproduced, relatively easily for a software of this size, developed about 25 years ago.
The reproduction was performed in a simple Virtual Machine, running the 32bit version of Ubuntu 16.04, which should be supported until April 2024

The ease of porting is mostly coming from the use of standard tools and programming languages, still well supported today (`f2c`, `perl`, `make`, `gzip`, `tar`, ...).
The program is one large code with very little external dependencies, as it internally contains all mathematical routines, an internal macro language, and its own GUI system.
The only external libraries which had to be installed are the X/Motif graphic and the `gdbm` the GNU simple database engine, both being still supported under Linux.
Also, the diversity of the Unix environment in the nineties, imposed the development a robust production pipeline easy to modify and to tune, which as a valuable tool in the reproduction.
The needed corrections were mostly finding and installing the correct software packet or library.
Some tools and some structures had to be modified because they were using obsolete constructs ( Netscape and HTML comments for instance !).

Reviving the *Gifa* program was an interesting experience, it reminded me the qualities - and flaws - of this program, and the efficiency of a monolithic program, with a tight memory management and an algorithmics optimized to the limited resources available 20 years ago.
Despite its limitations, such a program can still have many application today, for instance embedded in a larger environment.

This work also showed that monolithic programs, written with simple standard tools, with little dependencies on the environment, are quite easy to maintained and can see their life extended beyond 20



# Bibliography
@misc{f2c,
    title = {{FORTRAN to C convertor}},
    url = {https://www.netlib.org/f2c/}
}

@misc{linpack,
    title = {{LINPACK LINear algebra package}},
    url = {https://www.netlib.org/linpack/}
}

@article{Pons_1996,
    doi = {10.1007/bf00228146},
    url = {https://doi.org/10.1007%2Fbf00228146},
    year = 1996,
    month = {dec},
    publisher = {Springer Science and Business Media {LLC}},
    volume = {8},
    number = {4},
    author = {Jean-Luc Pons and Th{\`{e}}r{\`{e}}seE. Malliavin and MarcA. Delsuc},
    title = {Gifa V. 4: A complete package for {NMR} data set processing},
    journal = {Journal of Biomolecular {NMR}}
}

@article{Tramesel_2007,
    doi = {10.1016/j.jmr.2007.05.023},
    url = {https://doi.org/10.1016%2Fj.jmr.2007.05.023},
    year = 2007,
    month = {sep},
    publisher = {Elsevier {BV}},
    volume = {188},
    number = {1},
    pages = {56--67},
    author = {Dominique Tramesel and Vincent Catherinot and Marc-Andr{\'{e}} Delsuc},
    title = {Modeling of {NMR} processing, toward efficient unattended processing of {NMR} experiments},
    journal = {Journal of Magnetic Resonance}
}

@article{Spike2016,
    author = {Chiron, Lionel and Coutouly, Marie-Aude and Starck, Jean-Philippe and Rolando, Christian and Delsuc, Marc-Andr{\'e}},
    eprint = {1608.06777},
    eprintclass = {physics.chem-ph},
    eprinttype = {arxiv},
    journal = {arXiv},
    pages = {1608.06777},
    title = {{SPIKE} a Processing Software dedicated to Fourier Spectroscopies},
    year = {2016}
}

@book{press96,
    author = {Press, W.H. and Teukolsky, S.A. and Vetterling, W.T. and Flannery, B.P.},
    publisher = {Cambridge University Press},
    title = {Numerical recipes in Fortran 77},
    year = {1996}
}

@article{Margueritte_2018,
    doi = {10.1002/mrc.4683},
    url = {https://doi.org/10.1002%2Fmrc.4683},
    year = 2018,
    month = {jan},
    publisher = {Wiley},
    volume = {56},
    number = {6},
    pages = {469--479},
    author = {Laure Margueritte and Petar Markov and Lionel Chiron and Jean-Philippe Starck and Catherine Vonthron-S{\'{e}}n{\'{e}}cheau and M{\'{e}}lanie Bourjot and Marc-Andr{\'{e}} Delsuc},
    title = {Automatic differential analysis of {NMR} experiments in complex samples},
    journal = {Magnetic Resonance in Chemistry}
}

@misc{CECIlL,
    title = {{CeCILL 2.1 license}},
    url = {https://cecill.info/index.en.html},
    year = 2013
}
