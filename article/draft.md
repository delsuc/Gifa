---
title: Reproducing "Modelling of NMR processing, toward efficient unattended processing of NMR experiments"
author: M-A.Delsuc
---

# Introduction
When I entered the ten years challenge, I first thought of reproducing a 2007 program implementing a complete NMR processing suite.
After more consideration, I realized that reproducing a program written as a mix of FORTRAN 77, Java, and Python 2.1 was not worth the effort.
So I decided to fall back on the 1996 version of the same program *Gifa* (even though the 2007 was named NPK). 

The *Gifa* project is a carrier long effort.
It was started in 1987 as a way to gather various codes and ideas I had developed to handled and process  NMR datasets in the early days of 2D and 3D NMR[^1].
Since the first versions, *Gifa* has been build over a careful memory management, a tuned data organisation, and a large set of elementary functions meant to be pipelined. 
The version 4, published in 1996 was a major improvement over the previous published version (1988) as it brought a rather complete scripting language, a Motif widget library, both allowing to build a complete graphic user interface.
This program was not open-source at that time, but nevertheless profited from a large number of users, and some are still using it nowadays, despite the difficulty to maintain it.
This was an additional motivation to try to renew this program and to make it freely available on a code forge.

[^1]: I started doing NMR as early as 1979, for my French DEA project.
2D NMR was in its infancy (the initial paper from R.Ernst group is dated in 1976).
NMR spectrometers, computers, and programs were badly prepared for this new approach, where complex pulse sequences had to be prepared and launched in automation, generated large datasets, which had to be transformed, visualized and quantified while not fitting in the 64kB central memory, and even barely on the removable 14 inch disk pack that stored 2.5MB of data !

# Recovering the software
Some source file of *Gifa* was still stored somehow on my current hard drive and has been through two laboratory relocations (from Montpellier to Lyon and then to Strasbourg), many computer upgrades (with migration of the accounts), and one OS change (from MacoS to Linux Ubuntu).

No version control system was used at that time. 
I could find several versions of the source in in tar archives from version 4.31  dated Aug 2000, to version 4.5 dated Mars 2004.
In the absence of the article version, I decided to use the latest version, and moved it to https://github.com/delsuc/Gifa 



# Conclusions
- make it simple ()
- thank to tgz and ascii
