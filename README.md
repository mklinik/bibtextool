# bibtextool

Various manipulations of bibtex .bib files.

## Installing

```
$ git clone https://github.com/mklinik/bibtextool.git
$ cd bibtextool
$ stack install
```


## Finding duplicate bib entries

Based on the Levensthein distance of the paper title.

```
$ stack run -- --find-dupes ~/radboud/thesis/klinik-phd-thesis/bibliography/computer_science.bib 
distance: 0
journals/dpd/AalstHKB03
[Quoted "Workflow Patterns"]
Aalst2003Patterns
[Quoted "Workflow Patterns"]

distance: 0
koopman2018task
[Quoted "A task-based dsl for microcomputers"]
DBLP:conf/cgo/KoopmanLP18
[Quoted "A Task-Based {DSL} for Microcomputers"]

distance: 0
conf/ppdp/PlasmeijerLMAK12
[Quoted "Task-oriented programming in a pure functional language"]
Plasmeijer2012
[Quoted "Task-oriented programming in a pure functional\n     language"]

distance: 0
DBLP:conf/sfp/StutterheimPA14
[Quoted "Tonic: An Infrastructure to Graphically Represent the Definition and\n               Behaviour of Tasks"]
Stutterheim2014
[Quoted "Tonic: An Infrastructure to Graphically Represent the\n     Definition and Behaviour of Tasks"]

distance: 0
conf/ifl/KlinikJP17
[Quoted "The Sky is the Limit: Analysing Resource Consumption Over Time Using Skylines"]
KlinikJP2017
[Quoted "The Sky is the Limit: Analysing Resource Consumption Over Time Using Skylines"]

distance: 0
KoopmanATP2002
[Quoted "Gast: Generic Automated Software Testing"]
Koopman2002
[Quoted "Gast: Generic Automated Software Testing"]

distance: 4
Nielson1996a
[Quoted "Polymorphic Subtyping for Effect Analysis: The Static\n     Semantics"]
Amtoft1997
[Quoted "Polymorphic Subtyping for Effect Analysis: The Dynamic\n     Semantics"]
```
