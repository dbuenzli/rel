rel — Relational database programming for OCaml
===============================================
%%VERSION%%

Rel is a library for programming with relational databases in OCaml.
It provides:

- Typed combinators to describe database rows and their representation
  as custom OCaml values.
- Typed combinators to describe tables and indices. These descriptions
  can be translated to SQL data definitions or, conversly, generated for
  existing databases (external schema definition).
- Automated schema changes via schema diffing.
- Raw SQL statements typing and SQL generation helpers (parametric on SQL
  dialect).
- Query language embedded in *plain* OCaml. Queries are typed,
  composable and compile to a single, flat, SQL query (experimental).
- Support for using SQLite3 databases.

Rel is distributed under the ISC license. The basic library has no
dependencies. 

The SQLite support depends on the C SQLite3 library (at least
v3.26.2).

Homepage: https://erratique.ch/software/rel

# Installation

rel can be installed with `opam`:

    opam install rel
    opam install rel conf-sqlite3 # with SQLite support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online][doc] or via `odig doc rel`.

Questions are welcome but better reled on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/rel/doc
[ocaml-forum]: https://discuss.ocaml.org/

# Acknowledgements

The query language of Rel is based on the following line of papers.

* Ezra Cooper. The Script-Writer’s Dream: How to Write Great SQL in Your 
  Own Language, and Be Sure It Will Succeed. 2009.
  [Full text](https://doi.org/10.1007/978-3-642-03793-1_3)
  
* James Cheney et al. A practical theory of language-integrated query. 2013.
  [Full text](https://doi.org/10.1145/2544174.2500586)

* Suzuki et al. Finally, safely-extensible and efficient language-integrated 
  query. 2016.
  [Full text](https://doi.org/10.1145/2847538.2847542)
  
* Oleg Kiselyov et al. Sound and Efficient Language-Integrated Query -- 
  Maintaining the ORDER. 2017.
  [Full text](https://doi.org/10.1007/978-3-319-71237-6_18)
