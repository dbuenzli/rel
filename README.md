ask — Relational database programming for OCaml
===============================================
%%VERSION%%

Ask is a library for programming with relational databases in OCaml.
It provides:

* Typed combinators to describe tables and their representation 
  by client-defined OCaml values.
* Automatic database row and OCaml values translations from 
  the table descriptions.
* SQL data definition generation from the table descriptions.
* A mechanism for typing SQL statement parameters and results.
* SQL generation helpers.
* A query language embedded in *plain* OCaml. Queries are typed,
  composable and compile to a single, flat, SQL query (experimental).
* Support for SQLite3 databases (optional). Run SQL statements 
  and generate OCaml table descriptions for existing databases.

Ask is distributed under the ISC license. The basic library has no
dependencies. The SQLite support depends on the C SQLite3 library (at
least v3.26.2).

Homepage: https://erratique.ch/software/ask

# Installation

ask can be installed with `opam`:

    opam install ask
    opam install ask conf-sqlite3 # with SQLite support

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

# Documentation

The documentation can be consulted [online][doc] or via `odig doc ask`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/ask/doc
[ocaml-forum]: https://discuss.ocaml.org/

# Acknowlegements

The query language of Ask is based on the following line of papers.

* Ezra Cooper. The Script-Writer’s Dream: How to Write Great SQL in Your 
  Own Language, and Be Sure It Will Succeed. 2009 
  [doi](https://doi.org/10.1007/978-3-642-03793-1_3)
  
* James Cheney et al. A practical theory of language-integrated query. 2013
  [doi](https://doi.org/10.1145/2544174.2500586)

* Suzuki et al. Finally, safely-extensible and efficient language-integrated 
  query. 2016 
  [doi](https://doi.org/10.1145/2847538.2847542)
  
* Oleg Kiselyov et al. Sound and Efficient Language-Integrated Query -- 
  Maintaining the ORDER. 2017 
  [doi](https://doi.org/10.1007/978-3-319-71237-6_18)
