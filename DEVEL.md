This project uses (perhaps the development version of) [`b0`] for
development. Consult [b0 occasionally] for quick hints on how to
perform common development tasks.

[`b0`]: https://erratique.ch/software/b0
[b0 occasionally]: https://erratique.ch/software/b0/doc/occasionally.html

# Testing 

The test suite needs the Chinook database in `test`. Download it with:

    b0 -- download-chinook
    b0 test

If you need a refresh on Chinook's schema use: 

    b0 -- rel schema -f dot test/Chinook_Sqlite.sqlite | dot -Tsvg | \
    show-url -t chinook.svg
    




