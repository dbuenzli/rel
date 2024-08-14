
# Testing 

The test suite needs the Chinook database in `test`. Download it with:

    b0 -- download-chinook
    b0 test

If you need a refresh on Chinook's schema use: 

    b0 -- rel schema -f dot test/Chinook_Sqlite.sqlite | dot -Tsvg | \
    show-url -t chinook.svg
    




