# Builds the micron standard library
# This requires:
#   1) dsc - the D# compiler (https://github.com/jonathanvdc/Flame)
#   2) muc - the micron compiler

# First, compile the primops, primio and primlist modules
dsc primops.ds -platform ir -runtime clr -o bin/primops.flo -repeat-command $@
dsc primio.ds -platform ir -runtime clr -o bin/primio.flo -repeat-command $@
dsc primlist.ds -platform ir -runtime clr -o bin/primlist.flo -repeat-command $@
# Then, compile the stdlib module, and link it with the primitive modules.
muc stdlib.mu bin/primops.flo bin/primio.flo bin/primlist.flo -platform ir -runtime clr -flower-lambda -o bin/stdlib.flo -repeat-command $@
