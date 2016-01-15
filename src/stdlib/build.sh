# Builds the micron standard library
# This requires:
#   1) fecs - the Flame Enhanced C# compiler (https://github.com/jonathanvdc/Flame.Loyc)
#   2) muc - the micron compiler

# First, compile the primops module
fecs primops.cs -platform ir -runtime clr -o bin/primops.flo -repeat-command $@
# Then, compile the stdlib module, and link it with the
# primops module.
muc stdlib.mu bin/primops.flo -platform ir -indirect-platform clr -o bin/stdlib.flo -repeat-command $@
