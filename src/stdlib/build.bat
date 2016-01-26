dsc primops.ds -platform ir -runtime clr -o bin/primops.flo
dsc primio.ds -platform ir -runtime clr -o bin/primio.flo
muc stdlib.mu bin/primops.flo bin/primio.flo -platform ir -runtime clr -flower-lambda -o bin/stdlib.flo
