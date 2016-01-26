dsc primlist.ds -platform ir -runtime clr -o bin/primlist.flo
dsc primops.ds -platform ir -runtime clr -o bin/primops.flo
dsc primio.ds -platform ir -runtime clr -o bin/primio.flo

..\muc\bin\release\muc stdlib.mu bin/primops.flo bin/primio.flo bin/primlist.flo -platform ir -runtime clr -flower-lambda -o bin/stdlib.flo