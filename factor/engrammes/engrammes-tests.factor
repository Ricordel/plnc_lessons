USING: tools.test engrammes  ;


! === Tests pour prime-factors ===
{ V{ 1 } }      [ 2 prime-factors ] unit-test
{ V{ 0 1 } }    [ 3 prime-factors ] unit-test
{ V{ 2 } }      [ 4 prime-factors ] unit-test
{ V{ 0 0 1 } }  [ 5 prime-factors ] unit-test
{ V{ 1 1 } }    [ 6 prime-factors ] unit-test
{ V{ 2 2 } }    [ 36 prime-factors ] unit-test
{ V{ 6 } }      [ 64 prime-factors ] unit-test
