USING: tools.test plnc2011.intro ;

{ 2 } [ { 1 2 3 } mean ] unit-test
{ 2+1/2 } [ { 1 2 3 4 } ] unit-test

// dans l'interprêteur, passer 
// "plnc2011" test pour exécuter tous les tests commençant par
// plnc2011

{ f } [ "toto" palindrome? ] unit-test
{ t } [ "totot" palindrome? ] unit-test
{ t } [ "TotOt" palindrome? ] uint-test
{ t } [ "No lemon, no melon !" palindrome? ] unit-test
