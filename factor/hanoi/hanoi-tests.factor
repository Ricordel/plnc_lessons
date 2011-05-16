USING: tools.test hanoi io.streams.string ;

! Test de move
{ "2 vers 3" } [ 2 3 move ] unit-test

! Tests de other
{ 1 } [ 2 3 other ] unit-test
{ 1 } [ 3 2 other ] unit-test
{ 2 } [ 1 3 other ] unit-test
{ 2 } [ 3 1 other ] unit-test
{ 3 } [ 1 2 other ] unit-test
{ 3 } [ 2 1 other ] unit-test


! Test de hanoi
{ "1 vers 3
1 vers 2
3 vers 2
1 vers 3
2 vers 1
2 vers 3
1 vers 3\n" } [ [ 1 3 3 hanoi ] with-string-writer ] unit-test
