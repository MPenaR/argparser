# argparser
generic argument parser in modern fortran


## Compiling:

```bash
make test
gfortran -c argparser.f08
gfortran -c test_argparser.f08
gfortran test_argparser.o argparser.o -o test
```

## Usage:

```fortran
PROGRAM test_argparser
  USE argparser
  IMPLICIT NONE
  TYPE(argument_list) :: args
  INTEGER :: N, Nth
  REAL :: L
  CHARACTER(:), ALLOCATABLE :: fileroute

  CALL args%add_argument('N',30)
  CALL args%add_argument('L',0.1)
  CALL args%add_argument('fileroute','/home/')
  CALL args%add_argument('Nth',6)
  CALL args%PRINT()
  CALL args%parse_args()
  CALL args%PRINT()

  N = args%get('N')
  Nth = args%get('Nth')
  fileroute = args%get('fileroute')
  L = args%get('L')

  PRINT*, N
  PRINT*, Nth
  PRINT*, fileroute
  PRINT*, L

END PROGRAM test_argparser
```
```bash
./test L 0.5 fileroute '/home/documents/' N 40

Number of arguments defined:            4
label: N, type: integer, value:           30
label: L, type: real, value:   0.100000001
label: fileroute, type: character, value: /home/
label: Nth, type: integer, value:            6


Number of arguments defined:            4
label: N, type: integer, value:           40
label: L, type: real, value:   0.500000000
label: fileroute, type: character, value: /home/documents/
label: Nth, type: integer, value:            6
         40
          6
/home/documents/
 0.500000000

```
