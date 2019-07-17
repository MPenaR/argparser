PROGRAM test_argparser
  USE argparser
  IMPLICIT NONE
  TYPE(argument_list) :: args
  INTEGER :: N, Nth
  REAL :: L
  CHARACTER(:), ALLOCATABLE :: fileroute

  CALL args%add_argument('N',30)
  CALL args%add_argument('L',0.1)
  CALL args%add_argument('fileroute','../Data/CubeSpheres_PP.exp')
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
