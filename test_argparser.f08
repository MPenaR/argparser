PROGRAM test_argparser
  USE argparser
  IMPLICIT NONE

  TYPE(argument_list) :: args

  CALL args%add_argument('N',30)
  CALL args%add_argument('L',0.1)
  CALL args%add_argument('fileroute','/home/')
  CALL args%add_argument('Nth',6)
  CALL args%PRINT()
  CALL args%parse_args()
  CALL args%PRINT()
END PROGRAM test_argparser
