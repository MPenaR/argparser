program test_argparser
use argparser
IMPLICIT none

type(argument_list) :: args

call args%add_argument('N',30)
call args%add_argument('L',0.1)
call args%add_argument('fileroute','/home/manuel')
call args%add_argument('Nth',6)
call args%print()
call args%parse_args()
call args%print()
end program
