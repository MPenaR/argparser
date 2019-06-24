program test_argparser
use argparser
IMPLICIT none

type(arguments) :: args

print*, args%N_args
call args%add_argument('N',30)
print*, args%N_args
call args%print_args()
call args%add_argument('L',0.1)
print*, args%N_args
call args%print_args()
call args%add_argument('fileroute','/home/manuel')
print*, args%N_args
call args%print_args()
call args%add_argument('Nth',6)
print*, args%N_args
call args%print_args()

end program
