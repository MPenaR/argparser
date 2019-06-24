MODULE argparser
  IMPLICIT NONE

  TYPE :: argument
     CHARACTER(:), ALLOCATABLE :: label
     CLASS(*), ALLOCATABLE :: VALUE
  END TYPE argument

  TYPE, PUBLIC :: arguments
     TYPE(argument), ALLOCATABLE :: args(:)
     integer :: N_args
   CONTAINS
     PROCEDURE :: add_argument
  END TYPE arguments

CONTAINS

  SUBROUTINE add_argument(self,label,default_value)
    CLASS(arguments), INTENT(inout) :: self
    CHARACTER(*), INTENT(in) :: label
    CLASS(*), INTENT(in) :: default_value

    type(argument), allocatable :: known_arguments(:)

    allocate( known_arguments( self%N_args + 1) )

    known_arguments(1:self%N_args) = self%args
    known_arguments(self%N_args + 1 ) = ...
    MOVE_ALLOC(known_arguments,self%args)
    self%N_args = self%N_args + 1






  END SUBROUTINE add_argument

END MODULE argparser
