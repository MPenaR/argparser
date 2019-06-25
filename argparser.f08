MODULE argparser
  IMPLICIT NONE

  TYPE :: argument
     CHARACTER(:), ALLOCATABLE :: label
     CLASS(*), ALLOCATABLE :: VALUE
  END TYPE argument

  TYPE, PUBLIC :: argument_list
     TYPE(argument), ALLOCATABLE :: args(:)
     INTEGER :: N_args = 0
   CONTAINS
     PROCEDURE :: add_argument
     PROCEDURE :: print_args
  END TYPE argument_list

CONTAINS

  SUBROUTINE add_argument(self,label,default_value)
    CLASS(argument_list), INTENT(inout) :: self
    CHARACTER(*), INTENT(in) :: label
    CLASS(*), INTENT(in) :: default_value

    TYPE(argument), ALLOCATABLE :: known_arguments(:)

    ALLOCATE( known_arguments( self%N_args + 1) )

    known_arguments(1:self%N_args) = self%args
    known_arguments(self%N_args + 1 )%label = label
    ALLOCATE(known_arguments(self%N_args + 1 )%VALUE, source= default_value)

    CALL MOVE_ALLOC(known_arguments,self%args)
    self%N_args = self%N_args + 1

  END SUBROUTINE add_argument

  SUBROUTINE print_args(self)
    CLASS(argument_list), INTENT(in) :: self

    INTEGER :: i

    PRINT*,
    PRINT*, "Number of arguments defined: ", self%N_args

    DO i = 1, self%N_args

       SELECT TYPE( val => self%args(i)%VALUE)
       TYPE is(INTEGER)
          PRINT*, "label: ", self%args(i)%label, ", type: integer, value: ", val
       TYPE is(REAL)
          PRINT*, "label: ", self%args(i)%label, ", type: real, value: ", val
       TYPE is(CHARACTER(*))
          PRINT*, "label: ", self%args(i)%label, ", type: character, value: ", val
       END SELECT
    END DO

  END SUBROUTINE print_args

END MODULE argparser
