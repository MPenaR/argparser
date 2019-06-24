MODULE argparser
  IMPLICIT NONE

  TYPE :: argument
     CHARACTER(:), ALLOCATABLE :: label
     CLASS(*), ALLOCATABLE :: value
  END TYPE argument

  TYPE, PUBLIC :: arguments
     TYPE(argument), ALLOCATABLE :: args(:)
     INTEGER :: N_args = 0
   CONTAINS
     PROCEDURE :: add_argument
     PROCEDURE :: print_args
  END TYPE arguments

CONTAINS

  SUBROUTINE add_argument(self,label,default_value)
    CLASS(arguments), INTENT(inout) :: self
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
    CLASS(arguments), INTENT(in) :: self

    INTEGER :: i

    DO i = 1, self%N_args
       PRINT*, self%args(i)%label
       SELECT TYPE( val => self%args(i)%VALUE)
       TYPE is(INTEGER)
          PRINT*,"integer: ", val
       TYPE is(REAL)
          PRINT*, "real: ", val
       TYPE is(CHARACTER(*))
          PRINT*, "character: ", val
       END SELECT
    END DO

  END SUBROUTINE print_args

END MODULE argparser
