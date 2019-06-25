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
     PROCEDURE :: PRINT => print_args
     PROCEDURE :: parse_args
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
          CLASS default
          PRINT*, "print function not defined for that type"
       END SELECT

    END DO

  END SUBROUTINE print_args


  SUBROUTINE parse_args(self)
    CLASS(argument_list), INTENT(inout) :: self
    INTEGER :: N_args, i, j, l, label_pos
    CHARACTER(:), ALLOCATABLE :: label, fmt_value
    N_args = COMMAND_ARGUMENT_COUNT()
    PRINT*,
    IF ( MOD(N_args,2) == 1 ) THEN
       PRINT*, "missing argument"
    ELSE
       DO i = 1,N_args, 2
          CALL GET_COMMAND_ARGUMENT(number=i,length=l)
          ALLOCATE(CHARACTER(l) :: label )
          CALL GET_COMMAND_ARGUMENT(number=i,VALUE=label)
          do j = 1, self%N_args
            if (self%args(j)%label==label) label_pos = j
          end do
          CALL GET_COMMAND_ARGUMENT(number=i+1,length=l)
          ALLOCATE(CHARACTER(l) :: fmt_value)
          CALL GET_COMMAND_ARGUMENT(number=i+1,VALUE=fmt_value)
          select type( val => self%args(label_pos)%VALUE )
          ! type is (integer)
          !   read(fmt_value, *) self%args(label_pos)%VALUE
          ! type is (real)
          !   read(fmt_value, *) self%args(label_pos)%VALUE
          ! type is (character(*))
          !   read(fmt_value, *) self%args(label_pos)%VALUE
          type is (integer)
            read(fmt_value, *) VAL
          type is (real)
            read(fmt_value, *) VAL
          type is (character(*))
            read(fmt_value, *) VAL
          end select

          DEALLOCATE(label,fmt_value)
       END DO
    END IF
  END SUBROUTINE parse_args

END MODULE argparser
