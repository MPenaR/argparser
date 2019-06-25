MODULE argparser
  IMPLICIT NONE

  private

  public :: argument_list, assignment(=)

  interface assignment(=)
     module procedure int_ass
     module procedure real_ass
     module procedure char_ass
   end interface

  TYPE :: argument
     CHARACTER(:), ALLOCATABLE :: label
     CLASS(*), ALLOCATABLE :: VALUE
  END TYPE argument

  TYPE :: argument_list
     TYPE(argument), ALLOCATABLE :: args(:)
     INTEGER :: N_args = 0
   CONTAINS
     PROCEDURE :: add_argument
     PROCEDURE :: PRINT => print_args
     PROCEDURE :: parse_args
     procedure :: get => get_argument_by_label
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
          type is (integer)
            read(fmt_value, *) val
          type is (real)
            read(fmt_value, *) val
          type is (character(*))
            deallocate(self%args(label_pos)%VALUE)
            allocate(self%args(label_pos)%VALUE, source = fmt_value)
          end select

          DEALLOCATE(label,fmt_value)
       END DO
    END IF
  END SUBROUTINE parse_args

  function get_argument_by_label(self,label) result(value)
    class(argument_list), intent(in) :: self
    character(*), intent(in) :: label
    class(*), allocatable :: value

    integer :: j

    do j = 1, self%N_args
      if (self%args(j)%label==label) allocate(value, source=self%args(j)%value)
    end do
  end function

  subroutine int_ass(a,b)
    integer, intent(out) :: a
    class(*), intent(in) :: b
    select type (b)
      type is (integer)
        a = b
    end select
  end subroutine int_ass

  subroutine real_ass(a,b)
    real, intent(out) :: a
    class(*), intent(in) :: b
    select type (b)
      type is (real)
        a = b
    end select
  end subroutine real_ass

  subroutine char_ass(a,b)
    character(:), allocatable, intent(out) :: a
    class(*), intent(in) :: b
    select type (b)
    type is (character(*))
        a = b
    end select
  end subroutine char_ass


END MODULE argparser
