module char_mod
use cons
implicit none

public trim_char 
public space_char
public dot_char

contains

subroutine trim_char(string)
character(len=*) :: string

string = trim(string)
call dot_char(string)
call space_char(string)

end subroutine trim_char

subroutine space_char(string)
character(len=*) :: string
integer :: stringLen 
integer :: last, actual

stringLen = len (string)
last = 1
actual = 1

do while (actual < stringLen)
    if (string(last:last) == ' ') then
        actual = actual + 1
        string(last:last) = string(actual:actual)
        string(actual:actual) = ' '
    else
        last = last + 1
        if (actual < last) &
            actual = last
    endif
end do

end subroutine space_char

subroutine dot_char(string)
character(len=*) :: string
integer :: stringLen 
integer :: last, actual

stringLen = len (string)
last = 1
actual = 1                                                                                

do while (actual < stringLen)
    if (string(last:last) == '.') then
        actual = actual + 1 
        string(last:last) = string(actual:actual)
        string(actual:actual) = ' '
    else
        last = last + 1
        if (actual < last) &
            actual = last
    endif
end do

end subroutine dot_char




end module char_mod
