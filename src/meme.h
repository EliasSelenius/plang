

jmp_buf fucking_buffer;
int     fucking_error;

#define fuck_around if (setjmp(fucking_buffer) == 0)
#define find_out else
#define fuck_up(error_code) longjmp(fucking_buffer, fucking_error = error_code)

#define try fuck_around
#define catch find_out
#define throw fuck_up
