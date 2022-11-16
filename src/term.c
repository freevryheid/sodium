# include <termios.h>
# include <unistd.h>   /* needed for STDIN_FILENO which is an int file descriptor */

void setmode(int *mode)
{
	static struct termios tp, save;

	if (*mode != 0)
	{
		tcgetattr( STDIN_FILENO, &tp);              /* get existing terminal properties */
		save = tp;                                  /* save existing terminal properties */
		tp.c_lflag &= ~ECHO;                        /* only cause terminal echo off */
		tcsetattr( STDIN_FILENO, TCSAFLUSH, &tp );  /* set terminal settings */
	} else {
			tcsetattr( STDIN_FILENO, TCSANOW, &save); /* restore original terminal settings */
	}
}
