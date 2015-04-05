#include <stdio.h>
#include <X11/extensions/scrnsaver.h>

/* Based on getIdleTime.c by falconindy on ArchLinux BBS thread ID
   121863.  This function connects to the display in the DISPLAY
   environment variable and returns the idle time.  So the Haskell
   code is responsible for determining and setting DISPLAY if
   e.g. running from cron. */

int GetXIdleTime ()
{
    Display *dpy = XOpenDisplay(NULL);

    if (!dpy)
    {
        return(-1);
    }

    XScreenSaverInfo *info = XScreenSaverAllocInfo();
    XScreenSaverQueryInfo(dpy, DefaultRootWindow(dpy), info);

    return (info->idle);
}

/* int main (void)
{
    int idletime;

    idletime = GetXIdleTime();
    printf("%u", idletime);
    return 0;
} */
