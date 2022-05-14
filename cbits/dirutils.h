#ifndef POSIXPATHS_CBITS_DIRUTILS_H
#define POSIXPATHS_CBITS_DIRUTILS_H

#include <stdlib.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>


extern unsigned int
    __posixdir_d_type(struct dirent* d)
    ;

#endif
