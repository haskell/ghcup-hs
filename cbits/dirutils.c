#include "dirutils.h"

unsigned int
    __posixdir_d_type(struct dirent* d)
    {
      return(d -> d_type);
    }
