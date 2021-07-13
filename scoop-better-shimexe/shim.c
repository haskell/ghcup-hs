#pragma comment(lib, "SHELL32.LIB")

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <Windows.h>

#ifndef ERROR_ELEVATION_REQUIRED
#  define ERROR_ELEVATION_REQUIRED 740
#endif

#define MAX_FILENAME_SIZE 512

BOOL WINAPI ctrlhandler(DWORD fdwCtrlType)
{
  switch (fdwCtrlType) {
    // Ignore all events, and let the child process
    // handle them.
    case CTRL_C_EVENT:
    case CTRL_CLOSE_EVENT:
    case CTRL_LOGOFF_EVENT:
    case CTRL_BREAK_EVENT:
    case CTRL_SHUTDOWN_EVENT:
      return TRUE;

    default:
      return FALSE;
  }
}

int compute_program_length(const wchar_t* commandline)
{
  int i = 0;

  if (commandline[0] == L'"') {
    // Wait till end of string
    i++;

    for (;;) {
      wchar_t c = commandline[i++];

      if (c == 0)
        return i - 1;
      else if (c == L'\\')
        i++;
      else if (c == L'"')
        return i;
    }
  } else {
    for (;;) {
      wchar_t c = commandline[i++];

      if (c == 0)
        return i - 1;
      else if (c == L'\\')
        i++;
      else if (c == L' ')
        return i;
    }
  }
}

int main()
{
  DWORD exit_code = 0;

  wchar_t* path = NULL;
  wchar_t* args = NULL;
  wchar_t* cmd = NULL;

  // Find filename of current executable.
  wchar_t filename[MAX_FILENAME_SIZE + 2];
  const unsigned int filename_size = GetModuleFileNameW(NULL, filename, MAX_FILENAME_SIZE);

  if (filename_size >= MAX_FILENAME_SIZE) {
    fprintf(stderr, "The filename of the program is too long to handle.\n");

    exit_code = 1;
    goto cleanup;
  }

  // Use filename of current executable to find .shim
  filename[filename_size - 3] = L's';
  filename[filename_size - 2] = L'h';
  filename[filename_size - 1] = L'i';
  filename[filename_size - 0] = L'm';
  filename[filename_size + 1] =  0 ;

  FILE* shim_file;

  if ((shim_file = _wfsopen(filename, L"r,ccs=UTF-8", _SH_DENYNO)) == NULL) {
    fprintf(stderr, "Cannot open shim file for read.\n");

    exit_code = 1;
    goto cleanup;
  }

  size_t command_length = 256;
  size_t path_length;
  size_t args_length;

  // Read shim
  wchar_t linebuf[8192];

  for (;;) {
    const wchar_t* line = fgetws(linebuf, 8192, shim_file);

    if (line == NULL)
      break;

    if (line[4] != L' ' || line[5] != L'=' || line[6] != L' ')
      continue;

    const int linelen = wcslen(line);
    const int len = linelen - 8 + (line[linelen - 1] != '\n');

    if (line[0] == L'p' && line[1] == L'a' && line[2] == L't' && line[3] == L'h') {
      // Reading path
      path = calloc(len + 1, sizeof(wchar_t));
      wmemcpy(path, line + 7, len);

      command_length += len;
      path_length = len;

      continue;
    }

    if (line[0] == L'a' && line[1] == L'r' && line[2] == L'g' && line[3] == L's') {
      // Reading args
      args = calloc(len + 1, sizeof(wchar_t));
      wmemcpy(args, line + 7, len);

      command_length += len + 1;
      args_length = len;

      continue;
    }

    continue;
  }

  fclose(shim_file);

  if (path == NULL) {
    fprintf(stderr, "Could not read shim file.\n");

    exit_code = 1;
    goto cleanup;
  }

  // Find length of command to run
  wchar_t* given_cmd = GetCommandLineW();
  const int program_length = compute_program_length(given_cmd);

  given_cmd += program_length;

  const int given_length = wcslen(given_cmd);

  command_length += given_length;

  // Start building command to run, using '[path] [args]', as given by shim.
  cmd = calloc(command_length, sizeof(wchar_t));
  int cmd_i = 0;

  wmemcpy(cmd, path, path_length);
  cmd[path_length] = ' ';
  cmd_i += path_length + 1;

  if (args != NULL) {
    wmemcpy(cmd + path_length + 1, args, args_length);
    cmd[path_length + args_length + 1] = ' ';
    cmd_i += args_length + 1;
  }

  // Copy all given arguments to command
  wmemcpy(cmd + cmd_i, given_cmd, given_length);

  // Find out if the target program is a console app
  SHFILEINFOW sfi = {0};
  const BOOL is_windows_app = HIWORD(SHGetFileInfoW(path, -1, &sfi, sizeof(sfi), SHGFI_EXETYPE));

  if (is_windows_app)
    // Unfortunately, this technique will still show a window for a fraction of time,
    // but there's just no workaround.
    FreeConsole();

  // Create job object, which can be attached to child processes
  // to make sure they terminate when the parent terminates as well.
  JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli = {0};
  HANDLE jobHandle = CreateJobObject(NULL, NULL);

  jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE | JOB_OBJECT_LIMIT_SILENT_BREAKAWAY_OK;
  SetInformationJobObject(jobHandle, JobObjectExtendedLimitInformation, &jeli, sizeof(jeli));

  // Start subprocess
  STARTUPINFOW si = {0};
  PROCESS_INFORMATION pi = {0};

  if (CreateProcessW(NULL, cmd, NULL, NULL, TRUE, CREATE_SUSPENDED, NULL, NULL, &si, &pi)) {
    AssignProcessToJobObject(jobHandle, pi.hProcess);
    ResumeThread(pi.hThread);
  } else {
    if (GetLastError() == ERROR_ELEVATION_REQUIRED) {
      // We must elevate the process, which is (basically) impossible with
      // CreateProcess, and therefore we fallback to ShellExecuteEx,
      // which CAN create elevated processes, at the cost of opening a new separate
      // window.
      // Theorically, this could be fixed (or rather, worked around) using pipes
      // and IPC, but... this is a question for another day.
      SHELLEXECUTEINFOW sei = {0};

      sei.cbSize       = sizeof(SHELLEXECUTEINFOW);
      sei.fMask        = SEE_MASK_NOCLOSEPROCESS;
      sei.lpFile       = path;
      sei.lpParameters = cmd + path_length + 1;
      sei.nShow        = SW_SHOW;

      if (!ShellExecuteExW(&sei)) {
        fprintf(stderr, "Unable to create elevated process: error %li.", GetLastError());

        exit_code = 1;
        goto cleanup;
      }

      pi.hProcess = sei.hProcess;
    } else {
      fprintf(stderr, "Could not create process with command '%ls'.\n", cmd);

      exit_code = 1;
      goto cleanup;
    }
  }

  // Ignore Ctrl-C and other signals
  if (!SetConsoleCtrlHandler(ctrlhandler, TRUE))
    fprintf(stderr, "Could not set control handler; Ctrl-C behavior may be invalid.\n");

  // Wait till end of process
  WaitForSingleObject(pi.hProcess, INFINITE);

  GetExitCodeProcess(pi.hProcess, &exit_code);

  // Dispose of everything
  CloseHandle(pi.hThread);
  CloseHandle(pi.hProcess);
  CloseHandle(jobHandle);

cleanup:

  // Free obsolete buffers
  free(path);
  free(args);
  free(cmd);

  return (int)exit_code;
}
