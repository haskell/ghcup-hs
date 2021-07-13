# `shim.c`

[`shim.c`](./shim.c) is a simple Windows program that, when started:
1. Looks for a file with the exact same name as the running program, but with
   the extension `shim` (e.g. `C:\bin\foo.exe` will read the file `C:\bin\foo.shim`).
2. Reads and [parses](#shim-format) the files into a
   [Scoop](https://github.com/lukesampson/scoop) shim format.
3. Executes the target executable with the given arguments.

`shim.c` was originally made to replace [Scoop](https://github.com/lukesampson/scoop)'s
[`shim.cs`](https://github.com/lukesampson/scoop/blob/96de9c14bb483f9278e4b0a9e22b1923ee752901/supporting/shimexe/shim.cs)
since it had several important flaws:
1. [It was made in C#](https://github.com/lukesampson/scoop/tree/96de9c14bb483f9278e4b0a9e22b1923ee752901/supporting/shimexe),
   and thus required an instantiation of a .NET command line app everytime it was started,
   which can make a command run much slower than if it had been ran directly;
2. [It](https://github.com/lukesampson/scoop/issues/2339) [did](https://github.com/lukesampson/scoop/issues/1896)
   [not](https://github.com/felixse/FluentTerminal/issues/221) handle Ctrl+C and other
   signals correctly, which could be quite infuriating (and essentially killing REPLs and long-running apps).

[`shim.c`](./shim.c) is:
- **Faster**, because it does not use the .NET Framework, and parses the `.shim` file in a simpler way.
- **More efficient**, because by the time the target of the shim is started, all allocated memory will have been freed.
- And more importantly, it **works better**:
  - Signals originating from pressing `Ctrl+C` are ignored, and therefore handled directly by the spawned child.
    Your processes and REPLs will no longer close when pressing `Ctrl+C`.
  - Children are automatically killed when the shim process is killed. No more orphaned processes and weird behaviors.

> **Note**: This project is not affiliated with [Scoop](https://github.com/lukesampson/scoop).


## Installation for Scoop

- In a Visual Studio command prompt, run `cl /O1 shim.c`.
- Replace any `.exe` in `scoop\shims` by `shim.exe`.

An additional script, `repshims.bat`, is provided. It will replace all `.exe`s in the user's Scoop directory
by `shim.exe`.


## Example

Given the following shim `gs.shim`:
```
path = C:\Program Files\Git\git.exe
args = status -u
```

In this directory, where `gs.exe` is the compiled `shim.c`:
```
C:\Bin\
   gs.exe
   gs.shim
```

Then calling `gs -s` will run the program `C:\Program Files\Git\git.exe status -u -s`.


## Shim format

Shims follow the same format as Scoop's shims: line-separated `key = value` pairs.
```
path = C:\Program Files\Git\git.exe
args = status -uno
```

`path` is a required value, but `args` can be omitted. Also, do note that lines **must** end with a line feed.


## License

`SPDX-License-Identifier: MIT OR Unlicense`
