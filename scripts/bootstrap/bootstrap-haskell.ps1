<#
    .SYNOPSIS
    Script to bootstrap a Haskell environment

    .DESCRIPTION
    This is the windows GHCup installer, installing:

    * ghcup - The Haskell toolchain installer"
    * ghc   - The Glasgow Haskell Compiler"
    * msys2 - Unix-style toolchain needed for dependencies and tools
    * cabal - The Cabal build tool for managing Haskell software"
    * stack - (optional) A cross-platform program for developing Haskell projects"
    * hls   - (optional) A language server for developers to integrate with their editor/IDE"

	By default, the installation is non-interactive, unless you run it with 'Interactive $true'.
#>
param (
    # Run an interactive installation
    [switch]$Interactive,
    # Do minimal installation of ghcup and msys2 only
    [switch]$Minimal,
    # Run the final bootstrap script via 'bash' instead of a full newly spawned msys2 shell
    [switch]$InBash,
    # Overwrite (or rather backup) a previous install
    [switch]$Overwrite,
    # Skip adjusting cabal.config with mingw paths
    [switch]$NoAdjustCabalConfig,
    # Whether to install stack as well
    [switch]$InstallStack,
    # Whether to install hls as well
    [switch]$InstallHLS,
    # Specify the install root (default: 'C:\')
    [string]$InstallDir,
    # Specify the bootstrap url (default: 'https://www.haskell.org/ghcup/sh/bootstrap-haskell')
    [string]$BootstrapUrl,
    # Instead of installing a new MSys2, use an existing installation
    [string]$ExistingMsys2Dir,
    # Specify the cabal root directory (default: '$InstallDir\cabal')
    [string]$CabalDir,
    # Whether to disable use of curl.exe
    [switch]$DisableCurl,
    # The Msys2 version to download (e.g. 20221216)
    [string]$Msys2Version,
    # The Msys2 sha256sum hash
    [string]$Msys2Hash,
    # Whether to disable creation of several desktop shortcuts
    [switch]$DontWriteDesktopShortcuts,
    # Whether to disable adjusting bashrc (in msys2 env) with PATH
    [switch]$DontAdjustBashRc,
    # The msys2 environment to use, see https://www.msys2.org/docs/environments/ (defauts to MINGW64, MINGW32 or CLANGARM64, depending on the architecture)
    [string]$Msys2Env
)

$DefaultMsys2Version = "20221216"
$DefaultMsys2Hash = "18370d32b0264915c97e3d7c618f7b32d48ad80858923883fde5145acd32ca0f"

$Silent = !$Interactive

function Print-Msg {
  param ( [Parameter(Mandatory=$true, HelpMessage='String to output')][string]$msg, [string]$color = "Green" )
  Write-Host ('{0}' -f $msg) -ForegroundColor $color
}

function Create-Shortcut {
    param ( [Parameter(Mandatory=$true,HelpMessage='Target path')][string]$SourceExe
          , [Parameter(Mandatory=$true,HelpMessage='Arguments to the path/exe')][AllowEmptyString()]$ArgumentsToSourceExe
          , [Parameter(Mandatory=$true,HelpMessage='The destination of the desktop link')][string]$DestinationPath
          , [Parameter(Mandatory=$true,HelpMessage='Temporary path to create the link at')][string]$TempPath
           )
	# we save to a temp dir first due to
	# https://gitlab.haskell.org/haskell/ghcup-hs/-/issues/267
    $DesktopDir = [Environment]::GetFolderPath("Desktop")
    if (!(Test-Path -Path ('{0}' -f $TempPath))) {
      New-Item -Path $TempPath -ItemType "directory"
    }
    $TmpFile = ('{0}\{1}' -f $TempPath, $DestinationPath)
    $FinalDest = ('{0}\{1}' -f $DesktopDir, $DestinationPath)
    $WshShell = New-Object -comObject WScript.Shell
    $Shortcut = $WshShell.CreateShortcut($TmpFile)
    $Shortcut.TargetPath = $SourceExe
    if($ArgumentsToSourceExe) {
      $Shortcut.Arguments = $ArgumentsToSourceExe
    }
    $Shortcut.Save()
    if ((Test-Path -Path ('{0}' -f $FinalDest))) {
      Remove-Item -LiteralPath $FinalDest -Force
    }
    Move-Item -LiteralPath $TmpFile -Destination $FinalDest
}

function Add-EnvPath {
  param(
      [Parameter(Mandatory=$true,HelpMessage='The Path to add to Users environment')]
      [string] $Path,

      [ValidateSet('Machine', 'User', 'Session')]
      [string] $Container = 'Session'
  )

  if ($Container -eq 'Session') {
      $envPaths = [Collections.Generic.List[String]]($env:Path -split ([IO.Path]::PathSeparator))
      if ($envPaths -notcontains $Path) {
          $envPaths.Add($Path)
          $env:PATH = $envPaths -join ([IO.Path]::PathSeparator)
      }
  }
  else {
      [Microsoft.Win32.RegistryHive]$hive, $keyPath = switch ($Container) {
          'Machine' { 'LocalMachine', 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment' }
          'User' { 'CurrentUser', 'Environment' }
      }

      $hiveKey = $envKey = $null
      try {
          $hiveKey = [Microsoft.Win32.RegistryKey]::OpenRemoteBaseKey($hive, '')
          $envKey = $hiveKey.OpenSubKey($keyPath, $true)
          $rawPath = $envKey.GetValue('PATH', '', 'DoNotExpandEnvironmentNames')

          $envPaths = [Collections.Generic.List[String]]($rawPath -split ([IO.Path]::PathSeparator))
          if ($envPaths -notcontains $Path) {
              $envPaths.Add($Path)
              $envKey.SetValue('PATH', ($envPaths -join ([IO.Path]::PathSeparator)), 'ExpandString')
          }
      }
      finally {
          if ($envKey) { $envKey.Close() }
          if ($hiveKey) { $hiveKey.Close() }
      }
  }
}



filter Get-FileSize {
	'{0:N2} {1}' -f $(
	if ($_ -lt 1kb) { $_, 'Bytes' }
	elseif ($_ -lt 1mb) { ($_/1kb), 'KB' }
	elseif ($_ -lt 1gb) { ($_/1mb), 'MB' }
	elseif ($_ -lt 1tb) { ($_/1gb), 'GB' }
	elseif ($_ -lt 1pb) { ($_/1tb), 'TB' }
	else { ($_/1pb), 'PB' }
	)
}

function Get-FileWCSynchronous{
    param(
        [Parameter(Mandatory=$true)]
        [string]$url,
        [string]$destinationFolder="$env:USERPROFILE\Downloads",
        [switch]$includeStats
    )
    $wc = New-Object -TypeName Net.WebClient
    $wc.UseDefaultCredentials = $true
    $destination = Join-Path -Path $destinationFolder -ChildPath ($url | Split-Path -Leaf)
    $start = Get-Date
    $wc.DownloadFile($url, $destination)
    $elapsed = ((Get-Date) - $start).ToString('hh\:mm\:ss')
    $totalSize = (Get-Item -Path $destination).Length | Get-FileSize
    if ($includeStats.IsPresent){
        [PSCustomObject]@{Name=$MyInvocation.MyCommand;TotalSize=$totalSize;Time=$elapsed}
    }
    Get-Item -Path $destination | Unblock-File
}

function Test-AbsolutePath {
  Param (
      [Parameter(Mandatory=$True)]
      [ValidateScript({[System.IO.Path]::IsPathRooted($_)})]
      [String]$Path
  )
}

function Exec
{
    [CmdletBinding()]
    param(
        [Parameter(Position = 0, Mandatory = 1)][string]$cmd,
        [Parameter()][string]$errorMessage,
        [parameter(ValueFromRemainingArguments = $true)]
        [string[]]$Passthrough
    )
    & $cmd @Passthrough
    if ($lastexitcode -ne 0) {
        if (!($errorMessage)) {
          throw ('Exec: Error executing command {0} with arguments ''{1}''' -f $cmd, "$Passthrough")
        } else {
          throw ('Exec: ' + $errorMessage)
        }
    }
}

# Only x86 32/64-bit is supported
$SupportedArchitectures = 'AMD64', 'x86'
if (!$SupportedArchitectures.contains($env:PROCESSOR_ARCHITECTURE)) {
  Print-Msg -color Red -msg ("Unsupported processor architecture: {0}. Supported architectures: {1}." -f $env:PROCESSOR_ARCHITECTURE, ($SupportedArchitectures -join ", "))
  Exit 1
}

# set default Msys2Env if not set
if (!$Msys2Env) {
  if ($env:PROCESSOR_ARCHITECTURE -eq 'x86') {
	  $Msys2Env = 'MINGW32'
  } elseif ($env:PROCESSOR_ARCHITECTURE -eq 'AMD64') {
	  $Msys2Env = 'MINGW64'
  }
}

# parse Msys2Env and set the corresponding variables
if ($Msys2Env -eq 'MINGW32') {
	$ShellType = '-mingw32'
	$PkgConf = 'mingw-w64-i686-pkgconf'
} elseif ($Msys2Env -eq 'MINGW64') {
	$ShellType = '-mingw64'
	$PkgConf = 'mingw-w64-x86_64-pkgconf'
} elseif ($Msys2Env -eq 'MSYS') {
	$ShellType = '-msys2'
	$PkgConf = 'pkgconf'
} elseif ($Msys2Env -eq 'UCRT64') {
	$ShellType = '-ucrt64'
	$PkgConf = 'mingw-w64-ucrt-x86_64-pkgconf'
} elseif ($Msys2Env -eq 'CLANG64') {
	$ShellType = '-clang64'
	$PkgConf = 'mingw-w64-clang-x86_64-pkgconf'
} else {
  Print-Msg -color Red -msg ("Unsupported Msys2 environment: {0}. Supported environments are: MINGW64, MINGW32, MSYS, UCRT64, CLANG64" -f $Msys2Env)
  Exit 1
}

$ErrorActionPreference = 'Stop'

$GhcupBasePrefixEnv = [System.Environment]::GetEnvironmentVariable('GHCUP_INSTALL_BASE_PREFIX', 'user')

if (Get-Command -Name 'chocolatey.exe' -ErrorAction SilentlyContinue) {
  if (!($Silent)) {
    Print-Msg -color Magenta -msg (@'
Chocolatey was detected on your system. It is capable of installing the Haskell toolchain as well.
If you want to rather use that instead of ghcup, abort the installation and run the following at an
elevated command prompt:
    choco install haskell-dev
    refreshenv

'@)
    $decision = $Host.UI.PromptForChoice(''
    , 'Continue with GHCup installation?'
    , [System.Management.Automation.Host.ChoiceDescription[]] @('&Continue'
        '&Abort'), 0)
    if ($decision -eq 1) {
      Exit 0
    }
  }
}

if ($GhcupBasePrefixEnv) {
  $defaultGhcupBasePrefix = $GhcupBasePrefixEnv
} elseif (!($InstallDir)) {
  $partitions = Get-CimInstance win32_logicaldisk
  $defaultGhcupBasePrefix = $null
  foreach ($p in $partitions){
    try {
      if ($p."FreeSpace" -lt 5368709120) { # at least 5 GB are needed
        throw ("Not enough free space on {0}" -f $p."DeviceId")
      }
      $null = New-Item -Path ('{0}\' -f $p."DeviceId") -Name "ghcup.test" -ItemType "directory" -Force
      $defaultGhcupBasePrefix = ('{0}\' -f $p."DeviceId")
      Remove-Item -LiteralPath ('{0}\ghcup.test' -f $p."DeviceId")
      break
    } catch {
      Print-Msg -color Yellow -msg ("{0} not writable or not enough disk space, trying next device" -f $p."DeviceId")
    }
  }
  if ($defaultGhcupBasePrefix) {
    Print-Msg -color Green -msg ("Picked {0} as default Install prefix!" -f $defaultGhcupBasePrefix)
  } else {
    Print-Msg -color Red -msg "Couldn't find a writable partition with at least 5GB free disk space!"
    Exit 1
  }
}

# ask for base install prefix
if ($Silent -and !($InstallDir)) {
  $GhcupBasePrefix = $defaultGhcupBasePrefix
} elseif ($InstallDir) {
  if (!(Test-Path -LiteralPath ('{0}' -f $InstallDir) -IsValid)) {
    Print-Msg -color Red -msg "Not a valid directory! (InstallDir)"
    Exit 1
  } elseif (!(Split-Path -IsAbsolute -Path "$InstallDir")) {
    Print-Msg -color Red -msg "Non-absolute Path specified! (InstallDir)"
    Exit 1
  } else {
    $GhcupBasePrefix = $InstallDir
  }
} else {
  while ($true) {
	Print-Msg -color Magenta -msg (@'
Welcome to Haskell!

This script can download and install the following programs:
  * ghcup - The Haskell toolchain installer
  * ghc   - The Glasgow Haskell Compiler
  * msys2 - A linux-style toolchain environment required for many operations
  * cabal - The Cabal build tool for managing Haskell software
  * stack - (optional) A cross-platform program for developing Haskell projects
  * hls   - (optional) A language server for developers to integrate with their editor/IDE

Please note that ANTIVIRUS may interfere with the installation. If you experience problems, consider
disabling it temporarily.

Where to install to (this should be a short Path, preferably a Drive like 'C:\')?
If you accept this path, binaries will be installed into '{0}ghcup\bin' and msys2 into '{0}ghcup\msys64'.
Press enter to accept the default [{0}]:

'@ -f $defaultGhcupBasePrefix)


    $basePrefixPrompt = Read-Host
    $GhcupBasePrefix = ($defaultGhcupBasePrefix,$basePrefixPrompt)[[bool]$basePrefixPrompt]
    if (!($GhcupBasePrefix.EndsWith('\'))) {
      $GhcupBasePrefix = ('{0}\' -f $GhcupBasePrefix)
    }

    $GhcupBasePrefix = $GhcupBasePrefix.TrimEnd().TrimStart()
    if (!($GhcupBasePrefix)) {
      Print-Msg -color Red -msg "No directory specified!"
    } elseif (!(Test-Path -LiteralPath ('{0}' -f $GhcupBasePrefix))) {
      Print-Msg -color Red -msg "Directory does not exist, need to specify an existing Drive/Directory"
    } elseif (!(Split-Path -IsAbsolute -Path "$GhcupBasePrefix")) {
      Print-Msg -color Red -msg "Invalid/Non-absolute Path specified"
    } else {
      Break
    }
  }
}

Print-Msg -msg ('Setting env variable GHCUP_INSTALL_BASE_PREFIX to ''{0}''' -f $GhcupBasePrefix)
$null = [Environment]::SetEnvironmentVariable("GHCUP_INSTALL_BASE_PREFIX", $GhcupBasePrefix, [System.EnvironmentVariableTarget]::User)


$GhcupDir = ('{0}\ghcup' -f $GhcupBasePrefix)
if ($ExistingMsys2Dir) {
  if (!(Test-Path -LiteralPath ('{0}' -f $ExistingMsys2Dir) -IsValid)) {
    Print-Msg -color Red -msg "Not a valid directory! (ExistingMsys2Dir)"
    Exit 1
  } elseif (!(Split-Path -IsAbsolute -Path "$ExistingMsys2Dir")) {
    Print-Msg -color Red -msg "Non-absolute Path specified! (ExistingMsys2Dir)"
    Exit 1
  } else {
	$MsysDir = $ExistingMsys2Dir
  }
} else {
	$MsysDir = ('{0}\msys64' -f $GhcupDir)
}

$Bash = ('{0}\usr\bin\bash' -f $MsysDir)
if (!($BootstrapUrl)) {
  $BootstrapUrl = 'https://www.haskell.org/ghcup/sh/bootstrap-haskell'
}
$GhcupMsys2 = [System.Environment]::GetEnvironmentVariable('GHCUP_MSYS2', 'user')

Print-Msg -msg 'Preparing for GHCup installation...'

# ask what to do in case ghcup is already installed
if (Test-Path -LiteralPath ('{0}' -f $GhcupDir)) {
  Print-Msg -msg ('GHCup already installed at ''{0}''...' -f $GhcupDir)
  if ($Overwrite) {
    $decision = 0
  } elseif (!($Silent)) {
    $decision = $Host.UI.PromptForChoice('Install GHCup'
                                        , 'GHCup is already installed, what do you want to do?'
                                        , [System.Management.Automation.Host.ChoiceDescription[]] @('&Reinstall'
                                            '&Continue'
                                            '&Abort'), 1)
  } else {
    $decision = 1
  }

  if ($decision -eq 0) {
    $suffix = [IO.Path]::GetRandomFileName()
    Print-Msg -msg ('Backing up {0} to {0}-{1} ...' -f $GhcupDir, $suffix)
    Rename-Item -Path ('{0}' -f $GhcupDir) -NewName ('{0}-{1}' -f $GhcupDir, $suffix)
  } elseif ($decision -eq 1) {
    Print-Msg -msg 'Continuing installation...'
  } elseif ($decision -eq 2) {
    Exit 0
  }
}


$null = New-Item -Path ('{0}' -f $GhcupDir) -ItemType 'directory' -ErrorAction SilentlyContinue
$null = New-Item -Path ('{0}' -f $GhcupDir) -Name 'bin' -ItemType 'directory' -ErrorAction SilentlyContinue

# ask for cabal dir destination
if ($CabalDir) {
  $CabDirEnv = $CabalDir
  if (!($CabDirEnv)) {
    Print-Msg -color Red -msg "No directory specified!"
    Exit 1
  } elseif (!(Split-Path -IsAbsolute -Path "$CabDirEnv")) {
    Print-Msg -color Red -msg "Invalid/Non-absolute Path specified"
    Exit 1
  }
} elseif (!($Silent)) {
  while ($true) {

    $defaultCabalDir = ('{0}\cabal' -f $GhcupBasePrefix)
    Print-Msg -color Magenta -msg ('Specify Cabal directory (this is where haskell packages end up){1}Press enter to accept the default [{0}]:' -f $defaultCabalDir, "`n")
    $CabalDirPrompt = Read-Host
    $CabDirEnv = ($defaultCabalDir,$CabalDirPrompt)[[bool]$CabalDirPrompt]

    $CabDirEnv = $CabDirEnv.TrimEnd().TrimStart()
    if (!($CabDirEnv)) {
      Print-Msg -color Red -msg "No directory specified!"
    } elseif (!(Split-Path -IsAbsolute -Path "$CabDirEnv")) {
      Print-Msg -color Red -msg "Invalid/Non-absolute Path specified"
    } else {
      Break
    }
  }
} else {
  $CabDirEnv = ('{0}\cabal' -f $GhcupBasePrefix)
}

# ask whether to install HLS
if (!($InstallHLS)) {
  if (!($Silent)) {
    $HLSdecision = $Host.UI.PromptForChoice('Install HLS'
      , 'Do you want to install the haskell-language-server (HLS) for development purposes as well?'
      , [System.Management.Automation.Host.ChoiceDescription[]] @('&Yes'
          '&No'
          '&Abort'), 1)

    if ($HLSdecision -eq 0) {
      $InstallHLS = $true
    } elseif ($HLSdecision -eq 2) {
      Exit 0
    }
  }
}

# ask whether to install stack
if (!($InstallStack)) {
  if (!($Silent)) {
    $StackDecision = $Host.UI.PromptForChoice('Install stack'
      , 'Do you want to install stack as well?'
      , [System.Management.Automation.Host.ChoiceDescription[]] @('&Yes'
          '&No'
          '&Abort'), 1)

    if ($StackDecision -eq 0) {
      $InstallStack = $true
    } elseif ($StackDecision -eq 2) {
      Exit 0
    }
  }
}

if ($Interactive) {
	$DesktopDecision = $Host.UI.PromptForChoice('Create Desktop shortcuts'
                                        , 'Do you want to create convenience desktop shortcuts (e.g. for uninstallation and msys2 shell)?'
                                        , [System.Management.Automation.Host.ChoiceDescription[]] @('&Yes'
                                            '&No'
                                            '&Abort'), 0)
	if ($DesktopDecision -eq 0) {
      $InstallDesktopShortcuts = $true
    } elseif ($DesktopDecision -eq 2) {
      Exit 0
    }
} else {
	if ($Minimal) {
      $InstallDesktopShortcuts = $false
	} elseif ($DontWriteDesktopShortcuts) {
      $InstallDesktopShortcuts = $false
	} else {
      $InstallDesktopShortcuts = $true
	}
}

# mingw foo
Print-Msg -msg 'First checking for Msys2...'
if (!(Test-Path -Path ('{0}' -f $MsysDir))) {
  if ($Silent) {
    $msys2Decision = 0
  } else {
    $msys2Decision = $Host.UI.PromptForChoice('Install MSys2'
        , 'Do you want GHCup to install a default MSys2 toolchain (recommended)?'
        , [System.Management.Automation.Host.ChoiceDescription[]] @('&Yes'
            '&No'), 0)
  }

  if ($msys2Decision -eq 0) {
    Print-Msg -msg ('...Msys2 doesn''t exist, installing into {0}' -f $MsysDir)

	Print-Msg -msg 'Starting installation in 5 seconds, this may take a while...'
	Start-Sleep -s 5

    # Download the archive
	if (!($Msys2Version)) {
		$Msys2Version = $DefaultMsys2Version
	}
	if (!($Msys2Hash)) {
		$Msys2Hash = $DefaultMsys2Hash
	}
    Print-Msg -msg ('Downloading Msys2 archive {0}...' -f $Msys2Version)
    $archive = ('msys2-base-x86_64-{0}.sfx.exe' -f $Msys2Version)
    $msysUrl = ('https://downloads.haskell.org/ghcup/msys2/{0}' -f "$archive")
    $archivePath = ('{0}\{1}' -f ([IO.Path]::GetTempPath()), "$archive")

    if ((Get-Command -Name 'curl.exe' -ErrorAction SilentlyContinue) -and !($DisableCurl)) {
      Exec "curl.exe" '-o' "$archivePath" "$msysUrl"
    } else {
      Get-FileWCSynchronous -url "$msysUrl" -destinationFolder ([IO.Path]::GetTempPath()) -includeStats
    }
	$Msys2HashChecked = Get-FileHash -Algorithm SHA256 "${archivePath}"
	if (!($Msys2HashChecked.Hash -eq $Msys2Hash)) {
		Print-Msg -color Red -msg ("Hashes don't match, got {0}, but expected {1}" -f $Msys2HashChecked, $Msys2Hash)
		Exit 1
	}

    Print-Msg -msg 'Extracting Msys2 archive...'
    $null = & "$archivePath" '-y' ('-o{0}' -f $GhcupDir)  # Extract
    # We ignore errors because we don't want the installation script to fail just because a temporary file can't be removed.
    # Relevant issue: https://github.com/haskell/ghcup-hs/issues/952
    Remove-Item -Path "$archivePath" -ErrorAction Continue

    Print-Msg -msg 'Processing MSYS2 bash for first time use...'
    Exec "$Bash" '-lc' 'exit'

    Exec "$env:windir\system32\taskkill.exe" /F /FI "MODULES eq msys-2.0.dll"

    Print-Msg -msg 'Upgrading full system...'
    Exec "$Bash" '-lc' 'pacman --noconfirm -Syuu'

    Print-Msg -msg 'Upgrading full system twice...'
    Exec "$Bash" '-lc' 'pacman --noconfirm -Syuu'

    Print-Msg -msg 'Installing Dependencies...'
    Exec "$Bash" '-lc' ('pacman --noconfirm -S --needed curl autoconf {0}' -f $PkgConf)

    Print-Msg -msg 'Updating SSL root certificate authorities...'
    Exec "$Bash" '-lc' 'pacman --noconfirm -S ca-certificates'

    Print-Msg -msg 'Setting default home directory...'
    Exec "$Bash" '-lc' "sed -i -e 's/db_home:.*$/db_home: windows/' /etc/nsswitch.conf"

  } elseif ($msys2Decision -eq 1) {
    Print-Msg -color Yellow -msg 'Skipping MSys2 installation.'
    while ($true) {
      if ($GhcupMsys2) {
        $defaultMsys2Dir = $GhcupMsys2
        Print-Msg -color Magenta -msg ('Input existing MSys2 toolchain directory.{1}Press enter to accept the default [{0}]:' -f $defaultMsys2Dir, "`n")
        $MsysDirPrompt = Read-Host
        $MsysDir = ($defaultMsys2Dir,$MsysDirPrompt)[[bool]$MsysDirPrompt]
      } else {
        Print-Msg -color Magenta -msg 'Input existing MSys2 toolchain directory:'
        $MsysDir = Read-Host
      }
      $MsysDir = $MsysDir.TrimEnd().TrimStart()
      if (!($MsysDir)) {
        Print-Msg -color Red -msg "No directory specified!"
      } elseif (!(Test-Path -LiteralPath ('{0}' -f $MsysDir))) {
        Print-Msg -color Red -msg ('MSys2 installation at ''{0}'' could not be found!' -f $MsysDir)
      } elseif (!(Split-Path -IsAbsolute -Path "$MsysDir")) {
        Print-Msg -color Red -msg "Invalid/Non-absolute Path specified"
      } else {
        Break
      }
    }
    Print-Msg -msg ('Setting GHCUP_MSYS2 env var to ''{0}''' -f $MsysDir)
    $null = [Environment]::SetEnvironmentVariable("GHCUP_MSYS2", $MsysDir, [System.EnvironmentVariableTarget]::User)
    $Bash = ('{0}\usr\bin\bash' -f $MsysDir)
  }
} else {
    Print-Msg -msg ('...Msys2 found in {0} ...skipping Msys2 installation.' -f $MsysDir)

	Print-Msg -msg 'Starting installation in 5 seconds, this may take a while...'
	Start-Sleep -s 5
}


if ($InstallDesktopShortcuts) {

	Print-Msg -msg 'Creating shortcuts...'
	$uninstallShortCut = @'
$decision = $Host.UI.PromptForChoice('Uninstall Haskell'
, 'Do you want to uninstall all of the haskell toolchain, including GHC, Cabal, Stack and GHCup itself?'
, [System.Management.Automation.Host.ChoiceDescription[]] @('&Uninstall'
    '&Abort'), 0)

if ($decision -eq 1) {
  Exit 0
}

Write-Host 'Removing ghcup toolchain' -ForegroundColor Green
ghcup nuke

Write-Host 'Unsetting GHCUP_INSTALL_BASE_PREFIX' -ForegroundColor Green
[Environment]::SetEnvironmentVariable('GHCUP_INSTALL_BASE_PREFIX', $null, [System.EnvironmentVariableTarget]::User)

$ghcupMsys2 = [System.Environment]::GetEnvironmentVariable('GHCUP_MSYS2', 'user')
$GhcupBasePrefixEnv = [System.Environment]::GetEnvironmentVariable('GHCUP_INSTALL_BASE_PREFIX', 'user')

if ($ghcupMsys2) {
  $msys2Dir = [IO.Path]::GetFullPath($ghcupMsys2)
  $baseDir = [IO.Path]::GetFullPath('{0}\ghcup' -f $GhcupBasePrefixEnv)

  if ($msys2Dir.StartsWith($baseDir)) {
    Write-Host 'Unsetting GHCUP_MSYS2' -ForegroundColor Green
    [Environment]::SetEnvironmentVariable('GHCUP_MSYS2', $null, [System.EnvironmentVariableTarget]::User)
  } else {
    Write-Host ('GHCUP_MSYS2 env variable is set to a non-standard location {0}. Environment variable not unset. Uninstall manually.' -f $msys2Dir) -ForegroundColor Magenta
  }
} else {
  Write-Host 'Unsetting GHCUP_MSYS2' -ForegroundColor Green
  [Environment]::SetEnvironmentVariable('GHCUP_MSYS2', $null, [System.EnvironmentVariableTarget]::User)
}

Write-Host 'Removing ghcup from PATH env var' -ForegroundColor Green
$path = [System.Environment]::GetEnvironmentVariable(
    'PATH',
    'user'
)
$path = ($path.Split(';') | Where-Object { $_ -ne ('{0}\bin' -f $baseDir) }) -join ';'
[System.Environment]::SetEnvironmentVariable(
    'PATH',
    $path,
    'user'
)

Write-Host 'Removing desktop files' -ForegroundColor Green
$DesktopDir = [Environment]::GetFolderPath("Desktop")
Remove-Item -LiteralPath ('{0}\Install GHC dev dependencies.lnk' -f $DesktopDir) -Force
Remove-Item -LiteralPath ('{0}\Mingw haskell shell.lnk' -f $DesktopDir) -Force
Remove-Item -LiteralPath ('{0}\Mingw package management docs.url' -f $DesktopDir) -Force

Write-Host ('CABAL_DIR env variable is still set to {0} and will be used by cabal regardless of ghcup. You may want to uninstall this manually.' -f [System.Environment]::GetEnvironmentVariable('CABAL_DIR', 'user')) -ForegroundColor Magenta
Write-Host 'You may remove this script now.' -ForegroundColor Magenta

if ($Host.Name -eq "ConsoleHost")
{
    Write-Host "Press any key to continue..."
    $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyUp") > $null
}
'@

	$GhcInstArgs = ('{0} -mintty -c "pacman --noconfirm -S --needed base-devel gettext autoconf make libtool automake python p7zip patch unzip"' -f $ShellType)
	Create-Shortcut -SourceExe ('{0}\msys2_shell.cmd' -f $MsysDir) -ArgumentsToSourceExe $GhcInstArgs -DestinationPath 'Install GHC dev dependencies.lnk' -TempPath $GhcupDir
	Create-Shortcut -SourceExe ('{0}\msys2_shell.cmd' -f $MsysDir) -ArgumentsToSourceExe $ShellType -DestinationPath 'Mingw haskell shell.lnk' -TempPath $GhcupDir
	Create-Shortcut -SourceExe 'https://www.msys2.org/docs/package-management' -ArgumentsToSourceExe '' -DestinationPath 'Mingw package management docs.url' -TempPath $GhcupDir
	$DesktopDir = [Environment]::GetFolderPath("Desktop")
	$null = New-Item -Path $DesktopDir -Name "Uninstall Haskell.ps1" -ItemType "file" -Force -Value $uninstallShortCut
}

Print-Msg -msg ('Adding {0}\bin to Users Path...' -f $GhcupDir)
Add-EnvPath -Path ('{0}\bin' -f ([System.IO.Path]::GetFullPath("$GhcupDir"))) -Container 'User'



$CabalDirFull = [System.IO.Path]::GetFullPath("$CabDirEnv")
Print-Msg -msg ('Setting CABAL_DIR to ''{0}''' -f $CabalDirFull)
$null = [Environment]::SetEnvironmentVariable("CABAL_DIR", $CabalDirFull, [System.EnvironmentVariableTarget]::User)

Print-Msg -msg 'Starting GHCup installer...'

$Msys2Shell = ('{0}\msys2_shell.cmd' -f $MsysDir)

# The bootstrap script is always silent, since we ask relevant questions here
$SilentExport = 'export BOOTSTRAP_HASKELL_NONINTERACTIVE=1 ;'

if (!($InstallStack)) {
  $StackInstallExport = 'export BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 ;'
}

if ($InstallHLS) {
  $HLSInstallExport = 'export BOOTSTRAP_HASKELL_INSTALL_HLS=1 ;'
}

if (!($NoAdjustCabalConfig)) {
  $AdjustCabalConfigExport = 'export BOOTSTRAP_HASKELL_ADJUST_CABAL_CONFIG=1 ;'
}

if ($Minimal) {
  $MinimalExport = 'export BOOTSTRAP_HASKELL_MINIMAL=1 ;'
}

if ($DisableCurl) {
  $BootstrapDownloader = 'export BOOTSTRAP_HASKELL_DOWNLOADER=wget ;'
  $DownloadScript = 'wget -O /dev/stdout'
} else {
  $DownloadScript = 'curl --proto ''=https'' --tlsv1.2 -sSf'
}

if (!($DontAdjustBashRc)) {
  $AdjustBashRcExport = 'export BOOTSTRAP_HASKELL_ADJUST_BASHRC=1 ;'
}

# set msys2 env export for the shell bootstrap script
$Msys2EnvExport = ('export GHCUP_MSYS2_ENV={0} ;' -f $Msys2Env)
# export GHCUP_MSYS2_ENV
$null = [Environment]::SetEnvironmentVariable("GHCUP_MSYS2_ENV", $Msys2Env, [System.EnvironmentVariableTarget]::User)

if ((Get-Process -ID $PID).ProcessName.StartsWith("bootstrap-haskell") -Or $InBash) {
  Exec "$Bash" '-lc' ('{4} {6} {7} {8} {9} {10} {12} {13} [ -n ''{1}'' ] && export GHCUP_MSYS2=$(cygpath -m ''{1}'') ; [ -n ''{2}'' ] && export GHCUP_INSTALL_BASE_PREFIX=$(cygpath -m ''{2}/'') ; export PATH=$(cygpath -u ''{3}/bin''):$PATH ; export CABAL_DIR=''{5}'' ; [[ ''{0}'' = https* ]]  && {11} {0} | bash || cat $(cygpath -m ''{0}'') | bash' -f $BootstrapUrl, $MsysDir, $GhcupBasePrefix, $GhcupDir, $SilentExport, $CabalDirFull, $StackInstallExport, $HLSInstallExport, $AdjustCabalConfigExport, $MinimalExport, $BootstrapDownloader, $DownloadScript, $AdjustBashRcExport, $Msys2EnvExport)
} else {
  Exec "$Msys2Shell" $ShellType '-mintty'  '-shell' 'bash' '-c' ('{4} {6} {7} {8} {9} {10} {12} {13} [ -n ''{1}'' ] && export GHCUP_MSYS2=$(cygpath -m ''{1}'') ; [ -n ''{2}'' ] && export GHCUP_INSTALL_BASE_PREFIX=$(cygpath -m ''{2}/'') ; export PATH=$(cygpath -u ''{3}/bin''):$PATH ; export CABAL_DIR=''{5}'' ; trap ''echo Press any key to exit && read -n 1 && exit'' 2 ; [[ ''{0}'' = https* ]]  && {11} {0} | bash || cat $(cygpath -m ''{0}'') | bash ; echo ''Press any key to exit'' && read -n 1' -f $BootstrapUrl, $MsysDir, $GhcupBasePrefix, $GhcupDir, $SilentExport, $CabalDirFull, $StackInstallExport, $HLSInstallExport, $AdjustCabalConfigExport, $MinimalExport, $BootstrapDownloader, $DownloadScript, $AdjustBashRcExport, $Msys2EnvExport)
}


# SIG # Begin signature block
  # MIID4QYJKoZIhvcNAQcCoIID0jCCA84CAQExCzAJBgUrDgMCGgUAMGkGCisGAQQB
  # gjcCAQSgWzBZMDQGCisGAQQBgjcCAR4wJgIDAQAABBAfzDtgWUsITrck0sYpfvNR
  # AgEAAgEAAgEAAgEAAgEAMCEwCQYFKw4DAhoFAAQUVqKek181kF/Jx/P7z176herc
  # ZyCgggH/MIIB+zCCAWSgAwIBAgIQGOezhGS1A5tHh9VubW0liDANBgkqhkiG9w0B
  # AQUFADAYMRYwFAYDVQQDDA1KdWxpYW4gT3NwYWxkMB4XDTIxMDUzMDE4Mzk1OVoX
  # DTI1MDUzMDAwMDAwMFowGDEWMBQGA1UEAwwNSnVsaWFuIE9zcGFsZDCBnzANBgkq
  # hkiG9w0BAQEFAAOBjQAwgYkCgYEAs76XCXYPM14buR1RkVKhOB8pyM4Df6kPaz75
  # nkbA0nq1VmMhBfCYFWyYHd7jniqTH0LoAKGGquN1bniREaCP9j2pFWpMIgLpQH3H
  # +jpsfmxV2BTG8q+Jok88gTXS1FlAk72E85zO/Jhr6Fja1aFYAdibBRsRxcVMTVh7
  # 4AGLNGUCAwEAAaNGMEQwEwYDVR0lBAwwCgYIKwYBBQUHAwMwHQYDVR0OBBYEFC+R
  # hdhPo0Ty5HnzHyo1pN35IfZQMA4GA1UdDwEB/wQEAwIHgDANBgkqhkiG9w0BAQUF
  # AAOBgQAl3IdBVIwbJJDp7BksMYPeM4ivB3UyNvlw8aVxGwAzNgdSaezYIdMFtKXV
  # CSv5bd4VnFRAPDJW9dhW0h3SkeJUoklUxMjKXhR3qygQhSxPDjIatAuOCffGACba
  # ZZ7Om40b+pKXc6i/HnlApk9DGbXJ59bFcLGGcZ9QjoUae6Ex1DGCAUwwggFIAgEB
  # MCwwGDEWMBQGA1UEAwwNSnVsaWFuIE9zcGFsZAIQGOezhGS1A5tHh9VubW0liDAJ
  # BgUrDgMCGgUAoHgwGAYKKwYBBAGCNwIBDDEKMAigAoAAoQKAADAZBgkqhkiG9w0B
  # CQMxDAYKKwYBBAGCNwIBBDAcBgorBgEEAYI3AgELMQ4wDAYKKwYBBAGCNwIBFTAj
  # BgkqhkiG9w0BCQQxFgQUosm9nN1JgajqSBa1cUwxxhLrAsYwDQYJKoZIhvcNAQEB
  # BQAEgYCnKzfsH1aDjS6xkC/uymjaBowHSnh6nFu2AkjcKu8RgcBZzP5SLBXgU9wm
  # aED5Ujwyq3Qre+TGVRUqwkEauDhQiX2A008G00fRO6+di6yJRCRn5eaRAbdU3Xww
  # E5VhEwLBnwzWrvLKtdEclhgUCo5Tq87QMXVdgX4aRmunl4ZE+Q==
# SIG # End signature block







