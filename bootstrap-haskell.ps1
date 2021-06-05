function Print-Msg {
  param ( [Parameter(Mandatory=$true, HelpMessage='String to output')][string]$msg )
  Write-Host ('{0}' -f $msg) -ForegroundColor Green
}

function Create-Shortcut {
    param ( [Parameter(Mandatory=$true,HelpMessage='Target path')][string]$SourceExe, [Parameter(Mandatory=$true,HelpMessage='Arguments to the path/exe')][AllowEmptyString()]$ArgumentsToSourceExe, [Parameter(Mandatory=$true,HelpMessage='The destination of the desktop link')][string]$DestinationPath )
    $WshShell = New-Object -comObject WScript.Shell
    $Shortcut = $WshShell.CreateShortcut($DestinationPath)
    $Shortcut.TargetPath = $SourceExe
    if($ArgumentsToSourceExe) {
      $Shortcut.Arguments = $ArgumentsToSourceExe
    }
    $Shortcut.Save()
}

function Add-EnvPath {
    param(
        [Parameter(Mandatory=$true,HelpMessage='The Pathe to add to Users environment')]
        [string] $Path,

        [ValidateSet('Machine', 'User', 'Session')]
        [string] $Container = 'Session'
    )

    function Where-Something
    {
      param
      (
        [Parameter(Mandatory=$true, ValueFromPipeline=$true, HelpMessage='Data to filter')]
        $InputObject
      )
      process
      {
        if ($InputObject)
        {
          $InputObject
        }
      }
    }

  if ($Container -ne 'Session') {
        $containerMapping = @{
            Machine = [EnvironmentVariableTarget]::Machine
            User = [EnvironmentVariableTarget]::User
        }
        $containerType = $containerMapping[$Container]

        $persistedPaths = [Environment]::GetEnvironmentVariable('Path', $containerType) -split ';'
        if ($persistedPaths -notcontains $Path) {
            $persistedPaths = $persistedPaths + $Path | Where-Something
            [Environment]::SetEnvironmentVariable('Path', $persistedPaths -join ';', $containerType)
        }
    }

    $envPaths = $env:Path -split ';'
    if ($envPaths -notcontains $Path) {
        $envPaths = $envPaths + $Path | Where-Something
        $env:Path = $envPaths -join ';'
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

$ErrorActionPreference = 'Stop'

$GhcupDir = "$env:HOMEDRIVE\ghcup"
$MsysDir = ('{0}\msys64' -f $GhcupDir)
$Bash = ('{0}\usr\bin\bash' -f $MsysDir)

Print-Msg -msg 'Preparing for GHCup installation...'

if (Test-Path -Path ('{0}' -f $GhcupDir)) {
  $decision = $Host.UI.PromptForChoice('Install', 'GHCup is already installed, what do you want to do?', @('&Reinstall'
                                                                                                           '&Continue'
                                                                                                           '&Abort'), 1)
  if ($decision -eq 0) {
    $suffix = [IO.Path]::GetRandomFileName()
    Print-Msg -msg ('Backing up {0} to {0}-{1} ...' -f $GhcupDir, $suffix)
    Rename-Item -Path ('{0}' -f $GhcupDir) -NewName ('{0}-{1}' -f $GhcupDir, $suffix)
  } elseif ($decision -eq 1) {
    Print-Msg -msg 'Continuing installation...'
  } elseif ($decision -eq 2) {
    Exit
  }
}


$null = New-Item -Path ('{0}' -f $GhcupDir) -ItemType 'directory' -ErrorAction SilentlyContinue
$null = New-Item -Path ('{0}' -f $GhcupDir) -Name 'bin' -ItemType 'directory' -ErrorAction SilentlyContinue

Print-Msg -msg 'First checking for Msys2...'

if (!(Test-Path -Path ('{0}' -f $MsysDir))) {
    Print-Msg -msg ('...Msys2 doesn''t exist, installing into {0} ...this may take a while' -f $MsysDir)

    # Download the archive
    Print-Msg -msg 'Downloading Msys2 archive...'
    $archive = 'msys2-x86_64-latest.sfx.exe'
    
    if (Get-Command -Name 'curl.exe' -ErrorAction SilentlyContinue) {
      curl.exe -o ('{0}\{1}' -f $env:TEMP, $archive) ('https://repo.msys2.org/distrib/{0}' -f $archive)
    } else {
      Get-FileWCSynchronous -url ('https://repo.msys2.org/distrib/{0}' -f $archive) -destinationFolder "$env:TEMP" -includeStats
    }

    Print-Msg -msg 'Extracting Msys2 archive...'
    $null = & "$env:TEMP\$archive" '-y' ('-o{0}' -f $GhcupDir)  # Extract
    Remove-Item -Path ('{0}/{1}' -f $env:TEMP, $archive)

    Print-Msg -msg 'Processing MSYS2 bash for first time use...'
    & "$Bash" -lc 'exit'

    & "$env:windir\system32\taskkill.exe" /F /FI `"MODULES eq msys-2.0.dll`"

    Print-Msg -msg 'Upgrading full system...'
    & "$Bash" -lc 'pacman --noconfirm -Syuu'

    Print-Msg -msg 'Upgrading full system twice...'
    & "$Bash" -lc 'pacman --noconfirm -Syuu'

    Print-Msg -msg 'Installing GHC Build Dependencies...'
    & "$Bash" -lc 'pacman --noconfirm -S --needed git tar curl wget base-devel gettext binutils autoconf make libtool automake python p7zip patch unzip mingw-w64-x86_64-toolchain mingw-w64-x86_64-gcc mingw-w64-x86_64-gdb mingw-w64-x86_64-python2 mingw-w64-x86_64-python3-sphinx'

    Print-Msg -msg 'Updating SSL root certificate authorities...'
    & "$Bash" -lc 'pacman --noconfirm -S ca-certificates'

    Print-Msg -msg 'Setting default home directory...'
    & "$Bash" -lc "sed -i -e 's/db_home:.*$/db_home: windows/' /etc/nsswitch.conf"
} else {
    Print-Msg -msg ('...Msys2 found in {0} ...skipping Msys2 installation.' -f $MsysDir)
}

Print-Msg -msg 'Creating shortcuts...'
Create-Shortcut -SourceExe ('{0}\msys2_shell.cmd' -f $MsysDir) -ArgumentsToSourceExe '-mingw64' -DestinationPath ('{0}\Desktop\Mingw haskell shell.lnk' -f $HOME)
Create-Shortcut -SourceExe 'https://www.msys2.org/docs/package-management' -ArgumentsToSourceExe '' -DestinationPath ('{0}\Desktop\Mingw package management docs.url' -f $HOME)

Print-Msg -msg ('Adding {0}\bin to Users Path...' -f $GhcupDir)
Add-EnvPath -Path ('{0}\bin' -f $GhcupDir) -Container 'User'

Print-Msg -msg 'Starting GHCup installer...'
& "$Bash" -lc "export PATH=`"/c/ghcup/bin:`$PATH`" ; curl --proto =https --tlsv1.2 -sSf https://gitlab.haskell.org/haskell/ghcup-hs/-/raw/windows-support/bootstrap-haskell | bash"
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
