---
hide:
  - navigation
  - toc
---

<section class="index-ghcup-hero">
  <img alt="haskell logo" src="./haskell_logo.png" />
  <h1>GHCup</h1>
</section>

<p class="ghcup-intro">GHCup is the main installer for the general purpose language <a href="https://www.haskell.org">Haskell</a>.</p>

<div class="text-center main-buttons">
<a href="install/" class="btn btn-primary" role="button">Installation</a>
<a href="steps/" class="btn btn-primary" role="button">First steps</a>
<a href="guide/" class="btn btn-primary" role="button">User Guide</a>
</div>

<section class="qi-container">

    <div class="ghcup-os-container" id="ghcup-instructions-unix">
      <h3>To install on Linux, macOS, FreeBSD or <a href="https://docs.microsoft.com/en-us/windows/wsl/"> WSL2</a></h3>
      <p>run the following in a terminal (as a non-root user):<p>
      <div class="command-button">
	    <pre>
            <span class="ghcup-command" id="ghcup-command-linux">curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh</span>
          </pre>
        <button class="btn" onclick="copyToClipboardNux()" id="ghcup-linux-button"><i class="fa fa-copy"></i></button>
      </div>
      <span>
      </span>
	<div class="footer">
		<a href="https://github.com/haskell/ghcup-hs/blob/master/scripts/bootstrap/bootstrap-haskell" target="_blank">What does this do?</a> <b>&nbsp;&middot;&nbsp;</b> <a href="https://www.haskell.org/ghcup/install/#manual-installation">I don't like curl | sh</a> <div class="show-all-platforms"><b>&nbsp;&middot;&nbsp;</b> <a class="show-all-platforms-button" href="#">Show all platforms</a></div></p>
	</div>
    </div>

    <div class="ghcup-os-container" id="ghcup-instructions-win">
      <h3>To install on Windows</h3>
      <p>run the following in a PowerShell session (as a non-admin user):<p>

      <div class="command-button">
	    <pre>
          <span class="ghcup-command" id="ghcup-command-windows">Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
          </span>
        </pre>
        <button class="btn" onclick="copyToClipboardWin()" id="ghcup-windows-button"><i class="fa fa-copy"></i></button>
      </div>
	<div class="footer">
		<a href="https://github.com/haskell/ghcup-hs/blob/master/scripts/bootstrap/bootstrap-haskell.ps1" target="_blank">What does this do?</a> <b>&nbsp;&middot;&nbsp;</b> <a href="https://www.haskell.org/ghcup/install/#manual-installation">I don't like curl | sh</a> <div class="show-all-platforms"><b>&nbsp;&middot;&nbsp;</b> <a class="show-all-platforms-button" href="#">Show all platforms</a></div></p>
	</div>
    </div>
</section>

<p id="help" class="ghcup-help">
  Need help? Check the <a href="guide/#troubleshooting">Troubleshooting section</a> or ask on
  <span>
    <a href="https://kiwiirc.com/nextclient/irc.libera.chat/?nick=Guest%7C?#haskell,#haskell-ghcup">
      <img src="irc.svg" alt="" />
      IRC
    </a>
  </span>,
  <span>
    <a href="https://discord.gg/WDqsWsnZfR">
      <img src="Discord-Logo-Black.svg" alt="" />
      Discord
    </a>
  </span>,
  <span>
    <a href="https://matrix.to/#/#ghcup:matrix.org">
      <img src="Matrix_logo.svg" alt=""/>
    </a>
  </span>
   or
   <span>
     <a href="https://github.com/haskell/ghcup-hs/issues">
       report a bug
       <img src="Octicons-bug.svg" alt="" />
     </a>
   </span>
</p>

----


![GHCup](./ghcup.gif){: .center style="width:700px"}

<section class="index-cta-donate">
  <button class="donate-button">
    <a href="https://opencollective.com/ghcup#category-CONTRIBUTE" class="donate-badge" />
	</a>
    </button>
</section>
