---
hide:
  - navigation
  - toc
---

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<script src="javascripts/extra.js"></script>


# ![](./haskell_logo.png){: .main-logo style="width:100px"} GHCup

<p class="ghcup-intro">GHCup is an installer for the general purpose language <a href="https://www.haskell.org">Haskell</a>.</p>

<div class="text-center main-buttons">
<a href="install/" class="btn btn-primary" role="button">Getting Started</a>
<a href="guide/" class="btn btn-primary" role="button">User Guide</a>
<a href="https://gitlab.haskell.org/haskell/ghcup-hs/-/issues" class="btn btn-primary" role="button">Issue tracker</a>
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
		<a href="https://gitlab.haskell.org/haskell/ghcup-hs/-/blob/master/scripts/bootstrap/bootstrap-haskell" target="_blank">What does this do?</a> <b>&nbsp;&middot;&nbsp;</b> <a href="https://www.haskell.org/ghcup/install/#manual-install">I don't like curl | sh</a> <div class="show-all-platforms"><b>&nbsp;&middot;&nbsp;</b> <a class="show-all-platforms-button" href="#">Show all platforms</a></div></p>
	</div>
    </div>

    <div class="ghcup-os-container" id="ghcup-instructions-win">
      <h3>To install on Windows</h3>
      <p>run the following in a PowerShell session (as a non-admin user):<p>

      <div class="command-button">
	    <pre>
          <span class="ghcup-command" id="ghcup-command-windows">Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
          </span>
        </pre>
        <button class="btn" onclick="copyToClipboardWin()" id="ghcup-windows-button"><i class="fa fa-copy"></i></button>
      </div>
	<div class="footer">
		<a href="https://gitlab.haskell.org/haskell/ghcup-hs/-/blob/master/scripts/bootstrap/bootstrap-haskell.ps1" target="_blank">What does this do?</a> <b>&nbsp;&middot;&nbsp;</b> <a href="https://www.haskell.org/ghcup/install/#manual-install">I don't like curl | sh</a> <div class="show-all-platforms"><b>&nbsp;&middot;&nbsp;</b> <a class="show-all-platforms-button" href="#">Show all platforms</a></div></p>
	</div>
    </div>


</section>

<p id="help">
Need help? Ask on <a href="https://kiwiirc.com/nextclient/irc.libera.chat/?nick=Guest%7C?#haskell,#haskell-ghcup"><img src="irc.svg" alt="" />IRC</a>, <a href="https://discord.gg/pKYf3zDQU7"><img src="Discord-Logo-Black.svg" alt="" />Discord</a>, <a href="https://app.element.io/#/room/#haskell-tooling:matrix.org"><img src="Matrix_logo.svg" alt=""/></a> or <a href="https://gitlab.haskell.org/haskell/ghcup-hs/issues">report a bug <img src="Octicons-bug.svg" alt="" /></a>
<div id="collective" href="https://opencollective.com/ghcup#category-CONTRIBUTE" target="_blank">
<a href="https://opencollective.com/ghcup#category-CONTRIBUTE" class="donate-badge">
<img src="https://opencollective.com/webpack/donate/button@2x.png?color=blue" alt="Donate">
</a>
</div>
</p>

----

![GHCup](./ghcup.gif){: .center style="width:700px"}

