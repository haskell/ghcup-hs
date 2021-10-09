---
hide:
  - navigation
  - toc
---

<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">
<script src="javascripts/extra.js"></script>


# ![](./haskell_logo.png){: .main-logo style="width:100px"} GHCup

<p class="ghcup-intro">GHCup is an installer for the general purpose language <a href="https://www.haskell.org">Haskell</a>.</p>

<div class="text-center gh-badge">
<a href="https://kiwiirc.com/nextclient/irc.libera.chat/?nick=Guest%7C?#haskell,#haskell-ghcup"><img src="https://img.shields.io/badge/chat-on%20libera%20IRC-brightgreen.svg" alt="Join the chat at Libera.chat"></a>
<a href="https://app.element.io/#/room/#haskell-tooling:matrix.org"><img src="https://img.shields.io/matrix/haskell-tooling:matrix.org?label=chat%20on%20matrix.org" alt="Join the chat at Matrix.org"></a>
<a href="https://discord.gg/pKYf3zDQU7"><img src="https://img.shields.io/discord/280033776820813825?label=chat%20on%20discord" alt="Join the chat at Discord"></a>
<a href="https://gitter.im/haskell/ghcup?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge"><img src="https://badges.gitter.im/haskell/ghcup.svg" alt="Join the chat at https://gitter.im/haskell/ghcup"></a>
<a href="https://opencollective.com/ghcup#category-CONTRIBUTE" class="donate-badge"><img src="https://opencollective.com/webpack/donate/button@2x.png?color=blue" alt="Donate"></a>
</div>

----

GHCup makes it easy to install specific versions of GHC on GNU/Linux,
macOS (aka Darwin), FreeBSD and Windows and can also bootstrap a fresh [Haskell developer environment](./install/#supported-tools) from scratch.
It follows the unix UNIX philosophy of [do one thing and do it well](https://en.wikipedia.org/wiki/Unix_philosophy#Do_One_Thing_and_Do_It_Well). Similar in scope to [rustup](https://github.com/rust-lang-nursery/rustup.rs), [pyenv](https://github.com/pyenv/pyenv) and [jenv](http://www.jenv.be).

<div class="text-center main-buttons">
<a href="#quick-install" class="btn btn-primary" role="button">Quick Install</a>
<a href="install/" class="btn btn-primary" role="button">Getting Started</a>
<a href="guide/" class="btn btn-primary" role="button">User Guide</a>
<a href="https://gitlab.haskell.org/haskell/ghcup-hs/-/issues" class="btn btn-primary" role="button">Issue tracker</a>
</div>

[![GHCup](./ghcup.gif){: .center style="width:700px"}](install#installation)

<section class="qi-container">
    <h2 class="text-center" id="quick-install">Quick Install<a class="headerlink" href="#quick-install" title="Permanent link"></a></h2>

    <div class="ghcup-os-container">
      <h3>Linux, macOS, FreeBSD or <a href="https://docs.microsoft.com/en-us/windows/wsl/"> WSL2 </a></h3>
      <p>Run the following in a terminal (as a non-root user):<p>
      <div class="command-button">
	    <pre>
            <span class="ghcup-command" id="ghcup-command-linux">curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh</span>
          </pre>
        <button class="btn" onclick="copyToClipboardNux()" id="ghcup-linux-button"><i class="fa fa-copy"></i></button>
      </div>
      <span>
      </span>
    </div>

    <div class="ghcup-os-container">
      <h3>Windows</h3>
      <p>Run the following in a PowerShell session (as a non-admin user):<p>

      <div class="command-button">
	    <pre>
          <span class="ghcup-command" id="ghcup-command-windows">Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true
          </span>
        </pre>
        <button class="btn" onclick="copyToClipboardWin()" id="ghcup-windows-button"><i class="fa fa-copy"></i></button>
      </div>
    </div>
</section>
