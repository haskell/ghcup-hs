---
hide:
  - navigation
  - toc
---

# ![](./haskell_logo.png){: .main-logo style="width:100px"} GHCup

GHCup is an installer for the general purpose language [Haskell](https://www.haskell.org/).

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

<div class="wrap-collabsible text-center"> <input id="collapsible" class="toggle" type="checkbox"> 
<label for="collapsible" class="lbl-toggle btn btn-primary">Quick Install</label>
<a href="install/" class="btn btn-primary" role="button">Getting Started</a>
<a href="guide/" class="btn btn-primary" role="button">User Guide</a>
<a href="https://gitlab.haskell.org/haskell/ghcup-hs/-/issues" class="btn btn-primary" role="button">Issue tracker</a>
  <div class="collapsible-content">
    <div class="content-inner">
      <h2>Select your platform</h3>
		<div class="platforms">
			<div class="platform">
				<div class="platform-name"><img src="os-linux.svg" alt="Linux logo"> <h3>Linux</h3></div>
				<a class="expander" href="#linux-link">
					<div>
						<img src="expand-piece.svg" class="expand-1">
						<img src="expand-piece.svg" class="expand-2">
						<img src="expand-piece.svg" class="expand-3">
					</div>
				</a>
			</div>

			<div class="platform">
				<div class="platform-name"><img src="os-osx.svg" alt="Mac logo"> <h3>Mac OS X</h3></div>
					<a class="expander" href="#mac-link">
						<div>
							<img src="expand-piece.svg" class="expand-1">
							<img src="expand-piece.svg" class="expand-2">
							<img src="expand-piece.svg" class="expand-3">
						</div>
					</a>
			</div>

			<div class="platform">
				<div class="platform-name"><img src="os-freebsd.svg" alt="FreeBSD logo" style="fill: black;"> <h3>FreeBSD</h3></div>
				<a class="expander" href="#freebsd-link">
					<div>
						<img src="expand-piece.svg" class="expand-1">
						<img src="expand-piece.svg" class="expand-2">
						<img src="expand-piece.svg" class="expand-3">
					</div>
				</a>
			</div>

			<div class="platform">
				<div class="platform-name"><img src="os-windows.svg" alt="Windows logo"> <h3>Windows</h3></div>
				<a class="expander" href="#windows-link">
					<div>
						<img src="expand-piece.svg" class="expand-1">
						<img src="expand-piece.svg" class="expand-2">
						<img src="expand-piece.svg" class="expand-3">
					</div>
				</a>
			</div>

			<div class="platform">
				<div class="platform-name"><img src="os-windows.svg" alt="Windows logo"> <h3>WSL2</h3></div>
				<a class="expander" href="#wsl-link">
					<div>
						<img src="expand-piece.svg" class="expand-1">
						<img src="expand-piece.svg" class="expand-2">
						<img src="expand-piece.svg" class="expand-3">
					</div>
				</a>
			</div>

		</div>

		<div id="linux-link">
			<hr/>
			<h4>On Linux, run the following in your terminal (as a user other than root), then follow the onscreen instructions:</h4>
			<div class="command-button">
				<pre><span class="ghcup-command">curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh</span></pre>
			</div>
		</div>
		<div id="mac-link">
			<hr/>
			<h4>On Mac, run the following in your terminal (as a user other than root), then follow the onscreen instructions:</h4>
			<div class="command-button">
				<pre><span class="ghcup-command">curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh</span></pre>
			</div>
		</div>
		<div id="freebsd-link">
			<hr/>
			<h4>On FreeBSD, run the following in your terminal (as a user other than root), then follow the onscreen instructions:</h4>
			<div class="command-button">
				<pre><span class="ghcup-command">curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh</span></pre>
			</div>
		</div>
		<div id="windows-link" class="anchor">
			<hr/>
			<h4>On Windows, run the following in a powershell session (as a non-admin user), then follow the onscreen instructions:</h4>
			<div class="command-button">
				<pre><span class="ghcup-command">Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;Invoke-Command -ScriptBlock ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -ArgumentList $true</span></pre>
			</div>
		</div>

		<div id="wsl-link">
			<hr/>
			<h4>On WSL2, run the following in your terminal (as a user other than root), then follow the onscreen instructions:</h4>
			<div class="command-button">
				<pre><span class="ghcup-command">curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh</span></pre>
			</div>
		</div>
    </div>

  </div>
</div>


----

![GHCup](./ghcup.gif){: .center style="width:700px"}

