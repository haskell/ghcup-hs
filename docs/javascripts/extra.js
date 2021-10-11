var platforms = ["win", "unix"];
var platform_override = null;

function detect_platform() {
    "use strict";

    if (platform_override !== null) {
        return "unknown";
    }

    var os = "unknown";

    if (navigator.platform == "Linux x86_64")         {os = "unix";}
    if (navigator.platform == "Linux i686")           {os = "unix";}
    if (navigator.platform == "Linux i686 on x86_64") {os = "unix";}
    if (navigator.platform == "Linux aarch64")        {os = "unix";}
    if (navigator.platform == "Linux armv6l")         {os = "unix";}
    if (navigator.platform == "Linux armv7l")         {os = "unix";}
    if (navigator.platform == "Linux armv8l")         {os = "unix";}
    if (navigator.platform == "Linux ppc64")          {os = "unix";}
    if (navigator.platform == "Linux mips")           {os = "unix";}
    if (navigator.platform == "Linux mips64")         {os = "unix";}
    if (navigator.platform == "Mac")                  {os = "unix";}
    if (navigator.platform == "Win32")                {os = "win";}
    if (navigator.platform == "Win64" ||
        navigator.userAgent.indexOf("WOW64") != -1 ||
        navigator.userAgent.indexOf("Win64") != -1)   {os = "win";}
    if (navigator.platform == "FreeBSD x86_64")       {os = "unix";}
    if (navigator.platform == "FreeBSD amd64")        {os = "unix";}
    // if (navigator.platform == "NetBSD x86_64") {os = "unix";}
    // if (navigator.platform == "NetBSD amd64") {os = "unix";}

    // I wish I knew by now, but I don't. Try harder.
    if (os == "unknown") {
        if (navigator.appVersion.indexOf("Win")!=-1)     {os = "win";}
        if (navigator.appVersion.indexOf("Mac")!=-1)     {os = "unix";}
        if (navigator.appVersion.indexOf("FreeBSD")!=-1) {os = "unix";}
    }

    // Firefox Quantum likes to hide platform and appVersion but oscpu works
    if (navigator.oscpu) {
        if (navigator.oscpu.indexOf("Win32")!=-1)   {os = "win";}
        if (navigator.oscpu.indexOf("Win64")!=-1)   {os = "win";}
        if (navigator.oscpu.indexOf("Mac")!=-1)     {os = "unix";}
        if (navigator.oscpu.indexOf("Linux")!=-1)   {os = "unix";}
        if (navigator.oscpu.indexOf("FreeBSD")!=-1) {os = "unix";}
        // if (navigator.oscpu.indexOf("NetBSD")!=-1) {os = "unix";}
    }

    return os;
}

function adjust_for_platform() {
    "use strict";

    var platform = detect_platform();

	if (platforms.includes(platform)) {
		platforms.forEach(function (platform_elem) {
			var platform_div = document.getElementById("ghcup-instructions-" + platform_elem);
			platform_div.style.display = "none";
			if (platform == platform_elem) {
				platform_div.style.display = "block";
			}
		});
	}
}


function show_all_platforms() {
	platforms.forEach(function (platform_elem) {
		var platform_div = document.getElementById("ghcup-instructions-" + platform_elem);
		platform_div.style.display = "block";
	});
	
	var buttons = document.getElementsByClassName("show-all-platforms");
	console.log(buttons);
	Array.from(buttons).forEach(function (button) {
		button.style.display = "none";
	});

}

function set_up_default_platform_buttons() {
    var defaults_buttons = document.getElementsByClassName('show-all-platforms-button');
    for (var i = 0; i < defaults_buttons.length; i++) {
        defaults_buttons[i].onclick = show_all_platforms;
    }
}

function copyToClipboardNux() {
  const text = document.getElementById("ghcup-command-linux").innerText;
  const el = document.createElement('textarea');
  el.value = text;
  document.body.appendChild(el);
  el.select();
  document.execCommand('copy');
  document.body.removeChild(el);
  const button = document.getElementById("ghcup-linux-button");
  button.focus();
}

function copyToClipboardWin() {
  const text = document.getElementById("ghcup-command-windows").innerText;
  const el = document.createElement('textarea');
  el.value = text;
  document.body.appendChild(el);
  el.select();
  document.execCommand('copy');
  document.body.removeChild(el);
  const button = document.getElementById("ghcup-windows-button");
  button.focus();
}

(function () {
    adjust_for_platform();
    set_up_default_platform_buttons();
}());

