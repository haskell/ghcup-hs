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
