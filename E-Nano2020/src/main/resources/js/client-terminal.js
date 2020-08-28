// Terminal for client
var editor = ace.edit("terminal");
editor.setTheme("ace/theme/terminal");
document.getElementById("terminal").style.fontSize = "18px";
editor.setHighlightActiveLine(false);
editor.session.setValue(
  "Nano-E 1.0.0 (Default, Ago 27 2020, 12:08:20)\n" + "> fact(5)=120"
);
editor.setReadOnly(true);
