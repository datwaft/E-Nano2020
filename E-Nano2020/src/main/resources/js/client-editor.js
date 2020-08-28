// Editor for client
var editor = ace.edit("editor");
editor.setTheme("ace/theme/terminal");
editor.session.setMode("ace/mode/c_cpp"); // lenguage highlight syntax
document.getElementById("editor").style.fontSize = "18px";
editor.session.setTabSize(4);
editor.session.setValue(
  "val < int -> int > fact  = n ->  when ( n )  {\n" +
    "\t\t  0  ->  1\n" +
    "\t\t| _  ->  n * fact( n - 1 )\n" +
    "    }\n\n" +
    "main{\n" +
    '\t"fact(%s)=%d".format(5, fact(5)).println()\n' +
    "}\n"
);
//GET CODE
var code = editor.getValue();
console.log(code);
