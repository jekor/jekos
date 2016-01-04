var Terminal = require('term.js');
var term = new Terminal({
  cols: 80,
  rows: 24,
  useStyle: true
});
var socket = new WebSocket('wss://' + location.host + '/terminal');

term.on('data', function(data) {
  socket.send(data);
});
term.on('title', function(title) {
  document.title = title;
});
term.open(document.getElementById('terminal'));

socket.onmessage = function (e) {
  term.write(e.data);
};
socket.onclose = function (e) {
  term.write('-disconnected-');
};

document.getElementById('logout').addEventListener('click', function () {
  var xhr = new XMLHttpRequest();
  xhr.open('DELETE', '/authtoken', true);
  var timeout = setTimeout(function () { reject(); }, 30000);
  xhr.onload = function () { location.reload(); };
  xhr.send();
});
