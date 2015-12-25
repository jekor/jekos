var privateKey = null;

var keyFileInput = document.getElementById('key-file');
var keyFileButton = document.getElementById('load-key');

keyFileInput.addEventListener('change', function (event) {
  var reader = new FileReader();
  reader.onload = function (event) {
    console.log(reader.result);
    window.crypto.subtle.importKey('pkcs8', reader.result, {name: 'RSASSA-PKCS1-v1_5', hash: {name: 'SHA-256'}}, true, ['sign'])
      .then(function (key) {
        privateKey = key;
        document.body.removeChild(document.getElementById('login-screen'));
        loadAppStore();
      })
      .catch(function (e) {
        console.log(e);
        alert('Failed to import private key.');
      });
  };
  reader.readAsArrayBuffer(keyFileInput.files[0]);
});

keyFileButton.addEventListener('click', function (event) {
  keyFileInput.click();
});

function loadAppStore() {
  var appStore = require('./lib/appstore');

  var apps = [{"name":"hello","description":"This is the GNU Hello World program. It outputs a greeting and requires no additional resources."},{"name":"nginx","description":"the nginx web server"},{"name":"postgresql","description":"a relational database"}];

  var storeFront = appStore(apps);
  document.body.appendChild(storeFront);
}
