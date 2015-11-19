function syscall (kernel, method, url, body, privateKey) {
  var message = method.toLowerCase() + url + body;
  var encoder = new TextEncoder('utf-8');
  var encoded = encoder.encode(message);
  window.crypto.subtle.sign('RSASSA-PKCS1-v1_5', privateKey, encoded.buffer).
    then(function (sig) {
      var xhr = new XMLHttpRequest();
      xhr.open(method, 'http://' + kernel + url, true);
      xhr.setRequestHeader('Authorization', 'JEKOS-SIG signature="' + base64FromArrayBuffer(sig) + '"');
      xhr.onload = function () {
        console.log(xhr.response);
      };
      xhr.send(body);
    }).
    catch(function () {
      alert('Failed to sign request.');
    });
}

function base64FromArrayBuffer (buffer) {
    var binary = '';
    var bytes = new Uint8Array(buffer);
    var len = bytes.byteLength;
    for (var i = 0; i < len; i++) {
        binary += String.fromCharCode(bytes[i]);
    }
    return window.btoa(binary);
}

function arrayBufferFromBase64 (base64) {
    var binary_string =  window.atob(base64);
    var len = binary_string.length;
    var bytes = new Uint8Array( len );
    for (var i = 0; i < len; i++)        {
        bytes[i] = binary_string.charCodeAt(i);
    }
    return bytes.buffer;
}

document.addEventListener('DOMContentLoaded', function () {
  'use strict';

  if (!window.crypto || !window.crypto.subtle) {
    alert('Your current browser does not support the Web Cryptography API. The installer will not work.');
    return;
  }

  // Generate a key pair.
  window.crypto.subtle.generateKey({ name: 'RSASSA-PKCS1-v1_5'
                                   , modulusLength: 2048
                                   , publicExponent: new Uint8Array([1, 0, 1]) // 24 bit representation of 65537
                                   , hash: {name: 'SHA-256'}
                                   }
                                   , true // We will extract and send the public key to the kernel and extract and save the private key locally.
                                   , ['sign', 'verify']).
    then(function (key) {
      // Send the public key to the kernel.
      window.crypto.subtle.exportKey('spki', key.publicKey).
        then(function (rawKey) {
          // In testing, I install the private key manually:
          // echo copy-pasted-key | base64 -d | sudo -s "cat > /var/publickey"
          console.log(base64FromArrayBuffer(rawKey));
          debugger;
          syscall('localhost:8080', 'GET', '/time', '', key.privateKey);
        });
    }).
    catch(function () {
      alert('Failed to generate key pair.');
    });
});
