(function () {

  // TODO: Switch to use promises or some other method to return the error.
  function launchEC2 (base64PublicKey, privateKey, callback) {
    var ec2 = new AWS.EC2();

    // TODO: Provide an option to specify a keypair for those who want
    // to be able to SSH in to the instance and examine it.
    ec2.runInstances({ ImageId: 'ami-51233f30'
                     , InstanceType: 't2.micro'
                     , MinCount: 1, MaxCount: 1
                     , UserData: base64PublicKey
                     // TODO: Build the necessary security group if it doesn't exist.
                     , SecurityGroupIds: ['sg-34784b50']
                     }, function (err, data) {
      if (err) {
        alert('Failed to create EC2 instance.');
      } else {
        var instanceId = data.Instances[0].InstanceId;
        ec2.waitFor('instanceStatusOk', {InstanceIds: [instanceId]}, function (err, data) {
          if (err) {
            alert('EC2 instance failed to start correctly.');
          } else {
            ec2.describeInstances({InstanceIds: [instanceId]}, function (err, data) {
              if (err) {
                alert('Failed to lookup IP address of EC2 instance.');
              } else {
                callback({ InstanceId: instanceId
                         , PublicDnsName: data.Reservations[0].Instances[0].PublicDnsName});
              }
            });
          }
        });
      }
    });
  }

  function syscall (address, method, url, body, privateKey, callback) {
    var message = method.toLowerCase() + url + body;
    var encoder = new TextEncoder('utf-8');
    var encoded = encoder.encode(message);
    window.crypto.subtle.sign('RSASSA-PKCS1-v1_5', privateKey, encoded.buffer).
      then(function (sig) {
        var xhr = new XMLHttpRequest();
        xhr.open(method, 'http://' + address + ':13405' + url, true);
        xhr.setRequestHeader('Authorization', 'JEKOS-SIG signature="' + base64FromArrayBuffer(sig) + '"');
        xhr.onload = function () {
          callback(xhr.response);
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
    if (!window.crypto || !window.crypto.subtle) {
      alert('Your browser does not support the Web Cryptography API. This installer will not work.');
      return;
    }

    document.getElementById('install-form').addEventListener('submit', function (event) {
      event.preventDefault();

      AWS.config.update({accessKeyId: event.target.elements['aws-access-key'].value,
                         secretAccessKey: event.target.elements['aws-secret-key'].value});

      AWS.config.region = event.target.elements['aws-region'].value;

      AWS.config.apiVersions = {
        ec2: '2015-10-01'
      };

      var output = document.getElementById('output');

      // To launch an EC2 instance:
  
      // 1. Generate a key pair.
      output.appendChild(document.createTextNode('Generating key pair ... '));
      var key = null;
      window.crypto.subtle.generateKey({ name: 'RSASSA-PKCS1-v1_5'
                                       , modulusLength: 2048
                                       , publicExponent: new Uint8Array([1, 0, 1]) // 24 bit representation of 65537
                                       , hash: {name: 'SHA-256'}
                                       }
                                       , true // We will extract and send the public key to AWS and extract and save the private key locally.
                                       , ['sign', 'verify']
                                      ).
        then(function (newKey) {
          key = newKey;

          // Allow the user to download the private key.
          window.crypto.subtle.exportKey('pkcs8', key.privateKey).
            then(function (rawKey) {
              var url = URL.createObjectURL(new Blob([rawKey], {type: 'octet/stream'}));
              var a = document.createElement('a');
              a.href = url;
              a.download = 'jekos-private-key';
              a.appendChild(document.createTextNode('Download Private Key'));
              output.appendChild(a);
              output.appendChild(document.createElement('br'));
              // To read from OpenSSL:
              // openssl pkcs8 -inform DER -nocrypt -in keyfile

              // Attach the public key to the EC2 instance.
              window.crypto.subtle.exportKey('spki', key.publicKey).
                then(function (rawKey) {
                  output.appendChild(document.createTextNode('Launching EC2 instance (this can take a while, check your EC2 console to see progress) ... '));
                  // Encode the raw key as base64.
                  launchEC2(base64FromArrayBuffer(rawKey), key.privateKey, function (data) {
                    output.appendChild(document.createTextNode(data.InstanceId));
                    output.appendChild(document.createElement('br'));
                    output.appendChild(document.createTextNode('Asking the kernel for its time ... '));
                    syscall(data.PublicDnsName, 'GET', '/time', '', key.privateKey, function (time) {
                      output.appendChild(document.createTextNode((new Date(parseInt(time, 10) * 1000)).toLocaleString()));
                      output.appendChild(document.createElement('br'));
                      output.appendChild(document.createElement('br'));
                      output.appendChild(document.createTextNode('Your JekOS system is now reachable at '));
                      var a = document.createElement('a');
                      a.href = 'http://' + data.PublicDnsName + ':13405/';
                      a.appendChild(document.createTextNode(data.PublicDnsName));
                      output.appendChild(a);
                      output.appendChild(document.createTextNode('. Make sure to download your private key first or you won''t be able to log in.');
                    });
                  });
                }).
                catch(function () {
                  alert('Failed to export public key.');
                });
            }).
            catch(function () {
              alert('Failed to export private key.');
            });
        }).
        catch(function () {
          alert('Failed to generate key pair.');
        });

      // 2. Place the public key in the EC2 instance's user data.
    });
  });
})();
