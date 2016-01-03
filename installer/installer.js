(function () {

  // TODO: Switch to use promises or some other method to return the error.
  function launchEC2 (name, userData, callback) {
    var ec2 = new AWS.EC2();

    // TODO: Provide an option to specify a keypair for those who want
    // to be able to SSH in to the instance and examine it.
    ec2.runInstances({ ImageId: 'ami-ee31298f' // 'ami-51233f30'
                     , InstanceType: 't2.micro'
                     , MinCount: 1, MaxCount: 1
                     , UserData: base64JSON(userData)
                     // TODO: Build the necessary security group if it doesn't exist.
                     , SecurityGroupIds: ['sg-34784b50']
                     }, function (err, data) {
      if (err) {
        alert('Failed to create EC2 instance.');
      } else {
        var instanceId = data.Instances[0].InstanceId;
        ec2.createTags({ Resources: [instanceId]
                       , Tags: [{Key: 'Name', Value: name}] }, function(err, data) {
          if (err) console.log(err, err.stack); // an error occurred
          else     console.log(data);           // successful response
        });
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

  function setPassword (domain, password, privateKey, callback) {
    var message = 'PUT' + '/password' + password;
    var encoder = new TextEncoder('utf-8');
    var encoded = encoder.encode(message);
    window.crypto.subtle.sign('RSASSA-PKCS1-v1_5', privateKey, encoded.buffer).
      then(function (sig) {
        var xhr = new XMLHttpRequest();
        xhr.open('PUT', 'https://' + domain + '/password', true);
        xhr.setRequestHeader('Authorization', 'JEKOS-SIG signature="' + base64FromArrayBuffer(sig) + '"');
        xhr.setRequestHeader('Content-Type', 'text/plain; charset=utf-8');
        xhr.onload = function () {
          callback(xhr.status == 201);
        };
        xhr.send(password);
      }).
      catch(function (e) {
        console.log(e);
        alert('Failed to sign request.');
      });
  }

  // This only works with non-Unicode data.
  function base64JSON (data) {
    return btoa(JSON.stringify(data));
  }
  
  function base64FromArrayBuffer (buffer) {
      var binary = '';
      var bytes = new Uint8Array(buffer);
      var len = bytes.byteLength;
      for (var i = 0; i < len; i++) {
          binary += String.fromCharCode(bytes[i]);
      }
      return btoa(binary);
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

      var subdomain = event.target.elements['subdomain'].value;
      var password = event.target.elements['password'].value;

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
          // window.crypto.subtle.exportKey('pkcs8', key.privateKey).
          //   then(function (rawKey) {
              // var url = URL.createObjectURL(new Blob([rawKey], {type: 'octet/stream'}));
              // var a = document.createElement('a');
              // a.href = url;
              // a.download = 'jekos-private-key';
              // a.appendChild(document.createTextNode('Download Private Key'));
              // output.appendChild(a);
              // output.appendChild(document.createElement('br'));
              // To read from OpenSSL:
              // openssl pkcs8 -inform DER -nocrypt -in keyfile

              output.appendChild(document.createTextNode('done.'));
              output.appendChild(document.createElement('br'));
              // Attach the public key to the EC2 instance.
              window.crypto.subtle.exportKey('spki', key.publicKey).
                then(function (rawKey) {
                  output.appendChild(document.createTextNode('Launching EC2 instance (this can take a while, check your EC2 console to see progress) ... '));
                  // Encode the raw key as base64.
                  launchEC2(subdomain, {publicKey: base64FromArrayBuffer(rawKey), subdomain: subdomain}, function (data) {
                    output.appendChild(document.createTextNode(data.InstanceId));
                    output.appendChild(document.createElement('br'));
                    output.appendChild(document.createTextNode('Setting password ... '));
                    setPassword(subdomain + '.jekos.net', password, key.privateKey, function (success) {
                      if (success) {
                        output.appendChild(document.createTextNode('done.');
                        output.appendChild(document.createElement('br'));
                        output.appendChild(document.createElement('br'));
                        output.appendChild(document.createTextNode('Your JekOS network is now reachable at '));
                        var a = document.createElement('a');
                        var url = 'https://' + subdomain + '.jekos.net/';
                        a.href = url;
                        a.appendChild(document.createTextNode(url));
                        output.appendChild(a);
                      } else {
                        output.appendChild(document.createTextNode('failed to set password.'));
                        // TODO: This could be for multiple reasons. Provide options from here.
                      }
                    });
                  });
                }).
                catch(function () {
                  alert('Failed to export public key.');
                });
            // }).
            // catch(function () {
            //   alert('Failed to export private key.');
            // });
        }).
        catch(function () {
          alert('Failed to generate key pair.');
        });

      // 2. Place the public key in the EC2 instance's user data.
    });
  });
})();
