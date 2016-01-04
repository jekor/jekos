(function () {

  function launchEC2 (name, userData) {
    return new Promise(function (resolve, reject) {

      var ec2 = new AWS.EC2();

      ec2.runInstances({ ImageId: 'ami-39c9d158'
                       , InstanceType: 't2.nano'
                       , MinCount: 1, MaxCount: 1
                       , UserData: base64JSON(userData)
                       // TODO: Build the necessary security group if it doesn't exist.
                       , SecurityGroupIds: ['sg-34784b50']
                       }, function (err, data) {
        if (err) {
          console.log(err);
          reject();
        } else {
          var instanceId = data.Instances[0].InstanceId;
          ec2.createTags({ Resources: [instanceId]
                         , Tags: [{Key: 'Name', Value: name}] }, function(err, data) {
            if (err) { console.log(err, err.stack); }
          });
          ec2.waitFor('instanceStatusOk', {InstanceIds: [instanceId]}, function (err, data) {
            if (err) {
              console.log(err);
              reject();
            } else {
              ec2.describeInstances({InstanceIds: [instanceId]}, function (err, data) {
                if (err) {
                  console.log(err);
                  reject();
                } else {
                  resolve({ InstanceId: instanceId
                          , PublicDnsName: data.Reservations[0].Instances[0].PublicDnsName});
                }
              });
            }
          });
        }
      });
    });
  }

  function setPassword (domain, password, privateKey, callback) {
    return new Promise(function (resolve, reject) {
      var message = 'PUT' + '/password' + password;
      var encoder = new TextEncoder('utf-8');
      var encoded = encoder.encode(message);
      window.crypto.subtle.sign('RSASSA-PKCS1-v1_5', privateKey, encoded.buffer).
        then(function (sig) {
          var xhr = new XMLHttpRequest();
          xhr.open('PUT', 'https://' + domain + '/password', true);
          xhr.setRequestHeader('Authorization', 'JEKOS-SIG signature="' + base64FromArrayBuffer(sig) + '"');
          xhr.setRequestHeader('Content-Type', 'text/plain; charset=utf-8');
          // Note that browsers will sometimes not let us know if
          // there was an error since this was a cross-domain request,
          // so we have to rely on a timeout instead.
          var timeout = setTimeout(function () { reject(); }, 30000);
          xhr.onReadyStateChange = function () {
            console.log('state change', xhr);
            if (xhr.readyState === 4) {
              if (xhr.status === 201) {
                clearTimeout(timeout);
                resolve();
              } else {
                reject();
              }
            }
          };
          xhr.onload = function () {
            console.log('onload', xhr);
            if (xhr.status === 201) {
              clearTimeout(timeout);
              resolve();
            } else {
              reject();
            }
          };
          xhr.send(password);
        });
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

  function progress (stepName, state) {
    var step = document.getElementById('step-' + stepName);
    step.className = state;
    if (state === 'active') {
      var spinner = document.createElement('div');
      spinner.className = 'indicator spinner';
      for (var i = 1; i <= 3; i++) {
        var dot = document.createElement('div');
        dot.className = 'bounce' + i;
        spinner.appendChild(dot);
      }
      step.appendChild(spinner);
    } else if (state === 'done' || state === 'fail') {
      var spinner = step.querySelector('.spinner');
      if (spinner) {
        step.removeChild(spinner);
      }
      var status = document.createElement('div');
      status.className = 'indicator done';
      status.appendChild(document.createTextNode(state === 'done' ? '✔' : '✘'));
      step.appendChild(status);
    }
    if (state === 'fail') {
      document.getElementById('fail-' + stepName).style.display = 'block';
    }
    if (stepName === 'kernel' && state === 'done') {
      document.getElementById('info-success').style.display = 'block';
    }
  }
  
  document.addEventListener('DOMContentLoaded', function () {
    if (!window.crypto || !window.crypto.subtle) {
      alert('Your browser does not support the Web Cryptography API. This installer will not work.');
      return;
    }

    document.getElementById('install-form').addEventListener('submit', function (event) {
      event.preventDefault();
      document.getElementById('install-button').disabled = true;

      AWS.config.update({accessKeyId: event.target.elements['aws-access-key'].value,
                         secretAccessKey: event.target.elements['aws-secret-key'].value});

      AWS.config.region = event.target.elements['aws-region'].value;

      AWS.config.apiVersions = {
        ec2: '2015-10-01'
      };

      var subdomain = event.target.elements['subdomain'].value;
      var password = event.target.elements['password'].value;

      var domain = subdomain + '.jekos.net';
      for (var els = document.querySelectorAll('.domain'), i = 0, len = els.length; i < len; i++) {
        els[i].href = 'https://' + domain;
        els[i].appendChild(document.createTextNode(domain));
      }

      // Generate a key pair.
      document.getElementById('progress').style.display = '';
      progress('keypair', 'active');
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

          // Attach the public key to the EC2 instance.
          window.crypto.subtle.exportKey('spki', key.publicKey).
            then(function (rawKey) {
              progress('keypair', 'done');
              progress('launch', 'active');
              // Encode the raw key as base64.
              launchEC2(subdomain, {publicKey: base64FromArrayBuffer(rawKey), subdomain: subdomain}).
                then(function (data) {
                  progress('launch', 'done');
                  progress('kernel', 'active');
                  setPassword(domain, password, key.privateKey).
                    then(function () {
                      progress('kernel', 'done');
                    }).
                    catch(function (e) {
                      console.log(e);
                      progress('kernel', 'fail');
                    });
                }).
                catch(function (e) {
                  console.log(e);
                  progress('launch', 'fail');
                });
            }).
            catch(function (e) {
              console.log(e);
              progress('keypair', 'fail');
            });
        }).
        catch(function (e) {
          console.log(e);
          progress('keypair', 'fail');
        });
    });
  });
})();
