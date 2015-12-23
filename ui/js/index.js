// TODO: Provide an method to import private key.

var appStore = require('./lib/appstore');

var apps = [{"name":"hello","description":"This is the GNU Hello World program. It outputs a greeting and requires no additional resources."},{"name":"nginx","description":"the nginx web server"},{"name":"postgresql","description":"a relational database"}];

var storeFront = appStore(apps);
document.body.appendChild(storeFront);

// function installService (name) {
//   console.log('Installing ${name}...');
//   installableServices = installableServices.filter(x => x !== name);
//   update();
// }
