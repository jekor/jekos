function appStore (apps) {
  // TODO: Sort apps alphabetically.
  var mainDiv = document.createElement('div');
  mainDiv.className = 'storefront';
  var cards = apps.map(appCard);
  cards.map(c => mainDiv.appendChild(c));
  mainDiv.addEventListener('click', function (event) {
    if (event.target.tagName === 'BUTTON') {
      event.preventDefault();
      // TODO: Convert the install button into a progress bar.
      var name = event.target.parentNode.querySelector('h1').textContent;
      installApp(name, function () {
        // TODO: Update the card installation state.
      });
    }
  });
  return mainDiv;
}

function appCard (app) {
  // TODO: Provide a link to any running instances of this app.
  var mainDiv = document.createElement('div');
  mainDiv.className = 'card';
  var title = document.createElement('h1');
  title.appendChild(document.createTextNode(app.name));
  var install = document.createElement('button');
  install.appendChild(document.createTextNode('Install'));
  var desc = document.createElement('p');
  desc.appendChild(document.createTextNode(app.description));
  mainDiv.appendChild(title);
  mainDiv.appendChild(install);
  mainDiv.appendChild(desc);
  return mainDiv;
}

function installApp (appName) {
  console.log(`Installing ${appName}...`);
  // TODO: Ask the kernel to install the app.
}

module.exports = appStore;
