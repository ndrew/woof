
// Called when the user clicks on the browser action or via shortcut.
chrome.browserAction.onClicked.addListener(function(tab) {
  // No tabs or host permissions needed!

  // chrome.tabs.executeScript({
  //   code: '(function() { var $script = document.createElement(\'script\'); $script.setAttribute("type","text/javascript"); $script.setAttribute("src", "http://localhost:8081/scraper.js"); document.body.appendChild($script); })()'
  // });

	chrome.tabs.executeScript(tab.id, {file:"scraper.js"});

});
