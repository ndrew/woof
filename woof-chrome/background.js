
function autoRunOn() {
    return {
        url: [
            // specify a sub-set of events, see https://developer.chrome.com/docs/extensions/reference/events/#type-UrlFilter
            {hostSuffix: "site-you-want-to-scrape"}
            ]
    }
}


// Called when the user clicks on the browser action or via shortcut.
chrome.browserAction.onClicked.addListener(function(tab) {
  // No tabs or host permissions needed!

	chrome.tabs.executeScript(tab.id, {
	    file:"scraper.js"
	});

});



//
// comment this out if scraper auto-running is not required
chrome.webNavigation.onCompleted.addListener(function (details) {

        if (details.frameId === 0) {
            // see extensionTypes.InjectDetails
            var injectDetails = {
                file: "scraper.js"
            };

            var resultCb = function (result) {
                // console.log("result")
            };


            chrome.tabs.executeScript(
                details.tabId,
                injectDetails,
                resultCb);
        }
    },
    autoRunOn()
);


// if something needs to be done on chrome startup
chrome.runtime.onStartup.addListener(function() {
    // nav.resetDataStorage();
});

// webNavigation: API to receive notifications about the status of navigation requests in-flight.

// webNavigation events:
//
// onBeforeNavigate -> onCommitted -> onDOMContentLoaded -> onCompleted
// onErrorOccurred
// onTabReplaced

