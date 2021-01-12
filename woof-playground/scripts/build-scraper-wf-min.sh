#/usr/bin/bash

cd $PWD/..

# compile cljs to resources/public/scraper/scraper.js
clj -M:build --optimizations whitespace --compile-opts scripts/scraper.edn --output-to  resources/public/scraper/scraper.js -c woof.browser
# copy
cp resources/public/scraper/scraper.js ../woof-chrome/scraper.js

