#/usr/bin/bash

# for auto-build use
# fswatch -o ~/m/woof/woof-playground/src | xargs -n1 -I{} ~/m/woof/woof-playground/scripts/build-scraper-wf.sh


echo "START: scraper.js";

# compile cljs to resources/public/scraper/scraper.js
cd ~/m/woof/woof-playground/

clj -m cljs.main --optimizations whitespace --compile-opts scripts/scraper.edn --output-to  ~/m/woof/woof-playground/resources/public/scraper/scraper.js -c woof.browser

# copy it to chrome extension
cp ~/m/woof/woof-playground/resources/public/scraper/scraper.js ~/m/woof/woof-chrome/scraper.js

echo "DONE:  scraper.js";