#/usr/bin/bash

# for auto-build use
# fswatch -o ../src | xargs -n1 -I{} ./build-scraper-wf.sh

echo "START: scraper.js";

# compile cljs to resources/public/scraper/scraper.js
cd $PWD/..


OUTPUT=$( clj -M:build --optimizations whitespace --compile-opts scripts/scraper.edn --output-to  resources/public/scraper/scraper.js -c woof.browser 2>&1 );

if [ -z "$OUTPUT" ]; then
  # copy it to chrome extension
  cp resources/public/scraper/scraper.js ../woof-chrome/scraper.js

  echo "DONE:  scraper.js";
  say -v Victoria Build completed;
else
  echo "CAN'T COMPILE scraper.js";
  say -v Victoria Error;
fi

