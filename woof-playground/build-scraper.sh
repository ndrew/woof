#/usr/bin/bash

echo "start compilation";
time clj -m cljs.main --optimizations whitespace --output-to ~/m/woof/woof-playground/resources/public/scraper/scraper.js -c woof.browser
echo "re-compiled";