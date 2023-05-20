cd ..

C:\Software\clj -M:build --optimizations whitespace --compile-opts scripts/scraper.edn --output-to  resources\public\scraper\scraper.js -c woof.browser

copy resources\public\scraper\scraper.js ..\woof-chrome\scraper.js
