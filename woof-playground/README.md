# playground for woof

WIP

clj -Abuild-dev

## tests

http://localhost:9500/figwheel-extra-main/auto-testing

## devcards 

http://localhost:9500/figwheel-extra-main/devcards


-- 
## example 

You need an website:

What does this mean: 

There will be some html and resources will served from %domainname%.
(https://developer.mozilla.org/en-US/docs/Learn/Common_questions/What_is_a_web_server)

Usually, somebody else runs %domainname% server, so we need to figure out 
how to deliver website to server.

if its a **static web server** we need to deliver html files + resources 
 
if its a **dynamic web server** we need to deliver other deliverables:
 like also files (php, js), etc.
 
 
so step 1 is  
    
    sync deliverables to server (somehow) 
    
 
 --
 08.06.2019
 
 example
 
website -> choose hosting provider & deployment fmt => github

github -> sync via fs folder (via git)
