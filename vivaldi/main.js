// $("#main > div.inner > div:nth-child(2)").style = ""

/*
 *
 * Vivaldi doesn't allow to inject any of javascript code directly.
 * However, you can modify vivaldi browser files so it's using this script.
 * > cp main.js /opt/vivaldi/resources/vivaldi/customization.js
 */

function mvMenu() {
    const adr = document.querySelector('.mainbar > .toolbar');
    const menu = document.querySelector('.vivaldi');
	const tabs = document.querySelector('#tabs-container');
	tabs.setAttribute('style','padding-left: 1px')
    adr.appendChild(menu);
    menu.setAttribute('style','position:static;height:var(--toolbarHeight);');
    // proper fix with changing toolbar size. however, I don't need this toolbar at all
    // so I'm simply removing it as a whole
    document.querySelector("#header").remove()
};

console.log("customization script loaded");

setTimeout(function wait() {
    console.log("timer called");
    const browser = document.getElementById('browser');
    if (browser) {
        mvMenu();
        console.log("moved!")
    }
    else {
        setTimeout(wait, 300);
    }
}, 300);
