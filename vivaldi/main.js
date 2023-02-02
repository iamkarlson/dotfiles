// $("#main > div.inner > div:nth-child(2)").style = ""

function mvMenu() {
    const adr = document.querySelector('.UrlBar.toolbar');
    const menu = document.querySelector('.vivaldi');
	const tabs = document.querySelector('#tabs-container');
	tabs.setAttribute('style','padding-left: 1px')
    menu.setAttribute('style','position:static;height:var(--toolbarHeight);');
    adr.appendChild(menu);
};

setTimeout(function wait() {
    const browser = document.getElementById('browser');
    if (browser) {
        mvMenu();
    }
    else {
        setTimeout(wait, 300);
    }
}, 300);
