addEventListener("load", () => {
    const body = document.querySelector('body');
    document
        .querySelectorAll("#toggle-mobile-menu, #close-mobile-menu")
        .forEach(element => {
            element.addEventListener("click", () => {
                body.classList.toggle("open-menu");
            });
        })

    Array.from(document.links)
        .filter(link => link.hostname != window.location.hostname)
        .forEach(link => link.target = '_blank');
});

// The markdown code comment block have inline event handlers.
// We don't want anything to have in that case.
function hideTip() {}
function showTip() {}