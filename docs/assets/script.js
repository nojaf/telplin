addEventListener("load", () => {
    const body = document.querySelector('body');
    document
        .querySelectorAll("#toggle-mobile-menu, #close-mobile-menu")
        .forEach(element => {
            element.addEventListener("click", () => {
                body.classList.toggle("open-menu");
            });
        })
});

// The markdown code comment block have inline event handlers.
// We don't want anything to have in that case.
function hideTip() {}
function showTip() {}