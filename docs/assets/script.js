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