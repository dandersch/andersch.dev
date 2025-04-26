document.querySelectorAll('[id^="org"]').forEach(item => {
    item.addEventListener('click', () => {
        console.log("he");
        const content = item.parentElement;
        for (let i = 0; i < content.children.length; i++) {
            if (i === 0) {
                continue; // Skip the first child
            }

            const child = content.children[i];
            if (child.style.display === "none") {
                child.style.display = "block";
            } else {
                child.style.display = "none";
            }
        }
    });
});
