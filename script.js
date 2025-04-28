document.querySelectorAll('h2, h3, h4').forEach(item => {
    item.addEventListener('click', () => {
        // don't collapse if clicking on a link inside the heading
        if (event.target.tagName === 'A') { return; }
        const content = item.parentElement;
        for (let i = 0; i < content.children.length; i++) {
            if (i === 0) {
                continue; // Skip the first child
            }

            const child = content.children[i];

            // don't render display <script> elements
            if (child.tagName === 'SCRIPT') { return; }

            if (child.style.display === "none") {
                child.style.display = "block";
            } else {
                child.style.display = "none";
            }
        }
    });
});
