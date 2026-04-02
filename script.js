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

function XorCoder(key) {
    this.key = key;
}

XorCoder.prototype.decode = function (input) {
    let output = ''; 
    for (let i = 0; i < input.length; i += 2) {
        const hexInput = this.toHex(input, i);
        const hexOutput = hexInput ^ this.key;
        output += String.fromCharCode(hexOutput);
    }
    return output;
}

XorCoder.prototype.toHex = function (text, i) {
    const sequence = text.substr(i, 2);
    return parseInt(sequence, 16);
}

function Email(coder) {
    this.coder = coder;
}

Email.prototype.decodeNodes = function (selector) {
    const nodes = document.querySelectorAll(selector);
    for (let i = 0; i < nodes.length; ++i) {
        this.decodeNode(nodes[i]);
    }
}

Email.prototype.decodeNode = function (node) {
    for (let i = 0; i < node.childNodes.length; ++i) {
        let child = node.childNodes[i];
        if (child.nodeType === Node.ELEMENT_NODE) {
            child = child.firstChild;
        }
        child.nodeValue = this.coder.decode(child.nodeValue);
    }
    node.classList.remove('hidden');
}

window.addEventListener('DOMContentLoaded', function () {
    const coder = new XorCoder(221);
    const email = new Email(coder);
    email.decodeNodes('.email');
});
