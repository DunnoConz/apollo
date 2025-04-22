// Add copy button to code blocks
document.addEventListener('DOMContentLoaded', () => {
    const codeBlocks = document.querySelectorAll('pre code');
    codeBlocks.forEach(block => {
        const button = document.createElement('button');
        button.className = 'copy-button';
        button.textContent = 'Copy';
        
        button.addEventListener('click', async () => {
            try {
                await navigator.clipboard.writeText(block.textContent);
                button.textContent = 'Copied!';
                setTimeout(() => {
                    button.textContent = 'Copy';
                }, 2000);
            } catch (err) {
                console.error('Failed to copy:', err);
                button.textContent = 'Error';
            }
        });
        
        block.parentNode.insertBefore(button, block);
    });
});

// Add anchor links to headers
document.addEventListener('DOMContentLoaded', () => {
    const headers = document.querySelectorAll('h2, h3, h4, h5, h6');
    headers.forEach(header => {
        if (header.id) {
            const link = document.createElement('a');
            link.href = `#${header.id}`;
            link.className = 'header-link';
            link.innerHTML = '#';
            header.appendChild(link);
        }
    });
});

// Add callout support
document.addEventListener('DOMContentLoaded', () => {
    const callouts = document.querySelectorAll('blockquote');
    callouts.forEach(callout => {
        const firstLine = callout.textContent.trim().split('\n')[0].toLowerCase();
        if (firstLine.startsWith('warning:')) {
            callout.className = 'warning';
        } else if (firstLine.startsWith('info:')) {
            callout.className = 'info';
        } else if (firstLine.startsWith('tip:')) {
            callout.className = 'tip';
        }
    });
}); 