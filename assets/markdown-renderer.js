document.addEventListener('DOMContentLoaded', () => {
  const container = document.getElementById('markdown-content');
  if (!container) return;

  // Derive markdown filename from current HTML file
  const htmlFile = window.location.pathname.split('/').pop();
  const baseName = htmlFile.replace(/\.html$/i, '') || 'index';
  const mdFile = `${baseName}.md`;

  console.log("Logging of baseName: ", baseName);

  // Fetch and render Markdown file
  fetch(mdFile)
    .then(response => {
      if (!response.ok) throw new Error('Markdown file not found');
      return response.text();
    })
    .then(markdown => {
      // Remove YAML front matter (--- ... ---)
      markdown = markdown.replace(/^---[\s\S]*?---\s*/, '');

      // Robust plugin reference (works with ESM or UMD)
      const anchorPlugin =
        window.markdownitAnchor ||
        window.markdownItAnchor ||     // <-- added capital "I" variant
        window.markdownitAnchor?.default;
      if (!anchorPlugin) {
        console.error("❌ markdown-it-anchor plugin failed to load.");
        container.innerHTML = `<p class="text-danger">Markdown-It-Anchor plugin not found. Check your script includes.</p>`;
        return;
      }

      // Initialize markdown-it
      const md = window.markdownit({
        html: true,
        linkify: true,
        typographer: true,
        highlight: (str, lang) => {
          const safeLang = lang || 'none';
          return `<pre data-lang="${safeLang}"><code class="language-${safeLang}">${md.utils.escapeHtml(str)}</code></pre>`;
        }
      }).use(anchorPlugin, {
        // GitHub-style slug generation
        slugify: s => s.trim().toLowerCase()
          .replace(/[^\w\s-]/g, '')
          .replace(/\s+/g, '-'),
        permalink: false, // set to window.markdownitAnchor.permalink.headerLink() if you want clickable anchors
      });

      // Render Markdown → HTML
      container.innerHTML = md.render(markdown);

      // Convert fenced mermaid code blocks
      document.querySelectorAll('pre code.language-mermaid').forEach(code => {
        const pre = code.parentElement;

        const wrapper = document.createElement('div');
        wrapper.className = 'mermaid';
        wrapper.textContent = code.textContent;

        pre.replaceWith(wrapper);
      });

      // Run Mermaid
      if (window.mermaid) {
        try {
          mermaid.initialize({ startOnLoad: false });
          mermaid.run({ querySelector: '.mermaid' });
        } catch (e) {
          console.warn('Mermaid rendering failed:', e);
        }
      }

      // Add bootstrap table attributes to tables
      const observer = new MutationObserver(() => {
        document
          .querySelectorAll("#markdown-content table:not(.table)")
          .forEach(table => {
            table.classList.add("table");
          });
      });

      observer.observe(document.getElementById("markdown-content"), {
        childList: true,
        subtree: true,
      })

      // Add copy buttons for code blocks
      document.querySelectorAll('pre').forEach(block => {
        const lang = block.dataset.lang || '';
        if (/^(text|plain|none)$/i.test(lang)) return; // Skip plain/text blocks

        const wrapper = document.createElement('div');
        wrapper.className = 'code-block position-relative';

        const button = document.createElement('button');
        button.className = 'btn btn-sm btn-outline-secondary copy-btn position-absolute';
        button.style.top = '8px';
        button.style.right = '8px';
        button.textContent = 'Copy';

        wrapper.appendChild(button);
        block.parentNode.insertBefore(wrapper, block);
        wrapper.appendChild(block);
      });

      // Re-run Prism for syntax highlighting
      if (window.Prism) Prism.highlightAll();

      // Initialize copy button logic (if defined elsewhere)
      if (window.initCopyButtons) window.initCopyButtons();
      
    })
    .catch(err => {
      console.error(err);
      container.innerHTML = `<p class="text-danger">Failed to load content: ${err.message}</p>`;
    });
});
