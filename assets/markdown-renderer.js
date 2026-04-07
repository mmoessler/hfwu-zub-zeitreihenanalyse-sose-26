document.addEventListener('DOMContentLoaded', () => {
  const container = document.getElementById('markdown-content');
  if (!container) return;

  const params = new URLSearchParams(window.location.search);
  let page = params.get('page') || 'index';

  // allow both:
  // ?page=foo
  // ?page=foo.md
  if (!page.endsWith('.md')) {
    page += '.md';
  }

  console.log('Loading markdown page:', page);

  fetch(page)
    .then(response => {
      if (!response.ok) throw new Error(`Markdown file not found: ${page}`);
      return response.text();
    })
    .then(markdown => {
      markdown = markdown.replace(/^---[\s\S]*?---\s*/, '');

      const anchorPlugin =
        window.markdownitAnchor ||
        window.markdownItAnchor ||
        window.markdownitAnchor?.default;

      if (!anchorPlugin) {
        console.error('❌ markdown-it-anchor plugin failed to load.');
        container.innerHTML = `<p class="text-danger">Markdown-It-Anchor plugin not found. Check your script includes.</p>`;
        return;
      }

      const md = window.markdownit({
        html: true,
        linkify: true,
        typographer: true,
        highlight: function (str, lang) {
          const safeLang = lang || 'none';
          return `<pre data-lang="${safeLang}"><code class="language-${safeLang}">${md.utils.escapeHtml(str)}</code></pre>`;
        }
      }).use(anchorPlugin, {
        slugify: s => s.trim().toLowerCase()
          .replace(/[^\w\s-]/g, '')
          .replace(/\s+/g, '-'),
        permalink: false,
      });

      container.innerHTML = md.render(markdown);

      if (window.renderMathInElement) {
        renderMathInElement(container, {
          delimiters: [
            { left: '$$', right: '$$', display: true },
            { left: '\\[', right: '\\]', display: true },

            { left: '$', right: '$', display: false },
            { left: '\\(', right: '\\)', display: false }            
          ],
          throwOnError: false
        });
      }      

      document.querySelectorAll('pre code.language-mermaid').forEach(code => {
        const pre = code.parentElement;
        const wrapper = document.createElement('div');
        wrapper.className = 'mermaid';
        wrapper.textContent = code.textContent;
        pre.replaceWith(wrapper);
      });

      if (window.mermaid) {
        try {
          mermaid.initialize({ startOnLoad: false });
          mermaid.run({ querySelector: '.mermaid' });
        } catch (e) {
          console.warn('Mermaid rendering failed:', e);
        }
      }

      document
        .querySelectorAll('#markdown-content table:not(.table)')
        .forEach(table => {
          table.classList.add('table');
        });

      document.querySelectorAll('pre').forEach(block => {
        const lang = block.dataset.lang || '';
        if (/^(text|plain|none)$/i.test(lang)) return;

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

      if (window.Prism) Prism.highlightAll();
      if (window.initCopyButtons) window.initCopyButtons();
    })
    .catch(err => {
      console.error(err);
      container.innerHTML = `<p class="text-danger">Failed to load content: ${err.message}</p>`;
    });
});