window.initCopyButtons = function() {
  document.querySelectorAll('.copy-btn').forEach(button => {
    button.addEventListener('click', () => {
      const code = button.nextElementSibling.innerText.trim();
      navigator.clipboard.writeText(code).then(() => {
        button.textContent = 'Copied!';
        button.classList.remove('btn-outline-secondary');
        button.classList.add('btn-success');
        setTimeout(() => {
          button.textContent = 'Copy';
          button.classList.remove('btn-success');
          button.classList.add('btn-outline-secondary');
        }, 2000);
      });
    });
  });
};
