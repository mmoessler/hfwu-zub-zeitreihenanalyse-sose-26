
document.addEventListener('DOMContentLoaded', () => {
  // Load Navbar
  const navbarContainer = document.getElementById('navbar-container');
  if (navbarContainer) {
    fetch('top-navbar.html')
      .then(response => {
        if (!response.ok) throw new Error('Navbar file not found');
        return response.text();
      })
      .then(html => {
        navbarContainer.innerHTML = html;
      })
      .catch(err => {
        console.error(err);
        navbarContainer.innerHTML =
          '<p class="text-danger">Failed to load navigation.</p>';
      });
  }
});
