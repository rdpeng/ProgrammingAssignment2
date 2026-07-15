/* Alice Tran — v6: current-site recreation with new hero background.
   Minimal JS: mobile menu toggle only. */

(() => {
  const nav = document.querySelector('.nav');
  const toggle = nav?.querySelector('.nav__toggle');
  const links = nav?.querySelector('.nav__links');

  toggle?.addEventListener('click', () => {
    const open = nav.classList.toggle('is-open');
    toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
  });

  links?.querySelectorAll('a').forEach((a) =>
    a.addEventListener('click', () => {
      nav.classList.remove('is-open');
      toggle?.setAttribute('aria-expanded', 'false');
    })
  );
})();
