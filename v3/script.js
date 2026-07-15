/* Alice Tran — modern redesign
   Tiny, dependency-free interactions: reveal on scroll, nav state, mobile menu. */

(() => {
  // Current year in footer
  const year = document.getElementById('year');
  if (year) year.textContent = new Date().getFullYear();

  // Nav: scrolled state
  const nav = document.getElementById('nav');
  const setNavState = () => {
    if (!nav) return;
    nav.classList.toggle('is-scrolled', window.scrollY > 12);
  };
  setNavState();
  window.addEventListener('scroll', setNavState, { passive: true });

  // Mobile menu toggle
  const toggle = nav?.querySelector('.nav__toggle');
  const links = nav?.querySelector('.nav__links');
  toggle?.addEventListener('click', () => {
    const open = nav.classList.toggle('is-open');
    toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
  });
  links?.querySelectorAll('a').forEach(a =>
    a.addEventListener('click', () => {
      nav.classList.remove('is-open');
      toggle?.setAttribute('aria-expanded', 'false');
    })
  );

  // Reveal on scroll — stagger siblings a touch
  const revealables = document.querySelectorAll('[data-reveal]');
  if ('IntersectionObserver' in window && revealables.length) {
    const io = new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        if (!entry.isIntersecting) return;
        const el = entry.target;
        const siblings = Array.from(el.parentElement?.querySelectorAll(':scope > [data-reveal]') || []);
        const index = Math.max(0, siblings.indexOf(el));
        el.style.setProperty('--delay', `${Math.min(index, 5) * 80}ms`);
        el.classList.add('is-in');
        io.unobserve(el);
      });
    }, { rootMargin: '0px 0px -8% 0px', threshold: 0.08 });

    revealables.forEach((el) => io.observe(el));
  } else {
    revealables.forEach((el) => el.classList.add('is-in'));
  }

  // Contact form — friendly, no backend
  const form = document.querySelector('.contact__form');
  form?.addEventListener('submit', (e) => {
    e.preventDefault();
    const btn = form.querySelector('button[type="submit"]');
    if (!btn) return;
    const original = btn.textContent;
    btn.textContent = 'Thank you — talk soon.';
    btn.disabled = true;
    form.reset();
    setTimeout(() => {
      btn.textContent = original;
      btn.disabled = false;
    }, 3200);
  });
})();
