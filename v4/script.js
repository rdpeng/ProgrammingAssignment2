/* Alice Tran — v4 "The Quiet Room"
   Editorial variant: reveal on scroll + gentle form handling. */

(() => {
  const year = document.getElementById('year');
  if (year) year.textContent = new Date().getFullYear();

  const revealables = document.querySelectorAll('[data-reveal]');
  if ('IntersectionObserver' in window && revealables.length) {
    const io = new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        if (!entry.isIntersecting) return;
        const el = entry.target;
        const siblings = Array.from(el.parentElement?.querySelectorAll(':scope > [data-reveal]') || []);
        const index = Math.max(0, siblings.indexOf(el));
        el.style.setProperty('--delay', `${Math.min(index, 6) * 90}ms`);
        el.classList.add('is-in');
        io.unobserve(el);
      });
    }, { rootMargin: '0px 0px -8% 0px', threshold: 0.08 });

    revealables.forEach((el) => io.observe(el));
  } else {
    revealables.forEach((el) => el.classList.add('is-in'));
  }

  const form = document.querySelector('.letter__form');
  form?.addEventListener('submit', (e) => {
    e.preventDefault();
    const btn = form.querySelector('.btn-letter');
    if (!btn) return;
    const original = btn.textContent;
    btn.textContent = 'Sent — thank you.';
    btn.disabled = true;
    form.reset();
    setTimeout(() => { btn.textContent = original; btn.disabled = false; }, 3200);
  });
})();
