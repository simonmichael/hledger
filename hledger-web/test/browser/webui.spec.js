// Browser end-to-end tests for the hledger-web UI.
//
// These pin down the behaviors that Hledger/Web/Test.hs cannot see, because
// yesod-test never runs javascript: the add form, autocomplete, the date
// picker, keyboard shortcuts, sidebar state, and hash highlighting.
//
// Run with: npx playwright test  (see README.md)
const { test, expect } = require('@playwright/test');
const fs = require('fs');

// Collect uncaught page errors in every test; individual tests assert on them.
let pageErrors;
test.beforeEach(({ page }) => {
  pageErrors = [];
  page.on('pageerror', err => pageErrors.push(String(err)));
});

// Open the add-transaction modal via its keyboard shortcut and wait for it.
async function openAddForm(page) {
  await page.locator('body').press('a');
  await expect(page.locator('#addmodal')).toBeVisible();
}

// Hover an element, then nudge the pointer one pixel. hover() may scroll the
// element into view, and the scroll event, delivered on the next frame, hides
// the entry tooltip and cancels its pending show; the nudge is the small
// pointer movement a real hand always makes, which re-arms it.
async function hoverSettled(page, locator) {
  await locator.hover();
  const box = await locator.boundingBox();
  await page.mouse.move(box.x + box.width / 2 + 1, box.y + box.height / 2);
}

test.describe('page initialization', () => {

  test('journal page loads and starts up without javascript errors', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    // startup ran to completion: the add form was pre-filled with today's date
    await expect(page.locator('#addform input[name=date]')).not.toHaveValue('');
    expect(pageErrors).toEqual([]);
  });

  // A url hash can be anything, eg an old bookmark's numeric row id, and
  // '#3' is not a valid CSS selector, so any code passing location.hash to
  // querySelector must guard it. A failure here leaves the page
  // half-initialized: no date picker, no keyboard shortcuts, no sidebar handlers.
  test('register url with a numeric transaction hash initializes fully', async ({ page }) => {
    await page.goto('/register?q=inacct:assets:bank:checking#3');
    expect(pageErrors).toEqual([]);
    await openAddForm(page);
    await expect(page.locator('#addform input[name=date]')).not.toHaveValue('');
  });

  // The mark is a :target rule in hledger.css, so assert on the rendered
  // colour rather than on a class: that is what the reader actually sees.
  test('a transaction link marks its target row', async ({ page }) => {
    await page.goto('/journal');
    const link = page.locator('#main-content a[title="assets:bank:checking"]').first();
    const id = (await link.getAttribute('href')).split('#')[1];
    await link.click();
    await expect(page).toHaveURL(/register/);
    await expect(page.locator(`[id="${id}"]`)).toBeVisible();
    const colourOf = l => l.evaluate(el => getComputedStyle(el).backgroundColor);
    const target = page.locator(`[id="${id}"] > td`).first();
    const other = page.locator('#main-content tbody tr:not(:target) > td').first();
    expect(await colourOf(target)).not.toEqual(await colourOf(other));
    expect(pageErrors).toEqual([]);
  });

});

test.describe('add form', () => {

  test('starts with four posting rows', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await expect(page.locator('#addform .account-group')).toHaveCount(4);
  });

  test('typing in the last amount field adds a posting row', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await page.locator('#addform input[name=amount]').last().press('5');
    await expect(page.locator('#addform .account-group')).toHaveCount(5);
    expect(pageErrors).toEqual([]);
  });

  // Completion is a native <datalist>. The browser draws its dropdown outside
  // the page, so a test can only check that the field is wired to a list and
  // that the list offers the journal's accounts and descriptions.
  test('the account and description fields offer completions', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await expect(page.locator('#addform input[name=account]').first())
      .toHaveAttribute('list', 'accountnames');
    await expect(page.locator('#addform input[name=description]'))
      .toHaveAttribute('list', 'descriptionnames');
    await expect(page.locator('#accountnames option[value="assets:bank:checking"]')).toHaveCount(1);
    await expect(page.locator('#descriptionnames option[value="Cafe Luna"]')).toHaveCount(1);
  });

  test('the date field offers a picker and accepts a typed date', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    const date = page.locator('#addform input[name=date]');
    await date.fill('2025-03-04');
    await expect(date).toHaveValue('2025-03-04');
    expect(pageErrors).toEqual([]);
  });

  test('adds a transaction', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await page.locator('#addform input[name=description]').fill('E2eCreatePlain');
    await page.locator('#addform input[name=account]').nth(0).fill('expenses:food:dining');
    await page.locator('#addform input[name=amount]').nth(0).fill('12.34');
    await page.locator('#addform input[name=account]').nth(1).fill('assets:bank:checking');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#message')).toContainText('Transaction added.');
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toContain('E2eCreatePlain');
  });

  test('reports an unbalanced transaction without adding it', async ({ page }) => {
    await page.goto('/journal');
    await openAddForm(page);
    await page.locator('#addform input[name=description]').fill('E2eUnbalanced');
    await page.locator('#addform input[name=account]').nth(0).fill('expenses:food:dining');
    await page.locator('#addform input[name=amount]').nth(0).fill('10.00');
    await page.locator('#addform input[name=account]').nth(1).fill('assets:bank:checking');
    await page.locator('#addform input[name=amount]').nth(1).fill('99.00');
    await page.locator('#addform button[type=submit]').click();
    await expect(page.locator('#message')).toBeVisible();
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).not.toContain('E2eUnbalanced');
  });

});

test.describe('edit form', () => {

  test('edits a journal file', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('a[title="Manage journal files"]').click();
    await page.locator('a.btn', { hasText: 'Edit' }).first().click();
    const ta = page.locator('textarea');
    const text = await ta.inputValue();
    expect(text).toContain('Metro Transit');
    await ta.fill(text.replace('Metro Transit', 'Metro Transit EDITED'));
    await page.locator('input[type=submit][value=Save]').click();
    await expect(page.locator('#message')).toContainText('Saved journal');
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toContain('Metro Transit EDITED');
  });

  test('rejects an edit that would not parse, leaving the file alone', async ({ page }) => {
    await page.goto('/manage');
    await page.locator('a.btn', { hasText: 'Edit' }).first().click();
    const ta = page.locator('textarea');
    const before = fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8');
    await ta.fill('this is not a journal\n  and cannot parse\n');
    await page.locator('input[type=submit][value=Save]').click();
    await expect(page.locator('#message')).toContainText('Failed to load journal');
    expect(fs.readFileSync(process.env.BROWSER_JOURNAL, 'utf8')).toEqual(before);
  });

});

test.describe('search', () => {

  test('accepts documented query syntax containing slashes', async ({ page }) => {
    await page.goto('/journal');
    let dialogMessage = null;
    page.on('dialog', d => { dialogMessage = d.message(); d.dismiss().catch(() => {}); });
    await page.locator('#searchform input[name=q]').fill('date:2025/1/5');
    await page.locator('#searchform input[name=q]').press('Enter');
    await expect(page).toHaveURL(/date%3A2025/);
    expect(dialogMessage).toBeNull();
    // the filter was applied: only the matching transaction remains
    await expect(page.locator('#main-content')).toContainText('weekly shop');
    await expect(page.locator('#main-content')).not.toContainText('Cafe Luna');
  });

  test('an account query filters the register', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('#searchform input[name=q]').fill('acct:dining');
    await page.locator('#searchform input[name=q]').press('Enter');
    await expect(page.locator('#main-content')).toContainText('Cafe Luna');
    await expect(page.locator('#main-content')).not.toContainText('Metro Transit');
  });

});

test.describe('keyboard shortcuts', () => {

  test('"s" toggles the sidebar, and the choice survives navigation', async ({ page }) => {
    await page.goto('/journal');
    const sidebar = page.locator('#sidebar-menu');
    await expect(sidebar).toBeVisible();
    await page.locator('body').press('s');
    await expect(sidebar).not.toBeVisible();
    // the server reads the showsidebar cookie, so a fresh page load keeps it hidden
    await page.goto('/journal');
    await expect(sidebar).not.toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  test('shortcuts do not fire while typing in the search box', async ({ page }) => {
    await page.goto('/journal');
    const q = page.locator('#searchform input[name=q]');
    await q.click();
    // 's' would toggle the sidebar and 'a' would open the add form
    await q.pressSequentially('assets', { delay: 20 });
    await expect(q).toHaveValue('assets');
    await expect(page.locator('#sidebar-menu')).toBeVisible();
    await expect(page.locator('#addmodal')).not.toBeVisible();
  });

  test('"h" opens the help dialog', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('h');
    await expect(page.locator('#helpmodal')).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  // The rows of report controls sit behind a disclosure, closed by default;
  // opening it is remembered per browser, restored inline before the page
  // below paints.
  test('the controls disclosure starts closed, and stays open across pages', async ({ page }) => {
    await page.goto('/balance');
    const details = page.locator('#main-content details.report-controls');
    await expect(details.locator('p.report-links').first()).toBeHidden();
    await details.locator('> summary').click();
    await expect(details.locator('p.report-links').first()).toBeVisible();
    // the open state is saved from the toggle event, which fires
    // asynchronously; wait for the save before navigating away
    await page.waitForFunction(() => localStorage.getItem('controlsOpen') === '1');
    await page.goto('/journal');
    await expect(page.locator('#main-content details.report-controls p.report-links').first()).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  // Shift-clicking account types composes a type:AL.. search term; the
  // row marks each selected type.
  test('shift-clicking account types selects several', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('#main-content details.report-controls > summary').click();
    const types = page.locator('#main-content div.types');
    await types.locator('a', { hasText: 'Asset' }).click();
    await expect(page).toHaveURL(/type%3AA/);
    await types.locator('a', { hasText: 'Liability' }).click({ modifiers: ['Shift'] });
    await expect(page).toHaveURL(/type%3AAL/);
    await expect(types.locator('a.current')).toHaveText(['Asset', 'Liability']);
    expect(pageErrors).toEqual([]);
  });

  // Many commodities collapse to a line showing the selection; opening
  // the list is remembered across pages. Shift-clicking composes a
  // cur:A|B search term from the links' data attributes, and the row
  // marks each selected one.
  test('the commodity list opens, stays open, and shift-click selects several', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('#main-content details.report-controls > summary').click();
    const box = page.locator('#main-content div.commodities');
    await expect(box.locator('summary .sel')).toHaveText('All');
    await box.locator('summary').click();
    await box.locator('a', { hasText: 'USD' }).click();
    await expect(page).toHaveURL(/cur%3AUSD/);
    // the selection reads back, and the opened list is remembered
    await expect(box.locator('summary .sel')).toHaveText('USD');
    await expect(box.locator('details')).toHaveAttribute('open', '');
    await box.locator('a', { hasText: '$' }).click({ modifiers: ['Shift'] });
    await expect(page).toHaveURL(/cur%3AUSD%7C%5C%24/);
    await expect(box.locator('a.current')).toHaveText(['$', 'USD']);
    expect(pageErrors).toEqual([]);
  });

  // Zero balances state is the empty parameter; the row's links set it,
  // and the "e" key presses the row's other link.
  test('"e" and the Zero balances links switch a page between showing and hiding zero balances', async ({ page }) => {
    await page.goto('/balance');
    const current = page.locator('#main-content .zero-balances a.current');
    await expect(current).toHaveText('Shown');
    await page.locator('#main-content details.report-controls > summary').click();
    await page.locator('#main-content .zero-balances a', { hasText: 'Hidden' }).click();
    await expect(page).toHaveURL(/empty=0/);
    await expect(current).toHaveText('Hidden');
    await page.locator('body').press('e');
    await expect(page).not.toHaveURL(/empty=/);
    await expect(current).toHaveText('Shown');
    expect(pageErrors).toEqual([]);
  });

});

test.describe('modal dialogs', () => {

  // A <dialog> shown with showModal() should capture the shortcut keys; before
  // the guard, keys pressed with a dialog open acted on the page behind it.
  test('shortcut keys do nothing while a modal is open', async ({ page }) => {
    await page.goto('/register');
    await page.locator('body').press('h');
    await expect(page.locator('#helpmodal')).toBeVisible();
    await page.locator('body').press('j');   // would go to /journal
    await page.locator('body').press('a');   // would open the add form
    await expect(page).toHaveURL(/register/);
    await expect(page.locator('#helpmodal')).toBeVisible();
    await expect(page.locator('#addmodal')).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

  // showModal() throws if the dialog is already open; the add-form shortcut
  // must not raise an uncaught error when pressed with the form already up.
  test('re-triggering the add form while open does not error', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('a');
    await expect(page.locator('#addmodal')).toBeVisible();
    await page.locator('body').press('a');
    await expect(page.locator('#addmodal')).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  // Clicking the backdrop closes the dialog (the help text promises this).
  test('clicking outside the dialog closes it', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('h');
    await expect(page.locator('#helpmodal')).toBeVisible();
    await page.mouse.click(4, 4);   // top-left corner: the backdrop
    await expect(page.locator('#helpmodal')).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

});

test.describe('sidebar', () => {

  // Regression test for #2651: the sidebar's scroll position is restored
  // during page parse, so it must not be at 0 after navigating.
  test('keeps its scroll position across navigation', async ({ page }) => {
    await page.setViewportSize({ width: 1200, height: 300 });
    await page.goto('/journal');
    const sidebar = page.locator('#sidebar-menu');
    await sidebar.evaluate(el => { el.scrollTop = el.scrollHeight; });
    const scrolled = await sidebar.evaluate(el => el.scrollTop);
    test.skip(scrolled === 0, 'sidebar does not scroll at this viewport');
    await page.locator('#sidebar-menu a.acct-name', { hasText: 'groceries' }).first().click();
    await expect(page).toHaveURL(/register/);
    expect(await page.locator('#sidebar-menu').evaluate(el => el.scrollTop)).toBeGreaterThan(0);
  });

  test('"e" hides empty accounts, as the empty parameter', async ({ page }) => {
    await page.goto('/journal');
    await page.locator('body').press('e');
    await expect(page).toHaveURL(/\/journal\?empty=0/);
    await page.locator('body').press('e');
    await expect(page).not.toHaveURL(/empty=/);
    expect(pageErrors).toEqual([]);
  });

  test('links to the financial statements, marking the one shown', async ({ page }) => {
    // at a laptop width the sidebar is narrow; the report labels must still fit
    await page.setViewportSize({ width: 1000, height: 800 });
    await page.goto('/journal');
    const clipped = await page.locator('#sidebar-menu td.top').evaluateAll(
      tds => tds.filter(td => td.scrollWidth > td.clientWidth).map(td => td.textContent.trim()));
    expect(clipped).toEqual([]);
    await page.locator('#sidebar-menu a', { hasText: 'Income statement' }).click();
    await expect(page).toHaveURL(/\/incomestatement$/);
    await expect(page.locator('#main-content h2')).toContainText('Income Statement');
    await expect(page.locator('#sidebar-menu tr.inacct a')).toHaveText('Income statement');
    // from a report, the other reports are one click away in the Report row
    await page.locator('#main-content .report-links a', { hasText: 'Balance sheet with equity' }).click();
    await expect(page).toHaveURL(/\/balancesheetequity$/);
    expect(pageErrors).toEqual([]);
  });

});

test.describe('report tree folding', () => {

  // A tree-mode report serves every depth and folds to the search's
  // depth: term; the carets open and close groups in place, without
  // changing the URL.
  test('folds to the depth in the search, and carets unfold groups in place', async ({ page }) => {
    await page.goto('/balance?list=tree&q=depth:1');
    // expenses is the branching group; the boring chains squash to leaves
    const top = page.locator('td.account.depth-0', { hasText: 'expenses' });
    const deep = page.locator('td.account.depth-1').first();
    await expect(top).toBeVisible();
    await expect(deep).toBeHidden();
    await top.locator('.tree-caret').click();
    await expect(deep).toBeVisible();
    await expect(page).toHaveURL('/balance?list=tree&q=depth:1');
    await top.locator('.tree-caret').click();
    await expect(deep).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

  // The sidebar's accounts fold too, and the fold is remembered in
  // this browser, so the sidebar stays as you left it between pages.
  test('in tree mode the sidebar folds, and stays folded across navigation', async ({ page }) => {
    await page.goto('/journal?list=tree');
    const deep = () => page.locator('#sidebar-menu tr[data-acct="expenses:food:groceries"]');
    await expect(deep()).toBeVisible();
    await page.locator('#sidebar-menu tr[data-acct="expenses"] .tree-caret').click();
    await expect(deep()).toBeHidden();
    await page.goto('/balance?list=tree');
    await expect(deep()).toBeHidden();
    await page.locator('#sidebar-menu tr[data-acct="expenses"] .tree-caret').click();
    await expect(deep()).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  // A depth limit is each page's baseline: the sidebar starts folded to
  // it, groups opened or closed by hand are remembered over it, and a
  // caret clicked back to its page's baseline is forgotten.
  test('a depth limit folds the sidebar, and caret choices are remembered over it', async ({ page }) => {
    await page.goto('/journal?list=tree&q=depth:2');
    const food = () => page.locator('#sidebar-menu tr[data-acct="expenses:food"]');
    const groceries = () => page.locator('#sidebar-menu tr[data-acct="expenses:food:groceries"]');
    await expect(food()).toBeVisible();
    await expect(groceries()).toBeHidden();
    // opening past the limit is remembered on the next page
    await food().locator('.tree-caret').click();
    await expect(groceries()).toBeVisible();
    await page.goto('/balance?list=tree&q=depth:2');
    await expect(groceries()).toBeVisible();
    // closing it again returns it to the baseline, so a page without a
    // depth limit, whose baseline is fully open, shows it open
    await food().locator('.tree-caret').click();
    await expect(groceries()).toBeHidden();
    await page.goto('/journal?list=tree');
    await expect(groceries()).toBeVisible();
    expect(pageErrors).toEqual([]);
  });

  // The other direction: a group folded by hand stays folded on a page
  // whose depth limit would open it.
  test('a fold made by hand holds against a depth limit', async ({ page }) => {
    await page.goto('/journal?list=tree');
    const food = () => page.locator('#sidebar-menu tr[data-acct="expenses:food"]');
    await page.locator('#sidebar-menu tr[data-acct="expenses"] .tree-caret').click();
    await expect(food()).toBeHidden();
    await page.goto('/journal?list=tree&q=depth:3');
    await expect(food()).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

  // Without a depth, the tree starts fully open and can be folded.
  test('an unfolded tree collapses from any group', async ({ page }) => {
    await page.goto('/balance?list=tree');
    const deep = page.locator('td.account.depth-1').first();
    await expect(deep).toBeVisible();
    await page.locator('td.account.depth-0', { hasText: 'expenses' }).locator('.tree-caret').click();
    await expect(deep).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

});

// Hovering a transaction shows its journal entry in a tooltip drawn by
// hledger.js, in a fixed-width font (#2716). It behaves like a browser's own
// tooltip: it appears after a pause, and goes when the pointer leaves or clicks.
test.describe('entry tooltip', () => {

  test('hovering a journal entry shows it, until the pointer leaves or clicks', async ({ page }) => {
    await page.goto('/journal');
    const tip = page.locator('.entry-tooltip');
    const row = page.locator('#main-content tr.title', { hasText: 'Cafe Luna' });
    const description = row.locator('td').nth(1);
    // the row has no title of its own, so no browser tooltip shows as well
    await expect(row).not.toHaveAttribute('title');
    await hoverSettled(page, description);
    await expect(tip).toBeVisible();
    await expect(tip).toContainText('Cafe Luna');
    await expect(tip).toContainText('expenses:food:dining');
    await page.locator('#main-content h2').hover();
    await expect(tip).toBeHidden();
    // after a click it stays away while the pointer is still on the row
    await description.hover();
    await expect(tip).toBeVisible();
    await page.mouse.down();
    await page.mouse.up();
    await expect(tip).toBeHidden();
    await row.locator('td.date').hover();
    await page.waitForTimeout(800);
    await expect(tip).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

  test('a register row shows its entry too, but over an account link the link\'s title applies', async ({ page }) => {
    await page.goto('/register?q=inacct:assets:bank:checking');
    const tip = page.locator('.entry-tooltip');
    const row = page.locator('#main-content tbody tr', { hasText: 'Cafe Luna' });
    await hoverSettled(page, row.locator('td.description'));
    await expect(tip).toBeVisible();
    await expect(tip).toContainText('Cafe Luna');
    const link = row.locator('td.account a');
    await expect(link).toHaveAttribute('title', 'expenses:food:dining');
    await link.hover();
    await expect(tip).toBeHidden();
    await page.waitForTimeout(800);
    await expect(tip).toBeHidden();
    expect(pageErrors).toEqual([]);
  });

});
