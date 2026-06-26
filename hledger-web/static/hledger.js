/* hledger web ui javascript */

//----------------------------------------------------------------------
// STARTUP

document.addEventListener('DOMContentLoaded', function() {
  hledgerInitGlobal();
  hledgerInitPage();
  hledgerInitAjaxNavigation();
});

function hledgerInitGlobal() {
  // keyboard shortcuts
  // 'body' seems to hold focus better than document in FF
  document.addEventListener('keydown', function(e) {
    // Don't trigger shortcuts if typing in an input or textarea
    var activeElement = document.activeElement;
    var isInput = activeElement && (
      activeElement.tagName === 'INPUT' ||
      activeElement.tagName === 'TEXTAREA' ||
      activeElement.isContentEditable
    );
    if (isInput) return;

    if (e.key === 'h' && !e.ctrlKey && !e.altKey && !e.metaKey) {
      toggleModal('helpmodal');
      e.preventDefault();
    }
    if (e.key === '/' && e.shiftKey && !e.ctrlKey && !e.altKey && !e.metaKey) {
      toggleModal('helpmodal');
      e.preventDefault();
    }
    if (e.key === 'j' && !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) {
      location.href = document.hledgerWebBaseurl+'/journal';
      e.preventDefault();
    }
    if (e.key === 's' && !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) {
      sidebarToggle();
      e.preventDefault();
    }
    if (e.key === 'e' && !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) {
      emptyAccountsToggle();
      e.preventDefault();
    }
    if (e.key === 'a' && !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) {
      addformShow();
      e.preventDefault();
    }
    if (e.key === 'n' && !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) {
      addformShow();
      e.preventDefault();
    }
    if (e.key === 'f' && !e.ctrlKey && !e.altKey && !e.metaKey && !e.shiftKey) {
      var searchInput = document.querySelector('#searchform input');
      if (searchInput) searchInput.focus();
      e.preventDefault();
    }
  });

  // highlight the entry from the url hash
  if (window.location.hash && document.querySelector(window.location.hash)) {
    document.querySelector(window.location.hash).classList.add('highlighted');
  }
  window.addEventListener('hashchange', function() {
    document.querySelectorAll('.highlighted').forEach(function(el) {
      el.classList.remove('highlighted');
    });
    if (window.location.hash && document.querySelector(window.location.hash)) {
      document.querySelector(window.location.hash).classList.add('highlighted');
    }
  });
  document.querySelectorAll('[data-toggle="offcanvas"]').forEach(function(el) {
    el.addEventListener('click', function() {
      document.querySelectorAll('.row-offcanvas').forEach(function(row) {
        row.classList.toggle('active');
      });
    });
  });
  
  // Bootstrap 5 offcanvas toggle
  document.querySelectorAll('[data-bs-toggle="offcanvas"]').forEach(function(el) {
    el.addEventListener('click', function() {
      document.querySelectorAll('.row-offcanvas').forEach(function(row) {
        row.classList.toggle('active');
      });
    });
  });
}

function toggleModal(modalId) {
  var modal = document.getElementById(modalId);
  if (modal) {
    var modalInstance = bootstrap.Modal.getInstance(modal);
    if (modalInstance) {
      modalInstance.toggle();
    } else {
      new bootstrap.Modal(modal).toggle();
    }
  }
}

function hledgerInitPage() {

  // add form helpers XXX move to addForm ?

  // date picker - using HTML5 date input for simplicity
  var dateEl = document.querySelector('#dateWrap input');
  if (dateEl) {
    dateEl.type = 'date';
  }

  // focus and pre-fill the add form whenever it is shown
  var addmodal = document.getElementById('addmodal');
  if (addmodal) {
    addmodal.addEventListener('shown.bs.modal', function() {
      addformFocus();
    });
    addmodal.addEventListener('hidden.bs.modal', function() {
      // date picker cleanup if needed
    });
  }

  // ensure that the keypress listener on the final amount input is always active
  var addform = document.getElementById('addform');
  if (addform) {
    addform.addEventListener('focus', function(e) {
      if (e.target.classList.contains('amount-input') &&
          e.target === addform.querySelector('.amount-input:last')) {
        addformLastAmountBindKey();
      }
    }, true);
  }

  // set checkbox state from cookie
  var checkbox = document.getElementById('hideEmptyAccounts');
  if (checkbox) {
    checkbox.checked = getCookie('hideemptyaccts') === 'true';
  }

  // restore hide empty accounts state from cookie
  if (getCookie('hideemptyaccts')) {
    var emptyAccts = document.querySelectorAll('.acct.empty');
    emptyAccts.forEach(function(acct) {
      if (acct.parentElement) {
        acct.parentElement.classList.add('hide');
      }
    });
  }
  
  // initialize body class for sidebar state
  var isMobile = window.innerWidth <= 768;
  var showSidebarCookie = getCookie('showsidebar');
  
  // On mobile, always treat as sidebar closed
  if (isMobile || showSidebarCookie === 'false') {
    document.body.classList.add('sidebar-hidden');
    document.body.classList.remove('sidebar-open');
  } else {
    document.body.classList.add('sidebar-open');
    document.body.classList.remove('sidebar-hidden');
  }

  // sync sidebar grid classes with cookie state
  var sidebar = document.getElementById('sidebar-menu');
  var mainContent = document.getElementById('main-content');
  var spacer = document.getElementById('spacer');
  if (sidebar && mainContent && spacer) {
    if (isMobile || showSidebarCookie === 'false') {
      // sidebar should be hidden
      sidebar.classList.remove('col-md-4', 'col-sm-4');
      sidebar.classList.add('col-any-0');
      mainContent.classList.remove('col-md-8', 'col-sm-8');
      mainContent.classList.add('col-md-12', 'col-sm-12');
      spacer.classList.remove('col-md-4', 'col-sm-4');
      spacer.classList.add('col-any-0');
    } else {
      // sidebar should be visible
      sidebar.classList.add('col-md-4', 'col-sm-4');
      sidebar.classList.remove('col-any-0');
      mainContent.classList.add('col-md-8', 'col-sm-8');
      mainContent.classList.remove('col-md-12', 'col-sm-12');
      spacer.classList.add('col-md-4', 'col-sm-4');
      spacer.classList.remove('col-any-0');
    }
  }
}

function hledgerInitAjaxNavigation() {
  if (!window.history || !window.history.pushState || !window.DOMParser || !window.URL || !window.location.origin) {
    return;
  }

  document.addEventListener('click', function(ev) {
    var link = ev.target.closest('#sidebar-menu a[href], #main-content a[href]');
    if (link && hledgerAjaxCanHandleLink(link, ev)) {
      ev.preventDefault();
      hledgerAjaxNavigate(link.href, true);
    }
  });

  window.addEventListener('popstate', function() {
    hledgerAjaxNavigate(window.location.href, false);
  });
}

function hledgerAjaxCanHandleLink(link, ev) {
  if (ev.defaultPrevented || ev.metaKey || ev.ctrlKey || ev.shiftKey || ev.altKey) {
    return false;
  }
  if (link.target && link.target !== '_self') {
    return false;
  }

  var url = new URL(link.href, window.location.href);
  if (url.origin !== window.location.origin) {
    return false;
  }

  var path = url.pathname.replace(/\/$/, '');
  return path === hledgerAjaxRoutePath('/journal') || path === hledgerAjaxRoutePath('/register');
}

function hledgerAjaxRoutePath(route) {
  var base = new URL(document.hledgerWebBaseurl, window.location.href);
  var basePath = base.pathname.replace(/\/$/, '');
  return basePath + route;
}

function hledgerAjaxNavigate(href, pushHistory) {
  fetch(href, {
    method: 'GET',
    cache: 'no-cache',
    headers: {
      'Accept': 'text/html'
    }
  }).then(function(response) {
    if (!response.ok) {
      throw new Error('Network response was not ok');
    }
    return response.text();
  }).then(function(html) {
    if (!hledgerAjaxApplyPage(html, href, pushHistory)) {
      window.location.href = href;
    }
  }).catch(function() {
    window.location.href = href;
  });
}

function hledgerAjaxApplyPage(html, href, pushHistory) {
  var doc = new DOMParser().parseFromString(html, 'text/html');
  var newMain = doc.querySelector('#main-content');
  var newSidebar = doc.querySelector('#sidebar-menu');
  if (!newMain || !newSidebar) {
    return false;
  }

  var oldSidebar = document.getElementById('sidebar-menu');
  var sidebarScrollTop = oldSidebar.scrollTop;
  var scripts = Array.from(newMain.querySelectorAll('script'));
  scripts.forEach(function(s) { s.remove(); });

  var mainContent = document.getElementById('main-content');
  mainContent.replaceWith(newMain);
  
  var oldMainMenu = oldSidebar.querySelector('.main-menu');
  var newMainMenu = newSidebar.querySelector('.main-menu');
  if (oldMainMenu && newMainMenu) {
    oldMainMenu.replaceWith(newMainMenu);
  }
  oldSidebar.scrollTop = sidebarScrollTop;

  if (doc.title) {
    document.title = doc.title;
  }
  if (pushHistory) {
    window.history.pushState({hledgerAjax: true}, doc.title || '', href);
  }

  hledgerInitPage();
  hledgerAjaxRunScripts(scripts);
  hledgerHighlightHash();
  hledgerScrollToHashOrTop();
  return true;
}

function hledgerAjaxRunScripts(scripts) {
  scripts.forEach(function(script) {
    if (script.src) {
      var newScript = document.createElement('script');
      newScript.src = script.src;
      document.head.appendChild(newScript);
    } else {
      var code = script.text || script.textContent || script.innerHTML || '';
      var newScript = document.createElement('script');
      newScript.textContent = code;
      document.head.appendChild(newScript);
    }
  });
}

function hledgerHighlightHash() {
  document.querySelectorAll('.highlighted').forEach(function(el) {
    el.classList.remove('highlighted');
  });
  if (window.location.hash && document.querySelector(window.location.hash)) {
    document.querySelector(window.location.hash).classList.add('highlighted');
  }
}

function hledgerScrollToHashOrTop() {
  if (window.location.hash && document.querySelector(window.location.hash)) {
    var element = document.querySelector(window.location.hash);
    var rect = element.getBoundingClientRect();
    window.scrollTo(0, rect.top + window.scrollY);
  } else {
    window.scrollTo(0, 0);
  }
}

//----------------------------------------------------------------------
// ADD FORM

function addformShow(showmsg) {
  showmsg = typeof showmsg !== 'undefined' ? showmsg : false;
  addformReset(showmsg);
  var addmodal = document.getElementById('addmodal');
  if (addmodal) {
    new bootstrap.Modal(addmodal).show();
  }
}

// Make sure the add form is empty and clean and has the default number of rows.
function addformReset(showmsg) {
  showmsg = typeof showmsg !== 'undefined' ? showmsg : false;
  var addform = document.getElementById('addform');
  if (addform) {
    if (!showmsg) {
      var messageDiv = document.getElementById('message');
      if (messageDiv) messageDiv.innerHTML = '';
    }
    var addedRows = addform.querySelectorAll('.account-group.added-row');
    addedRows.forEach(function(row) { row.remove(); });
    addformLastAmountBindKey();
    addform.reset();
    // reset typeahead state (though not fetched completions)
    var typeaheads = addform.querySelectorAll('.typeahead');
    typeaheads.forEach(function(t) {
      if (t.typeahead) t.typeahead('val', '');
    });
    var dropdowns = document.querySelectorAll('.tt-dropdown-menu');
    dropdowns.forEach(function(d) { d.style.display = 'none'; });
  }
}

// Set the add-new-row-on-keypress handler on the add form's current last amount field, only.
// (NB: removes all other keypress handlers from all amount fields).
function addformLastAmountBindKey() {
  var amountInputs = document.querySelectorAll('input[name=amount]');
  amountInputs.forEach(function(input) {
    input.removeEventListener('keypress', addformAddPosting);
  });
  var lastAmountInput = amountInputs[amountInputs.length - 1];
  if (lastAmountInput) {
    lastAmountInput.addEventListener('keypress', addformAddPosting);
  }
}

// Pre-fill today's date and focus the description field in the add form.
function addformFocus() {
  var dateInput = document.querySelector('#addform input[name=date]');
  if (dateInput) dateInput.value = isoDate();
  var descInput = document.querySelector('#addform input[name=description]');
  if (descInput) focus(descInput);
}

function isoDate() {
  return new Date().toLocaleDateString("sv");  // https://stackoverflow.com/a/58633651/84401
}

// Focus an element, working around http://stackoverflow.com/a/7046837.
function focus(el) {
  setTimeout(function (){
    el.focus();
  }, 0);
}

// Insert another posting row in the add form.
function addformAddPosting() {
  var addform = document.getElementById('addform');
  if (!addform || addform.style.display === 'none') { return; }

  // Clone the last row.
  var lastRow = addform.querySelector('.account-group:last');
  var newrow = lastRow.cloneNode(true);
  newrow.classList.add('added-row');
  var newnum = addform.querySelectorAll('.account-group').length + 1;

  // Clear the new account and amount fields and update their placeholder text.
  var accountfield = newrow.querySelector('input[name=account]');
  var amountfield  = newrow.querySelector('input[name=amount]');
  if (accountfield) {
    accountfield.value = '';
    accountfield.placeholder = 'Account '+newnum;
  }
  if (amountfield) {
    amountfield.value = '';
    amountfield.placeholder = 'Amount '+newnum;
  }

  // Enable autocomplete in the new account field.
  // We must first remove these typehead helper elements cloned from the old row,
  // or it will recursively add helper elements for those, causing confusion (#2215).
  var ttHints = newrow.querySelectorAll('.tt-hint');
  ttHints.forEach(function(h) { h.remove(); });
  var ttInputs = newrow.querySelectorAll('.tt-input');
  ttInputs.forEach(function(i) { i.classList.remove('tt-input'); });
  if (accountfield && accountfield.typeahead && globalThis.accountsCompleter) {
    accountfield.typeahead({ highlight: true }, { source: globalThis.accountsCompleter.ttAdapter() });
  }

  // Add the new row to the page.
  var postings = addform.querySelector('.account-postings');
  if (postings) postings.appendChild(newrow);

  // And move the keypress handler to the new last amount field.
  addformLastAmountBindKey();
}

// Remove the add form's last posting row, if empty, keeping at least two.
function addformDeletePosting() {
  var addform = document.getElementById('addform');
  if (!addform) return;
  var accountGroups = addform.querySelectorAll('.account-group');
  if (accountGroups.length <= 2) {
    return;
  }
  // remember if the last row's field or button had focus
  var lastAccountInput = addform.querySelector('.account-input:last');
  var lastAmountInput = addform.querySelector('.amount-input:last');
  var focuslost =
    (lastAccountInput && document.activeElement === lastAccountInput)
    || (lastAmountInput && document.activeElement === lastAmountInput);
  // delete last row
  var lastGroup = addform.querySelector('.account-group:last');
  if (lastGroup) lastGroup.remove();
  if (focuslost) {
    var newLastAccountInput = addform.querySelector('.account-input:last');
    if (newLastAccountInput) focus(newLastAccountInput);
  }
  // move the keypress handler to the new last amount field
  addformLastAmountBindKey();
}

//----------------------------------------------------------------------
// SIDEBAR

function sidebarToggle() {
  var sidebar = document.getElementById('sidebar-menu');
  var mainContent = document.getElementById('main-content');
  var spacer = document.getElementById('spacer');
  
  // Check if we're on mobile
  var isMobile = window.innerWidth <= 768;
  
  if (isMobile) {
    // Mobile: toggle offcanvas and body classes
    if (sidebar) {
      sidebar.classList.toggle('active');
      // Update body classes for mobile close sidebar arrow visibility
      if (sidebar.classList.contains('active')) {
        document.body.classList.add('sidebar-open');
        document.body.classList.remove('sidebar-hidden');
      } else {
        document.body.classList.add('sidebar-hidden');
        document.body.classList.remove('sidebar-open');
      }
    }
  } else {
    // Desktop: toggle grid classes
    if (sidebar) {
      sidebar.classList.toggle('col-md-4');
      sidebar.classList.toggle('col-sm-4');
      sidebar.classList.toggle('col-any-0');
    }
    if (mainContent) {
      mainContent.classList.toggle('col-md-8');
      mainContent.classList.toggle('col-sm-8');
      mainContent.classList.toggle('col-md-12');
      mainContent.classList.toggle('col-sm-12');
    }
    if (spacer) {
      spacer.classList.toggle('col-md-4');
      spacer.classList.toggle('col-sm-4');
      spacer.classList.toggle('col-any-0');
    }
    var showSidebar = sidebar && !sidebar.classList.contains('col-any-0') ? 'true' : 'false';
    setCookie('showsidebar', showSidebar, 365);
    
    // Toggle sidebar-open
    if (showSidebar === 'true') {
      document.body.classList.add('sidebar-open');
      document.body.classList.remove('sidebar-hidden');
    } else {
      document.body.classList.add('sidebar-hidden');
      document.body.classList.remove('sidebar-open');
    }
  }
}

function emptyAccountsToggle() {
  var emptyAccts = document.querySelectorAll('.acct.empty');
  emptyAccts.forEach(function(acct) {
    if (acct.parentElement) {
      acct.parentElement.classList.toggle('hide');
    }
  });
  var hideEmpty = getCookie('hideemptyaccts') === 'true' ? 'false' : 'true';
  setCookie('hideemptyaccts', hideEmpty, 365);
  
  // Sync checkbox state
  var checkbox = document.getElementById('hideEmptyAccounts');
  if (checkbox) {
    checkbox.checked = hideEmpty === 'true';
  }
}

function emptyAccountsToggleCheckbox() {
  var checkbox = document.getElementById('hideEmptyAccounts');
  var shouldHide = checkbox.checked;
  
  var emptyAccts = document.querySelectorAll('.acct.empty');
  emptyAccts.forEach(function(acct) {
    if (acct.parentElement) {
      if (shouldHide) {
        acct.parentElement.classList.add('hide');
      } else {
        acct.parentElement.classList.remove('hide');
      }
    }
  });
  
  setCookie('hideemptyaccts', shouldHide ? 'true' : 'false', 365);
}

// Cookie helper functions
function setCookie(name, value, days) {
  var expires = '';
  if (days) {
    var date = new Date();
    date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
    expires = '; expires=' + date.toUTCString();
  }
  document.cookie = name + '=' + value + expires + '; path=/';
}

function getCookie(name) {
  var nameEQ = name + '=';
  var ca = document.cookie.split(';');
  for (var i = 0; i < ca.length; i++) {
    var c = ca[i];
    while (c.charAt(0) === ' ') c = c.substring(1, c.length);
    if (c.indexOf(nameEQ) === 0) return c.substring(nameEQ.length, c.length);
  }
  return null;
}
