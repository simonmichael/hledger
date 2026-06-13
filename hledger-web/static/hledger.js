/* hledger web ui javascript */

//----------------------------------------------------------------------
// STARTUP

$(document).ready(function() {
  hledgerInitGlobal();
  hledgerInitPage();
  hledgerInitAjaxNavigation();
});

function hledgerInitGlobal() {
  // keyboard shortcuts
  // 'body' seems to hold focus better than document in FF
  $('body').bind('keydown', 'h',       function(){ $('#helpmodal').modal('toggle'); return false; });
  $('body').bind('keydown', 'shift+/', function(){ $('#helpmodal').modal('toggle'); return false; });
  $('body').bind('keydown', 'j',       function(){ location.href = document.hledgerWebBaseurl+'/journal'; return false; });
  $('body').bind('keydown', 's',       function(){ sidebarToggle(); return false; });
  $('body').bind('keydown', 'e',       function(){ emptyAccountsToggle(); return false; });
  $('body').bind('keydown', 'a',       function(){ addformShow(); return false; });
  $('body').bind('keydown', 'n',       function(){ addformShow(); return false; });
  $('body').bind('keydown', 'f',       function(){ $('#searchform input').focus(); return false; });

  // highlight the entry from the url hash
  if (window.location.hash && $(window.location.hash)[0]) {
    $(window.location.hash).addClass('highlighted');
  }
  $(window).on('hashchange', function() {
    $('.highlighted').removeClass('highlighted');
    $(window.location.hash).addClass('highlighted');
  });
  $('[data-toggle="offcanvas"]').click(function () {
      $('.row-offcanvas').toggleClass('active');
  });
}

function hledgerInitPage() {

  // add form helpers XXX move to addForm ?

  // date picker
  // http://bootstrap-datepicker.readthedocs.io/en/latest/options.html
  var dateEl = $('#dateWrap').datepicker({
    showOnFocus: false,
    autoclose: true,
    format: 'yyyy-mm-dd',
    todayHighlight: true,
    weekStart: 1 // Monday
  });;

  // focus and pre-fill the add form whenever it is shown
  $('#addmodal')
    .off('shown.bs.modal.hledger hidden.bs.modal.hledger')
    .on('shown.bs.modal.hledger', function() {
      addformFocus();
    })
    .on('hidden.bs.modal.hledger', function() {
      // close the date picker if open
      dateEl.datepicker('hide');
    });

  // ensure that the keypress listener on the final amount input is always active
  $('#addform')
    .off('focus.hledger')
    .on('focus.hledger', '.amount-input:last', function() {
      addformLastAmountBindKey();
    });
}

function hledgerInitAjaxNavigation() {
  if (!window.history || !window.history.pushState || !window.DOMParser || !window.URL || !window.location.origin) {
    return;
  }

  $(document).off('click.hledgerAjaxNavigation')
    .on('click.hledgerAjaxNavigation', '#sidebar-menu a[href], #main-content a[href]', function(ev) {
      if (!hledgerAjaxCanHandleLink(this, ev)) {
        return;
      }
      ev.preventDefault();
      hledgerAjaxNavigate(this.href, true);
    });

  $(window).off('popstate.hledgerAjaxNavigation')
    .on('popstate.hledgerAjaxNavigation', function() {
      hledgerAjaxNavigate(window.location.href, false);
    });
}

function hledgerAjaxCanHandleLink(link, ev) {
  if (ev.isDefaultPrevented() || ev.metaKey || ev.ctrlKey || ev.shiftKey || ev.altKey) {
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
  $.ajax({
    url: href,
    method: 'GET',
    dataType: 'html',
    cache: false
  }).done(function(html) {
    if (!hledgerAjaxApplyPage(html, href, pushHistory)) {
      window.location.href = href;
    }
  }).fail(function() {
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

  var $oldSidebar = $('#sidebar-menu');
  var sidebarScrollTop = $oldSidebar.scrollTop();
  var $newMain = $(newMain);
  var scripts = $newMain.find('script').remove().toArray();

  $('#main-content').replaceWith($newMain);
  $oldSidebar.find('.main-menu').replaceWith($(newSidebar).find('.main-menu'));
  $oldSidebar.scrollTop(sidebarScrollTop);

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
  $.each(scripts, function(_i, script) {
    if (script.src) {
      $.ajax({url: script.src, dataType: 'script', async: false});
    } else {
      $.globalEval(script.text || script.textContent || script.innerHTML || '');
    }
  });
}

function hledgerHighlightHash() {
  $('.highlighted').removeClass('highlighted');
  if (window.location.hash && $(window.location.hash)[0]) {
    $(window.location.hash).addClass('highlighted');
  }
}

function hledgerScrollToHashOrTop() {
  if (window.location.hash && $(window.location.hash)[0]) {
    window.scrollTo(0, $(window.location.hash).offset().top);
  } else {
    window.scrollTo(0, 0);
  }
}

//----------------------------------------------------------------------
// ADD FORM

function addformShow(showmsg) {
  showmsg = typeof showmsg !== 'undefined' ? showmsg : false;
  addformReset(showmsg);
  $('#addmodal').modal('show');
}

// Make sure the add form is empty and clean and has the default number of rows.
function addformReset(showmsg) {
  showmsg = typeof showmsg !== 'undefined' ? showmsg : false;
  if ($('form#addform').length > 0) {
    if (!showmsg) $('div#message').html('');
    $('#addform .account-group.added-row').remove();
    addformLastAmountBindKey();
    $('#addform')[0].reset();
    // reset typehead state (though not fetched completions)
    $('.typeahead').typeahead('val', '');
    $('.tt-dropdown-menu').hide();
  }
}

// Set the add-new-row-on-keypress handler on the add form's current last amount field, only.
// (NB: removes all other keypress handlers from all amount fields).
function addformLastAmountBindKey() {
  $('input[name=amount]').off('keypress');
  $('input[name=amount]:last').keypress(addformAddPosting);
}

// Pre-fill today's date and focus the description field in the add form.
function addformFocus() {
  $('#addform input[name=date]').val(isoDate());
  focus($('#addform input[name=description]'));
}

function isoDate() {
  return new Date().toLocaleDateString("sv");  // https://stackoverflow.com/a/58633651/84401
}

// Focus a jquery-wrapped element, working around http://stackoverflow.com/a/7046837.
function focus($el) {
  setTimeout(function (){
    $el.focus();
  }, 0);
}

// Insert another posting row in the add form.
function addformAddPosting() {
  if (!$('#addform').is(':visible')) { return; }

  // Clone the last row.
  var newrow = $('#addform .account-group:last').clone().addClass('added-row');
  var newnum = $('#addform .account-group').length + 1;

  // Clear the new account and amount fields and update their placeholder text.
  var accountfield = newrow.find('input[name=account]');
  var amountfield  = newrow.find('input[name=amount]');
  accountfield.val('').prop('placeholder', 'Account '+newnum);
  amountfield.val('').prop('placeholder', 'Amount '+newnum);

  // Enable autocomplete in the new account field.
  // We must first remove these typehead helper elements cloned from the old row,
  // or it will recursively add helper elements for those, causing confusion (#2215).
  newrow.find('.tt-hint').remove();
  newrow.find('.tt-input').removeClass('tt-input');
  accountfield.typeahead({ highlight: true }, { source: globalThis.accountsCompleter.ttAdapter() });

  // Add the new row to the page.
  $('#addform .account-postings').append(newrow);

  // And move the keypress handler to the new last amount field.
  addformLastAmountBindKey();
}

// Remove the add form's last posting row, if empty, keeping at least two.
function addformDeletePosting() {
  if ($('#addform .account-group').length <= 2) {
    return;
  }
  // remember if the last row's field or button had focus
  var focuslost =
    $('.account-input:last').is(':focus')
    || $('.amount-input:last').is(':focus');
  // delete last row
  $('#addform .account-group:last').remove();
  if (focuslost) {
    focus($('.account-input:last'));
  }
  // move the keypress handler to the new last amount field
  addformLastAmountBindKey();
}

//----------------------------------------------------------------------
// SIDEBAR

function sidebarToggle() {
  $('#sidebar-menu').toggleClass('col-md-4 col-sm-4 col-any-0');
  $('#main-content').toggleClass('col-md-8 col-sm-8 col-md-12 col-sm-12');
  $('#spacer').toggleClass('col-md-4 col-sm-4 col-any-0');
  $.cookie('showsidebar', $('#sidebar-menu').hasClass('col-any-0') ? '0' : '1');
}

function emptyAccountsToggle() {
  $('.acct.empty').parent().toggleClass('hide');
  $.cookie('hideemptyaccts', $.cookie('hideemptyaccts') === '1' ? '0' : '1')
}
