/* hledger web ui javascript */

//----------------------------------------------------------------------
// STARTUP

$(document).ready(function() {

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
    .on('shown.bs.modal', function() {
      addformFocus();
    })
    .on('hidden.bs.modal', function() {
      // close the date picker if open
      dateEl.datepicker('hide');
    });

  // ensure that the keypress listener on the final amount input is always active
  $('#addform')
    .on('focus', '.amount-input:last', function() {
      addformLastAmountBindKey();
    });

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
});

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
