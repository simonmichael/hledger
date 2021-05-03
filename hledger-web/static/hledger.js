/* hledger web ui javascript */

//----------------------------------------------------------------------
// STARTUP

$(document).ready(function() {
  // date picker
  // http://bootstrap-datepicker.readthedocs.io/en/latest/options.html
  var dateEl = $('#dateWrap').datepicker({
    showOnFocus: false,
    autoclose: true,
    format: 'yyyy-mm-dd',
    weekStart: 1 // Monday
  });;

  // ensure add form always focuses its first field
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
// REGISTER CHART

//eslint-disable-next-line no-unused-vars
function registerChart($container, series) {
  // https://github.com/flot/flot/blob/master/API.md
  return $container.plot(
    series,
    { /* general chart options */
      xaxis: {
        mode: "time",
        timeformat: "%Y/%m/%d",
      },
      selection: {
        mode: "x"
      },
      legend: {
        position: 'sw'
      },
      grid: {
        markings: function () {
          var now = Date.now();
          return [
            {
              xaxis: { to: now }, // past
              yaxis: { to: 0 },   // <0
              color: '#ffdddd',
            },
            {
              xaxis: { from: now }, // future
              yaxis: { from: 0 },   // >0
              color: '#e0e0e0',
            },
            {
              xaxis: { from: now }, // future
              yaxis: { to: 0 },     // <0
              color: '#e8c8c8',
            },
            {
              yaxis: { from: 0, to: 0 }, // =0
              color: '#bb0000',
              lineWidth:1
            },
          ];
        },
        hoverable: true,
        autoHighlight: true,
        clickable: true,
      },
      /* https://github.com/krzysu/flot.tooltip */
      tooltip: true,
      tooltipOpts: {
        xDateFormat: "%Y/%m/%d",
        content:
          function(label, x, y, flotitem) {
            var data = flotitem.series.data[flotitem.dataIndex];
            return data[3]+" balance on %x after "+data[2]+" posted by transaction:<pre>"+data[4]+"</pre>";
          },
        onHover: function(flotitem, $tooltipel) {
          $tooltipel.css('border-color',flotitem.series.color);
        },
      },
    }
  ).data("plot");
}

function registerChartClick(ev, pos, item) {
  if (!item) {
    return;
  }
  var targetselector = '#' + item.series.data[item.dataIndex][5];
  var $target = $(targetselector);
  if ($target.length) {
    window.location.hash = targetselector;
    $('html, body').animate({
      scrollTop: $target.offset().top
    }, 1000);
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
  $('.amount-input').off('keypress');
  $('.amount-input:last').keypress(addformAddPosting);
}

// Focus the first add form field.
function addformFocus() {
  focus($('#addform input#date'));
}

// Focus a jquery-wrapped element, working around http://stackoverflow.com/a/7046837.
function focus($el) {
  setTimeout(function (){
    $el.focus();
  }, 0);
}

// Insert another posting row in the add form.
function addformAddPosting() {
  if (!$('#addform').is(':visible')) {
    return;
  }
  // Clone the old last row to make a new last row
  $('#addform .account-postings').append( $('#addform .account-group:last').clone().addClass('added-row') );
  // renumber and clear the new last account and amount fields
  var n = $('#addform .account-group').length;
  $('.account-input:last').prop('placeholder', 'Account '+n).val('');
  $('.amount-input:last').prop('placeholder','Amount '+n).val('');  // XXX Enable typehead on dynamically created inputs
  // and move the keypress handler to the new last amount field
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
