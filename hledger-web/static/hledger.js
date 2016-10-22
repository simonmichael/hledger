/* hledger web ui javascript */

//----------------------------------------------------------------------
// STARTUP

$(document).ready(function() {

  // ensure add form always focusses its first field
  $('#addmodal')
    .on('shown.bs.modal', function (e) {
      addformFocus();
    })

  // show add form if ?add=1
  if ($.url.param('add')) { addformShow(true); }

  // sidebar account hover handlers
  $('#sidebar td a').mouseenter(function(){ $(this).parent().addClass('mouseover'); });
  $('#sidebar td').mouseleave(function(){ $(this).removeClass('mouseover'); });

  // keyboard shortcuts
  // 'body' seems to hold focus better than document in FF
  $('body').bind('keydown', 'h',       function(){ $('#helpmodal').modal('toggle'); return false; });
  $('body').bind('keydown', 'shift+/', function(){ $('#helpmodal').modal('toggle'); return false; });
  $('body').bind('keydown', 'j',       function(){ location.href = document.hledgerWebBaseurl+'/journal'; return false; });
  $('body').bind('keydown', 's',       function(){ sidebarToggle(); return false; });
  $('body').bind('keydown', 'a',       function(){ addformShow(); return false; });
  $('body').bind('keydown', 'n',       function(){ addformShow(); return false; });
  $('body').bind('keydown', 'f',       function(){ $('#searchform input').focus(); return false; });
  $('body, #addform input, #addform select').bind('keydown', 'ctrl+shift+=', addformAddPosting);
  $('body, #addform input, #addform select').bind('keydown', 'ctrl+=',       addformAddPosting);
  $('body, #addform input, #addform select').bind('keydown', 'ctrl+-',       addformDeletePosting);
  $('.amount-input:last').keypress(addformAddPosting);


  // highlight the entry from the url hash
  if (window.location.hash && $(window.location.hash)[0]) {
    $(window.location.hash).addClass('highlighted');
  }
  $(window).on('hashchange', function(event) {
    $('.highlighted').removeClass('highlighted');
    $(window.location.hash).addClass('highlighted');
  });
  $('[data-toggle="offcanvas"]').click(function () {
      $('.row-offcanvas').toggleClass('active');
  });
});

//----------------------------------------------------------------------
// REGISTER CHART

function registerChart($container, series) {
  // https://github.com/flot/flot/blob/master/API.md
  return $container.plot(
    series,
    { /* general chart options */
      xaxis: {
        mode: "time",
        timeformat: "%Y/%m/%d"
      },
      legend: {
        position: 'sw'
      },
      grid: {
        markings:
         function (axes) {
          var now = Date.now();
          var markings = [
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
          return markings;
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
  if (item) {
    targetselector = '#'+item.series.data[item.dataIndex][5];
    $target = $(targetselector);
    if ($target.length) {
      window.location.hash = targetselector;
      $('html, body').animate({
        scrollTop: $target.offset().top
      }, 1000);
    }
  }
}

//----------------------------------------------------------------------
// ADD FORM

function addformShow(showmsg) {
  showmsg = typeof showmsg !== 'undefined' ? showmsg : false;
  addformReset(showmsg);
  $('#addmodal').modal('show');
}

// Make sure the add form is empty and clean for display.
function addformReset(showmsg) {
  showmsg = typeof showmsg !== 'undefined' ? showmsg : false;
  if ($('form#addform').length > 0) {
    if (!showmsg) $('div#message').html('');
    $('form#addform')[0].reset();
    // reset typehead state (though not fetched completions)
    $('.typeahead').typeahead('val', '');
    $('.tt-dropdown-menu').hide();
    $('input#date').val(''); // #322 don't set a default, typeahead(?) clears it on tab. See also Foundation.hs
  }
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
  $('.amount-input:last').off('keypress');
  // do nothing if it's not currently visible
  if (!$('#addform').is(':visible')) return;
  // save a copy of last row
  var lastrow = $('#addform .form-group:last').clone();

  // replace the submit button with an amount field, clear and renumber it, add the keybindings
  var num = $('#addform .account-group').length;

  // insert the new last row
  $('#addform .account-postings').append(lastrow);
  // TODO: Enable typehead on dynamically created inputs

  var $acctinput = $('.account-input:last');
  var $amntinput = $('.amount-input:last');
  // clear and renumber the field, add keybindings
  $acctinput
    .val('')
    .prop('id','account'+(num+1))
    .prop('name','account'+(num+1))
    .prop('placeholder','Account '+(num+1));
  //lastrow.find('input') // not :last this time
  $acctinput
    .bind('keydown', 'ctrl+shift+=', addformAddPosting)
    .bind('keydown', 'ctrl+=', addformAddPosting)
    .bind('keydown', 'ctrl+-', addformDeletePosting);

  $amntinput
    .val('')
    .prop('id','amount'+(num+1))
    .prop('name','amount'+(num+1))
    .prop('placeholder','Amount '+(num+1))
    .keypress(addformAddPosting);

  $acctinput
    .bind('keydown', 'ctrl+shift+=', addformAddPosting)
    .bind('keydown', 'ctrl+=', addformAddPosting)
    .bind('keydown', 'ctrl+-', addformDeletePosting);

}

// Remove the add form's last posting row, if empty, keeping at least two.
function addformDeletePosting() {
  var num = $('#addform .account-group').length;
  if (num <= 2) return;
  // remember if the last row's field or button had focus
  var focuslost =
    $('.account-input:last').is(':focus')
    || $('.amount-input:last').is(':focus');
  // delete last row
  $('#addform .account-group:last').remove();
  if(focuslost){
    focus($('account-input:last'));
  }
  // Rebind keypress
  $('.amount-input:last').keypress(addformAddPosting);
}

//----------------------------------------------------------------------
// SIDEBAR

function sidebarToggle() {
  $('#sidebar-menu').toggleClass('col-md-4 col-sm-4 col-any-0');
  $('#main-content').toggleClass('col-md-8 col-sm-8 col-md-12 col-sm-12');
  $.cookie('showsidebar', $('#sidebar-menu').hasClass('col-any-0') ? '0' : '1');
}

//----------------------------------------------------------------------
// MISC

function enableTypeahead($el, suggester) {
  return $el.typeahead(
    {
      highlight: true
    },
    {
      source: suggester.ttAdapter()
    }
  );
}

// function journalSelect(ev) {
//   var textareas = $('textarea', $('form#editform'));
//   for (i=0; i<textareas.length; i++) {
//     textareas[i].style.display = 'none';
//     textareas[i].disabled = true;
//   }
//   var targ = getTarget(ev);
//   if (targ.value) {
//     var journalid = targ.value+'_textarea';
//     var textarea = document.getElementById(journalid);
//   }
//   else {
//     var textarea = textareas[0];
//   }
//   textarea.style.display = 'block';
//   textarea.disabled = false;
//   return true;
// }

// // Get the current event's target in a robust way.
// // http://www.quirksmode.org/js/events_properties.html
// function getTarget(ev) {
//   var targ;
//   if (!ev) var ev = window.event;
//   if (ev.target) targ = ev.target;
//   else if (ev.srcElement) targ = ev.srcElement;
//   if (targ.nodeType == 3) targ = targ.parentNode;
//   return targ;
// }
