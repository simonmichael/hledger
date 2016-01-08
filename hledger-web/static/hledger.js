/* hledger web ui javascript */

//----------------------------------------------------------------------
// STARTUP

$(document).ready(function() {

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
  $('#addform tr.posting:last > td:first input').bind('keydown', 'tab', addformAddPostingWithTab);


  // highlight the entry from the url hash
  if (window.location.hash && $(window.location.hash)[0]) {
    $(window.location.hash).addClass('highlighted');
  }
  $(window).on('hashchange', function(event) {
    $('.highlighted').removeClass('highlighted');
    $(window.location.hash).addClass('highlighted');
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
  $('#addmodal')
    .on('shown.bs.modal', function (e) {
      addformFocus();
    })
    .modal('show');
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
    $('input#date').val('today');
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
  // do nothing if it's not currently visible
  if (!$('#addform').is(':visible')) return;
  // save a copy of last row
  var lastrow = $('#addform tr.posting:last').clone();

  // replace the submit button with an amount field, clear and renumber it, add the keybindings
  $('#addform tr.posting:last > td:last')
    .html( $('#addform tr.posting:first > td:last').html() );
  var num = $('#addform tr.posting').length;
  $('#addform tr.posting:last > td:last input:last') // input:last here and elsewhere is to avoid autocomplete's extra input
    .val('')
    .prop('id','amount'+num)
    .prop('name','amount'+num)
    .prop('placeholder','Amount '+num)
    .bind('keydown', 'ctrl+shift+=', addformAddPosting)
    .bind('keydown', 'ctrl+=', addformAddPosting)
    .bind('keydown', 'ctrl+-', addformDeletePosting);

  // set up the new last row's account field.
  // First typehead, it's hard to enable on new DOM elements
  var $acctinput = lastrow.find('.account-input:last');
  // XXX nothing works
  // $acctinput.typeahead('destroy'); //,'NoCached');
  // lastrow.on("DOMNodeInserted", function () {
  //   //$(this).find(".typeahead").typeahead();
  //   console.log('DOMNodeInserted');
  //  // infinite loop
  //  console.log($(this).find('.typeahead'));
  //   //enableTypeahead($(this).find('.typeahead'), accountsSuggester);
  // });
  // setTimeout(function (){
  //   $('#addform tr.posting:last input.account-input').typeahead('destroy');
  //   enableTypeahead($('#addform tr.posting:last input.account-input:last'), accountsSuggester);
  // }, 1000);

  // insert the new last row
  $('#addform > table > tbody').append(lastrow);

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
    .bind('keydown', 'ctrl+-', addformDeletePosting)
    .bind('keydown', 'tab', addformAddPostingWithTab);
}

// Insert another posting row by tabbing within the last field, also advancing the focus.
function addformAddPostingWithTab(ev) {
  // do nothing if called from a non-last row (don't know how to remove keybindings)
  if ($(ev.target).is('#addform input.account-input:last')) {
    addformAddPosting();
    focus($('#addform input.amount-input:last')); // help FF
    return false;
  }
  else
    return true;
}

// Remove the add form's last posting row, if empty, keeping at least two.
function addformDeletePosting() {
  var num = $('#addform tr.posting').length;
  if (num <= 2
      || $('#addform tr.posting:last > td:first input:last').val() != ''
     ) return;
  // copy submit button
  var btn = $('#addform tr.posting:last > td:last').html();
  // remember if the last row's field or button had focus
  var focuslost =
    $('#addform tr.posting:last > td:first input:last').is(':focus')
    || $('#addform tr.posting:last button').is(':focus');
  // delete last row
  $('#addform tr.posting:last').remove();
  // remember if the last amount field had focus
  focuslost = focuslost || 
    $('#addform tr.posting:last > td:last input:last').is(':focus');
  // replace new last row's amount field with the button
  $('#addform tr.posting:last > td:last').css('text-align','right').html(btn);
  // if deleted row had focus, focus the new last row
  if (focuslost) $('#addform tr.posting:last > td:first input:last').focus();
}

//----------------------------------------------------------------------
// SIDEBAR

function sidebarToggle() {
  var visible = $('#sidebar').is(':visible');
  // if opening sidebar, start an ajax fetch of its content
  if (!visible) {
    $.get("sidebar"
         ,null
         ,function(data) {
            $("#sidebar-body" ).html(data);
          })
          .fail(function() {
            alert("Loading the sidebar did fail");
          });
  }
  // set a cookie to communicate the new sidebar state to the server
  $.cookie('showsidebar', visible ? '0' : '1');
  $('#sidebar').animate({'width': visible ? 'hide' : 'show'});
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
