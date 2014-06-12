/* hledger web ui javascript */
/* depends on jquery etc. */

// /* show/hide things based on locally-saved state */
// happens too late with large main content in chrome, visible glitch
// if (localStorage.getItem('sidebarVisible') == "false")
// 	$('#sidebar').hide();
// /* or request parameters */
// if ($.url.param('sidebar')=='' || $.url.param('sidebar')=='0')
//   $('#sidebar').hide();
// else if ($.url.param('sidebar')=='1')
//   $('#sidebar').show();

if ($.url.param('add')) {
  $('#addform').collapse('show');
  $('#addform input[name=description]').focus();
}

$(document).ready(function() {

    /* sidebar account hover handlers */
    $('#sidebar td a').mouseenter(function(){ $(this).parent().addClass('mouseover'); });
    $('#sidebar td').mouseleave(function(){ $(this).removeClass('mouseover'); });

    /* keyboard shortcuts */
    $(document).bind('keydown', 'shift+/', function(){ $('#searchhelpmodal').modal('toggle'); return false; });
    $(document).bind('keydown', 'h',       function(){ $('#searchhelpmodal').modal('toggle'); return false; });
    $(document).bind('keydown', 'j',       function(){ location.href = '/journal'; return false; });
    $(document).bind('keydown', 's',       function(){ sidebarToggle(); return false; });
    $(document).bind('keydown', 'a',       function(){ addformFocus(); return false; });
    $('#addform input,#addform button,#addformlink').bind('keydown', 'esc', addformCancel);
    $(document).bind('keydown', '/',       function(){ $('#searchform input').focus(); return false; });
    $('#addform input,#addform button,#addformlink').bind('keydown', 'ctrl+shift+=', addformAddPosting);
    $('#addform input,#addform button,#addformlink').bind('keydown', 'ctrl+=', addformAddPosting);
    $('#addform input,#addform button,#addformlink').bind('keydown', 'ctrl+-', addformDeletePosting);

});

function sidebarToggle() {
  console.log('sidebarToggle');
  var visible = $('#sidebar').is(':visible');
  console.log('sidebar visibility was',visible);
  // if opening sidebar, start an ajax fetch of its content
  if (!visible) {
    //console.log('getting sidebar content');
    $.get("sidebar"
         ,null
         ,function(data) {
					  //console.log( "success" );
            $("#sidebar-body" ).html(data);
          })
					.done(function() {
					  //console.log( "success 2" );
					})
					.fail(function() {
					  //console.log( "error" );
					});
  }
	// localStorage.setItem('sidebarVisible', !visible);
  // set a cookie to communicate the new sidebar state to the server
  $.cookie('showsidebar', visible ? '0' : '1');
  // horizontally slide the sidebar in or out
  // how to make it smooth, without delayed content pop-in ?
  //$('#sidebar').animate({'width': 'toggle'});
  //$('#sidebar').animate({'width': visible ? 'hide' : '+=20m'});
  //$('#sidebar-spacer').width(200);
  $('#sidebar').animate({'width': visible ? 'hide' : 'show'});
}

function addformToggle() {
  if (location.pathname != '/journal') {
    location.href = '/journal?add=1';
  }
  else {
    $('#addform').collapse('toggle');
    $('#addform input[name=description]').focus();
  }
}

function addformFocus() {
  if (location.pathname != '/journal') {
    location.href = '/journal?add=1';
  }
  else {
    $('#addform').collapse('show');
    $('#addform input[name=description]').focus();
  }
}

function addformCancel() {
  $('#addform input[type=text]').typeahead('val','');
  $('#addform')
    .each( function(){ this.reset();} )
    .collapse('hide');
  // try to keep keybindings working in safari
  //$('#addformlink').focus();
}

function addformAddPosting() {
  var rownum = $('#addform tr.posting').length + 1;
  // XXX duplicates markup in Common.hs
  // duplicate last row
  $('#addform > table').append($('#addform > table tr:last').clone());
  // fix up second-last row
  $('#addform > table > tr.lastrow:first > td:last').html('');
  $('#addform > table > tr.lastrow:first').removeClass('lastrow');

  // fix up last row
  $('#addform table').append($('#addform table tr:last').clone());
  //     '<tr class="posting">' +
  //     '<td style="padding-left:2em;">' +
  //     '<input id="account'+rownum+'" class="form-control input-lg" style="width:100%;" type="text"' +
  //     ' name=account'+rownum+'" placeholder="Account '+rownum+'">'
  // );

  // $('#addbtncell').appendTo($('#addform table tr:last'))
  //                  );
}

function addformDeletePosting() {
}

function editformJournalSelect(ev) {
 var textareas = $('textarea', $('form#editform'));
 for (i=0; i<textareas.length; i++) {
   textareas[i].style.display = 'none';
   textareas[i].disabled = true;
 }
 var targ = getTarget(ev);
 if (targ.value) {
   var journalid = targ.value+'_textarea';
   var textarea = document.getElementById(journalid);
 }
 else {
   var textarea = textareas[0];
 }
 textarea.style.display = 'block';
 textarea.disabled = false;
 return true;
}

/*
// Get the current event's target in a robust way.
// http://www.quirksmode.org/js/events_properties.html
function getTarget(ev) {
  var targ;
  if (!ev) var ev = window.event;
  if (ev.target) targ = ev.target;
  else if (ev.srcElement) targ = ev.srcElement;
  if (targ.nodeType == 3) targ = targ.parentNode;
  return targ;
}
*/
