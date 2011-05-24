/* hledger web ui javascripts */
/* depends on jquery, other support libs, and additional js inserted inline */

$(document).ready(function() {

    /* maybe show forms */
    if ($.url.param('add')) addformToggle();
    else if ($.url.param('edit')) editformToggle();

});

function filterformToggle() {
 var a = document.getElementById('addform');
 var e = document.getElementById('editform');
 var f = document.getElementById('filterform');
 var i = document.getElementById('importform');
 var t = document.getElementById('transactions');
 var alink = document.getElementById('addformlink');
 var elink = document.getElementById('editformlink');
 var flink = document.getElementById('filterformlink');
 var ilink = document.getElementById('importformlink');
 var jlink = document.getElementById('journallink');
 var rlink = document.getElementById('registerlink');

 if (f.style.display == 'none') {
  flink.style['font-weight'] = 'bold';
  f.style.display = 'block';
 } else {
  flink.style['font-weight'] = 'normal';
  f.style.display = 'none';
 }
 return false;
}

function addformToggle(ev) {
 var a = document.getElementById('addform');
 var e = document.getElementById('editform');
 var f = document.getElementById('filterform');
 var i = document.getElementById('importform');
 var t = document.getElementById('transactions');
 var alink = document.getElementById('addformlink');
 var elink = document.getElementById('editformlink');
 var flink = document.getElementById('filterformlink');
 var ilink = document.getElementById('importformlink');
 var jlink = document.getElementById('journallink');
 var rlink = document.getElementById('registerlink');

 if (a.style.display == 'none') {
  alink.style['font-weight'] = 'bold';
  elink.style['font-weight'] = 'normal';
  ilink.style['font-weight'] = 'normal';
  jlink.style['font-weight'] = 'normal';
  rlink.style['font-weight'] = 'normal';
  a.style.display = 'block';
  e.style.display = 'none';
  i.style.display = 'none';
  t.style.display = 'none';
 } else {
  alink.style['font-weight'] = 'normal';
  elink.style['font-weight'] = 'normal';
  ilink.style['font-weight'] = 'normal';
  a.style.display = 'none';
  e.style.display = 'none';
  i.style.display = 'none';
  t.style.display = 'block';
 }
 return false;
}

function editformToggle(ev) {
 var a = document.getElementById('addform');
 var e = document.getElementById('editform');
 var ej = document.getElementById('journalselect');
 var f = document.getElementById('filterform');
 var i = document.getElementById('importform');
 var t = document.getElementById('transactions');
 var alink = document.getElementById('addformlink');
 var elink = document.getElementById('editformlink');
 var flink = document.getElementById('filterformlink');
 var ilink = document.getElementById('importformlink');
 var jlink = document.getElementById('journallink');
 var rlink = document.getElementById('registerlink');

 if (e.style.display == 'none') {
  alink.style['font-weight'] = 'normal';
  elink.style['font-weight'] = 'bold';
  ilink.style['font-weight'] = 'normal';
  jlink.style['font-weight'] = 'normal';
  rlink.style['font-weight'] = 'normal';
  a.style.display = 'none';
  i.style.display = 'none';
  t.style.display = 'none';
  e.style.display = 'block';
  editformJournalSelect(ev);
 } else {
  alink.style['font-weight'] = 'normal';
  elink.style['font-weight'] = 'normal';
  ilink.style['font-weight'] = 'normal';
  a.style.display = 'none';
  e.style.display = 'none';
  i.style.display = 'none';
  t.style.display = 'block';
 }
 return false;
}

function editformJournalSelect(ev) {
  // http://www.quirksmode.org/js/events_properties.html
 if (!ev) var e = window.event;
 if (ev.target) targ = ev.target;
 else if (ev.srcElement) targ = ev.srcElement;
 if (targ.nodeType == 3) targ = targ.parentNode;

 var textareas = $('textarea', $('form#editform'));
 for (i=0; i<textareas.length; i++) {
   textareas[i].style.display = 'none';
   textareas[i].disabled = true;
 }
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

function importformToggle(ev) {
 var a = document.getElementById('addform');
 var e = document.getElementById('editform');
 var f = document.getElementById('filterform');
 var i = document.getElementById('importform');
 var t = document.getElementById('transactions');
 var alink = document.getElementById('addformlink');
 var elink = document.getElementById('editformlink');
 var flink = document.getElementById('filterformlink');
 var ilink = document.getElementById('importformlink');
 var jlink = document.getElementById('journallink');
 var rlink = document.getElementById('registerlink');

 if (i.style.display == 'none') {
  alink.style['font-weight'] = 'normal';
  elink.style['font-weight'] = 'normal';
  ilink.style['font-weight'] = 'bold';
  jlink.style['font-weight'] = 'normal';
  rlink.style['font-weight'] = 'normal';
  a.style.display = 'none';
  e.style.display = 'none';
  i.style.display = 'block';
  t.style.display = 'none';
 } else {
  alink.style['font-weight'] = 'normal';
  elink.style['font-weight'] = 'normal';
  ilink.style['font-weight'] = 'normal';
  a.style.display = 'none';
  e.style.display = 'none';
  i.style.display = 'none';
  t.style.display = 'block';
 }
 return false;
}
