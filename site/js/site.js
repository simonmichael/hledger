$(document).ready( function() {
  highlightDocVersion();
});

function highlightDocVersion() {
  $('.versions').each( function() {
    var parts = window.location.pathname.split('/');
    var dir = parts.length > 1 ? parts[parts.length-2] : '';
    var ver = $.isNumeric(dir) ? dir : '0.28';
    $(this).find('a').each( function() {
      if ($(this).html() == ver)
        $(this)
        .removeAttr('href')
        .css('text-decoration', 'none')
        .css('color', 'initial')
        // .css('font-weight','bold');
        .hide()
    });
  });
}
